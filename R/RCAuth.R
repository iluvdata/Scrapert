#' REDCap Authentication
#'
#' @description
#' Performs authentication to REDCap
#' @param config \link{Config} object
#' @return a list of functions for plumber to reference
#' @export
RCAuth <- function(config) {
  config$addConfig(
    RCAuthApi = list(label = "REDCap Auth API URL", type="url", required=TRUE),
    RCAuthPID = list(),
    RCAuthApiKey = list(label = "REDCap Auth Project API Key", type="password")
  )
  return(list(auth = doRCAuth, checkAuth = RCAuthProcess, setUser = RCsetUser, filterex = "(^/authep)|(^/RCConfig.html)|(^/css.*)|(^/js)"))
}

#' Auth function for RECCap
#' 
#' @description
#' This function checks to see if the current session is authenticated, and if not, redirect them to REDCap to log in.  This can serve as a template for other authenication mechanisms
#' @param req Plumber req object
#' @param res Plumber res object
#' @param config \link{Config} object
#'
#' @return NULL if authenticated, a string URL to the REDCap project providing the authentication active link (see \link{RCAuthProcess}) 
#' for more information about the format of the REDCap active link).  Otherwise will return \code{list(err = "error message here")}
#' @export
doRCAuth <- function(req, res, config) {
  if(!is.null(req$session$plumber$username)) {
    # Do we need to recheck our auth
    if (req$session$plumber$expires < lubridate::now()) {
      result <- RCAuthToken(req$session$plumber$token, config)
      if (!is.null(result$username)) return(NULL)
      # is this an ajax request?
      if (is.character(req$HTTP_X_REQUESTED_WITH))
        if (tolower(req$HTTP_X_REQUESTED_WITH) == "xmlhttprequest") res$status <- 401L # not authenticated
    }
    else return(NULL)
  }
  if (is.null(config$config$RCAuthApi$value)) {
    warning("RCAuthApi not set in config")
    res$status <- 307
    res$setHeader("Location", "RCConfig.html")
    return (list(redirect = TRUE))
  }
  key <- tryCatch({
    keyring::key_get("Scrapert-RCAuthApiKey")
  }, error = function (e) {
    return(NULL)
  })
  if (is.null(key)) {
    warning("RCAuthApiKey not set in keyring")
    res$status <- 307
    res$setHeader("Location", "RCConfig.html")
    return (list(redirect = TRUE))
  }
  # Build the correct url based on version
  response <- httr::POST(config$config$RCAuthApi$value,
                         body = list(
                           token = key,
                           content = "version",
                           format = "json"),
                         encode = "form")
  if (response$status_code != 200) stop(paste("Unable to get REDCap project url",
                                              httr::content(response)))
  v <- httr::content(response, as="text")
  if(is.null(config$config$RCAuthPID$value)) {
    response <- httr::POST(config$config$RCAuthApi$value,
                           body = list(
                             token = key,
                             content = "project",
                             format = "json",
                             returnFormat = "json"),
                           encode = "form")
    if (response$status_code != 200) stop(paste("Unable to get REDCap project url",
                                                httr::content(response)))
    pid <- httr::content(response)$project_id
    config$config$RCAuthPID$value <- as.integer(httr::content(response))
    config$saveToFile("config.yml")
  }
  url <- paste(stringr::str_extract(config$config$RCAuthApi$value, ".*(?=(/api/$))"),
                paste0("redcap_v", v),
                paste0("index.php?pid=", config$config$RCAuthPID$value),
                sep = "/")
  # if res$status == 401 then this is an ajax request
  if (res$status == 401) return (list(url = url))
  res$status <- 307
  res$setHeader("Location", url)
}

#' Process REDCap Responses
#' 
#' @description
#' Process the REDCap active link.
#' @details
#' The active link should post it's response to \code{http://localhost:port/authep}.  No other variables are required.
#' This will also set RCApi if it's posted as an request arg
#' 
#' @param req Plumber req object
#' @param res Plumber res object
#' @param config \link{Config} object
#'
#' @return \code{list(username = username)} if successful otherwise \code{list(err = "error message")} and/or sending
#' \code{res$status <- 401} (not authenticated). Will redirect if RCAuthAPI is posted as an arg after saving the config
#' @export
RCAuthProcess = function(req, res, config) {
  # Are we trying to set the REDCap API URL and key?
  if (!is.null(req$args$RCAuthAPI)) {
    config$config$RCAuthApi$value <- req$args$RCAuthAPI
    config$saveToFile("config.yml")
    keyring::key_set_with_value(service = "Scrapert-RCAuthApiKey", password = req$args$RCAuthAPIKey)
    res$status <- 303 # temp redirect
    res$setHeader("Location", "/") #try again
    return(list())
  }
  # work around to get the data from the active link
  authkey <- rawToChar(req$body$authkey$value)
  status <- RCAuthToken(authkey, config)
  config$config$RCAuthPID$value <- status$project_id
  config$saveToFile("config.yml")
  if(is.null(req$session$plumber)) req$session$plumber <- list()
  req$session$plumber$username = status$username
  req$session$plumber$expires = lubridate::now() + lubridate::minutes(5)
  req$session$plumbertoken <- authkey
  fullname <- RCsetUser(status$username, config, req)
  if (!is.null(fullname)) req$session$plumber$fullame <- fullname
  res$status <- 303
  res$setHeader("Location", "/")
}

RCAuthToken <- function(token, config) {
  postData <- list(authkey = token,
                   format = "json")
  result <- httr::POST(config$config$RCAuthApi$value, body = postData, encode = "form")
  status <- NULL
  if (result$status == 200L) {
    # REDCap actually returns HTML so we have to specify type here
    status <- httr::content(result, type="application/json")
  } else if (result$status == 403L) {
    # return an empty list
    return(list())
  } else {
    # this is a legit error
    rawtext <- httr::content(result)
    stop(sprintf("There was an error verifying authkey. Status %i. Message: %s", result$status, rawtext))
  }
  list(project_id = status$project_id, username = status$username)
}

#' REDCap Util:  set user
#'
#' If able to get full name from REDCap will set session cookie along with username, 
#' othersiwse just sets the username.
#' 
#' Called by \link{RCAuthProcess} and when settings are uploaded (in case the key/url were set)
#'
#' @param uname REDCap username
#' @param config \link{Config} object
#' @param req Plumber \code{req} object
#' @return \code{fullname} if success, \code{NULL} otherwise
RCsetUser <- function(uname, config, req) {
  key <- tryCatch({
    keyring::key_get("Scrapert-RCAuthApiKey")
  }, error = function (e) {
    return(NULL)
  })
  if (!is.null(key)) {
    # We are authenticated and can access the API so try and get the user
    response <- httr::POST(config$config$RCAuthApi$value,  
                           body = list (token = key, content = "user",  format = "csv",  returnFormat = "json"), 
                           encode = "form")
    if (response$status_code != 200) warning(paste("Unable to get user full name", httr::content(response)))
    httr::content(response, show_col_types = FALSE) %>% filter(username == uname) %>% 
      mutate(fullname = paste(firstname, lastname)) %>% pull(fullname)
  } else {
    warning("Unable to get user full name as REDCap Auth API key was not set")
    NULL
  } 
}