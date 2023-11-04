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
  needsKey <- tryCatch({
    keyring::key_get("Scrapert-RCAuthApiKey")
    FALSE
  }, error = function (e) {
    TRUE
  })
  if (needsKey) config$config$RCAuthApiKey$needed = TRUE
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
  if(!is.null(req$session)) {
    if(!is.null(req$session$plumber$username)) return(NULL)
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
    config$config$RCAuthApiKey$needed <- TRUE
    # Display a page stating we need the key value set
    # This cookie will let the browser know we need a setting
    res$setHeader("Content-Type", "text/html")
    rcurl <- paste0(stringr::str_extract(config$config$RCAuthApi$value, ".*(?=(/api/$))"), "/index.php?action=myprojects")
    return(list(err = paste("<head><title>Not Authenticated</title><body><h1>Xpert Import Tool</h1><hr>",
                            "The REDCap Auth API Key is not set. You must:<ol>", 
                            "<li>Access this app from the REDCap Auth Project Active Link.</li>",
                            "<li>Enter the REDCap Auth API Key on the \"Settings\" tab.</li></ol>",
                            "<p>REDCap URL: <a href='", rcurl, "'>", rcurl, "</a></p></body></html>")))
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
  res$status <- 307
  res$setHeader("Location", paste(stringr::str_extract(config$config$RCAuthApi$value, ".*(?=(/api/$))"),
                          paste0("redcap_v", v),
                          paste0("index.php?pid=", config$config$RCAuthPID$value),
                          sep = "/"))
  return(list(redirect = TRUE))
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
#' \code{res$status <- 401} (not authenticated)
#' @export
RCAuthProcess = function(req, res, config) {
  # Are we trying to set the REDCap API URL?
  if (!is.null(req$args$RCAuthAPI)) {
    config$config$RCAuthApi$value <- req$args$RCAuthAPI
    config$saveToFile("config.yml")
    res$status <- 303 # temp redirect
    res$setHeader("Location", "/") #try again
    return(NULL)
  }
  # work around to get the data from the active link
  authkey <- rawToChar(req$body$authkey$value)
  postData <- list(authkey = authkey,
                   format = "json")
  result <- httr::POST(config$config$RCAuthApi$value, body = postData, encode = "form")
  status <- list()
  if (result$status == 200L) {
    # REDCap actually returns HTML so we have to specify type here
    status <- httr::content(result, type="application/json")
  } else {
    rawtext <- httr::content(result)
    res$status <- 401
    res$setHeader("Content-Type", "text/plain")
    return(sprintf("There was an error verifying authkey. Status %i. Message: %s", req$status, rawtext))
  }
  config$config$RCAuthPID$value <- status$project_id
  config$saveToFile("config.yml")
  RCsetUser(status$username, config, req)
  res$status <- 303
  res$setHeader("Location", "/")
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
RCsetUser <- function(uname, config, req) {
  key <- tryCatch({
    keyring::key_get("Scrapert-RCAuthApiKey")
  }, error = function (e) {
    return(NULL)
  })
  fullname <- if (!is.null(key)) {
    # We are authenticated and can access the API so try and get the user
    response <- httr::POST(config$config$RCAuthApi$value,  
                           body = list (token = key, content = "user",  format = "csv",  returnFormat = "json"), 
                           encode = "form")
    if (response$status_code != 200) warning(paste("Unable to get user full name", httr::content(response)))
    fullname <- httr::content(response, show_col_types = FALSE) %>% filter(username == uname) %>% 
      mutate(fullname = paste(firstname, lastname)) %>% pull(fullname)
  } else {
    warning("Unable to get user full name as REDCap Auth API key was not set")
    NULL
  } 
  req$session$plumber <- if (!is.null(fullname)) list(username = uname, fullname = fullname)
    else list(username = uname)
}