library(R6)

#' @title Scrapert REDCap R6 Class 
#' 
#' @description
#' methods for interacting with REDCap
#' @param pool \link{pool} scrapert database connection pool
#' @export
RC <- R6Class("RC",
  public = list(
    #' @description New RC class
    #'
    #' @param config \link{Config} R6 object
    #' @return RC class
    initialize = function (config) {
      private$config <- config
      private$key <- rlang::try_fetch({
        if(keyring::keyring_is_locked()) keyring::keyring_unlock(password = readLines("krs"))
        keyring::key_get("Scrapert-apikey")
      }, error = function(cnd) {
        rlang::warn("Unable to REDCap api key", parent = cnd)
        return(NULL)
      })
    },
    #' @description
    #' Do we have enough setting to communicate with REDCap
    #' 
    #' @param throw_if_not_set \code{logical} specifying if should return \code{FALSE} or throw errors
    #'
    #' @return \code{TRUE} if REDCap config set
    hasConf = function (throw_if_not_set = FALSE) {
      if (throw_if_not_set) {
        if (!is.character(private$config$config$api)) stop("REDCap Api URL is not set")
        if (!is.character(private$key)) stop("REDCap API Key is not set")
      }
      return (is.character(private$config$config$api) && is.character(private$key))
    },
    #' @description Set a new REDCap Project API Token
    #'
    #' @param key REDCAp project API token
    setKey = function(key) {
      if(missing(key) || !is.character(key)) stop("key must contain a character string")
      private$key <- key
      if(keyring::keyring_is_locked()) keyring::keyring_unlock(password = readLines("krs"))
      keyring::key_set_with_value(service = "Scrapert-apikey", password = key)
    },
    #' @description 
    #' check if REDCAp config is present (redirects otherwise to \code{RCConfig.html}).  if session cookie is set,
    #' check to see if still authenticated every two minutes.  If not authenticated, will redirect to REDCap to reauthenicate
    #' via the active linke
    #'
    #' @param req Plumber req object
    #' @param res Plumber res object
    #'
    #' @return \code{NULL} if authenticated, other wise redirect to \code{RCConfig.html} or REDCap
    auth = function(req, res) {
      if (!self$hasConf()) {
        logger::log_warn("REDCap api or apikey not set")
        res$status <- 303
        res$setHeader("Location", "RCConfig.html")
        return (list(redirect = TRUE))
      }
      # Did we already log in --> follow up with RC?
      if(!is.null(req$session) && is.character(req$session$username)) {
        # Do we need to recheck our auth
        if (req$session$expires < lubridate::now()) {
          # check in
          result <- private$authToken(req$session$token)
          if (is.character(result$username)) {
            # Check again in 2 mins
            req$session$expires <- lubridate::now() + lubridate::minutes(2)
            return(NULL)
          } else logger::log_warn("RC time out.")
        }
        else return(NULL)
      }
      # Build the correct url based on version
      v <- private$POST(list(content = "version"), as="text")
      if(!is.integer(private$config$config$project_id)) {
        pid <- private$POST(list(content = "project", format = "json", returnFormat = "json"))$project_id
        private$config$config$project_id <- as.integer(pid)
        private$config$saveToFile("config.yml")
      }
      url <- paste(stringr::str_extract(private$config$config$api, ".*(?=(/api/$))"),
                   paste0("redcap_v", v),
                   paste0("index.php?pid=", private$config$config$project_id),
                   sep = "/")
      res$status <- 303
      res$setHeader("Location", url)
    },
    #' @description
    #' Process the REDCap active link.
    #' @details
    #' The active link should post it's response to \code{http://localhost:port/authep}.  No other variables are required.
    #' This will also set RCApi if it's posted as an request arg
    #' 
    #' @param req Plumber req object
    #' @param res Plumber res object
    #'
    #' @return  Will redirect if  api is posted as an arg after saving the config
    checkAuth = function(req, res) {
      # Are we trying to set the REDCap API URL and key?
      if (is.character(req$args$api)) {
        private$config$config$api <- req$args$api
        private$config$saveToFile("config.yml")
        self$setKey(req$args$apikey)
        res$status <- 303 # temp redirect
        res$setHeader("Location", ".") #try again
        return()
      }
      # work around to get the data from the active link
      authkey <- rawToChar(req$body$authkey$value)
      status <- private$authToken(authkey)
      private$config$config$project_id <- status$project_id
      private$config$saveToFile("config.yml")
      req$session$username = status$username
      req$session$expires = lubridate::now() + lubridate::minutes(2)
      req$session$token <- authkey
      fullname <- self$getUser(status$username)
      if (!is.null(fullname)) req$session$fullname <- fullname
      res$status <- 303 # otherwise 307 just forwards a post!
      res$setHeader("Location", ".")
      return(res)
    },
    #' @description If able to get full name from REDCap will set session cookie along with username, 
    #' othersiwse just sets the username.
    #' 
    #' Called by self$checkAuth and when settings are uploaded (in case the key/url weren't set)
    #'
    #' @param uname REDCap username
    #' 
    #' @return \code{fullname} if success, \code{NULL} otherwise
    getUser = function(uname) {
      # We are authenticated and can access the API so try and get the user
      private$POST(list (content = "user",  format = "csv",  returnFormat = "json"), show_col_types = FALSE) %>% 
        dplyr::filter(username == uname) %>% 
        dplyr::mutate(fullname = paste(firstname, lastname)) %>% dplyr::pull(fullname)
    },
    #' @description
    #' get auth filter exclusions
    #'
    #' @return a regex used to match auth filter exclusions
    authfilter = function() {
      "(^/authep)|(^/RCConfig.html)|(^/css.*)|(^/js)"
    },
    #' @description Lookup PIDs in REDCap
    #'
    #' @param sampleid vector of length one or more of sample ids for which pids/mrns are needed
    #'
    #' @return tibble of columns sample_id and pid
    getPID = function(sampleid) {
      data <- list(
        content = "record",
        format = "csv",
        type = "flat",
        fields = "sample_id,pid",
        returnFormat = "json",
        filterLogic = paste(paste0("[sample_id]='", sampleid, "'"), collapse = " OR ")
      )
      piddb <- private$POST(data, show_col_types = FALSE, col_types = "cc")
      pids <- dplyr::tibble(sample_id = sampleid)
      pids <- pids %>% dplyr::left_join(piddb, by="sample_id")
      pids
    },
    #' @description
    #'  Upload CRFs
    #'
    #' @param sn A vector (length > 0) cartridge_sn in the sqlite database to be uploaded
    #'
    #' @return character vector of cartridge_sn uploaded
    updateCRF = function(sn, pool) {
      # Build data frames
      df <- dplyr::tbl(pool, "xpert_results") %>% dplyr::filter(cartridge_sn %in% local(sn) & !is.na(pid)) %>% 
        dplyr::select(id, sample_id, cartridge_sn, test_result, end_time, processed_by, pid) %>% dplyr::collect()
      if (nrow(df) == 0) {
        stop (sprintf("pid is missing for %s", sn))
      };
      df <- df %>% # Get the record numbers
        private$getRecordID() %>% dplyr::filter(!is.na(record_id))
      # upload xpert results
      data <- list(
        content = "record",
        format = "csv",
        type = "flat",
        overWriteBehavior = "normal",
        forceAutoNumber = "false",
        returnContent = "count",
        returnFormat = "json",
        data = readr::format_csv(df %>% dplyr::select(xpert_result = test_result, xpert_timestamp = end_time, record_id, 
                                               cartridge_sn, xpert_processed_by = processed_by, -sample_id, -id) %>%
                                   dplyr::mutate(xpert_result_complete = 2))
      )
      private$POST(data)
      # Now for the files
      apply(df, MARGIN = 1, FUN = function(x) {
        x <- as.list(x)
        pdf <- dplyr::tbl(pool, "xpert_results") %>% dplyr::filter(id == local(x$id)) %>% dplyr::select(pdf) %>% dplyr::collect() %>%
          dplyr::pull(pdf)
        if (!is.na(pdf)) {
          temp <- file(paste0(tempdir(), "/", x$sample_id, ".pdf"), "wb")
          base64enc::base64decode(pdf, output = temp)
          flush(temp)
          temppath <- summary(temp)$description
          data = list(
            action = "import",
            content = "file",
            record = x$record_id,
            field = "xpert_data",
            event= "",
            returnFormat = "json",
            file = httr::upload_file(temppath)
          ) 
          private$POST(data, encode = "multipart")
          close(temp)
          file.remove(temppath)
          # remove from the database
          if(pool %>% pool::dbExecute(paste("UPDATE xpert_results SET pdf = NULL WHERE id =", x$id)) != 1) 
            stop("Error removing pdf from local database")
        } else logger::log_warn("{ x$cartridge_sn } pdf missing from sqlite3, perhpas it's uploaded already")
        pdf <- NULL
      })
      pool %>% pool::dbExecute("VACUUM")
      df$cartridge_sn
    },
    #' @description REDCap Delete CRF
    #'
    #' @param sn the cartridge_sn in xpertdb
    #'
    #' @return \code{NULL} if successful
    CRFDelete = function(sn, pool) {
      df <- dplyr::tbl(pool, "xpert_results") %>% dplyr::filter(cartridge_sn %in% local(sn)) %>% 
        dplyr::select(pid) %>% dplyr::collect()
      if (is.na(df$pid)) {
        logger::log_info("No PID set for sn {sn}, unable to delete from REDCap")
        return()
      }
      # get the record numbers
      df <- df %>% private$getRecordID()
      # delete the files first
      data <- list(
        action = "delete",
        content = "file",
        record = df$record_id,
        field = "xpert_data",
        event= "",
        returnFormat = "json"
      )
      private$POST(data)
      # We don't delete just remove values
      data <- list(
        content = "record",
        format = "csv",
        type = "flat",
        overwriteBehavior = "overwrite",
        forceAutoNumber = "false",
        returnContent = "count",
        returnFormat = "json",
        data = readr::format_csv(df %>% dplyr::select(record_id, -pid) %>%
                                   dplyr::mutate(xpert_result_complete = "", xpert_result = "", xpert_timestamp = "",
                                          cartridge_sn = "", xpert_processed_by = ""))
      )
      count <- as.integer(private$POST(data)$count)
      if (count != length(sn)) stop(paste("REDCap only deleted", count, "records but deletion ", length(sn), "records was requested"))
      return()
    },
    #' @description
    #' Get PDF from REDCap
    #' 
    #' @param pid 
    #'
    #' @return raw data (pdf)
    getPDF = function(pid) {
      df <- private$getRecordID(tibble(pid = pid))
      data <- list(
        content = "file",
        action = "export",
        record = df$record_id,
        field = "xpert_data",
        event = "",
        returnFormat = "json"
      )
      private$POST(data, as = "raw")
    },
    #' @description
    #' backup xpertdb and config.yml to REDCap file repository
    #' 
    #' @return filename of .zip file in REDCap
    backup = function() {
      # Get the old file
      data = list(
        content = "fileRepository",
        action = "list",
        format = "json",
        returnFormat = "json"
      )
      result <- private$POST(data)
      file <- purrr::keep(result, \(x) {
        x$name == "Scrapert_Backup.zip"
      })
      # Delete the file if exists
      if(length(file) == 1) {
        data$action <- "delete"
        data$doc_id <- file[[1]]$doc_id
        private$POST(data)
        data$doc_id <- NULL
      }
      temppath <- paste0(tempdir(), "/", "Scrapert_Backup.zip")
      if (file.exists(temppath)) file.remove(temppath)
      result <- zip(temppath, c("config.yml", "xpertdb"))
      data$action <- "import"
      data$file <- httr::upload_file(temppath, type = "application/x-zip")
      private$POST(data, encode = "multipart")
      # cleanup
      file.remove(temppath)
      return (list(fileinfo = "Scrapert_Backup.zip"))
    },
    #' @description
    #' restore backup from REDCap
    #' 
    restore = function() {
      data = list(
        content = "fileRepository",
        action = "list",
        format = "json",
        returnFormat = "json"
      )
      result <- private$POST(data)
      file = purrr::keep(result, \(x) {
        x$name == "Scrapert_Backup.zip"
      })
      data$action <- "export"
      data$doc_id <- file[[1]]$doc_id
      tempfile = tempfile(fileext = ".zip")
      response <- httr::POST(url = private$config$config$api, body = purrr::list_assign(data, token = private$key), 
                    httr::write_disk(tempfile))
      if (response$status_code != 200) {
        stop(sprintf("RC Download Backup File Error: %s", httr::content(response)$error))
      }
      unzip(tempfile)
      file.remove(tempfile)
      private$config$saveToFile("config.yml")
      return()
    }
  ),
  private = list(
    key = NULL,
    config = NULL,
    POST = function(data, encode = "form", ...) {
      data <- purrr::list_assign(data, token = private$key)
      # Build the correct url based on version
      response <- httr::POST(url = private$config$config$api, body = purrr::list_assign(data, token = private$key), 
                             encode = encode)
      result <- httr::content(response, ...)
      if (response$status_code != 200) {
        rlang::abort("RC POST Error: { result$error }")
      }
      result
    },
    authToken = function(token) {
      postData <- list(authkey = token,
                       format = "json")
      result <- httr::POST(private$config$config$api, body = postData, encode = "form")
      status <- NULL
      if (result$status == 200L) {
        # REDCap actually returns HTML so we have to specify type here
        status <- httr::content(result, type="application/json")
        # The following means we had an auth code but now the session is redcap is over (it returned a 0)
        if (!is.list(status)) return(list())  
      } else if (result$status == 403L) {
        # return an empty list if the auth code isn't valid (anymore) and we should reauthenticate
        return(list())
      } else {
        # this is a legit error
        rawtext <- httr::content(result)
        stop(sprintf("There was an error verifying authkey. Status %i. Message: %s", result$status, rawtext))
      }
      list(project_id = status$project_id, username = status$username)
    },
    # REDCap Map sample id to record id
    # df - data frame with at least sample_id
    # return df with record_id column appended
    getRecordID = function(df) {
      data <- list(
        content = "record",
        format = "csv",
        type = "flat",
        returnFormat = "json",
        fields = "record_id, pid",
        filterLogic = paste(paste0("[pid]='", df$pid, "'"), collapse = " OR ")
      )
      rid <- private$POST(data, col_types="cc")
      df %>% dplyr::left_join(rid, by="pid")
    }
  )
)
