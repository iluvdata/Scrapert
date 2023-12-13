#* @apiTitle Scrapert
#* @apiDescription API to scrape data from Xpert HPV

# load config
config <- Config$new("config.yml")

# change permissions
if (as.integer(file.info("config.yml")$mode & as.octmode("177"))) {
  logger::log_info(sprintf("config.yml permissions were %o, changing to 600", file.info("config.yml")$mod))
  Sys.chmod("config.yml", mode = "0600")
}

# this R6 class communicates with REDCap
rc <- RC$new(config)
# pool for database connections
pool <- pool::dbPool(drv = RSQLite::SQLite(), dbname= "xpertdb")

# Process pdf upload
#* @post /upload
function (req, res) {
  xpert <- lapply(req$body, function(x) {
    if (x$content_type != "application/pdf") stop(paste(x$filename, "is not a valid pdf file."))
    list(pdf = x$value, filename = x$filename)
  })
  processed_by <- req$session$username
  if (is.character(req$session$fullname)) processed_by <- paste0(req$session$fullname, " (", processed_by, ")")
  parsePDF(xpert, processed_by, config, pool)
}

#* Get PID if not sample ID
#*
#* Will use sample ID to look up PID in REDCap
#* @post /pid
#* @param sampleid:[int]
function(sampleid, res, req) {
  # This is platform specific so in a separate file
  result <-
    tryCatch({
      rc$getPID(sampleid)
    }, error = function(e) {
      res$status <- 500
      return(list(msg = "PID Lookup failed", err = e$message))
    })
  # Did we get an error?  result should be a tibble
  if ("data.frame" %in% class(result)) {
    tryCatch({
      # upddate pids
      con = pool::poolCheckout(pool)
      result <- dbplyr::get_returned_rows(tbl(con, "xpert_results") %>%
                                            rows_update(copy_to(con, result, "pids", overwrite =T),
                                                        by = "sample_id", unmatched = "ignore",
                                                        in_place = T, returning=c("sample_id", "pid")))
      pool::poolReturn(con)
    }, error = function(e){
      res$status <- 500
      result <<- list(msg = "Unable to save PID to database", err = e$message)
    })
  }
  return(result)
}

#* Upload to CRF
#*
#* by cartrige serial number
#* @post /crf
#* @param sn:[string] Xpert cartrige serial number
function(sn, res) {
  # This is platform specific so in a separate file
  result <- tryCatch({
    rc$updateCRF(sn, pool)
  }, error = function(e) {
    res$status <- 500
    return(list(msg = "CRF upload failed", err = e$message))
  })
  if(!is.null(result) & class(result) != "list") { #did we get an error or a result
    result <- tryCatch({
      con <- pool::poolCheckout(pool)
      result <- dbplyr::get_returned_rows(tbl(con, "xpert_results") %>%
                                  rows_update(copy_to(con, tibble(cartridge_sn = result) %>%
                                                                    mutate(uploaded = format(lubridate::now(tzone = "GMT"))), 
                                                      "uploads", overwrite=T),
                                              by = "cartridge_sn", unmatched = "ignore",
                                              in_place = T, returning=c("cartridge_sn", "uploaded")))
      pool::poolReturn(con)
      result
    }, error = function(e) {
      res$status <- 500
      return(list(msg = "Unable to save CRF update time to database", err = e$message))
    })
  }
  result
}

#* Delete a sample (and CRF data)
#* @delete /delete
#* @param sn
function(sn, res) {
  tryCatch({
    rc$CRFDelete(sn)
  }, error = function(e) {
    stop(paste0("Unable to delete CRF ", e$message))
  })
  # Delete from the local database 
  count <- DBI::dbExecute(pool, paste0('DELETE FROM xpert_results WHERE cartridge_sn = "', sn, '"'))
  if (count != length(id)) {
    res$status_code <- 500
    return(list(msg="Unable to delete from database",
                err = paste("Deleted", count, "samples but expected to delete", length(id), "samples")))
  }
  list()
}


#* Return xpert pdf
#* @get /pdf
#* @param sn
function(sn, dl, res) {
  df <- tbl(pool, "xpert_results") %>% filter(cartridge_sn == local(sn)) %>% select(pid, sample_id, pdf) %>% collect()
  if(is.na(df$pdf) & is.na(df$pid)) {
    res$status <- 404
    res$body <- "PDF not found"
    res$setHeader("Content-Type", "text/plain")
  } else {
    # the pdf might be in REDCap
    if (is.na(df$pdf))  res$body <- rc$getPDF(df$pid)
    else res$body <- base64enc::base64decode(df$pdf)
    if(!missing(dl)) res$setHeader("Content-Disposition", glue::glue("attachment; filename={df$sample_id}.pdf"))
    res$setHeader("Content-Type", "application/pdf")
  }
  res
}

#* Return xpert csv
#* @get /xpert
function(res) {
  csv <- NULL
  tryCatch({
    csv <- tbl(pool, "xpert_results") %>% select(!c(pdf, id, raw_text)) %>% collect() %>%
      modifiedXpert(config)
    names(csv$pid) <- config$config$pidName
    if(is.data.frame(csv)) {
      res$body <- readr::format_csv(csv, na="")
      res$setHeader("Content-Dispostion", "attachment; filename=xpert.csv")
      res$setHeader("Content-Type", "text/csv")
    }
  }, error = function(e) { 
    res$status <- 404
    res$setHeader("Content-Type", "text/plain")
    res$body <- paste0("Database maybe empty. Hit back to upload results. Error: ", e$message)
  })
  res
}

#* Search by Sample ID or PID
#* @get /search
function(q, res) {
  if (nchar(q) < 2) return("")
  df <- NULL
  tryCatch({
    df <- tbl(pool, "xpert_results") %>% select(!c(pdf, raw_text)) %>%
      filter(sample_id %like% local(paste0("%", q, "%")) | pid %like%  local(paste0("%", q, "%"))) %>% collect()
  }, error = function(e) {
    res$status <- 500
    df <<- list(list(msg = "Search error, database is empty", err = e$message))
  })
  return(df)
}

#* Get config
#* @get /config
function() {
  return (append(config$getWebConfig(), list(debug = interactive(), server = server)));
}

#* Update settings
#* @post /config
function(req) {
  args <- req$args
  config$saveWebConfig(args)
  return()
}

#* backup db and settings to REDCap
#* @post /backup
function() {
  rc$backup()
}

#* restore config.yml and xpertdb from REDcap
#* @post /restore
function() {
  rc$restore()
}

#* auth endpoint
#* @post /authep
function(req, res) {
  rc$checkAuth(req, res)
}

# Auth Filter
# This will only be called if the auth function exists (is linked by source).  See README
authFilter <- function(req, res) {
  # reset the timeout if we haven't connected a websocket (in which case sto is NULL)
  if(is.function(sto)) {
    sto()
    sto <<- tofuns()
  }
  # are we trying to access the auth end point?
  if (!grepl(rc$authfilter(), req$PATH_INFO)) {
    rc$auth(req, res)
    # Did auth tell us to go somewhere or directly return an error code?
    if (res$status != 200) return()
  }
  forward()
}

# the following is for the websocket tracking
clients <- list()

# are we running in server mode?  Don't shut down
server <- getOption("scrapertserver")

# Start a timer, basically if this isn't cancelled by a websocket connection withing 5 mins shut down (no zombie processes) 
tofuns <- function() {
  later::later(function() {
    logger::log_info("Scrapert is timing out (initial timeout)")
    if (!interactive()) exitPlumber()
  }, 300)
}
sto <- {
  if(!server) tofuns()
  else NULL
}

exitPlumber <- function() {
  if (is.function(sto)) sto()
  clients <- NULL
  pool::poolClose(pool)
  logger::log_info("Scrapert out!")
  if(!interactive()) {
    file.remove("plumber.lock")
    q("no")
  }
}

# Scrapert entry point
#* @plumber
function(pr) {
  options_plumber(
    port = 5500
  )
  # Check for running instances
  # is this already running locally? and not in Rstudio
  if (!interactive()) {
    if (file.exists("plumber.lock")) {
      pid <- readLines("plumber.lock")
      # kill the old process
      logger::log_info("Scrapert already running, attempting to stop prior process: \n")
      system(paste("kill -3", pid))
    }
    # New lock file
    write(Sys.getpid(), file = "plumber.lock")
  }
  key <-
    rlang::try_fetch({
      keyring::key_get("plumber_api")
    }, error = function(cnd) {
      rlang::warn("Unable to get key plumber_api", parent = cnd)
      keyring::key_set_with_value(service = "plumber_api", password =  plumber::random_cookie_key())
      return (keyring::key_get("plumber_api"))
    })
  pr <- pr %>% pr_cookie(key, name="plumber", path="/", expiration = config$config$timeout) %>%
    pr_filter("auth", authFilter) 
  # add logout ep for testing
  if (interactive()) pr <- pr %>% pr_get("/test", function(action, req, res) {
      if (action == "logout") {
        req$session <- list()
        res$status <- 303
        res$setHeader("Location", ".")
      } else if (action == "deletedb") {
        pool::dbExecute(pool, "DROP TABLE IF EXISTS xpert_results")
        pool::dbExecute(pool, "DROP TABLE IF EXISTS version")
        pool::dbExecute(pool, "VACUUM")
        return (list())
      }
    })
  if (!server) {
  # Add websocket support to monitor connections
    pr$websocket(
      function(ws) {
        ws$request$uuid <- uuid::UUIDgenerate()
        # Cancel the timeout
        if (is.function(sto)) {
          sto()
          sto <<- NULL
        }
        clients[[ws$request$uuid]] <<- ws
        ws$onMessage(function(binary, message){
          if (message == "shutdown") {
            logger::log_info ("Server shutdown requested\n")
            if (!interactive()) exitPlumber()
          }
        })
        ws$onClose(function() {
          clients[[ws$request$uuid]] <<- NULL
          logger::log_info("Removed ", ws$request$uuid, " ", length(clients), " client(s) connected\n")
          # use a promise so this thread keeps running but only if all the clients are gone
          if (is.list(clients) & length(clients) == 0) {
            logger::log_info("starting shutdown timeout\n")
            later::later(function(value) {
                # shut this down if not interactive and the client list is still empty in ~ 30s
                if (length(clients) == 0) {
                  logger::log_info ("shutting down\n")
                  if(!interactive()) exitPlumber()
                } else logger::log_info("aborted shutdown ", length(clients), " client(s) is connected\n")
              }, 30)
          } # end if
        })
      }
    )
  } # end websocket setup
  pr %>% pr_static("/", system.file("/www/", package="Scrapert")) %>%
    pr_set_docs(TRUE) %>% pr_set_serializer(serializer_unboxed_json())  %>%
    pr_hook("exit", exitPlumber) %>%
    pr_set_docs_callback(function(url) {
       # Hijack this function to open a browswer now the api is ready
       if(!interactive() && !server)  browseURL(paste0("http://localhost:", config$config$server_port))
       return (NULL)
    })
}


