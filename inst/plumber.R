#* @apiTitle Scrapert
#* @apiDescription API to scrape data from Xpert HPV

library(dplyr, warn.conflicts = F)
library(tidyr)

# load config
config <- Config$new("config.yml")
  # change permissions
if (as.integer(file.info("config.yml")$mode & as.octmode("177"))) {
  message(sprintf("config.yml permissions were %o, changing to 600", file.info("config.yml")$mod))
  Sys.chmod("config.yml", mode = "0600")
}

config$addConfig(
    xpertTZ = list(label = "Time Zone of Xpert Machine", opts = OlsonNames, required = TRUE)
)
auth <- if (!is.null(config$modules$auth))  do.call(config$modules$auth, list(config = config))
pid <- if (!is.null(config$modules$pid)) do.call(config$modules$pid, list(config = config))
crf <- if (!is.null(config$modules$crf)) do.call(config$modules$crf, list(config = config))
# 
# Process pdf upload
#* @post /upload
function (req, res) {
  xpert <- lapply(req$body, function(x) {
    if (x$content_type != "application/pdf") stop(paste(x$filename, "is not a valid pdf file."))
    list(pdf = x$value, filename = x$filename, raw_text = pdftools::pdf_text(x$value))
  })
  # Split into reports (remove signature page)
  xpert <- lapply(xpert , function(x) {
    # which pages have data
    pages <- x$raw_text %>% stringr::str_detect("Sample ID")
    # are there no valid pages?f
    if (!any(pages)) stop(glue::glue("{x$filename} is not a valid Xpert HPV output file."))
    x$filename <- NULL
    # subset pages and get pdfs
    x$raw_text <- x$raw_text[pages]
    pdfpath <- tempfile(fileext = ".pdf")
    pdf <- file(pdfpath, "wb")
    writeBin(x$pdf, pdf)
    close(pdf)
    # Split into pages and return paths
    x$pdf <- pdftools::pdf_split(pdfpath)
    # Delete unwanted files
    file.remove(x$pdf[!pages])
    x$pdf <- x$pdf[pages]
    # compress remaining pages as pdf_split isn't efficient
    x$pdf <- sapply(x$pdf, function(x) {
      pdfpath <- tempfile(fileext = ".pdf")
      pdftools::pdf_compress(x, pdfpath)
      file.remove(x)
      pdfpath
    })
    x
  })
  xpert <- bind_rows(xpert)
  # Get the tabular results
  tbl <- stringr::str_replace_all(
    stringr::str_extract(xpert$raw_text, "SAC[[:graph:]\\s\\n]+(?=\\n{3}User:)"),
    "(?<=HPV)\\s(?=\\d{2})", "_") %>%
    lapply(readr::read_table, col_names = c("analyte", "ct", "end_point", "result", "probe_check")) %>%
    lapply(select, c("analyte", "ct", "result"))
  df <- tibble(
    sample_id = keyed_value("Sample ID\\*?", xpert$raw_text),
    test_result =
      stringr::str_replace_all(
        stringr::str_extract(xpert$raw_text, "(?<=Test\\sResult:\\s{1,30})HPV[[:graph:]\\s\\n]+(?=\\n\\-\\nAnalyte)"),
        "\\n\\s{2,}", " "),
    status = keyed_value("Status", xpert$raw_text),
    error = keyed_value("Error Status", xpert$raw_text),
    error_message = stringr::str_extract(xpert$raw_text, "(?<=Errors\\n)[:graph:]+"),
    start_time = keyed_ts("Start Time", xpert$raw_text, config$config$xpertTZ$value),
    end_time = keyed_ts("End Time", xpert$raw_text, config$config$xpertTZ$value),
    instrument_sn = keyed_value("Instrument S/N", xpert$raw_text),
    cartridge_sn = keyed_value("Cartridge S/N\\*?", xpert$raw_text),
    reagant_lot = keyed_value("Reagent Lot ID\\*?", xpert$raw_text),
    notes = keyed_value("Notes", xpert$raw_text),
    raw_text = xpert$raw_text,
    pdf = xpert$pdf,
    results = tbl) %>%
    unnest(cols = results) %>%
    pivot_wider(
      names_from = analyte,
      values_from = c(ct, result),
      names_glue = "{analyte}_{.value}"
    ) %>%
    relocate(starts_with(c("SAC", "HPV_16", "HPV_18", "P3", "P4", "P5")), .after="error")
  rm(tbl, xpert)
  # Write to DB
  con <- DBI::dbConnect(RSQLite::SQLite(), "xpertdb")
  if (!DBI::dbExistsTable(con, "xpert_results")) {
    # Create the table
    sql <- paste0("CREATE TABLE xpert_results (id INTEGER PRIMARY KEY ASC, sample_id TEXT,  pid TEXT, test_result TEXT, ",
                  "status TEXT,  error TEXT,  uploaded TEXT,",
                  "SAC_ct REAL,  SAC_result TEXT,  HPV_16_ct REAL,  HPV_16_result TEXT,  HPV_18_45_ct REAL,  ",
                  "HPV_18_45_result TEXT,  P3_ct REAL,  P3_result TEXT,  P4_ct REAL,  P4_result TEXT,  P5_ct REAL,",
                  "P5_result TEXT,  error_message TEXT,  start_time TEXT,  end_time TEXT,  instrument_sn TEXT,  ",
                  "cartridge_sn TEXT,  reagant_lot TEXT, processed_by TEXT, notes TEXT, raw_text TEXT, pdf BLOB)")
    rs <- DBI::dbSendStatement(con, sql)
    DBI::dbClearResult(rs)
    rs <- DBI::dbSendStatement(con, "CREATE UNIQUE INDEX samp_run ON xpert_results (sample_id, cartridge_sn)")
    DBI::dbClearResult(rs)
    # add the version
    version <- tibble(version = as.character(packageVersion("Scrapert")))
    copy_to(con, version, temporary = FALSE)
  }

  # Add in user data if exists
  if(!is.null(req$session$plumber$username)) {
    if(!is.null(req$session$plumber$fullname)) {
      df$processed_by <- paste0(req$session$plumber$fullname, " (", req$session$plumber$username, ")")
    } else df$processed_by <- req$session$plumber$username
  } else {
    # local user
    df$processed_by <- paste0(Sys.info()["user"], " on ", Sys.info()["nodename"])
  }

  # Read in the pdfs using the tempfile paths and base64enc since sqlite doesn't do binary well.
  df <- df %>% mutate(pdfbin = sapply(df$pdf, base64enc::base64encode, USE.NAMES = F))
  file.remove(df$pdf)
  df <- df %>% select (!pdf, pdf = pdfbin) %>%
    # fix dates
    mutate(start_time = as.character(start_time),
           end_time = as.character(end_time))

  msg <- list()
  # appended to the database
  tryCatch({
    DBI::dbAppendTable(con, "xpert_results", df)
  }, error = function(e) {
    # Create error message
    msg <<- list(msg = paste("One or more results already exists in the local database.",
                                 "Please verify import status"), err = e$message)
  })

  # Now load with db id's and run xpert results
  df <- tbl(con, "xpert_results") %>%
    select(!c(pdf, raw_text)) %>% filter(cartridge_sn %in% local(df$cartridge_sn)) %>% collect() %>%
    modifiedXpert(config)
  DBI::dbDisconnect(con)
  list(results = df, message = msg)
}

#* Get PID if not sample ID
#*
#* Will use sample ID to look up PID in REDCap
#* @post /pid
#* @param sampleid:[int]
function(sampleid, res) {
  # This is platform specific so in a separate file
  result <-
    tryCatch({
      pid$getPID(sampleid, config)
    }, error = function(e) {
      res$status <- 500
      return(list(msg = "PID Lookup failed", err = e$message))
    })
  # Did we get an error?  result should be a tibble
  if ("data.frame" %in% class(result)) {
    tryCatch({
      con <- DBI::dbConnect(RSQLite::SQLite(), "xpertdb")
      # upddate pids
      result <- dbplyr::get_returned_rows(tbl(con, "xpert_results") %>%
                                            rows_update(dbplyr::copy_inline(con, result), by = "sample_id", unmatched = "ignore",
                                                        in_place = T, returning=c("sample_id", "pid")))
      DBI::dbDisconnect(con)
    }, error = function(e){
      res$status <- 500
      result <<- list(msg = "Unable to save PID to database", err = e$message)
    })
  }
  return(result)
}

#* Upload to CRF
#*
#* by row ID
#* @post /crf
#* @param id:[int]
function(id, res) {
  # This is platform specific so in a separate file
  result <- tryCatch({
    crf$updateCRF(id, config)
  }, error = function(e) {
    res$status <- 500
    return(list(msg = "CRF upload failed", err = e$message))
  })
  if(class(result) != "list") { #did we get an error or a result
    result <- tryCatch({
      con <-DBI::dbConnect(RSQLite::SQLite(), "xpertdb")
      result <- dbplyr::get_returned_rows(tbl(con, "xpert_results") %>%
                                  rows_update(dbplyr::copy_inline(con, tibble(id = result) %>%
                                                                    mutate(uploaded = format(lubridate::now(tzone = "GMT")))),
                                              by = "id", unmatched = "ignore",
                                              in_place = T, returning=c("id", "uploaded")))
      DBI::dbDisconnect(con)
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
#* @param id:int
function(id, res) {
  tryCatch({
    crf$crfDelete(id, config)
  }, error = function(e) {
    res$status <- 500
    return(list(msg="Unable to Delete CRF", err=e$message))
  })
  # Delete from the local database 
  con <- DBI::dbConnect(RSQLite::SQLite(), "xpertdb")
  result <-  DBI::dbSendStatement(con, paste("DELETE FROM xpert_results WHERE id =", id))
  count <- DBI::dbGetRowsAffected(result)
  DBI::dbClearResult(result)
  DBI::dbDisconnect(con)
  if (count != length(id)) {
    res$status_code <- 500
    return(list(msg="Unable to delete from database",
                err = paste("Deleted", count, "samples but expected to delete", length(id), "samples")))
  }
  list()
}

#* Return xpert pdf
#* @get /pdf
#* @param id:int
#* @serializer contentType list(type="application/pdf")
function(id, dl, res) {
  con <- DBI::dbConnect(RSQLite::SQLite(), "xpertdb")
  df <- tbl(con, "xpert_results") %>% filter(id == local(id)) %>% select(sample_id, pdf) %>% collect()
  DBI::dbDisconnect(con)
  if(length(df) == 0) {
    res$status = 404
    return(list(error="PDF not found."))
  }
  if(missing(dl)) base64enc::base64decode(df$pdf)
  else as_attachment(base64enc::base64decode(df$pdf), filename = paste0(df$sample_id, ".pdf"))
}

#* Return xpert csv
#* @get /xpert
#* @serializer csv list(na="")
function(id, res) {
  csv <- NULL
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), "xpertdb")
    csv <- tbl(con, "xpert_results") %>% select(!c(pdf, id, raw_text)) %>% collect() %>%
      modifiedXpert(config)
    names(csv$pid) <- config$config$pidName$value
    DBI::dbDisconnect(con)
  }, error = function(e) { 
    res$status <- 404
    res$headers <- list(contentType = "text/plain")
    res$body <- paste0("Database empty. Hit back to upload results. Error: ", e$message)
  })
  if (is.null(csv)) return(res)
  as_attachment(csv, filename = "xpertresults.csv")
}

#* Search by Sample ID or PID
#* @get /search
function(q, res) {
  if (nchar(q) < 2) return("")
  df <- NULL
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), "xpertdb")
    df <- tbl(con, "xpert_results") %>% select(!c(pdf, raw_text)) %>%
      filter(sample_id %like% local(paste0("%", q, "%")) | pid %like%  local(paste0("%", q, "%"))) %>% collect() %>%
      modifiedXpert(config)
    DBI::dbDisconnect(con)
  }, error = function(e) {
    res$status <- 500
    df <<- list(list(msg = "Search error, database is empty", err = e$message))
  })
  return(df)
}

#* Get config
#* @get /config
function() {
  return (list(usePID = config$config$usePID$value, pidName = config$config$pidName$value, 
               configNeeded = config$configNeeded(), debug = interactive()))
}

#* Get settings
#* @get /settings
function() {
  # compile list
  config$getWebConfig()
}

#* Update settings
#* @post /settings
#* @param setting:object
function(setting, req) {
  config$saveWebConfig(setting)
  # Maybe we have enough information to get a result
  if(!is.null(auth)) auth$setUser(req$session$plumber$username, config, req)
  list()
}

#* auth endpoint
#* @post /authep
function(req, res) {
  result <- auth$checkAuth(req, res, config)
}

# Auth Filter
# This will only be called if the auth function exists (is linked by source).  See README
authFilter <- function(req, res) {
  # reset the timeout if we haven't connected a websocket (in which case sto is NULL)
  if(!is.null(sto)) {
    sto()
    sto <<- tofuns()
  }
  # are we trying to access the auth end point?
  if (!grepl(auth$filterex, req$PATH_INFO)) {
    result <- auth$auth(req, res, config)
    # Did auth tell us to go somewhere or directly return an error code?
    if (res$status != 200) return(result)
    else if(!is.null(result$err)) {
      res$status <- 503
      res$body <- result$err
      return (res)
    }
  }
  forward()
}

# the following is for the websocket tracking
clients <- list()
on.exit({ clients <- NULL }, add = TRUE)

# Start a timer, basically if this isn't cancelled by a websocket connection withing 5 mins shut down (no zombie processes) 
tofuns <- function() {
  later::later(function() {
    message("Scrapert is timing out")
    if (!interactive()) quit("no")
  }, 300)
}
sto <- tofuns()
if(!interactive()) on.exit({ file.remove("plumber.lock") }, add = TRUE)
on.exit({
  if (!is.null(sto)) {
    sto()
    sto <- NULL
  }
}, add=TRUE)

# Scrapert entry point
#* @plumber
function(pr) {
  options_plumber(
    port = config$config$server_port$value
  )
  # Check for running instances
  # is this already running locally? and not in Rstudio
  if (!interactive()) {
    if (file.exists("plumber.lock")) {
      pid <- readLines("plumber.lock")
      # kill the old process
      cat("Scrapert already running, attempting to stop prior process: \n")
      system(paste("kill -3", pid))
    }
    # New lock file
    write(Sys.getpid(), file = "plumber.lock")
    # Delete lock file on exist
    if(!interactive()) on.exit({ file.remove("plumber.lock") }, add = TRUE)
  }
  # set session key if auth enabled, and add filter
  if (!is.null(auth)) {
    key <-
      tryCatch({
        keyring::key_get("plumber_api")
      }, error = function(e) {
        keyring::key_set_with_value(service = "plumber_api", password =  plumber::random_cookie_key())
        keyring::key_get("plumber_api")
      })
    pr <- pr %>% pr_cookie(key, name="plumber", path="/", expiration = config$config$timeout$value) %>%
      pr_filter("auth", authFilter) 
    # add logout ep for testing
    if (interactive()) pr <- pr %>% pr_get("/test", function(action, req, res) {
        if (action == "logout") req$session$plumber <- NULL
        else if (action == "deletedb") file.remove("xpertdb")
        res$status <- 307
        res$setHeader("Location", "/")
      })
  }
  # Add websocket support to monitor connections
  pr$websocket(
    function(ws) {
      ws$request$uuid <- uuid::UUIDgenerate()
      # Cancel the timeout
      if (!is.null(sto)) {
        sto()
        sto <<- NULL
      }
      clients[[ws$request$uuid]] <<- ws
      ws$onMessage(function(binary, message){
        if (message == "shutdown") {
          cat ("Server shutdown requested\n")
          if (!interactive()) quit("no")
        }
      })
      ws$onClose(function() {
        clients[[ws$request$uuid]] <<- NULL
        cat("Removed ", ws$request$uuid, " ", length(clients), " client(s) connected\n")
        # use a promise so this thread keeps running but only if all the clients are gone
        if (!is.null(clients) & length(clients) == 0) {
          cat("starting shutdown timeout\n")
          later::later(function(value) {
              # shut this down if not interactive and the client list is still empty in ~ 30s
              if (length(clients) == 0) {
                cat ("shutting down\n")
                if(!interactive()) quit(save="no")
              } else cat("aborted shutdown ", length(clients), " client(s) is connected\n")
            }, 30)
        } # end if
      })
    }
  )
  pr %>% pr_static("/", system.file("/www/", package="Scrapert")) %>%
    pr_set_docs(TRUE) %>% pr_set_serializer(serializer_unboxed_json()) %>%
    pr_hook("exit", function() {
      message("Scrapert out!")
    }) %>%
     pr_set_docs_callback(function(url) {
       # Hijack this function to open a browswer now the api is ready
       if(!interactive())  browseURL(paste0("http://localhost:", config$config$server_port$value))
       return (NULL)
    })
}


