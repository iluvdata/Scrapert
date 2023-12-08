library(dplyr, warn.conflicts = F)
library(tidyr)
#' Get Xpert Keyed Value
#'
#' @details
#' \code{keyed_ts} returns the \code{POSIXct} date/time of the keyed value in UCT/GMT time
#' 
#'
#' @param key key of value to extract
#' @param txt source of extraction
#'
#' @return the value after the key
#' @export
keyed_value <- function(key, txt) {
  stringr::str_extract(txt, paste0("(?<=", stringr::str_replace_all(key, "\\s", "\\\\s"), ":\\s{1,30})[:alnum:]+"))
}

#' @rdname keyed_value
#' @param tz Olson Name tz of value
#' @export
keyed_ts <- function(key, txt, tz) {
  value <- as.POSIXct(stringr::str_extract(txt, paste0("(?<=", stringr::str_replace_all(key, "\\s", "\\\\s"),
                                              ":\\s{1,30})[:graph:]{8}\\s[:graph:]{8}")),
                      tryFormats = "%m/%d/%y %H:%M:%S", tz = tz)
  # Convert to UTC
  attr(value, "tzone") <- "GMT"
  value
}

#' Add Optimized Xpert Results
#' 
#' Add modificed Xpert results  to df using values in \link{Config}
#' @param df dataframe with Xpert data
#' @param config \link{Config} R6 class
#' @return dataframe with \code{restricted_result} and \code{mod_ct_result} columns added
#' @export
modifiedXpert <- function(df, config) {
  ct <- list(); use <- list()
  keys <- c("HPV_16", "HPV_18_45", "P3", "P4", "P5")
  lapply(keys, \(e) {
    channel <- config$xpert[[stringr::str_replace_all(e, "HPV_", "")]]
    # Assign max cutoffs for the channel.
    ct[[e]] <<- ifelse(is.integer(channel$ct), as.integer(channel$ct), ifelse(stringr::str_starts(e, "HPV"), 40, 38))
    use[[e]] <<- channel$use;
  })
  # Modify every row
  df$restrict_result <- df %>% dplyr::select(paste0(keys, "_result")) %>% purrr::pmap_chr(function(...) {
      xpert = list(...)
      ifelse(any(xpert == "POS" & unlist(use)), "POS", "NEG")
    })
  df$mod_ct_result <- df %>% dplyr::select(paste0(keys, "_ct")) %>% purrr::pmap_chr(function(...) {
    xpert = list(...)
    ifelse(any(xpert != 0 & xpert < unlist(ct)), "POS", "NEG")
  })
  return(df)
}

#' Parse the pdf document
#' 
#' @description
#' Will parse xpert results from pdf and store in a database.  This function does not upload to remote sites nor
#' does it check if the files are pdfs (other than using \link{pdftools} functions) 
#'
#' @param xpert path of pdf OR raw binary (from req)
#' @param processed_by character string of who or what is processing this xpert result
#' @param config \link{Config} R6 class
#' @param pool \link{Pool} database connection pool
#'
#' @return \code{list(results = df)} or \code{list(result = df, msg = "error message")} where \code{df} contains xpert results
#' written to a database 
#' @export
parsePDF <- function(xpert, processed_by, config, pool) {
  # Parse Split into reports (remove signature page)
  xpert <- lapply(xpert , function(x) {
    x$raw_text <- pdftools::pdf_text(x$pdf)
    # which pages have data
    pages <- x$raw_text %>% stringr::str_detect("Sample ID")
    # are there no valid pages?f
    if (!any(pages)) stop(glue::glue("{x$filename} is not a valid Xpert HPV output file."))
    x$filename <- NULL
    # subset pages and get pdfs
    x$raw_text <- x$raw_text[pages]
    pdfpath <- tempfile(fileext = ".pdf")
    # are we raw or a file?
    if(inherits(x$pdf, "raw")) {
      pdf <- file(pdfpath, "wb")
      writeBin(x$pdf, pdf)
      close(pdf)
    } else {
      file.copy(x$pdf, pdfpath, overwrite = T)
    }
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
    start_time = keyed_ts("Start Time", xpert$raw_text, config$config$xpertTZ),
    end_time = keyed_ts("End Time", xpert$raw_text, config$config$xpertTZ),
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
  if (!pool::dbExistsTable(pool, "xpert_results")) {
    # Create the table
    sql <- paste0("CREATE TABLE xpert_results (id INTEGER PRIMARY KEY ASC, sample_id TEXT,  pid TEXT, test_result TEXT, ",
                  "status TEXT,  error TEXT,  uploaded TEXT,",
                  "SAC_ct REAL,  SAC_result TEXT,  HPV_16_ct REAL,  HPV_16_result TEXT,  HPV_18_45_ct REAL,  ",
                  "HPV_18_45_result TEXT,  P3_ct REAL,  P3_result TEXT,  P4_ct REAL,  P4_result TEXT,  P5_ct REAL,",
                  "P5_result TEXT,  error_message TEXT,  start_time TEXT,  end_time TEXT,  instrument_sn TEXT,  ",
                  "cartridge_sn TEXT,  reagant_lot TEXT, processed_by TEXT, notes TEXT, raw_text TEXT, pdf BLOB)")
    rs <- pool::dbExecute(pool, sql)
    rs <- pool::dbExecute(pool, "CREATE UNIQUE INDEX samp_run ON xpert_results (sample_id, cartridge_sn)")
    # add the version
    version <- tibble(version = as.character(packageVersion("Scrapert")))
    pool::dbWriteTable(pool, "version", version)
  }
  
  # Add in user data
  df$processed_by <- processed_by
  
  # Read in the pdfs using the tempfile paths and base64enc since sqlite doesn't do binary well.
  df <- df %>% mutate(pdfbin = sapply(df$pdf, base64enc::base64encode, USE.NAMES = F))
  file.remove(df$pdf)
  df <- df %>% select (!pdf, pdf = pdfbin) %>%
    # fix dates
    mutate(start_time = as.character(start_time),
           end_time = as.character(end_time))
  
  msg <- NULL
  # appended to the database
  tryCatch({
    pool::dbAppendTable(pool, "xpert_results", df)
  }, error = function(e) {
    # Create error message
    msg <<- list(msg = paste("One or more results already exists in the local database.",
                             "Please verify import status"), err = e$message)
  })
  
  # Now load with db id's and run xpert results
  df <- tbl(pool, "xpert_results") %>%
    select(!c(pdf, raw_text)) %>% filter(cartridge_sn %in% local(df$cartridge_sn)) %>% collect()
  return({
    if (is.null(msg)) list(results = df)
    else list(results = df, message = msg)
  })
}

