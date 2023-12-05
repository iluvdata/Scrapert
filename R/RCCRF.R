#' REDCap CRF Upload
#'
#' @description
#' Performs CRF upload to REDCap
#' @param config \link{Config} object
#' @return a list of functions for plumber to reference
#' @export
RCCRF <- function(config) {
  return(list(updateCRF = RCupdateCRF, crfDelete = RCCRFDelete, getPDF = RCgetPDF))
}
#' Upload CRFs
#'
#' @param sn A vector (length > 0) cartridge_sn in the sqlite database to be uploaded
#' @param config Scrapert config
#'
#' @return tibble 2 cols (id, uploaded) with the sqlite id, and an uploaded timestamp (UCT time POSIXct)
#' @export
RCupdateCRF <- function(sn, config) {
  key <- RCCRFgetKey(config)
  # Build data frames
  con <- dbConnect(RSQLite::SQLite(), "xpertdb")
  df <- tbl(con, "xpert_results") %>% filter(cartridge_sn %in% local(sn) & !is.na(pid)) %>% 
    select(id, sample_id, cartridge_sn, test_result, end_time, processed_by, pid) %>% collect()
  if (nrow(df) == 0) {
    dbDisconnect(con);
    stop (sprintf("pid is missing for %s", sn))
  };
  df <- df %>% # Get the record numbers
    RCCRFgetRecordId(config, key) %>% filter(!is.na(record_id))
  # upload xpert results
  data <- list(
    token = key,
    content = "record",
    format = "csv",
    type = "flat",
    overWriteBehavior = "normal",
    forceAutoNumber = "false",
    returnContent = "count",
    returnFormat = "json",
    data = readr::format_csv(df %>% select(xpert_result = test_result, xpert_timestamp = end_time, record_id, 
                                           cartridge_sn, xpert_processed_by = processed_by, -sample_id, -id) %>%
                               mutate(xpert_result_complete = 2))
  )
  response <- httr::POST(config$config$api, body = data, encode = "form")
  if (response$status_code != 200) stop(paste("REDCap Error updating CRF data", response$status_code, httr::content(response)))
  # Now for the files
  apply(df, MARGIN = 1, FUN = function(x) {
    x <- as.list(x)
    pdf <- tbl(con, "xpert_results") %>% filter(id == local(x$id)) %>% select(pdf) %>% collect() %>%
      pull(pdf)
    if (!is.na(pdf)) {
      temp <- file(paste0(tempdir(), "/", x$sample_id, ".pdf"), "wb")
      base64enc::base64decode(pdf, output = temp)
      flush(temp)
      temppath <- summary(temp)$description
      data = list(
        token = key,
        action = "import",
        content = "file",
        record = x$record_id,
        field = "xpert_data",
        event= "",
        returnFormat = "json",
        file = httr::upload_file(temppath)
      ) 
      response <- httr::POST(config$config$api, body = data, encode = "multipart")
      if (response$status_code != 200) stop(paste("REDCap File Upload Error", response$status_code, httr::content(response)))
      close(temp)
      file.remove(temppath)
      # remove from the database
      if(con %>% dbExecute(paste("UPDATE xpert_results SET pdf = NULL WHERE id =", x$id)) != 1) 
        stop("Error removing pdf from local database")
    } else logger::log_warn("{ x$cartridge_sn } pdf missing from sqlite3, perhpas it's uploaded already")
    pdf <- NULL
  })
  dbDisconnect(con)
  df$cartridge_sn
}

#' REDCap Delete CRF
#'
#' @param sn the cartridge_sn in xpertdb
#' @param config \link{Config} object
#'
#' @return \code{NULL} if successful
#' @export
RCCRFDelete <- function(sn, config) {
  key <- RCCRFgetKey(config)
  con <- dbConnect(RSQLite::SQLite(), "xpertdb")
  df <- tbl(con, "xpert_results") %>% filter(cartridge_sn %in% local(sn)) %>% 
    select(pid) %>% collect()
  if (is.na(df$pid)) {
    logger::log_info("No PID set for sn {sn}, unable to delete from REDCap")
    return()
  }
    # get the record numbers
  df <- df %>% RCCRFgetRecordId(config, key)
  # delete the files first
  dbDisconnect(con)
  data <- list(
    token = key,
    action = "delete",
    content = "file",
    record = df$record_id,
    field = "xpert_data",
    event= "",
    returnFormat = "json"
  )
  response <- httr::POST(config$config$api, body = data, encode = "form")
  if (response$status_code != 200) logger::log_warn("REDCap File delete Error { response$status_code }- {httr::content(response , as='text')}")
  # We don't delete just remove values
  data <- list(
    token = key,
    content = "record",
    format = "csv",
    type = "flat",
    overwriteBehavior = "overwrite",
    forceAutoNumber = "false",
    returnContent = "count",
    returnFormat = "json",
    data = readr::format_csv(df %>% select(record_id, -pid) %>%
                               mutate(xpert_result_complete = "", xpert_result = "", xpert_timestamp = "",
                                      cartridge_sn = "", xpert_processed_by = ""))
  )
  response <- httr::POST(config$config$api, body = data, encode = "form")
  if (response$status_code != 200) stop(paste("REDCap Error deleting CRF data", response$status_code, httr::content(response)))
  count <-as.integer(httr::content(response)$count)
  if (count != length(sn)) stop(paste("REDCap only deleted", count, "records but deletion ", length(sn), "records was requested"))
  NULL
}

#' Get API Key
#' 
#' Get the API Key, throw error is missing.  Check for URL as well. 
#'
#' @param config \link{Config} Object
#' @return API Key
RCCRFgetKey <- function (config) {
  key <- tryCatch({
    keyring::key_get("Scrapert-apikey")
  }, error = function(e) {
    return(NULL)
  })
  if (!is.character(key)) stop("REDCap API Key is not set")
  if (!is.character(config$config$api)) stop("REDCap Api URL is not set")
  key
}

RCgetPDF <- function(pid, config) {
  key = RCCRFgetKey(config)
  df <- RCCRFgetRecordId(tibble(pid = pid), config, key)
  data <- list(
    token = key,
    content = "file",
    action = "export",
    record = df$record_id,
    field = "xpert_data",
    event = "",
    returnFormat = "json"
  )
  response <- httr::POST(config$config$api, body = data, encode = "form")
  if (response$status_code != 200) stop(paste("REDCap Error getting pdf", response$status_code, httr::content(response)))
  pdf <- httr::content(response, as="raw")
  pdf
}

#' REDCap Map row id to record id
#'
#' @param df data frame with at least id, sample_id
#' @param config \link{Config} object
#' @param key REDCap CRF Api Key
#'
#' @return \code{df} with \code{record_id} column appended
RCCRFgetRecordId <- function(df, config, key) {
  data <- list(
    token = key,
    content = "record",
    format = "csv",
    type = "flat",
    returnFormat = "json",
    fields = "record_id, pid",
    filterLogic = paste(paste0("[pid]='", df$pid, "'"), collapse = " OR ")
  )
  response <- httr::POST(config$config$api, body = data, encode = "form")
  if (response$status_code != 200) stop(paste("REDCap Error getting record_id", response$status_code, httr::content(response)))
  rid <- httr::content(response, col_types="cc")
  df %>% left_join(rid, by="pid")
}
