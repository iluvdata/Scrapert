#' REDCap CRF Upload
#'
#' @description
#' Performs CRF upload to REDCap
#' @param config \link{Config} object
#' @return a list of functions for plumber to reference
RCCRF <- function(config) {
  config$addConfig(
    RCCRFApi = list(label = "REDCap CRF API URL", type="url", required=TRUE),
    RCCRFApiKey = list(label = "REDCap CRF Project API Key", type="password")
  )
  return(list(updateCRF = RCupdateCRF, crfDelete = RCCRFDelete))
}
#' Upload CRFs
#'
#' @param id A vector (length > 0) row ids in the sqlite database to be uploaded
#' @param config Scrapert config
#'
#' @return tibble 2 cols (id, uploaded) with the sqlite id, and an uploaded timestamp (UCT time POSIXct)
#' @export
RCupdateCRF <- function(id, config) {
  key <- RCCRFgetKey(config)
  # Build data frames
  con <- dbConnect(RSQLite::SQLite(), "xpertdb")
  df <- tbl(con, "xpert_results") %>% filter(id %in% local(id)) %>% 
    select(id, sample_id, cartridge_sn, test_result, end_time, processed_by) %>% collect() %>%
    # Get the record numbers
    RCCRFgetRecordId(config, key)
  # upload xpert results
  data <- list(
    token = keyring::key_get("Scrapert-RCCRFApiKey"),
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
  response <- httr::POST(config$config$RCCRFApi$value, body = data, encode = "form")
  if (response$status_code != 200) stop(paste("REDCap Error updating CRF data", response$status_code, httr::content(response)))
  count <-as.integer(httr::content(response)$count)
  if (count != length(id)) stop(paste("REDCap only updated", count, "records but updates to", length(id), "records was requested"))
  # Now for the files
  apply(df, MARGIN = 1, FUN = function(x) {
    x <- as.list(x)
    temp <- file(paste0(tempdir(), "/", x$sample_id, ".pdf"), "wb")
    base64enc::base64decode(tbl(con, "xpert_results") %>% filter(id == local(x$id)) %>% select(pdf) %>% collect() %>%
                            pull(pdf), output = temp)
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
    response <- httr::POST(config$config$RCCRFApi$value, body = data, encode = "multipart")
    if (response$status_code != 200) stop(paste("REDCap File Upload Error", response$status_code, httr::content(response)))
    close(temp)
    file.remove(temppath)
  })
  dbDisconnect(con)
  df$id
}

#' REDCap Delete CRF
#'
#' @param id the row id in xpertdb
#' @param config \link{Config} object
#'
#' @return \code{NULL} if successful
RCCRFDelete <- function(id, config) {
  key <- RCCRFgetKey(config)
  con <- dbConnect(RSQLite::SQLite(), "xpertdb")
  df <- tbl(con, "xpert_results") %>% filter(id %in% local(id)) %>% 
    select(id, sample_id) %>% collect() %>% 
    # get the record numbers
    RCCRFgetRecordId(config, key)
  # We don't delete just remove values
  dbDisconnect(con)
  data <- list(
    token = keyring::key_get("Scrapert-RCCRFApiKey"),
    content = "record",
    format = "csv",
    type = "flat",
    overWriteBehavior = "normal",
    forceAutoNumber = "false",
    returnContent = "count",
    returnFormat = "json",
    data = readr::format_csv(df %>% select(record_id, -sample_id, -id) %>%
                               mutate(xpert_result_complete = 0, xpert_result = "", xpert_timestamp = "",
                                      cartridge_sn = "", xpert_processed_by = ""))
    
  )
  response <- httr::POST(config$config$RCCRFApi$value, body = data, encode = "form")
  if (response$status_code != 200) stop(paste("REDCap Error deleting CRF data", response$status_code, httr::content(response)))
  count <-as.integer(httr::content(response)$count)
  if (count != length(id)) stop(paste("REDCap only deleted", count, "records but deletion ", length(id), "records was requested"))
  # Now for the file
  data <- list(
    token = key,
    action = "delete",
    content = "file",
    record = df$record_id,
    field = "xpert_data",
    event= "",
    returnFormat = "json"
  )
  response <- httr::POST(config$config$RCCRFApi$value, body = data, encode = "form")
  if (response$status_code != 200) stop(paste("REDCap File delete Error", response$status_code, httr::content(response)))
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
    keyring::key_get("Scrapert-RCCRFApiKey")
  }, error = function(e) {
    return(NULL)
  })
  if (is.null(key)) stop("REDCap CRF API Key is not set")
  if (is.null(config$config$RCCRFApi$value)) stop("REDCap CRF Api URL is not set")
  key
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
    fields = "record_id, sample_id",
    filterLogic = paste(paste0("[sample_id]='", df$sample_id, "'"), collapse = " OR ")
  )
  response <- httr::POST(config$config$RCCRFApi$value, body = data, encode = "form")
  if (response$status_code != 200) stop(paste("REDCap Error getting record_id", response$status_code, httr::content(response)))
  rid <- httr::content(response, show_col_types = FALSE)
  df %>% left_join(rid, by="sample_id")
}