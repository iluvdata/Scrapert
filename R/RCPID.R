#' REDCap PID lookup
#'
#' @description
#' Performs pid lookup from REDCap
#' @param config \link{Config} object
#' @return a list of functions for plumber to reference
RCPID <- function(config) {
  config$addConfig(
    RCPIDApi = list(label = "REDCap PID Project API URL", type="url", required=TRUE),
    RCPIDApiKey = list(label = "REDCap PID Project API Key", type="password")
  )
  return(list(getPID = getPID))
}

#' Lookup PIDs in REDCap
#'
#' @param sampleid vector of length one or more of sample ids for which pids/mrns are needed
#' @param config Scrapert config
#'
#' @return tibble of columns sample_id and pid
#' @export
getPID <- function(sampleid, config) {
  key <- 
    tryCatch({
      keyring::key_get("Scrapert-RCPIDApiKey")
    }, error = function(e) {
      return(NULL)
    })
  if (is.null(key)) stop("REDCap PID API Key is not set")
  data <- list(
    token = key,
    content = "record",
    format = "csv",
    type = "flat",
    fields = "sample_id,pid",
    returnFormat = "json",
    filterLogic = paste(paste0("[sample_id]='", sampleid, "'"), collapse = " OR ")
  )
  response <- httr::POST(config$config$RCPIDApi$value, body = data, encode = "form")
  if (response$status_code != 200) stop(paste("REDCap Error", response$status_code, httr::content(response)))
  piddb <- httr::content(response, show_col_types = FALSE, col_types = "cc")
  pids <- tibble(sample_id = sampleid)
  pids <- pids %>% left_join(piddb, by="sample_id")
  pids
}