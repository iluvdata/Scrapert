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
  df$restrict_result <- df %>% select(paste0(keys, "_result")) %>% purrr::pmap_chr(function(...) {
      xpert = list(...)
      ifelse(any(xpert == "POS" & unlist(use)), "POS", "NEG")
    })
  df$mod_ct_result <- df %>% select(paste0(keys, "_ct")) %>% purrr::pmap_chr(function(...) {
    xpert = list(...)
    ifelse(any(xpert != 0 & xpert < unlist(ct)), "POS", "NEG")
  })
  return(df)
}