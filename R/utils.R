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
  df <- df %>% mutate(
    use16 = config$xpert[["16"]]$use,
    use18_45 = config$xpert[["18_45"]]$use,
    useP3 = config$xpert$P3$use,
    useP4 = config$xpert$P4$use,
    useP5 = config$xpert$P5$use,
    ct16 = ifelse(config$xpert[["16"]]$ct != "", config$xpert[["16"]]$ct, 40),
    ct18_45 = ifelse(config$xpert[["18_45"]]$ct != "", config$xpert[["18_45"]]$ct, 40),
    ctP3 = ifelse(config$xpert$P3$ct != "", config$xpert$P3$ct, 38),
    ctP4 = ifelse(config$xpert$P4$ct != "", config$xpert$P4$ct, 38),
    ctP5 = ifelse(config$xpert$P5$ct != "", config$xpert$P5$ct, 38),
    restrict_result =
      if_else((HPV_16_result == "POS" & use16) |
                (HPV_18_45_result == "POS" & use18_45) |
                (P3_result == "POS" &  useP3) |
                (P4_result == "POS" &  useP4) |
                (P5_result == "POS" &  useP5), "POS", "NEG"),
    mod_ct_result =
      if_else((HPV_16_ct < ct16 & HPV_16_ct != 0) |
                (HPV_18_45_ct < ct18_45 & HPV_16_ct != 0) |
                (P3_ct < ctP3 & P3_ct != 0) |
                (P4_ct < ctP4 & P4_ct != 0) |
                (P5_ct < ctP5 & P5_ct != 0), "POS", "NEG")) %>%
    select(!c(use16, use18_45, useP3, useP4, useP5, ct16, ct18_45, ctP3, ctP4, ctP5))
}
