#' Launch Scrapert 
#' 
#' This function will launch Scrapert and log results to "scrapert.log" including installing any missing packages
#' 
#' @details
#' The database, the config file, and the log will all be located in the working directory 
#' 
#' @param wd a working directory or not supplied to use the current working directory
#' @return 0 (zero) if successful
#' @export
#'
#' @examples
#' # Rscript -e 'Scrapert::launch()' 
#' 
#' # Scrapert::launch("/path/to/working_dir")
#' 
launch <- function(wd = getwd()) {
  message("Using working directory ", wd)
  setwd(wd)
  if(!interactive()) {
    file.remove("scrapert.log")
    log <- file("scrapert.log", "wt")
    sink(log, append = TRUE, type = "output")
    sink(log, append = TRUE, type = "message")
    on.exit({flush(log)})
  }
  if(!file.exists("config.yml")) {
    warning("No config found, using basic configuration")
    file.copy(system.file("config.sample.yml", package="Scrapert"), "config.yml")
  }
  # plumber uses the files directory as the working directory... annoying
  #file.copy(system.file("plumber.R", package="Scrapert"), ".plumber.R")
  message("Launching Scapert\n")
  PlumberWebSocket$new(system.file("plumber.R", package="Scrapert"))$run()
}
