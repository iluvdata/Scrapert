#' Launch Scrapert 
#' 
#' This function will launch Scrapert and log results to "scrapert.log" including installing any missing packages
#' 
#' @details
#' The database, the config file, and the log will all be located in the working directory 
#' 
#' @param wd a working directory or not supplied to use the current working directory
#' @param server launch in server (daemon mode) `TRUE/FALSE`
#' @return 0 (zero) if successful
#' @export
#'
#' @examples
#' # Rscript -e 'Scrapert::launch()' 
#' 
#' # Scrapert::launch("/path/to/working_dir")
#' 
launch <- function(wd = getwd(), server = FALSE) {
  setwd(wd)
  logger::log_layout(logger::layout_glue_colors)
  options(scrapertserver = server)
  logger::log_info("Using working directory ", wd)
  if(!file.exists("config.yml")) {
    logger::log_warn("No config found, using basic configuration")
    file.copy(system.file("config.sample.yml", package="Scrapert"), "config.yml")
  }
  if(T || tolower(Sys.info()[["sysname"]]) == "linux") {
    options("keyring_backend" = "file", "keyring_keyring" = "scrapert_kr")
    if(!file.exists("krs"))  {
      cat(plumber::random_cookie_key(), "\n", sep="", file="krs")
      keyring::keyring_create("scrapert_kr", password = readLines("krs"))
    }
  }
  host <- "127.0.0.1"
  if(server) {
    logger::log_success("Running in server mode")
    host <- "0.0.0.0"
  } else logger::log_success("Running in standalone (app) mode")
  logger::log_info("Launching Scapert\n")
  PlumberWebSocket$new(system.file("plumber.R", package="Scrapert"))$run(host = host)
}
