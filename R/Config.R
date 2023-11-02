#' @title Scrapert Config
#' 
#' @description
#' Holds configuration for Scrapert
#' @export
Config <- R6Class("Config",
  public = list(
    #' @description
    #' Creates a new instance of this class.
    #' @param file file or path to yaml config file
    #' @examples
    #' # config <- Config$new("config.yml")
    initialize = function(file) {
      self$loadFromFile(file)
      self$xpert <- purrr::list_merge(self$xpert,
        '16' = list(label = "HPV 16"), '18_45' = list(label = "HPV 18/45"),
        P3 = list(label = "P3"), P4 = list(label = "P4"), P5 = list(label = "P5"))
    },
    #' @field config \code{list} holding the configuration.  Access with \code{config$config$xpertTZ$value}
    config = list(),
    #' @field xpert \code{list} holding the xpert interpretation values.  Access with \code{config$xpert["16"]$label}
    xpert = list(),
    #' @field modules \code{list} holding R6 module class (Auth, PID, CRF).  Access with \code{config$modules$Auth$checkAuth(res,req)}
    modules = list(),
    #' @description
    #' load values from a yaml file
    #' @param file file or path to yaml config file
    #' @examples
    #' # config$loadFromFile("config.yml")
    loadFromFile = function(file) {
      conf <- yaml::read_yaml(file)
      self$modules <- conf$modules
      self$xpert <- purrr::list_modify(self$xpert, !!!conf$xpert)
      conf$modules <- NULL
      conf$xpert <- NULL
      self$config <- purrr::list_modify(self$config, !!!purrr::map(conf, \(x) list(value = x)))
    },
    #' @description
    #' Save current configuration to yaml file
    #' @param file file or path to yaml config file
    #' @examples
    #' # config$saveToFile("config.yml")
    saveToFile = function(file) {
      l <- lapply(self$config, function(x) { x$value })
      l$xpert <- lapply(self$xpert, function(x) { list(use = x$use, ct = x$ct) })
      l$modules <- self$modules
      yaml::write_yaml(l, file)
    },
    #' @description
    #' Get nested list of values. Replaces  opts functions with strings and module classes with names of the class
    #' @return list() with xpert and module lists as top level items
    getNestedList = function() {
      l <- purrr::map(self$config, function(x) {
        if(!is.null(x$opts)) x$opts <- paste(c(head(x$opts[[1]](), 4), "etc."), collapse=", ")
        x
      })
      l$xpert <- self$xpert
      l$modules <- self$modules
      l
    },
    #' @description
        #' get nest yaml
        #' @param ... arguments passed to \link[yaml]{as.yaml}
        #' @return character string of config in yaml
    yaml = function(...) {
      yaml::as.yaml(self$getNestedList(), ...)
    },
    #' @description
        #' print to terminal in yaml
        #' @examples
                #' #config
    print = function() {
      cat(self$yaml())
    },
    #' @description
        #' get nested lists for json serialization
        #' @return nested lists of items
    getWebConfig = function() {
      l <- purrr::keep(self$config, function(x) { !is.null(x$label) }) %>%
        purrr::map_if(\(x) !is.null(x$opts), function(x) { purrr::list_modify(x, opts = x$opts[[1]]())})
      l$xpert = self$xpert
      l
    },
    #' @description
        #' a setting is needed
        #' @return \code{TRUE} if a setting is needed
    configNeeded = function() {
      purrr::some(self$config, \(x) !is.null(x$needed))
    },
    #' @description 
    #' Save the web configure (form data)
    #' @param setting list of settings posted via ajax
    saveWebConfig = function(setting) {
      # let's save the passwords first
      purrr::iwalk(purrr::keep(self$config, \(x) if (!is.null(x$type)) return(x$type == "password") else FALSE),
                   function(x, idx) {
                     if(setting[[idx]] != "") keyring::key_set_with_value(service = paste0("Scrapert-", idx), password = setting[[idx]])
                   })
      # Now the config
      purrr::iwalk(purrr::keep(self$config, \(x) if (!is.null(x$type)) return(x$type != "password") else TRUE),
                   function(x, idx) {
                     if(!is.null(setting[[idx]])) self$config[[idx]]$value = setting[[idx]]
                   })
      # We should have needed values by now
      self$config <- purrr::modify_if(self$config, \(x) !is.null(x$needed), function(x) {
          x$needed <- NULL
          x
        })
      # Now xpert
      purrr::iwalk(self$xpert, function(x, idx) {
        ct <- setting[[paste0("ct", idx)]]
        self$xpert[[idx]]$use <- !is.null(setting[[paste0("use", idx)]])
        self$xpert[[idx]]$ct <- ifelse(ct == "", "", as.integer(ct))
      })
      self$saveToFile("config.yml")
    },
    #' @description
        #' Add a configuration item to the stack or add metadata to existing entry
        #' @param ... named list to add (see examples)
        #' @examples
                #' config$addConfig(
                #'   xpertTZ = list(label="Timezone of Xpert Machine", required = TRUE, opts = OlsonNames)
                #' )
    addConfig = function(...) {
      self$config <- purrr::list_merge(self$config, ...)
    }
  )
)
