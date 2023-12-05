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
    },
    #' @field config \code{list} holding the configuration.  Access with \code{config$config$xpertTZ}
    config = list(),
    #' @field xpert \code{list} holding the xpert interpretation values.  Access with \code{config$xpert["16"]}
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
      self$config <- conf
    },
    #' @description
    #' Save current configuration to yaml file
    #' @param file file or path to yaml config file
    #' @examples
    #' # config$saveToFile("config.yml")
    saveToFile = function(file) {
      l <- self$config
      l$config$debug <- NULL
      l$xpert <- self$xpert
      l$modules <- self$modules
      l$version <- as.character(packageVersion("Scrapert"))
      yaml::write_yaml(l, file)
    },
    #' @description
    #' Get nested list of values. Replaces  opts functions with strings and module classes with names of the class
    #' @return list() with xpert and module lists as top level items
    getNestedList = function() {
      l <- self$config
      l$xpert <- self$xpert
      l$modules <- self$modules
      l
    },
    #' @description
    #' Get nested list of values. Replaces  opts functions with strings and module classes with names of the class
    #' strips modules
    #' @return list() with xpert and module lists as top level items
    getWebConfig = function() {
      l <- self$getNestedList();
      l$RC <-  list(api = l$api, apikey = ifelse(
        tryCatch({ keyring::key_get("Scrapert-apikey") ; TRUE}, error = function (e) { return(FALSE)}),
        "set", NULL))
      l$api <- NULL
      l$modules <-NULL
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
    #' Save the web configure (form data)
    #' @param setting list of settings posted via ajax
    saveWebConfig = function(setting) {
      # let's save the passwords first
      if(is.character(setting$apikey)) {
         if(setting$apikey != "set") keyring::key_set_with_value(service = "Scrapert-apikey", password = setting$apikey)
      }
      self$config$api = setting$api
      setting$server <- NULL
      setting$debug <- NULL
      
      # Now the settings
      purrr::keep_at(setting, ~ !(.x %in% c("xpert", "RC"))) %>% 
        purrr::iwalk(\(x,idx) {
          self$config[[idx]] <- x
        })
      # Now xpert
      self$xpert <- lapply(setting$xpert, \(x) {
        x$ct = as.integer(x$ct)
        x
      })
      self$saveToFile("config.yml");
    }
  )
)
