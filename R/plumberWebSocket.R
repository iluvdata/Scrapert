library(websocket)
library(R6)
library(plumber)

#' @title Plumber Web Sockets
#' @description
#' \link[plumber]{Plumber} wrapper to support Websockets
#' @export
PlumberWebSocket <- R6Class("PlumberWebSocket", 
  inherit = Plumber,
  public = list(
    #' @description
    #' see \link[plumber]{Plumber}
    #' 
    #' @param f location of file to be plumbed
    #' @param wd working directory to launch in and override Plumber's default (where the script resides)
    #' 
    #' This wrapper is used to correct the working directory
    initialize = function(f, wd = getwd()) {
      file.copy(f, paste0(wd, "/.plumber.R"))
      super$initialize(".plumber.R")
      pfile <- ".plumber.R"
      on.exit({file.remove(pfile)}, add = TRUE)
    },
    #' @description
    #' event fired when websocket is opened
    #' 
    #' @param ws websocket
    onWSOpen = function(ws) {
      if (is.function(private$ws_open)) {
        private$ws_open(ws)
      }
      invisible(self)
    },
    #' @description
    #' assign a websocket
    #' 
    #' @param open new websocket
    websocket = function(open = NULL) {
      if (!is.null(open)) stopifnot(is.function(open))
      private$ws_open <- open
    }
  ), 
  private = list(
    ws_open = NULL
  )
)