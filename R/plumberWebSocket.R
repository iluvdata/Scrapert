library(websocket)
library(R6)
library(plumber)

#' @title Plumber Web Sockets
#' @description
#' \link[plumber]{Plumber} wrapper to support Websockets
#' 
PlumberWebSocket <- R6Class("PlumberWebSocket", 
                            inherit = Plumber,
                            public = list(
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