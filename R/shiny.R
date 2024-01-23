# Wrapper functions for shiny and esquisse to run in Jupyter at SSB (on prem and Dapla)

getport <- function(){
  port <- NULL
  while (is.null(port)){
    port <- shiny:::p_randomInt(3000, 8000)
    if (!port %in% c(3659, 4045, 5060, 5061, 6000,
                     6566, 6665:6669, 6697)) {
      port <- NULL
    }
  }
  port
}

#' Wrapper function for running shiny apps in Jupyter at SSB
#'
#' @param ... Parameters to send to runApp()
#'
#' @return None
#' @export
runApp_ssb <- function(...){
  port <- getport()
  usr <- initialer_funk()
  if (env_check() %in% c("BakkeProd", "BakkeTest")) {
    appUrl <- paste("https://sl-jupyter-p.ssb.no/user/", usr, "/proxy/", port, "/", sep = "")
  } else {
    appUrl <- paste("https://jupyter.dapla.ssb.no/user/", usr, "@ssb.no/proxy/", port, "/", sep = "")
  }
  message(paste("App launching at:", appUrl))
  suppressMessages(shiny::runApp(port = port, ...))
}

#' Wrapper function for running exampleshiny apps in Jupyter at SSB
#'
#' @param example Example shiny to run
#' @param ... Parameters to send to runExample()
#'
#' @return None
#' @export
runExample_ssb <- function(example = NA, ...){
  port <- getport()
  usr <- initialer_funk()
  if (env_check() %in% c("BakkeProd", "BakkeTest")) {
    appUrl <- paste("https://sl-jupyter-p.ssb.no/user/", usr, "/proxy/", port, "/", sep = "")
  } else {
    appUrl <- paste("https://jupyter.dapla.ssb.no/user/", usr, "@ssb.no/proxy/", port, "/", sep = "")
  }
  message(paste("Example launching here:", appUrl))
  suppressMessages({
    shiny::runExample(example, port = port, launch.browser=FALSE, ...)
  })
}


#' Wrapper function for running esquisse in Jupyter at SSB
#'
#' @param data Data frame to use for plotting
#' @param ... Additional parameters for send to esquisser()
#'
#' @return None
#' @export
esquisser_ssb <- function(data, ...){
  port <- getport()
  options("shiny.port" = port)
  usr <- initialer_funk()
  if (env_check() %in% c("BakkeProd", "BakkeTest")) {
    appUrl <- paste("https://sl-jupyter-p.ssb.no/user/", usr, "/proxy/", port, "/", sep = "")
  } else {
    appUrl <- paste("https://jupyter.dapla.ssb.no/user/", usr, "@ssb.no/proxy/", port, "/", sep = "")
  }

  message((paste("Esquisser launching at:", appUrl)))
  options("shiny.port" = port)
  suppressMessages(esquisse::esquisser(data, ...))
}
