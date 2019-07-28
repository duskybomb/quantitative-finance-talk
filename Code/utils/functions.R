intervalData <- function(name, instrument, interval) {
    stopifnot(length(name) == 1)
    stopifnot(length(interval) == 1)

    ## Selecting relevant data as specified by the user
    if (name == "nifty" || name == "niftyJunior") {
            tick.name <- "data.nifty"
        } else {
                tick.name <- paste0("data.stock.", instrument)
            }

    if (tick.name == "data.stock.futures"){
            ticker.name <- paste0(name, ".futures")
        } else if
    (tick.name == "data.stock.cash"){
                ticker.name <- paste0(name, ".cash")
            } else if
    (tick.name == "data.nifty"){
                    ticker.name <- "data.nifty"
                } else {
                        paste0("Error in argument provided")
                    }

    if (!is.null(instrument)) {
            if (instrument == "cash") {
                        tick.data <- data.stock.cash[[ticker.name]]
                    } else if (instrument == "futures") {
                                tick.data <- data.stock.futures[[ticker.name]]
                            }
      } else {
              tick.data <- data.nifty
          }

    final.data <- lapply (tick.data, function(x) {
            difference.data <- to.period (x, period = "seconds", k = interval, OHLC = FALSE)
            return (difference.data)
        })
    return (final.data)
}

