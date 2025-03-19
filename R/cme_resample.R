
#' Resample CME limit order book data
#'
#' Resampling CME limit order book data using \code{highfrequncy::aggregatePrice()}.
#'
#' @param mbp_order_book Limit order book data generated via `order_book()`.
#' @param trading.tz Time zone of the limit order book and trade summary data.
#' Default is \code{America/Chicago}.
#' @param resample_freq Resample frequency, could be \code{milliseconds}, \code{secs},
#' \code{mins}, or \code{hours}.
#' @param resample_period Resample period (interval), e.g., 1, 10, or 100.
#' @param resample_start The starting timestamp of resample data in \code{HH:MM:SS} format.
#' @param resample_end The ending timestamp of resample data in \code{HH:MM:SS} format.
#' @param fill Fill the gaps if timestamps have irregularities after the resampling. Default is \code{TRUE}.
#'
#' @returns A data.table contains the resamped data
#' @export
#'
#' @import data.table highfrequency
#'
#' @examples
#' # This function requires a CME data license to run
#' # 100-millisecond resampling
#' # Starting timestamp 08:30:00
#' # Ending timestamp 08:32:00
#' \dontrun{
#' resample <- cme_resample(book, trading.tz = "America/Chicago",
#' resample_freq = "milliseconds", resample_preiod = 100,
#' resample_start = "08:30:00", resample_end = "08:32:00", fill = TRUE)
#' }
#'
cme_resample <- function(mbp_order_book, trading.tz="America/Chicago",
                      resample_freq=c("milliseconds", "secs", "mins", "hours"),
                      resample_period, resample_start, resample_end, fill=TRUE){

  Time <- DT <- NULL


  if (is.data.table(mbp_order_book)){
    if("DT" %in% colnames(mbp_order_book) == TRUE){

      setnames(mbp_order_book, "DT", "Time")

    } else {
      stop("Input has to be data.table")
    }
  }



if(class(mbp_order_book$Time)[1]=="character"){

  mbp_order_book[, Time:= paste0(substr(Time, 1, 4), "-", substr(Time, 5, 6), "-",
                           substr(Time, 7, 8),
                           " ", substr(Time, 9, 10),
                           ":", substr(Time, 11, 12), ":", substr(Time, 13, 14),
                           ".", substr(Time, 15, 23))] ## dafult as.posixct time format


  mbp_order_book <- mbp_order_book[, `:=`(Time=as.POSIXct(Time, tz="GMT", "%Y-%m-%d %H:%M:%OS"))][, -c("Date", "Seq", "MsgSeq")]

}else{

  mbp_order_book <- mbp_order_book[, -c("Date", "Seq", "MsgSeq")]

}
col_names <- colnames(mbp_order_book)

## change timezone to UTC
resample_start <- as.POSIXct(resample_start, trading.tz)
resample_end <- as.POSIXct(resample_end, trading.tz)
attr(resample_start, "tzone") <- "GMT"
attr(resample_end, "tzone") <- "GMT"

if("DT" %in% colnames(mbp_order_book) == F){

  setnames(mbp_order_book, old = "Time", new = "DT")

}


### high frequency package needs the time-based column to be renamed as "DT"

mbp_order_book <- mbp_order_book[DT %between% c(resample_start, resample_end)]

if(substr(resample_start, 1, 10) != substr(resample_end, 1, 10)){

  first_day.end <- as.POSIXct(paste0(substr(resample_start, 1, 10), " ", "23:59:59"), "GMT")
  second_day.start <- as.POSIXct(paste0(substr(resample_end, 1, 10), " ", "00:00:00"), "GMT")


  mbp_order_book <- rbindlist(list(first = aggregatePrice(mbp_order_book[DT %between% c(resample_start, first_day.end)],
                                                    alignBy = resample_freq, alignPeriod = resample_period,
                                                    marketOpen = substr(resample_start, 12, 19), marketClose = "23:59:59", fill=fill),

                             second=aggregatePrice(mbp_order_book[DT %between% c(second_day.start, resample_end)],
                                                   alignBy = resample_freq, alignPeriod = resample_period,
                                                   marketOpen = "00:00:00", marketClose = substr(resample_end, 12, 19), fill=fill)))


}else{  ### start and end time are not in the same day


  mbp_order_book <- aggregatePrice(mbp_order_book, alignBy = resample_freq, alignPeriod = resample_period,
                             marketOpen = substr(resample_start, 12, 19), marketClose = substr(resample_end, 12, 19), fill=fill)

}

colnames(mbp_order_book) <- col_names
attr(mbp_order_book$DT, "tzone") <- trading.tz
return(mbp_order_book)

}


