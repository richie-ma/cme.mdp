
#' Visualization of limit order book during a period
#'
#' `heatmap_time()` visualizes the limit order book using heatmap plot during a period.
#'
#' @param mbp_order_book Limit order book data generated via `order_book()`,
#' can be either outright, implied, or consolidated books.
#' @param trading.tz Time zone of the limit order book and trade summary data.
#' Default is \code{America/Chicago}.
#' @param level Number of books depths are visualized. Default is \code{NULL} and
#' all book depths are visualized.
#' @param start_timestamp The starting timestamp of the data for the plot in \code{HH:MM:SS} format.
#' @param end_timestamp The ending timestamp of the data for the plot in \code{HH:MM:SS} format.
#'
#' @returns A ggplot2 figure of limit order book heatmap.
#' @export
#'
#' @import data.table ggplot2 ggnewscale
#'
#' @section Data recommendation:
#' One would better use resampled data instead of tick-level data to reduce the load
#' in plotting. Resampling can be done via `cme_resample()`.
#'
#' @examples
#' # This function requires a CME data license to run
#' # Starting timestamp 08:30:00
#' # Ending timestamp 08:32:00
#' \dontrun{
#' heatmap <- heatmap_time(book, trading.tz = "America/Chicago",
#' start_timestamp = "08:30:00", end_timestamp = "08:32:00")
#' }

heatmap_time <- function(mbp_order_book, trading.tz = "America/Chicago", level=NULL, start_timestamp, end_timestamp){

  Date <- Time <- Seq  <- Code   <-    Bid_PX_10 <-  Bid_Qty_10 <- Bid_Ord_10 <-  Bid_PX_9 <-  Bid_Qty_9 <- Bid_Ord_9  <- Bid_PX_8  <-
    Bid_Qty_8  <- Bid_Ord_8  <- Bid_PX_7  <-  Bid_Qty_7  <- Bid_Ord_7  <-  Bid_PX_6  <-  Bid_Qty_6  <- Bid_Ord_6  <- Bid_PX_5  <-  Bid_Qty_5  <- Bid_Ord_5  <-
    Bid_PX_4  <-  Bid_Qty_4  <- Bid_Ord_4  <- id_PX_3  <-  Bid_Qty_3  <- Bid_Ord_3  <- Bid_PX_2  <-  Bid_Qty_2   <-Bid_Ord_2   <-Bid_PX_1   <- Bid_Qty_1  <-
    Bid_Ord_1  <- Ask_PX_1  <-  Ask_Qty_1  <- Ask_Ord_1  <- Ask_PX_2  <-  Ask_Qty_2  <- Ask_Ord_2   <- Ask_PX_3   <- Ask_Qty_3   <-Ask_Ord_3  <- Ask_PX_4   <-
    Ask_Qty_4  <- Ask_Ord_4  <- Ask_PX_5  <-  Ask_Qty_5  <- Ask_Ord_5  <- Ask_PX_6   <- Ask_Qty_6  <-  Ask_Ord_6  <- Ask_PX_7  <-  Ask_Qty_7  <- Ask_Ord_7  <-
    Ask_PX_8   <- Ask_Qty_8  <- Ask_Ord_8  <- Ask_PX_9  <-  Ask_Qty_9   <-Ask_Ord_9 <-  Ask_PX_10  <- Ask_Qty_10  <-Ask_Ord_10  <- MsgSeq   <- NULL

  fields <- side <- depth <- values <- px <- Q <- O <- cum_Q <- type <- NULL

  if (is.data.table(mbp_order_book)){
    if("DT" %in% colnames(mbp_order_book) == TRUE){

      setnames(mbp_order_book, "DT", "Time")

    } else {
      stop("Input has to be data.table")
    }
  }


  if("DT" %in% colnames(mbp_order_book) == T){

    setnames(mbp_order_book, "DT", "Time")

  }


  if(class(mbp_order_book$Time)[1]=="character"){

    mbp_order_book[, Time:= paste0(substr(Time, 1, 4), "-", substr(Time, 5, 6), "-",
                                   substr(Time, 7, 8),
                                   " ", substr(Time, 9, 10),
                                   ":", substr(Time, 11, 12), ":", substr(Time, 13, 14),
                                   ".", substr(Time, 15, 23))] ## dafult as.posixct time format

    mbp_order_book <- mbp_order_book[, `:=`(Time=as.POSIXct(Time, tz="GMT", "%Y-%m-%d %H:%M:%OS"))]

  }

  ## change timezone to UTC
  start_timestamp <- as.POSIXct(start_timestamp, trading.tz)
  end_timestamp <- as.POSIXct(end_timestamp, trading.tz)
  attr(start_timestamp, "tzone") <- "GMT"
  attr(end_timestamp, "tzone") <- "GMT"

  mbp_order_book <- mbp_order_book[Time %between% c(start_timestamp, end_timestamp)][, -c("Date", "Seq", "MsgSeq")]
  attr(mbp_order_book$Time, "tzone") <- trading.tz

  if(is.null(level)==F){

    columns <- c(unlist(mapply(function(x) c(paste0("Bid_PX_", x), paste0("Bid_Qty_", x), paste0("Bid_Ord_", x)), level:1)),
                 unlist(mapply(function(x) c(paste0("Ask_PX_", x), paste0("Ask_Qty_", x), paste0("Ask_Ord_", x)), 1:level)))

    columns_delete <- colnames(mbp_order_book)[5: (length(colnames(mbp_order_book))-1)][which(colnames(mbp_order_book)[5: (length(colnames(mbp_order_book))-1)] %in% columns ==F)]

    mbp_order_book <- mbp_order_book[, columns_delete:=NULL]
  }

  mbp_order_book <- melt(data, c("Time", "Code"), measure.vars =patterns("PX_|Qty_|Ord_"),
                         variable.name= "fields",
                         value.name = c("values"))[, `:=`(fields=substr(fields, 5, 5), side=substr(fields, 1, 3),
                                                depth=as.numeric(fifelse(str_sub(fields, -1)=="0", "10", str_sub(fields, -1))))][, px:=values[fields=="P"], by=c("Time", "side", "depth")][! fields=="P"]

  mbp_order_book <- dcast(mbp_order_book, Time + Code + px + side + depth ~ fields, value.var = c("values"))

  setkey(mbp_order_book, Time, side, depth)


  heatmap_book <- ggplot(mbp_order_book)+
    geom_tile(data = mbp_order_book[side=="Ask"], aes(x =Time, y = px, fill = Q),
             stat="identity", alpha=0.5) +
    geom_line(data = mbp_order_book[side=="Ask" & depth==1], aes(x = Time, y = px), color="blue") +
    labs(fill = "Liquidity: Ask")+

    scale_fill_gradientn(
      colors = c("green1", "yellow"),  ### Ask
      limits = c(mbp_order_book[side == "Ask", min(Q)], mbp_order_book[side == "Ask", max(Q)]),
      guide = guide_legend(order = 1, reverse = TRUE)
    )+

    new_scale_fill() +
    geom_tile(data = mbp_order_book[side=="Bid"], aes(x = Time, y = px, fill = Q),
             stat="identity", alpha=0.5) +
    geom_line(data = mbp_order_book[side=="Bid" & depth==1], aes(x = Time, y = px), color="black")+
    labs(fill = "Liquidity: Bid")+

    scale_fill_gradientn(
      colors = c("red1", "yellow"),  ###Bid
      limits = c(mbp_order_book[side == "Bid", min(Q)], mbp_order_book[side == "Bid", max(Q)]),
      guide = guide_legend(order = 2, reverse = TRUE)
    )+
    labs(title = paste0(mbp_order_book[, .SD[1,Time]],"--", mbp_order_book[, .SD[.N,Time]], "   ", mbp_order_book[, Code]), ## bbo info
         x = "Price",
         y = "Quantity") +
    theme_minimal()

  return(heatmap_book)

}
