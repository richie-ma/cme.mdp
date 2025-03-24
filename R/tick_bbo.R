

#' Visualization of trade price and best-bid-offer prices
#'
#' `tick_bbo()` plots the trade price and best-bid-offer prices during a period.
#'
#' @param mbp_order_book Limit order book data generated via `order_book()`.
#' @param trade_summary Trade summary data generated via `trade_summary()`.
#' @param trading.tz Time zone of the limit order book and trade summary data.
#' Default is \code{America/Chicago}.
#' @param start_timestamp The starting timestamp of the data for the plot in \code{HH:MM:SS} format.
#' @param end_timestamp The ending timestamp of the data for the plot in \code{HH:MM:SS} format.
#'
#' @returns A ggplot2 figure of trade price and best-bid-offer prices.
#' @export
#'
#' @import data.table ggplot2 ggnewscale
#'
#' @examples
#' # This function requires a CME data license to run
#' # Starting timestamp 08:30:00
#' # Ending timestamp 08:32:00
#' \dontrun{
#' tick_plot <- tick_bbo(book, trade, trading.tz = "America/Chicago",
#' start_timestamp = "08:30:00", end_timestamp = "08:32:00")
#' }

tick_bbo <- function(mbp_order_book,
                     trade_summary,
                     trading.tz = "America/Chicago",
                     start_timestamp,
                     end_timestamp) {
  Date <- Time <- Seq  <- Code   <-    Bid_PX_10 <-  Bid_Qty_10 <- Bid_Ord_10 <-  Bid_PX_9 <-  Bid_Qty_9 <- Bid_Ord_9  <- Bid_PX_8  <-
    Bid_Qty_8  <- Bid_Ord_8  <- Bid_PX_7  <-  Bid_Qty_7  <- Bid_Ord_7  <-  Bid_PX_6  <-  Bid_Qty_6  <- Bid_Ord_6  <- Bid_PX_5  <-  Bid_Qty_5  <- Bid_Ord_5  <-
    Bid_PX_4  <-  Bid_Qty_4  <- Bid_Ord_4  <- id_PX_3  <-  Bid_Qty_3  <- Bid_Ord_3  <- Bid_PX_2  <-  Bid_Qty_2   <-
    Bid_Ord_2   <- Bid_PX_1   <- Bid_Qty_1  <-
    Bid_Ord_1  <- Ask_PX_1  <-  Ask_Qty_1  <- Ask_Ord_1  <- Ask_PX_2  <-  Ask_Qty_2  <- Ask_Ord_2   <- Ask_PX_3   <- Ask_Qty_3   <-
    Ask_Ord_3  <- Ask_PX_4   <-
    Ask_Qty_4  <- Ask_Ord_4  <- Ask_PX_5  <-  Ask_Qty_5  <- Ask_Ord_5  <- Ask_PX_6   <- Ask_Qty_6  <-  Ask_Ord_6  <- Ask_PX_7  <-  Ask_Qty_7  <- Ask_Ord_7  <-
    Ask_PX_8   <- Ask_Qty_8  <- Ask_Ord_8  <- Ask_PX_9  <-  Ask_Qty_9   <-
    Ask_Ord_9 <-  Ask_PX_10  <- Ask_Qty_10  <- Ask_Ord_10  <- MsgSeq   <- NULL

  PX <- Size <- agg <-  trd_price <- trd_size <- trd_agg <- trd_price_fill <- NULL

  if (class(mbp_order_book$Time)[1] == "character") {
    mbp_order_book[, Time := paste0(
      substr(Time, 1, 4),
      "-",
      substr(Time, 5, 6),
      "-",
      substr(Time, 7, 8),
      " ",
      substr(Time, 9, 10),
      ":",
      substr(Time, 11, 12),
      ":",
      substr(Time, 13, 14),
      ".",
      substr(Time, 15, 23)
    )] ## dafult as.posixct time format

    mbp_order_book <- mbp_order_book[, `:=`(Time = as.POSIXct(Time, tz =
                                                                "GMT", "%Y-%m-%d %H:%M:%OS"))]

  }

  if (class(trade_summary$Time)[1] == "character") {
    trade_summary[, Time := paste0(
      substr(Time, 1, 4),
      "-",
      substr(Time, 5, 6),
      "-",
      substr(Time, 7, 8),
      " ",
      substr(Time, 9, 10),
      ":",
      substr(Time, 11, 12),
      ":",
      substr(Time, 13, 14),
      ".",
      substr(Time, 15, 23)
    )] ## dafult as.posixct time format

    trade_summary <- trade_summary[, `:=`(Time = as.POSIXct(Time, tz = "GMT", "%Y-%m-%d %H:%M:%OS"))]

  }


  ## change timezone to UTC
  start_timestamp <- as.POSIXct(start_timestamp, trading.tz)
  end_timestamp <- as.POSIXct(end_timestamp, trading.tz)
  attr(start_timestamp, "tzone") <- "GMT"
  attr(end_timestamp, "tzone") <- "GMT"

  trade_summary <- trade_summary[Time %between% c(start_timestamp, end_timestamp)][, list(
    Time,
    Code,
    trd_price = PX,
    trd_size = Size,
    trd_agg = agg
  )]

  mbp_order_book <- mbp_order_book[Time %between% c(start_timestamp, end_timestamp)][, c("Time",
                                                                                         "Code",
                                                                                         "Bid_PX_1",
                                                                                         "Ask_PX_1",
                                                                                         "Bid_Qty_1",
                                                                                         "Ask_Qty_1")]
  attr(mbp_order_book$Time, "tzone") <- trading.tz
  attr(trade_summary$Time, "tzone") <- trading.tz
  ## merge the two datasets with full join

  plot_data <- merge(trade_summary, mbp_order_book, all = TRUE)
  plot_data[, c("trd_price_fill")] <- nafill(plot_data[, trd_price], "locf")


  attr(plot_data$Time, "tzone") <- trading.tz
  #plot_data[, c("trd_price", "trd_size", "trd_agg")] <- nafill(plot_data[, .(trd_price, trd_size, trd_agg)], "locf")



  tickbbo <- ggplot() +
    geom_line(
      data = plot_data,
      aes(x = Time, y = Bid_PX_1, color = 'Bid_PX_1'),
      alpha = 1,
      linewidth = 1
    ) +
    geom_line(
      data = plot_data,
      aes(x = Time, y = Ask_PX_1, color = 'Ask_PX_1'),
      alpha = 1,
      linewidth = 1
    ) +
    geom_step(
      data = plot_data,
      aes(x = Time, y = trd_price_fill, color = 'trd_price_fill'),
      alpha = 1,
      linewidth = 1
    ) +
    scale_color_manual(
      name = NULL,
      values = c(
        'Ask_PX_1' = "green",
        'Bid_PX_1' = "red",
        'trd_price_fill' = "blue"
      ),
      labels = c("Best Bid", "Best Offer", "Last Trade")
    ) +
    geom_point(
      data = plot_data[trd_agg == 1],
      aes(x = Time, y = trd_price, size = trd_size),
      shape = 17,
      color = 'springgreen4',
      fill = 'springgreen4',
      alpha = 1
    ) +
    scale_size_continuous(name = "Buy Trade Size") +

    new_scale("size") +

    geom_point(
      data = plot_data[trd_agg == 2],
      aes(x = Time, y = trd_price, size = trd_size),
      shape = 25,
      color = 'firebrick',
      fill = 'firebrick',
      alpha = 1
    ) +
    scale_size_continuous(name = "Sell Trade Size") +

    new_scale("size") +
    geom_point(
      data = plot_data[trd_agg == 0],
      aes(x = Time, y = trd_price, size = trd_size),
      shape = 1,
      color = 'orange',
      fill = 'orange',
      alpha = 1,
      show.legend = F
    ) +
    geom_ribbon(
      data = plot_data,
      aes(x = Time, ymin = Bid_PX_1, ymax = Ask_PX_1),
      fill = 'gray',
      alpha = 0.25
    ) +

    scale_x_datetime(labels = scales::date_format("%H:%M:%S", tz = trading.tz)) +

    #mbp_order_book2[.N, Bid_Qty_1], " x ", mbp_order_book2[.N, Ask_Qty_1]), ## bbo info
    labs(title = plot_data[, unique(Code)], x = "Time", y = "Price") +
    theme_minimal()



  # plot_data[, Time2:=1:.N]
  #
  # tickbbo <- ggplot()+
  #   geom_line(data = plot_data, aes(x = Time2, y = Bid_PX_1, color='Bid_PX_1'), alpha=1, linewidth=1) +
  #   geom_line(data = plot_data, aes(x = Time2, y = Ask_PX_1, color='Ask_PX_1'), alpha=1, linewidth=1) +
  #   geom_step(data = plot_data, aes(x = Time2, y = trd_price_fill, color='trd_price_fill'), alpha=1, linewidth=1) +
  #   scale_color_manual(name = NULL, values = c('Ask_PX_1'="green",'Bid_PX_1'= "red",'trd_price_fill'="blue"),
  #                      labels = c("Best Bid", "Best Offer", "Last Trade"))+
  #   geom_point(data = plot_data[trd_agg==1], aes(x = Time2, y = trd_price, size = trd_size),
  #              shape = 17,color='springgreen4', fill='springgreen4', alpha=1) +
  #   scale_size_continuous(name="Buy Trade Size")+
  #
  #   new_scale("size")+
  #
  #   geom_point(data = plot_data[trd_agg==2], aes(x = Time2, y = trd_price, size = trd_size),
  #              shape = 25, color='firebrick', fill='firebrick', alpha=1) +
  #   scale_size_continuous(name="Sell Trade Size")+
  #
  #   new_scale("size")+
  #   geom_point(data = plot_data[trd_agg==0], aes(x = Time2, y = trd_price, size = trd_size),
  #              shape = 1, color='orange', fill='orange', alpha=1, show.legend = F) +
  #   geom_ribbon(data = plot_data, aes(x = Time2, ymin = Bid_PX_1, ymax = Ask_PX_1), fill='gray', alpha=0.25) +
  #
  #   scale_x_continuous( breaks = plot_data[seq(1, .N, 5000),Time2], labels = plot_data[seq(1, .N, 5000),substr(Time, 12, 19)])+
  #
  #   #mbp_order_book2[.N, Bid_Qty_1], " x ", mbp_order_book2[.N, Ask_Qty_1]), ## bbo info
  #   labs(title = plot_data[, unique(Code)],
  #        x = "Price",
  #        y = "Time") +
  #   theme_minimal()+
  #   view_step_manual(
  #     pause_length = 0,
  #     step_length = 100,
  #     xmin = plot_data[1:(.N-100), Time2],
  #     xmax = plot_data[101:.N, Time2],
  #     ymin = plot_data[, min(Bid_PX_1, na.rm = T)],
  #     ymax = plot_data[, max(Ask_PX_1, na.rm = T)],
  #     wrap = FALSE,
  #     ease = 'linear')
  #  ### It shouldn't be view_zoom_manual, especially when you have a pretty long time series.
  #
  #
  # tickbbo <- animate(tickbbo, fps=10, height=400, width = 1000)




  return(tickbbo)

}
