

#' Build the TBBO data from the trades and quotes data
#'
#' `tbbo()` builds the TBBO data where records the trade summary with best-bid-offer
#' information immediately prior to trades happening. This function also provides
#' Lee and Ready (1991)'s trade assignment assessment.
#'
#' @param trades Trade summary data generated from `trade_summary()`.
#' @param book Limit order book data generated from `order_book()`.
#' @param merge The method to merge the trade and quote data, which could be either
#' Transaction Time (\code{TransacTime}) or Sequence Number (\code{Seq_number}).
#' @param assign_trades Assess trade direction based on Lee and Ready (1991), including
#' both quote test and tick test
#'
#' @returns A data.table contains the trades and quotes infomation immediately prior to the trades
#' @export
#' @references Charles M. C. Lee, and Mark J. Ready. 1991. “Inferring Trade Direction from Intraday Data.” Journal of Finance 46, 733–46.
#' @import data.table
#'
#' @examples
#' # This function requires a CME data license to run
#' # Example showing how to build the TBBO
#' # Merge by Transaction Time and assess trade direction
#' \dontrun{
#' TBBO <- tbbo(trades, book, merge = "TransactTime", assign_trades = TRUE)
#'
#' # Merge by Sequence Number and assess trade direction
#' TBBO <- tbbo(trades, book, merge = "Seq_number", assign_trades = TRUE)
#' }
#'
tbbo <- function(trades,
                 book,
                 merge = c("TransactTime", "Seq_number"),
                 assign_trades = FALSE) {
  Date <- Seq <- Bid_PX_1 <- Bid_Qty_1 <- Bid_Ord_1 <- Ask_PX_1 <- Ask_Qty_1 <- Ask_Ord_1 <-
    TransactTime <- agg <- PX <- NULL

  if (is.data.table(trades) & is.data.table(book)) {
    if (trades[, as.Date(unique(Date))] != book[, as.Date(unique(Date))]) {
      stop('Trading dates of book and trades need to be the same')

    }

  } else {
    stop("Inputs have to be data.table")
  }



  ## TBBO should merge the trades with the quotes immediately before trades happen

  ## merged by the sequence number-- More accurate

  if (merge == "Seq_number") {
    tbbo <- book[, list(Seq,
                        Bid_PX_1,
                        Bid_Qty_1,
                        Bid_Ord_1,
                        Ask_PX_1,
                        Ask_Qty_1,
                        Ask_Ord_1)][trades, on = c("Seq"), roll = Inf]



  } else if (merge == "TransactTime") {
    tbbo <- book[, list(TransactTime,
                        Bid_PX_1,
                        Bid_Qty_1,
                        Bid_Ord_1,
                        Ask_PX_1,
                        Ask_Qty_1,
                        Ask_Ord_1)][trades, on = c("TransactTime"), roll = Inf]




  } else{
    stop('Merge between trades and quotes should be either TransactTime or Seq')

  }

  setcolorder(
    tbbo,
    c(
      "Date",
      "MsgSeq",
      "SendingTime",
      "TransactTime",
      "Code",
      "Seq",
      "PX",
      "Size",
      "Ord",
      "agg",
      "Bid_PX_1",
      "Bid_Qty_1",
      "Bid_Ord_1",
      "Ask_PX_1",
      "Ask_Qty_1",
      "Ask_Ord_1"
    )
  )


  if (isTRUE(assign_trades)) {
    ### Only assign trades that are not defined by the CME

    if (0 %in% tbbo[, unique(agg)] == FALSE) {
      stop( = )

    }

    setkey(tbbo, Seq)


    ### According to the Lee and Ready (1991)

    ## Lee & Ready (1991) Algorithm to infer trade direction for unassigned trades

    #### Nearly 95% of trades are assigned by the CME itself, and we only need to deal with the trades
    #### that have not been assigned by the CME

    ### Presumably, the most unassigned trades are those that match against the implied orders, or the combinations
    ### of implied orders and customer orders.

    ## quote test
    tbbo <- na.omit(tbbo)
    tbbo <- tbbo[agg == 0, agg := fifelse(PX > (Bid_PX_1 + Ask_PX_1) / 2, 1, fifelse(PX <  (Bid_PX_1 + Ask_PX_1) /
                                                                                       2, 2, 0))]

    ## tick test



    repeat {
      if (1 %in% tbbo[agg == 0, which = TRUE]) {
        ## The first rwo is with agg = 0, needs to stop
        break
      }

      tbbo[, agg := fifelse(agg != 0, agg, fifelse(
        agg == 0 & 1 == shift(agg, 1, "lag", fill = NA),
        1,
        fifelse(agg == 0 &
                  2 == shift(agg, 1, "lag", fill = NA), 2, 0)
      ))]

      if (tbbo[agg == 0, .N] == 0) {
        break
      }
    }

  }

  return(tbbo)



}
