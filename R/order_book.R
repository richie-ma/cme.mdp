


#' Reconstruct limit order book(s) from the quote messages extracted from the CME Market by Price data
#'
#'`order_book()` reconstructs limit order book(s) from the quote messages extracted from the CME Market by Price data.
#' This function can process outright limit order book that is reconstructed by all outright quotes, implied order book
#' that is reconstructed by all implied quotes, or consolidated order book that aggregates information of both outright
#' and implied quotes. Outright order book can be supported up to 10 depths, while implied order book can be supported up
#' to 2 depths, according the CME definition.
#'
#' @param mbp_quote_msgs_list A list of quote messages extracted from the MBP data using `quote_messages()`, which must \bold{include} Sunday's file.
#' @param level The maximum book depth that can be supported by the CME, which could be 5 or 10 for futures, 3 for options. Users can choose any levels below the maximum.
#' @param consolidate If \code{TRUE} (default), return the consolidated order book.
#' @param security A symbol of a single contract of a financial security or a vector of multiple single contracts, should be
#' in the format like "CLK9" (crude oil), "ESH9" (E-mini S&P 500), etc. These can
#' be obtained from CME website for a contract specification.
#'
#' @returns A list of outright, implied, and consolidated (if \code{consolidate} = \code{TRUE})
#' limit order books for all tradeable contracts in a trading day stored in a list.
#' @export
#'
#' @import data.table
#'
#' @section Why \code{mdp_quote_msgs_list} needs Sunday's file:
#' One should store all quote messages extracted from the MBP data including Sunday's file
#' in a list because CME disseminates the initial snapshot of the book on Sunday. To ensure the
#' accuracy of book reconstruction, the Sunday's file is needed.
#'
#' @section Processing specific contract(s):
#' In practice, users might not need to process the limit order books of all tradable contracts,
#' which is substantially time-consuming. This function provides a way to only reconstruct the
#' limit order book of a single or multiple trade contracts.
#'
#' @section Limit order book reconstruction:
#' Outright limit order book and implied limit order book are reconstructed separately at first
#' and they should be consolidated finally according to book depths. For the addition update,
#' it indicates a better price than a current book depth enters the market and one should put the previous depths backward,
#' and fill up to ten depths of each side. When a message updated with modification action, one only needs to update the quantity
#' and number of orders with respect to a book depth, while no move occurs. In terms of cancellation,
#' it indicates a current depth will be deleted in a side of the book. It also moves the rest depths forward and fill the first ten depths.
#' Implied limit order book could be constructed with the same logic though the number of orders for each level is not defined in the MBP.
#' The consolidated LOB can be reconstructed according to timestamps or sequence numbers chronologically.
#' If a book depth that exists in the outright book, the quantity of implied book is added to the outright book to reconstruct the
#' consolidated book. When a book depth of implied book is better than the outright book, this depth for the consolidated LOB is the one at implied book
#' and one should move book depths behind this level in the outright LOB backward. Consolidated book is the one that traders monitor during trading.
#'
#' This code does not need explicit loops, which substantially improves the speed of processing.
#'
#' @section Level of limit order book:
#' One should reconstruct the limit order book with the maximum book depth stipulated by the CME to avoid possible errors.
#'
#' @seealso [quote_message()]
#'
#' @examples
#' # This function requires a CME data license to run
#' # Example showing how to reconstruct the limit order book(s)
#' # Know book depth
#' \dontrun{
#' book <- order_book(weekly_msg_quotes_list, "2019-01-07", level = 10)
#' }
#'
mbp_order_book <- function(mbp_quote_msgs_list,
                           level = NULL,
                           consolidate = TRUE,
                           security = NULL) {
  Date <- Code <- Implied <- Symbol  <- Seq <- Side <- qty_diff <- Update <- Qty <- PX <-
    ord_diff <- Ord <- Qty <- PX_depth <- current_price <- depth <- NULL

  ### Order book reconstruction is based on each contract


  if (is.list(mbp_quote_msgs_list) == FALSE) {
    stop("Input should be a list that consists all MDP quote messages")

  }

  if (is.null(security)) {
    message_all <-  split(rbindlist(lapply(mbp_quote_msgs_list, rbindlist)), by =
                            "Code")

  } else{
    message_all <-  split(rbindlist(lapply(mbp_quote_msgs_list, rbindlist)), by =
                            "Code")[security]

    if (length(list) == 0) {
      stop('Cannot find the security in the data input')
    }
  }

  rm(mbp_quote_msgs_list)


  message_all <- lapply(message_all, unique)


  cat("Starting limit order book processing...\n")

  order_book_single <- function(messages, consolidate, level, ...) {
    cat("Processing", messages[, unique(Code)], "...\n")
    messages$Update <- as.numeric(messages$Update)
    messages$Seq <- as.numeric(messages$Seq)
    messages$PX <- as.numeric(messages$PX)
    messages$Qty <- as.numeric(messages$Qty)
    messages$Ord <- as.numeric(messages$Ord)
    messages$PX_depth <- as.numeric(messages$PX_depth)
    messages$MsgSeq <- as.numeric(messages$MsgSeq)


    setkey(messages, Seq)

    messages[Side == "E", Side := 0]
    messages[Side == "F", Side := 1]
    messages[, Side := as.numeric(Side)]

    # calculating the quantity and order updated
    # Please note that if the previous message is deletion, one should not do
    # such a calculation as the submission message will only occur when
    # the previous message is a cancellation

    message_outright <- messages[Implied == "N"]

    message_outright[, qty_diff := fifelse(shift(Update, 1, "lag", fill = NA) !=
                                             2,
                                           Qty - shift(Qty, 1, "lag", fill = NA),
                                           Qty), by = c("Side", "PX")]


    message_outright[, ord_diff := fifelse(shift(Update, 1, "lag", fill = NA) !=
                                             2,
                                           Ord - shift(Ord, 1, "lag", fill = NA),
                                           Ord), by = c("Side", "PX")]

    message_outright[Update == 2, `:=`(qty_diff = -Qty, ord_diff = -Ord)]
    message_outright[is.na(qty_diff), qty_diff := Qty]
    message_outright[is.na(ord_diff), ord_diff := Ord]
    message_outright[, `:=`(Qty = cumsum(qty_diff), Ord = cumsum(ord_diff)), by =
                       c("Side", "PX")]

    setkey(message_outright, Seq)

    if ("Y" %in% messages[, unique(Implied)]) {
      message_implied <- messages[Implied == "Y"]

      ## fill the implied order book order as 0
      message_implied[Implied == "Y", Ord := 0]


      message_implied[, qty_diff := fifelse(shift(Update, 1, "lag", fill = NA) !=
                                              2,
                                            Qty - shift(Qty, 1, "lag", fill = NA),
                                            Qty), by = c("Side", "PX")]


      ## deletion quantity and orders should be negative

      message_implied[Update == 2, `:=`(qty_diff = -Qty)]
      message_implied[is.na(qty_diff), qty_diff := Qty]

      ## calculate the cumsum

      message_implied[, `:=`(Qty = cumsum(qty_diff)), by = c("Side", "PX")]



      setkey(message_implied, Seq)
    }



    ## limit order book reconstruction

    book_reconstruction_main <- function(quotes,
                                         level = NULL,
                                         conso_quotes = FALSE) {
      if (!conso_quotes) {
        quotes <- quotes[!Qty == 0]
      }


      bid <- quotes[Side == 0]

      if (dim(bid)[1] != 0) {
        if (is.null(level)) {
          bid_level <- bid[, length(unique(PX_depth))]
        } else{
          if (level > bid[, length(unique(PX_depth))]) {
            stop("Price level given is greater than what the data suggest")
          } else{
            bid_level <- level
          }
        }
        bid_rank <- dcast(bid, Seq ~ PX_depth, value.var = c("PX"))
        bid_rank[, 2:(bid_level + 1)] <- nafill(bid_rank[, 2:(bid_level + 1)], "locf")
        bid_rank[, 2:(bid_level + 1)] <- nafill(bid_rank[, 2:(bid_level + 1)], "const", 0)
        bid_rank[, current_price := bid[, PX]]


        bid_rank[, depth := apply(.SD, 1, function(row) {
          which(unlist(row)[(bid_level + 2)] == sort(unique(unlist(row)[2:(bid_level + 1)][which(unlist(row)[2:(bid_level +
                                                                                                                  1)] >
                                                                                                   0)]), decreasing = TRUE))
        })]

        bid[, PX_depth := bid_rank[, depth]]

      }

      #### offer side ###
      offer <- quotes[Side == 1]

      if (dim(offer)[1] != 0) {
        if (is.null(level)) {
          ## note it should not be the max one, you should use the count of unique numbers
          offer_level <- offer[, length(unique(PX_depth))]
        } else{
          if (level > offer[, length(unique(PX_depth))]) {
            stop("Price level given is greater than what the data suggest")
          } else{
            offer_level <- level
          }
        }


        offer_rank <- dcast(offer, Seq ~ PX_depth, value.var = c("PX"))
        offer_rank[, 2:(offer_level + 1)] <- nafill(offer_rank[, 2:(offer_level + 1)], "locf")
        offer_rank[, 2:(offer_level + 1)] <- nafill(offer_rank[, 2:(offer_level + 1)], "const", 0)
        offer_rank[, current_price := offer[, PX]]

        offer_rank[, depth := apply(.SD, 1, function(row) {
          which(unlist(row)[(offer_level + 2)] == sort(unique(unlist(row)[2:(offer_level + 1)][which(unlist(row)[2:(offer_level +
                                                                                                                      1)] >
                                                                                                       0)]), decreasing = FALSE))
        })]

        offer[, PX_depth := offer_rank[, depth]]

      }

      quotes <- rbind(bid, offer)
      setkey(quotes, Seq)

      quotes[, Side := fifelse(Side == 0, "Bid", "Ask")]

      book <- dcast(
        quotes,
        Date + MsgSeq + SendingTime + TransactTime + Code + Seq ~ Side + PX_depth,
        value.var = c("PX", "Qty", "Ord")
      )
      setkey(book, Seq)

      book[, 7:dim(book)[2]] <- nafill(book[, 7:dim(book)[2]], "locf")
      book[, 7:dim(book)[2]] <- nafill(book[, 7:dim(book)[2]], "const", 0)

      return(book)

    }


    if ((exists("message_outright")) &
        (!exists("message_implied"))) {
      if (isTRUE(consolidate)) {
        LOB_conso <- book_reconstruction_main(message_outright)
      } else{
        LOB_conso <- NULL
      }
      LOB_outright <- book_reconstruction_main(message_outright)
      LOB_implied <- NULL
      cat(
        "No implied orders and the consolidated limit order book is the same as the outright limit order book\n"
      )

    } else if ((!exists("message_outright")) &
               (exists("message_implied"))) {
      if (isTRUE(consolidate)) {
        LOB_conso <- book_reconstruction_main(message_implied)
      } else{
        LOB_conso <- NULL
      }
      LOB_implied <- book_reconstruction_main(message_implied)
      LOB_outright <- NULL
      cat(
        "No outright orders and the consolidated limit order book is the same as the implied limit order book\n"
      )
    } else{
      LOB_implied <- book_reconstruction_main(message_implied)
      LOB_outright <- book_reconstruction_main(message_outright)
      if (isTRUE(consolidate)) {
        cat("Both outright order book and implied order book detected\n")

        conso_quotes <- rbind(message_outright, message_implied, fill = TRUE)
        conso_quotes[is.na(ord_diff), ord_diff := 0]
        setkey(conso_quotes, Seq)

        ### aggregate the implied quantity and outright quantity
        conso_quotes[, `:=`(Qty = cumsum(qty_diff), Ord = cumsum(ord_diff)), by = c("Side", "PX")]
        LOB_conso <- book_reconstruction_main(conso_quotes)

      } else{
        LOB_conso <- NULL
      }

    }

    results <- list(
      LOB_conso = LOB_conso,
      LOB_outright = LOB_outright,
      LOB_implied = LOB_implied
    )
    return(results)
  }

  books <- lapply(message_all,
                  order_book_single,
                  level = level,
                  consolidate = consolidate)
  return(books)


}
