

#' Extract quote messages from CME Market by Price data
#'
#' `quote_message()` extracts all quotes messages based on price levels in a
#' limit order from the whole CME Market by Price (MBP) data, including
#' timestamps, quote update (add/modify/cancel),
#' quote price, quote side (bid or offer), quote depth, quote quantity,
#' number of orders in a price depth, etc. Implied quote messages are also
#' considered if they are available in a market.
#'
#' @param mbp_input Market by Price raw data.
#' @param date Trading date associated with the raw data, which can be found in
#' the file name easily in \code{YYYYMMDD} format. This argument is required to select
#' the parsing format.
#' @param price_displayformat Display format of trade price, which could be 1,
#' 0.01, or 0.001, etc. If it is unknown, the nearest Sunday's (not a CME trading holiday)
#' MBP/MBO data need to be provided to extract the display format.
#' @param sunday_input When \code{price_displayformat} is not provided, the nearest
#' Sunday's (not a CME trading holiday) MBP/MBO data needs to be provided to extract
#' the price display format.
#'
#' @returns a list of data.table that contains all quote messages (including
#' implied quote messages if they are available) for all tradeable contracts in a trading
#' day.
#' @seealso [meta_data()]
#' @export
#'
#' @import data.table stringr
#'
#' @section Quote messages:
#' Quote messages include both outright quotes and implied quotes. Outright quotes
#' are those submitted by real traders while the implied quotes are those generated through
#' other markets. In terms of outright quotes, each update defines how the outright limit order book updates for a book depth.
#' Quote messages only show updates to the book up to ten depths. The quoted quantity
#' and the number of orders resting in each depth can be defined for outright orders exclusively. For
#' the addition, it indicates a better price than a current quote entries the market.
#' When a message updated with modification, it indicates the quantity of an existing depth
#' is changed.In terms of update of cancellation, it indicates a current depth is deleted in the book.
#' Similarly, implied quote messages show updates for the implied limit order book and only two depths are dissminated.
#'
#' @section Fields of quote messages:
#' \itemize{
#' \item Date: Trading date. The night trading session on Sundays is assigned
#' the trading date as the next Monday.
#' \item MsgSeq: Message sequence number for each packaet. This is unique for every
#' message, however, it is not security unique.
#' \item SendingTime: The timestamp (in UTC) indicates the time when outbound messages to clients
#' leave the segment gateway (Tag 60).
#' \item TransactTime: The timestamp (in UTC) indicates the time when incoming messages hit the segment gateway (Tag 52).
#' \item Code: Contract symbol.
#' \item Seq: Sequence number, which is security unique. A single packaet may contain information
#' for multiple tradeable contracts with the single message sequence number, while every
#' tradeable contract has its unique sequence number.
#' \item Update: Indicate whether the quote is for addition (0), modification (1), or cancellation (2).
#' \item Side: A quote is on either bid side (0 or E) or ask side (1 or F).
#' \item PX: Quoted price.
#' \item Size: Quote size displayed as the number of contracts for a depth in the book.
#' \item Ord: Number of limit orders joining the queue for a depth in the book.
#' \item Implied: Whether a quote is for either implied orders (Y) or outright orders (N).
#' \item PX_depth: Book depth where a quote update occurs.
#' For example, 1 indicates the BBO and 2 indicates the second best depth, etc. Outright books
#' support up to 10 depths, and implied books support up to 2 depths.
#'}
#'
#' @examples
#' # This function requires a CME data license to run
#' # Example showing how to extract quote messages
#' # extract quote messages
#' # Know the display format of quote price
#' \dontrun{
#' quotes <- quote_message(file, "2019-01-07", price_displayformat = 1)
#'
#' # Unknown price_displayformat
#' quotes <- quote_message(file, "2019-01-07", sunday_mbp_input = sunday_file)
#' }
#'
quote_message <- function(mbp_input,
                          date,
                          price_displayformat = NULL,
                          sunday_input = NULL) {
  Code <- Implied <- Seq <- PX <- DisplayFactor <- NULL

  date <- as.Date(date)

  if (inherits(date, "Date") == FALSE) {
    stop("date should be in the format as YYYY-MM-DD.")

  }



  data <- fread(mbp_input,
                header = F,
                sep = "\\",
                fill = TRUE)[[1L]]




  ## let R know whether it is FIX or MDP data
  ## For the FIX data

  if (date < "2015-11-20") {
    Index <- str_subset(data, "\001269=[01]|\001276=RK")
    rm(data)

    Index <- str_subset(Index, "\00135=X")
    Index <- str_replace_all(Index, "\001", ",")

    if (length(Index) != 0) {
      Index <-  str_replace_all(Index,
                                "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),",
                                "")
      Index <-  str_replace_all(Index, ",268=([^,]*),", ",")
      Index <-  str_replace_all(Index, ",22=([^,]*),", ",")
      Index <-  str_replace_all(Index, ",48=([^,]*),", ",")
      Index <-  str_replace_all(Index, ",273=([^,]*),", ",")
      Index <-  str_replace_all(Index, ",336=([^,]*),", ",")
      Index <-  str_replace_all(Index, ",10=([^,]*),", ",")
      Index <-  str_replace_all(Index, ",5797=([^,]*),", ",")


      ##----------------------------------------LOB cleaning-------------------------------------
      ###################################### outright orders only since they have the info of number of orders#################################
      ## we should find the following items
      ## in terms of add tag52 tag75 tag279=0(add) or 1(change) or 2(delete), tag83(sequence), tag107 (Code) tag269=0(bid) or 1(ask), tag270(PX),271(Qty),346(# of orders),1023(PX_depth)
      if (length(str_subset(Index, "269=[01]")) != 0) {
        part1 <- str_subset(Index,
                            "279=[012],83=([^,]*),107=([^,]*),269=([01]*),") ## find both tag269=0 and tag269=1
        part1_dt <- str_extract_all(part1, "34=([^,]*),52=([^,]*),75=([^,]*),")
        part1_info <- str_match_all(
          part1,
          "279=([^,]*),83=([^,]*),107=([^,]*),269=([^,]*),270=([^,]*),271=([^,]*),346=([^,]*),1023=([^,]*),"
        )
        n_row_part1_info <- sapply(part1_info, nrow)
        part1_dt <- str_dup(part1_dt, n_row_part1_info)
        part1_dt <- str_match_all(part1_dt, "34=([^,]*),52=([^,]*),75=([^,]*),")
        part1_dt <- as.data.table(do.call(rbind, part1_dt))[, -1]
        part1_info <- as.data.table(do.call(rbind, part1_info))[, -1]
        message <- cbind(part1_dt, part1_info)
        names(message)[c(1:11)] <- c(
          "MsgSeq",
          "SendingTime",
          "Date",
          "Update",
          "Seq",
          "Code",
          "Side",
          "PX",
          "Qty",
          "Ord",
          "PX_depth"
        )
        message[, Implied := "N"]


      }
      ####################################### Implied orders only #################################################
      #We should consider the implied order actually since it affects the limit order book
      ## we should find the following items
      ## in terms of add 279=0(add) or 1(change) or 2(delete), 269=E(implied bid) or 1(implied ask), 83(sequence),270(PX),271(Qty),346(# of orders),1023(PX_depth)
      if (length(str_subset(Index, "276=K")) != 0) {
        part2 <- str_subset(Index, "276=K") ## find both tag269=0 and tag269=1
        part2_dt <- str_extract_all(part2, "34=([^,]*),52=([^,]*),75=([^,]*),")
        part2_info <- str_match_all(
          part2,
          "279=([^,]*),83=([^,]*),107=([^,]*),269=([^,]*),270=([^,]*),271=([^,]*),276=([^,]*),1023=([^,]*),"
        )
        n_row_part2_info <- sapply(part2_info, nrow)
        part2_dt <- str_dup(part2_dt, n_row_part2_info)
        part2_dt <- str_match_all(part2_dt, "34=([^,]*),52=([^,]*),75=([^,]*),")
        part2_dt <- as.data.table(do.call(rbind, part2_dt))[, -1]
        part2_info <- as.data.table(do.call(rbind, part2_info))[, -1]
        message_2 <- cbind(part2_dt, part2_info)
        names(message_2)[c(1:11)] <- c(
          "MsgSeq",
          "SendingTime",
          "Date",
          "Update",
          "Seq",
          "Code",
          "Side",
          "PX",
          "Qty",
          "Implied",
          "PX_depth"
        )
        message_2[, Implied := "Y"]

        message_all <- rbind(message, message_2, fill = TRUE)

        message_all$MsgSeq <- as.numeric(message_all$MsgSeq)
        message_all$Update <- as.numeric(message_all$Update)
        message_all$Seq <- as.numeric(message_all$Seq)
        message_all$PX <- as.numeric(message_all$PX)
        message_all$Qty <- as.numeric(message_all$Qty)
        message_all$Ord <- as.numeric(message_all$Ord)
        message_all$PX_depth <- as.numeric(message_all$PX_depth)
        setkey(message_all, Code, Seq)

      } else{
        if (class(message)[1] == "data.table") {
          message_all <- message

        }
      }

      setcolorder(
        message_all,
        c(
          "Date",
          "MsgSeq",
          "SendingTime",
          "Code",
          "Seq",
          "Update",
          "Side",
          "PX",
          "Qty",
          "Ord",
          "Implied",
          "PX_depth"
        )
      )
      results <- split(message_all, by = "Code")
      #rm(message,message_2,part1_dt,part1_info,part2_dt,part2_info,Index,n_row_part1_info,n_row_part2_info,part1,part2)
    } else{
      results <- NULL
      cat("No quote information detected",
          "\n",
          "null list returned",
          "\n")
    }
  }
  else{
    ## remove the unnecessary tags
    Index <- str_subset(data, "\001269=[01EF]")
    rm(data)

    if (length(Index) != 0) {
      Index <- str_subset(Index, "\00135=X")
      Index <- str_replace_all(Index, "\001", ",")

      Index <-  str_replace_all(Index,
                                "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),",
                                "")
      Index <-  str_replace_all(Index, ",5799=([^,]*),", ",")
      Index <-  str_replace_all(Index, ",268=([^,]*),", ",")
      Index <-  str_replace_all(Index, ",48=([^,]*),", ",")
      Index <-  str_replace_all(Index, ",10=([^,]*),", ",")


      ##----------------------------------------LOB cleaning-------------------------------------
      ###################################### outright orders only since they have the info of number of orders#################################
      ## we should find the following items
      ## in terms of add 279=0(add) or 1(change) or 2(delete), 269=0(bid) or 1(ask), 83(sequence),270(PX),271(Qty),346(# of orders),1023(PX_depth)
      if (length(str_subset(Index, "269=[01]")) != 0) {
        part1 <- str_subset(Index, "279=[012],269=([01]*),") ## find both tag269=0 and tag269=1
        part1_dt <- str_extract_all(part1,
                                    "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
        part1_info <- str_match_all(
          part1,
          "279=([^,]*),269=([^,]*),55=([^,]*),83=([^,]*),270=([^,]*),271=([^,]*),346=([^,]*),1023=([^,]*),"
        )
        n_row_part1_info <- sapply(part1_info, nrow)
        part1_dt <- str_dup(part1_dt, n_row_part1_info)
        part1_dt <- str_match_all(part1_dt,
                                  "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
        part1_dt <- as.data.table(do.call(rbind, part1_dt))[, -1]
        part1_info <- as.data.table(do.call(rbind, part1_info))[, -1]
        message <- cbind(part1_dt, part1_info)
        names(message)[c(1:12)] <- c(
          "Date",
          "MsgSeq",
          "SendingTime",
          "TransactTime",
          "Update",
          "Side",
          "Code",
          "Seq",
          "PX",
          "Qty",
          "Ord",
          "PX_depth"
        )
        message[, Implied := "N"]


      }
      ####################################### Implied orders only #################################################
      #We should conisder the implied order actually since it affects the limit order book
      ## we should find the following items
      ## in terms of add 279=0(add) or 1(change) or 2(delete), 269=E(implied bid) or 1(implied ask), 83(sequence),270(PX),271(Qty),346(# of orders),1023(PX_depth)
      if (length(str_subset(Index, "279=[012],269=([EF]*),")) != 0) {
        part2 <- str_subset(Index, "279=[012],269=([EF]*),") ## find both tag269=0 and tag269=1
        part2_dt <- str_extract_all(part2,
                                    "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
        part2_info <- str_match_all(
          part2,
          "279=([^,]*),269=([^,]*),55=([^,]*),83=([^,]*),270=([^,]*),271=([^,]*),1023=([^,]*),"
        )
        n_row_part2_info <- sapply(part2_info, nrow)
        part2_dt <- str_dup(part2_dt, n_row_part2_info)
        part2_dt <- str_match_all(part2_dt,
                                  "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
        part2_dt <- as.data.table(do.call(rbind, part2_dt))[, -1]
        part2_info <- as.data.table(do.call(rbind, part2_info))[, -1]
        message_2 <- cbind(part2_dt, part2_info)
        names(message_2)[c(1:11)] <- c(
          "Date",
          "MsgSeq",
          "SendingTime",
          "TransactTime",
          "Update",
          "Side",
          "Code",
          "Seq",
          "PX",
          "Qty",
          "PX_depth"
        )
        message_2[, Implied := "Y"]



        message_all <- rbind(message, message_2, fill = TRUE)


        message_all$MsgSeq <- as.numeric(message_all$MsgSeq)
        message_all$Update <- as.numeric(message_all$Update)
        message_all$Seq <- as.numeric(message_all$Seq)
        message_all$PX <- as.numeric(message_all$PX)
        message_all$Qty <- as.numeric(message_all$Qty)
        message_all$PX_depth <- as.numeric(message_all$PX_depth)

        setkey(message_all, Code, Seq)
      } else{
        if (class(message)[1] == "data.table") {
          message_all <- message

        }


      }
      setcolorder(
        message_all,
        c(
          "Date",
          "MsgSeq",
          "SendingTime",
          "TransactTime",
          "Code",
          "Seq",
          "Update",
          "Side",
          "PX",
          "Qty",
          "Ord",
          "Implied",
          "PX_depth"
        )
      )
      results <- split(message_all, by = "Code")
      # rm(message,message_2,part1_dt,part1_info,part2_dt,part2_info,Index,n_row_part1_info,n_row_part2_info,part1,part2)
    } else{
      results <- NULL
      cat("No quote information detected",
          "\n",
          "null list returned",
          "\n")
    }

  }


  if (is.null(price_displayformat)) {
    if (is.null(sunday_input)) {
      stop(
        "Sunday's security definition at the same week must be provided to get the price display format"
      )
    }

    definition <- meta_data(sunday_input, date = date)

    setnames(definition, "Symbol", "Code")

    definition <- definition[Code %in% names(results)]

    results <- lapply(results, function(x) {
      x[, PX := as.numeric(PX) * definition[Code == unique(x$Code), as.numeric(DisplayFactor)]]
      return(x)
    })



  } else{
    results <- lapply(results, function(x){
      x[, PX := as.numeric(PX) * as.numeric(price_displayformat)]
      return(x)
    })

  }


  cat("CME MDP 3.0 Quote Messages",
      "\n",
      "contracts:",
      names(results))
  return(results)



}
