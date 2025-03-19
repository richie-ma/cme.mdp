
#' Extract individual order details from CME Market by Order data
#'
#' `mbo_quote_queue()` extracts individual order details for all outright limit orders
#'  from CME Market by Order data, including all fields extracted from `quote_messages()`
#'  and order IDs, order priority, etc.
#'
#' @param mbo_input A CME MBO raw data.
#' @param implied_quotes \code{FALSE} by default. if \code{TRUE}, return the implied quote messages.
#' @param price_displayformat Display format of trade price, which could be 1,
#' 0.01, or 0.001, etc. If it is unknown, the nearest Sunday's (not a
#' CME trading holiday) MBP/MBO data need to be provided to extract the display format.
#' @param sunday_input When \code{price_displayformat} is not provided, the nearest
#' Sunday's (not a national holiday)
#' MBP/MBO data need to be provided to extract the price display format.
#'
#' @returns a list of data.table that contains all individual order details
#' for all tradeable contracts in a trading day.
#' @seealso [quote_messages()], [meta_data()]
#' @export
#'
#' @import data.table stringr
#'
#' @section Individual order details:
#' MBO data record each order with a unique order ID though the order may be revised,
#' cancelled, filled, or discarded. The life of an order is tracked by the unique order ID. For each
#' update other than execution, MBO data assign order priority number, from lowest to highest
#' values, to position the order against other orders of the same side and price. According to
#' CME Globex rulebook, an order loses its original order priority if the order quantity increases
#' while the original order priority can be saved if the order quantity decreases. A lower value
#' of order priority number means a higher priority. Any revision of a limit order, including
#' price, and quantity will be recorded into MBO with the unique order ID. However, MBO data
#' do not flag what type of a limit order is, for instance, if an order is day order that will be
#' discarded automatically by CME if it does not conclude per se, or a GTC (Good till Cancel)
#' order that can rest in the LOB until this order will be filled. Similarly, the MBO does not
#' flag stop orders, a FOK (Fill or Kill) order, or a FAK (Fill and Kill) order per se. Market
#' orders are also assigned with unique order IDs and order priorities, which could be checked
#' at the first row of a trade record in general.Not all MBO messages are assigned unique sequence numbers
#' like the MBP but all messages are assigned message numbers though they may not be unique in a few
#' scenarios.
#'
#' @section Quote messages---MBO vs MBP:
#' CME generally uses two templates for disseminating the individual order details.
#' MBO update may correspond with an update of MBP. When it happens, the MBO data
#' disseminate this scenario with dual updates in a message under a combined template, which
#' indicates that the change in MBP is driven by the update in MBO. This message can clarify
#' the reason why the limit order book is updated from an order perspective. However,
#' an incremental refresh of MBO may not result
#' in an update of MBP since the order submitted at a price that where MBP cannot record.
#' This could happen in the following scenarios: 1) the depth is behind the best 10th level;
#' 2) A cluster of (or massive) order activities occur in a short period of time and MBP only
#' update their joint update; 3) When a submitted limit order that improves the best-bid-offer
#' price in book, the MBP may not be updated simultaneously but with certain time delay.
#' In this scenario, different message template is used. In addition, if an update of MBP is
#' not caused by an MBO update, the combined template will be applied but with asymmetric
#' refreshes. This scenario may take place when market orders consume all liquidity at one or
#' multiple depths and the rest depths need to be moved forward. This moving process is sent
#' by updating messages of MBP though no specific order activities are pertinent to this.
#'
#'
#' @examples
#' # This function requires a CME data license to run
#' # Example showing how to extract individual order details
#' # Know the display format of trade price
#' \dontrun{
#' order_details <- mbo_quote_queue(file, "2019-01-07", price_displayformat = 1)
#'
#' # Unknown price_displayformat
#' order_details <- mbo_quote_queue(file, "2019-01-07", sunday_input = sunday_file)
#' }
#'
mbo_quote_queue <- function(mbo_input,implied_quotes=FALSE,
                          price_displayformat=NULL, sunday_input=NULL){


  Code <- PX <- DisplayFactor <- Ref_ID <- NULL

  data <- fread(mbo_input, header = F, sep="\\", fill=TRUE)[[1L]]

### MBO is only available since 2017 so only one template is available.

  Index <- str_subset(data, "\001269=[01]|\001276=RK")
  rm(data)

  Index <- str_subset(Index, "\00135=X")
  Index <- str_replace_all(Index, "\001",",")

  ## select the new order submissions in the file
  ## there are two parts
  ## one is from the tag279=0 with tag269=0 or 1 (0: bid; 1: ask), tag37706 (order quantity), tag37707 (order priority, lower value higher priority)
  ## and tag37 (order id)

  Index <- str_replace_all(Index, "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),", "")
  Index <- str_replace_all(Index, ",5799=([^,]*),", ",")
  Index <- str_replace_all(Index, ",268=([^,]*),",",")
  Index <- str_replace_all(Index, ",48=([^,]*),",",")
  Index <- str_replace_all(Index, ",10=([^,]*),", ",")

  mbo <- list()
  ##----------------------------------------Order submission/modification/cancellation-------------------------------------

  ############################### Order submission/modification/cancellation part 1 #####################################
  ## MBO does not have a sequence number
  ## MBO and MDP combined format have sequence number

  ## bid/ask order (tag269=0 or 1)
  part1 <- str_subset(Index, "279=[012],269=([01]*),") ## find both tag269=0 and tag269=1

  if(length(part1)!=0){

  ## extract MBP info
  ## we need to include the implied orders here.
  part1_order <- str_subset(part1, "279=[012],269=([01]*),55=([^,]*),270=([^,]*),37706=([^,]*),37707=([^,]*),37=([^,]*),")
  info_part1 <- str_extract_all(part1_order, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
  order_part1 <- str_match_all(part1_order, "279=([012]*),269=([01]*),55=([^,]*),270=([^,]*),37706=([^,]*),37707=([^,]*),37=([^,]*),")
  n_row_1 <- sapply(order_part1, nrow)
  info_part1 <- str_dup(info_part1, n_row_1)
  info_part1 <- str_match_all(info_part1, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
  order_part1 <- as.data.table(do.call(rbind,order_part1))[,-1]
  info_part1 <- as.data.table(do.call(rbind, info_part1))[,-1]
  part1_order <- cbind(info_part1,order_part1)
  names(part1_order)[c(1:11)] <- c("Date","MsgSeq","SendingTime","TransactTime",
                                   "Update","Side","Code","PX","Qty","Order_priority","Order_id")

  part1_order$MsgSeq <- as.numeric(part1_order$MsgSeq)
  part1_order$Update <- as.numeric(part1_order$Update) # 1 represents submission, 2 represents modification, 3 represents cancellation
  part1_order$PX <- as.numeric(part1_order$PX)
  part1_order$Qty <- as.numeric(part1_order$Qty)
  part1_order$Order_priority <- as.numeric(part1_order$Order_priority)
  part1_order$Order_id <- as.numeric(part1_order$Order_id)
  mbo[[1]] <- part1_order
  rm(info_part1, order_part1, part1, part1_order)



  }
  ###################################### Complete submission/modification/cancellation part 1 ###############################################
  #
  # ### implied orders
  #

  if(isTRUE(implied_quotes)){

  part1.1 <- str_subset(Index, "279=[012],269=([EF]*),") ## find both tag269=0 and tag269=1

  if(length(part1.1)!=0){

    ## extract MBP info
    ## we need to include the implied orders here.
    part1.1_order <- str_subset(part1.1, "279=[012],269=([EF]*),55=([^,]*),83=([^,]*),270=([^,]*),271=([^,]*),")
    info_part1.1 <- str_extract_all(part1.1_order, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
    order_part1.1 <- str_match_all(part1.1_order, "279=([012]*),269=([EF]*),55=([^,]*),83=([^,]*),270=([^,]*),271=([^,]*),")
    n_row_1.1 <- sapply(order_part1.1, nrow)
    info_part1.1 <- str_dup(info_part1.1, n_row_1.1)
    info_part1.1 <- str_match_all(info_part1.1, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
    order_part1.1 <- as.data.table(do.call(rbind,order_part1.1))[,-1]
    info_part1.1 <- as.data.table(do.call(rbind, info_part1.1))[,-1]
    part1.1_order <- cbind(info_part1.1,order_part1.1)
    names(part1.1_order)[c(1:10)] <- c("Date","MsgSeq","SendingTime","TransactTime",
                                       "Update","Side","Code","Seq","PX","Qty")

    part1.1_order$MsgSeq <- as.numeric(part1.1_order$MsgSeq)
    part1.1_order$Update <- as.numeric(part1.1_order$Update) # 1 represents submission, 2 represents modification, 3 represents cancellation
    part1.1_order$PX <- as.numeric(part1.1_order$PX)
    part1.1_order$Qty <- as.numeric(part1.1_order$Qty)
    part1.1_order$Seq <- as.numeric(part1.1_order$Seq)

    mbo[[2]] <- part1.1_order
    rm(info_part1.1, order_part1.1, part1.1, part1.1_order)

  }

  }

  ############################ Order submission/modification/cancellation part 2 ############################################################
  ## multiple book update records
  ## pattern
  ## tag75, tag60, tag268, tag279, tag269, tag55, tag270, tag271, tag346, tag1023, tag37, tag37707, tag37706, tag9633, tag37708=0

  part2 <- str_subset(Index, "279=[012],269=([01]*),")
  part2 <- str_subset(part2, "9633=([^,]*),")

  if(length(part2)!=0){

  order_part2 <- str_subset(part2, "37=([^,]*),37707=([^,]*),37706=([^,]*),9633=([^,]*),37708=([^,]*),")

  order_part2.order <- str_match_all(order_part2, "37=([^,]*),37707=([^,]*),37706=([^,]*),9633=([^,]*),37708=([^,]*),")
  n_row_2 <- sapply(order_part2.order, nrow)

  rm(part2)

  for(z in 1:length(order_part2.order)){

    order_part2.order[[z]] <- as.data.table(order_part2.order[[z]])[,-1]
  }

  order_part2.order <- rbindlist(order_part2.order, idcol = TRUE)

  setnames(order_part2.order, c("V2", "V3", "V4", "V5", "V6"), c("Order_id", "Order_priority", "Qty", "Ref_ID", "Update"))
  order_part2.details <- order_part2.order

  info_part2 <- str_match_all(order_part2, "269=([^,]*),55=([^,]*),83=([^,]*),270=([^,]*),271=([^,]*),")
  info_part2.time <- str_extract_all(order_part2, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
  n_row_3 <- sapply(info_part2, nrow)

  rm(order_part2)

  info_part2.time <- str_dup(info_part2.time, n_row_3)

  info_part2.time <- str_match_all(info_part2.time, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")

  for(z in 1:length(info_part2.time)){

    info_part2.time[[z]] <- as.data.table(info_part2.time[[z]])[,-1]
  }

  info_part2.time <- rbindlist(info_part2.time, idcol = TRUE)

  setnames(info_part2.time, c("V2", "V3", "V4", "V5"), c("Date", "MsgSeq","SendingTime", "TransactTime"))


  for(z in 1:length(info_part2)){

    info_part2[[z]] <- as.data.table(info_part2[[z]])[,-1]
  }

  info_part2 <- rbindlist(info_part2, idcol = FALSE)

  setnames(info_part2, c("V2", "V3", "V4", "V5", "V6"), c("Side", "Code", "Seq", "PX", "Qty"))

  order_part2.info <- cbind(info_part2.time, info_part2)
  #order_part2.info <- order_part2.info[, -6]
  rm(info_part2, info_part2.time, order_part2.order)

  gc()

  order_part2.details$MsgSeq <- as.numeric(order_part2.details$MsgSeq)
  order_part2.details$Ref_ID <- as.numeric(order_part2.details$Ref_ID)
  order_part2.details$Order_priority <- as.numeric(order_part2.details$Order_priority)
  order_part2.details$Qty <- as.numeric(order_part2.details$Qty)
  order_part2.details$Update <- as.numeric(order_part2.details$Update)
  order_part2.details$.id <- as.numeric(order_part2.details$.id)

  order_part2.info$Side <- as.numeric(order_part2.info$Side)
  order_part2.info$Seq <- as.numeric(order_part2.info$Seq)
  order_part2.info$PX <- as.numeric(order_part2.info$PX)
  order_part2.info$Qty <- as.numeric(order_part2.info$Qty)
  order_part2.info$.id <- as.numeric(order_part2.info$.id)

  order_part2.info[, Ref_ID:=1:.N, by=c(".id")] ## here note that it should be sort by sequence number while not MsgSeq. Seq # is unique in
                                                        ## MBO-MBP template.



  part2_order <- order_part2.info[, -c("Qty")][order_part2.details[, c("Order_id", "Order_priority", "Qty","Ref_ID,Update", ".id")], on=c(".id", "Ref_ID"), nomatch=NULL][, -c("Ref_ID", ".id")]
  mbo[[3]] <- part2_order
  rm(order_part2.details, order_part2.info, part2_order)

  }


  MBO <- rbindlist(mbo,fill = TRUE)

  MBO <- split(MBO, by="Code")

  if(is.null(price_displayformat)){



    if(is.null(sunday_input)){

      stop("Sunday's security definition at the same week must be provided to get the price display format")
    }

    definition <- meta_data(sunday_input, date=date)

    setnames(definition, "Symbol", "Code")

    definition <- definition[Code %in% names(MBO)]

    MBO <- lapply(MBO, function(x) {
      x[, PX:= as.numeric(PX) * definition[Code == unique(x$Code), as.numeric(DisplayFactor) ]]
      return(x)
    })



  }else{

    MBO <- lapply(MBO, function(x) x[, grep("PX", colnames(x)):=lapply(.SD, function(x) as.numeric(x)*as.numeric(price_displayformat)), .SDcols = patterns("PX")])

  }


  cat("CME MDP 3.0 Quote Messages with Queue Information", "\n",
      "contracts:", names(MBO))
  return(MBO)

  gc()


}


