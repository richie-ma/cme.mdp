

#' Extract daily and session statistics from the CME Market by Price and/or Market by Order data
#'
#' `statistics()` extracts daily and session statistics from the CME Market by Price
#' and/or Market by Order data, including indicative (open) price, settlement price,
#' open interest, trading volume, low price, high price, highest bid, lowest offer, etc.
#'
#' @param input CME MBO/MBP data
#' @param date Trading date associated with the raw data, which can be found in
#' the file name easily in \code{YYYYMMDD} format. This argument is required to select
#' the parsing format.
#'
#' @returns a data.table contains the statistics of all tradeable contracts of a security in the CME.
#'
#' @import data.table stringr
#' @export
#'
#' @section Supported statistics:
#' \itemize{
#' \item open: Indicative open and open prices
#' \item settle: Settlement prices
#' \item high_px: Session high trade Price
#' \item low_px: Session low trade price
#' \item low_offer: Session lowest offer price
#' \item high_bid: Session highest bid price
#' \item volume: Daily trading volume
#' \item open_int: Daily open interest
#' \item elec_volume: Number of contracts traded in a session
#' \item limit: Limit high and low price
#' \item simulate_buy (only supported prior to Nov 20, 2015): Pre-open simulated buy price
#' \item simulate_sell (only supported prior to Nov 20, 2015): Pre-open simulated sell price
#' }
#'
#'
#' @examples
#' # This function requires a CME data license to run
#' # Example showing how to extract daily and session statistics
#' \dontrun{
#' All tradable contracts in E-mini S&P 500 futures
#' es.stat <- statistics(file, "2019-01-07")
#' }
#'
statistics <- function(input, date) {
  Flag <- DisplayFactor <- Code <- Seq <- high_limit <- low_limit <- NULL

  date <- as.Date(date)

  if (inherits(date, "Date") == FALSE) {
    stop("date should be in the format as YYYY-MM-DD.")

  }

  stat_main <- function(data, date) {
    if (date < "2015-11-20") {
      data <- str_replace_all(data, "\001", ",")


      data <- str_replace_all(data, "1128=([^,]*),9=([^,]*),", "")
      data <- str_replace_all(data, "49=([^,]*),34=([^,]*)", "")
      data <- str_replace_all(data, ",5799=([^,]*),", ",")
      data <- str_replace_all(data, ",268=([^,]*),", ",")
      data <- str_replace_all(data, ",279=([^,]*),", ",")
      data <- str_replace_all(data, ",22=([^,]*),", ",")
      data <- str_replace_all(data, ",48=([^,]*),", ",")
      data <- str_replace_all(data, ",273=([^,]*),", ",")
      data <- str_replace_all(data, ",274=([^,]*),", ",")
      data <- str_replace_all(data, ",451=([^,]*),", ",")
      data <- str_replace_all(data, ",1003=([^,]*),", ",")
      data <- str_replace_all(data, ",1020=([^,]*),", ",")
      data <- str_replace_all(data, ",277=([^,]*),", ",")


      OPEN <- str_subset(data, "269=4")
      SETTLE <- str_subset(data, "269=6")
      HIGH_PX <- str_subset(data, "269=7")
      LOW_PX <- str_subset(data, "269=8")
      HIGH_BID <- str_subset(data, "269=N")
      LOW_OFFER <- str_subset(data, "269=O")
      VOLUME <- str_subset(data, "269=B")
      OPEN_INT <- str_subset(data, "269=C")
      SIMULATE_SELL <- str_subset(data, "269=E")
      SIMULATE_BUY <- str_subset(data, "269=F")
      LIMIT <- str_subset(data, "35=f") ##g =Threshold Limits and Price Band Variation

      rm(data)


      ########################## OPEN ####################################################
      if (length(OPEN) != 0) {
        open <- str_match_all(OPEN,
                              "83=([^,]*),107=([^,]*),269=4,270=([^,]*),286=([^,]*),")

        n_row <- sapply(open, nrow)
        open.info <- unlist(str_extract_all(OPEN, "52=([^,]*),75=([^,]*),"))
        open.info <- str_dup(open.info, n_row)
        open <- as.data.table(do.call(rbind, open))[, -1]
        names(open)[c(1:4)] <- c("Seq", "Code", "OPEN_PX", "Flag")

        open[Flag == 5, Flag := "IndicativeOpen"][Flag == 0, Flag := "DailyOpen"]

        open.info <- str_match_all(open.info, "52=([^,]*),75=([^,]*),")
        open.info <- as.data.table(do.call(rbind, open.info))[, -1]
        names(open.info)[c(1:2)] <- c("Time", "Date")

        open$Seq <- as.numeric(open$Seq)

        open <- cbind(open.info, open)
        rm(OPEN, open.info, n_row)

        setkey(open, Code, Seq)


      }else{
        open <- NA
      }
      ######################## SETTLE ###########################################
      if (length(SETTLE) != 0) {
        settle <- str_match_all(SETTLE, "83=([^,]*),107=([^,]*),269=6,270=([^,]*),")

        n_row <- sapply(settle, nrow)
        settle.info <- unlist(str_extract_all(SETTLE, "52=([^,]*),75=([^,]*),"))
        settle.info <- str_dup(settle.info, n_row)
        settle <- as.data.table(do.call(rbind, settle))[, -1]
        names(settle)[c(1:3)] <- c("Seq", "Code", "SETTLE_PX")


        settle.info <- str_match_all(settle.info, "52=([^,]*),75=([^,]*),")
        settle.info <- as.data.table(do.call(rbind, settle.info))[, -1]
        names(settle.info)[c(1:2)] <- c("Time", "Date")

        settle$Seq <- as.numeric(settle$Seq)
        settle$SETTLE_PX <- as.numeric(settle$SETTLE_PX)

        settle <- cbind(settle.info, settle)
        rm(SETTLE, settle.info, n_row)

        setkey(settle, Code, Seq)

      }else{
        settle <- NA
      }
      #################### SESSION HIGH PX #######################################
      if (length(HIGH_PX) != 0) {
        high_px <- str_match_all(HIGH_PX,
                                 "83=([^,]*),107=([^,]*),269=7,270=([^,]*),")

        n_row <- sapply(high_px, nrow)
        high_px.info <- unlist(str_extract_all(HIGH_PX, "52=([^,]*),75=([^,]*),"))
        high_px.info <- str_dup(high_px.info, n_row)
        high_px <- as.data.table(do.call(rbind, high_px))[, -1]
        names(high_px)[c(1:3)] <- c("Seq", "Code", "HIGH")


        high_px.info <- str_match_all(high_px.info, "52=([^,]*),75=([^,]*),")
        high_px.info <- as.data.table(do.call(rbind, high_px.info))[, -1]
        names(high_px.info)[c(1:2)] <- c("Time", "Date")

        high_px$Seq <- as.numeric(high_px$Seq)
        high_px$HIGH <- as.numeric(high_px$HIGH)

        high_px <- cbind(high_px.info, high_px)
        rm(HIGH_PX, high_px.info, n_row)

        setkey(high_px, Code, Seq)


      }else{
        high_px <- NA
      }
      ############### SESSION LOW PRICE ###################################
      if (length(LOW_PX) != 0) {
        low_px <- str_match_all(LOW_PX, "83=([^,]*),107=([^,]*),269=8,270=([^,]*),")

        n_row <- sapply(low_px, nrow)
        low_px.info <- unlist(str_extract_all(LOW_PX, "52=([^,]*),75=([^,]*),"))
        low_px.info <- str_dup(low_px.info, n_row)
        low_px <- as.data.table(do.call(rbind, low_px))[, -1]
        names(low_px)[c(1:3)] <- c("Seq", "Code", "LOW")


        low_px.info <- str_match_all(low_px.info, "52=([^,]*),75=([^,]*),")
        low_px.info <- as.data.table(do.call(rbind, low_px.info))[, -1]
        names(low_px.info)[c(1:2)] <- c("Time" , "Date")

        low_px$Seq <- as.numeric(low_px$Seq)
        low_px$LOW <- as.numeric(low_px$LOW)

        low_px <- cbind(low_px.info, low_px)
        rm(LOW_PX, low_px.info, n_row)

        setkey(low_px, Code, Seq)


      }else{
        low_px <- NA
      }
      ############### SESSION HIGH BID ###################################
      if (length(HIGH_BID) != 0) {
        high_bid <- str_match_all(HIGH_BID,
                                  "83=([^,]*),107=([^,]*),269=N,270=([^,]*),")

        n_row <- sapply(high_bid, nrow)
        high_bid.info <- unlist(str_extract_all(HIGH_BID, "52=([^,]*),75=([^,]*),"))
        high_bid.info <- str_dup(high_bid.info, n_row)
        high_bid <- as.data.table(do.call(rbind, high_bid))[, -1]
        names(high_bid)[c(1:3)] <- c("Seq", "Code", "HIGH_BID")


        high_bid.info <- str_match_all(high_bid.info, "52=([^,]*),75=([^,]*),")
        high_bid.info <- as.data.table(do.call(rbind, high_bid.info))[, -1]
        names(high_bid.info)[c(1:2)] <- c("Time", "Date")

        high_bid$Seq <- as.numeric(high_bid$Seq)
        high_bid$HIGH_BID <- as.numeric(high_bid$HIGH_BID)

        high_bid <- cbind(high_bid.info, high_bid)
        rm(HIGH_BID, high_bid.info, n_row)

        setkey(high_bid, Code, Seq)


      }else{
        high_bid <- NA
      }
      ############# SESSION LOW OFFER ###############################
      if (length(LOW_OFFER) != 0) {
        low_offer <- str_match_all(LOW_OFFER,
                                   "83=([^,]*),107=([^,]*),269=O,270=([^,]*),")

        n_row <- sapply(low_offer, nrow)
        low_offer.info <- unlist(str_extract_all(LOW_OFFER, "52=([^,]*),75=([^,]*),"))
        low_offer.info <- str_dup(low_offer.info, n_row)
        low_offer <- as.data.table(do.call(rbind, low_offer))[, -1]
        names(low_offer)[c(1:3)] <- c("Seq", "Code", "LOW_OFFER")


        low_offer.info <- str_match_all(low_offer.info, "52=([^,]*),75=([^,]*),")
        low_offer.info <- as.data.table(do.call(rbind, low_offer.info))[, -1]
        names(low_offer.info)[c(1:2)] <- c("Time", "Date")

        low_offer$Seq <- as.numeric(low_offer$Seq)
        low_offer$LOW_OFFER <- as.numeric(low_offer$LOW_OFFER)

        low_offer <- cbind(low_offer.info, low_offer)
        rm(LOW_OFFER, low_offer.info, n_row)

        setkey(low_offer, Code, Seq)

      }else{
        low_offer <- NA
      }
      ############# VOLUME  ###############################
      if (length(VOLUME) != 0) {
        volume <- str_match_all(VOLUME, "83=([^,]*),107=([^,]*),269=B,271=([^,]*),")

        n_row <- sapply(volume, nrow)
        volume.info <- unlist(str_extract_all(VOLUME, "52=([^,]*),75=([^,]*),"))
        volume.info <- str_dup(volume.info, n_row)
        volume <- as.data.table(do.call(rbind, volume))[, -1]
        names(volume)[c(1:3)] <- c("Seq", "Code", "VOLUME")


        volume.info <- str_match_all(volume.info, "52=([^,]*),75=([^,]*),")
        volume.info <- as.data.table(do.call(rbind, volume.info))[, -1]
        names(volume.info)[c(1:2)] <- c("Time", "Date")

        volume$Seq <- as.numeric(volume$Seq)
        volume$VOLUME <- as.numeric(volume$VOLUME)

        volume <- cbind(volume.info, volume)
        rm(VOLUME, volume.info, n_row)

        setkey(volume, Code, Seq)

      }else{
        volume <- NA
      }
      ############# OPEN INTEREST  ###############################
      if (length(OPEN_INT) != 0) {
        open_int <- str_match_all(OPEN_INT,
                                  "83=([^,]*),107=([^,]*),269=C,271=([^,]*),")

        n_row <- sapply(open_int, nrow)
        open_int.info <- unlist(str_extract_all(OPEN_INT, "52=([^,]*),75=([^,]*),"))
        open_int.info <- str_dup(open_int.info, n_row)
        open_int <- as.data.table(do.call(rbind, open_int))[, -1]
        names(open_int)[c(1:3)] <- c("Seq", "Code", "OPEN_INT")


        open_int.info <- str_match_all(open_int.info, "52=([^,]*),75=([^,]*),")
        open_int.info <- as.data.table(do.call(rbind, open_int.info))[, -1]
        names(open_int.info)[c(1:2)] <- c("Time", "Date")

        open_int$Seq <- as.numeric(open_int$Seq)
        open_int$OPEN_INT <- as.numeric(open_int$OPEN_INT)

        open_int <- cbind(open_int.info, open_int)
        rm(OPEN_INT, open_int.info, n_row)

        setkey(open_int, Code, Seq)


      }else{
        open_int <- NA
      }

      ############# LIMIT  ###############################

      ## 1148
      ## Allowable low limit price for the trading day.  orders submitted with px below this will be rejected
      ##  1149
      ## Allowable high limit price for the trading day.  orders submitted with px above this will be rejected
      ## 1143 	MaxPriceVariation for price banding
      if (length(LIMIT) != 0) {
        LIMIT <- str_replace_all(LIMIT,
                                 "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),34=([^,]*),",
                                 "")
        LIMIT <- str_replace_all(LIMIT, "52=([^,]*),22=([^,]*),", ",")
        LIMIT <- str_replace_all(LIMIT, ",48=([^,]*),", ",")
        LIMIT <- str_replace_all(LIMIT , ",10=([^,]*),", ",")


        ## generate the trade data (without order flow)
        limit <- str_match_all(LIMIT ,
                               "75=([^,]*),107=([^,]*),332=([^,]*),333=([^,]*),")
        limit <- as.data.table(do.call(rbind, limit))[, -1]
        names(limit)[c(1:4)] <- c("Date", "Code", "high_limit", "low_limit")
        limit[, `:=`(high_limit = as.numeric(high_limit),
                     low_limit = as.numeric(low_limit))]


        rm(LIMIT)


      }else{
        limit <- NA
      }


      if (length(SIMULATE_SELL) != 0) {
        simulate_sell <- str_match_all(SIMULATE_SELL,
                                       "83=([^,]*),107=([^,]*),269=E,270=([^,]*),271=([^,]*)")

        n_row <- sapply(simulate_sell, nrow)
        simulate_sell.info <- unlist(str_extract_all(SIMULATE_SELL, "52=([^,]*),75=([^,]*),"))
        simulate_sell.info <- str_dup(simulate_sell.info, n_row)
        simulate_sell <- as.data.table(do.call(rbind, simulate_sell))[, -1]
        names(simulate_sell)[c(1:4)] <- c("Seq", "Code", "PX", "Qty")


        simulate_sell.info <- str_match_all(simulate_sell.info, "52=([^,]*),75=([^,]*),")
        simulate_sell.info <- as.data.table(do.call(rbind, simulate_sell.info))[, -1]
        names(simulate_sell.info)[c(1:2)] <- c("Time", "Date")

        simulate_sell$Seq <- as.numeric(simulate_sell$Seq)
        simulate_sell$PX <- as.numeric(simulate_sell$PX)
        simulate_sell$Qty <- as.numeric(simulate_sell$Qty)

        simulate_sell <- cbind(simulate_sell.info, simulate_sell)
        rm(SIMULATE_SELL, simulate_sell.info, n_row)

        setkey(simulate_sell, Code, Seq)



      }else{
        simulate_sell <- NA
      }

      if (length(SIMULATE_BUY) != 0) {
        simulate_buy <- str_match_all(SIMULATE_BUY,
                                      "83=([^,]*),107=([^,]*),269=F,270=([^,]*),271=([^,]*)")

        n_row <- sapply(simulate_buy, nrow)
        simulate_buy.info <- unlist(str_extract_all(SIMULATE_BUY, "52=([^,]*),75=([^,]*),"))
        simulate_buy.info <- str_dup(simulate_buy.info, n_row)
        simulate_buy <- as.data.table(do.call(rbind, simulate_buy))[, -1]
        names(simulate_buy)[c(1:4)] <- c("Seq", "Code", "PX", "Qty")


        simulate_buy.info <- str_match_all(simulate_buy.info, "52=([^,]*),75=([^,]*),")
        simulate_buy.info <- as.data.table(do.call(rbind, simulate_buy.info))[, -1]
        names(simulate_buy.info)[c(1:2)] <- c("Time", "Date")

        simulate_buy$Seq <- as.numeric(simulate_buy$Seq)
        simulate_buy$PX <- as.numeric(simulate_buy$PX)
        simulate_buy$Qty <- as.numeric(simulate_buy$Qty)

        simulate_buy <- cbind(simulate_buy.info, simulate_buy)
        rm(SIMULATE_BUY, simulate_buy.info, n_row)

        setkey(simulate_buy, Code, Seq)


      }else{
        simulate_buy <- NA
      }


      info_list <- list(
        open = open,
        settle = settle,
        high_px = high_px,
        low_px = low_px,
        high_bid = high_bid,
        low_offer = low_offer,
        volume = volume,
        open_int = open_int,
        simulated_buy = simulate_buy,
        simulated_sell = simulate_sell,
        limit = limit
      )

    } else {
      data <- str_replace_all(data, "\001", ",") # delete all "\001" for each tag and replace it with "," (comma)



      ######################################################################################################

      data <- str_replace_all(data,
                              "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),",
                              "")
      data <- str_replace_all(data, ",34=([^,]*),", ",")
      data <- str_replace_all(data, ",5799=([^,]*),", ",")
      data <- str_replace_all(data, ",268=([^,]*),", ",")
      data <- str_replace_all(data, ",279=([^,]*),", ",")
      data <- str_replace_all(data, ",48=([^,]*),", ",")
      data <- str_replace_all(data, ",37705=([^,]*),", ",")
      data <- str_replace_all(data, ",37=([^,]*),", ",")
      data <- str_replace_all(data, ",32=([^,]*),", ",")
      data <- str_replace_all(data, ",10=([^,]*),", ",")

      OPEN <- str_subset(data, "269=4")
      SETTLE <- str_subset(data, "269=6")
      HIGH_PX <- str_subset(data, "269=7")
      LOW_PX <- str_subset(data, "269=8")
      HIGH_BID <- str_subset(data, "269=N")
      LOW_OFFER <- str_subset(data, "269=O")
      VOLUME <- str_subset(data, "269=B")
      OPEN_INT <- str_subset(data, "269=C")
      ELEC_VOLUME <- str_subset(data, "269=e")
      LIMIT <- str_subset(data, "269=g") ##g =Threshold Limits and Price Band Variation

      rm(data)

      ########################## OPEN ####################################################
      if (length(OPEN) != 0) {
        open <- str_match_all(OPEN,
                              "269=4,55=([^,]*),83=([^,]*),270=([^,]*),286=([^,]*),")

        n_row <- sapply(open, nrow)
        open.info <- unlist(str_extract_all(OPEN, "75=([^,]*),52=([^,]*),60=([^,]*),"))
        open.info <- str_dup(open.info, n_row)
        open <- as.data.table(do.call(rbind, open))[, -1]
        names(open)[c(1:4)] <- c("Code", "Seq", "OPEN_PX", "Flag")

        open[Flag == 5, Flag := "IndicativeOpen"][Flag == 0, Flag := "DailyOpen"]

        open.info <- str_match_all(open.info, "75=([^,]*),52=([^,]*),60=([^,]*),")
        open.info <- as.data.table(do.call(rbind, open.info))[, -1]
        names(open.info)[c(1:3)] <- c("Date", "SendingTime", "TransactTime")

        open$Seq <- as.numeric(open$Seq)

        open <- cbind(open.info, open)
        rm(OPEN, open.info, n_row)

        setkey(open, Code, Seq)

      }else{
        open <- NA
      }
      ######################## SETTLE ###########################################
      if (length(SETTLE) != 0) {
        settle <- str_match_all(SETTLE, "269=6,55=([^,]*),83=([^,]*),270=([^,]*),")

        n_row <- sapply(settle, nrow)
        settle.info <- unlist(str_extract_all(SETTLE, "75=([^,]*),52=([^,]*),60=([^,]*),"))
        settle.info <- str_dup(settle.info, n_row)
        settle <- as.data.table(do.call(rbind, settle))[, -1]
        names(settle)[c(1:3)] <- c("Code", "Seq", "SETTLE_PX")


        settle.info <- str_match_all(settle.info, "75=([^,]*),52=([^,]*),60=([^,]*),")
        settle.info <- as.data.table(do.call(rbind, settle.info))[, -1]
        names(settle.info)[c(1:3)] <- c("Date", "SendingTime", "TransactTime")

        settle$Seq <- as.numeric(settle$Seq)
        settle$SETTLE_PX <- as.numeric(settle$SETTLE_PX)

        settle <- cbind(settle.info, settle)
        rm(SETTLE, settle.info, n_row)

        setkey(settle, Code, Seq)

      }else{
        settle <- NA
      }
      #################### SESSION HIGH PX #######################################
      if (length(HIGH_PX) != 0) {
        high_px <- str_match_all(HIGH_PX, "269=7,55=([^,]*),83=([^,]*),270=([^,]*),")

        n_row <- sapply(high_px, nrow)
        high_px.info <- unlist(str_extract_all(HIGH_PX, "75=([^,]*),52=([^,]*),60=([^,]*),"))
        high_px.info <- str_dup(high_px.info, n_row)
        high_px <- as.data.table(do.call(rbind, high_px))[, -1]
        names(high_px)[c(1:3)] <- c("Code", "Seq", "HIGH")


        high_px.info <- str_match_all(high_px.info, "75=([^,]*),52=([^,]*),60=([^,]*),")
        high_px.info <- as.data.table(do.call(rbind, high_px.info))[, -1]
        names(high_px.info)[c(1:3)] <- c("Date", "SendingTime", "TransactTime")

        high_px$Seq <- as.numeric(high_px$Seq)
        high_px$HIGH <- as.numeric(high_px$HIGH)

        high_px <- cbind(high_px.info, high_px)
        rm(HIGH_PX, high_px.info, n_row)

        setkey(high_px, Code, Seq)

      }else{
        high_px <- NA
      }
      ############### SESSION LOW PRICE ###################################
      if (length(LOW_PX) != 0) {
        low_px <- str_match_all(LOW_PX, "269=8,55=([^,]*),83=([^,]*),270=([^,]*),")

        n_row <- sapply(low_px, nrow)
        low_px.info <- unlist(str_extract_all(LOW_PX, "75=([^,]*),52=([^,]*),60=([^,]*),"))
        low_px.info <- str_dup(low_px.info, n_row)
        low_px <- as.data.table(do.call(rbind, low_px))[, -1]
        names(low_px)[c(1:3)] <- c("Code", "Seq", "LOW")


        low_px.info <- str_match_all(low_px.info, "75=([^,]*),52=([^,]*),60=([^,]*),")
        low_px.info <- as.data.table(do.call(rbind, low_px.info))[, -1]
        names(low_px.info)[c(1:3)] <- c("Date", "SendingTime", "TransactTime")

        low_px$Seq <- as.numeric(low_px$Seq)
        low_px$LOW <- as.numeric(low_px$LOW)

        low_px <- cbind(low_px.info, low_px)
        rm(LOW_PX, low_px.info, n_row)

        setkey(low_px, Code, Seq)


      }else{
        low_px <- NA
      }
      ############### SESSION HIGH BID ###################################
      if (length(HIGH_BID) != 0) {
        high_bid <- str_match_all(HIGH_BID,
                                  "269=N,55=([^,]*),83=([^,]*),270=([^,]*),")

        n_row <- sapply(high_bid, nrow)
        high_bid.info <- unlist(str_extract_all(HIGH_BID, "75=([^,]*),52=([^,]*),60=([^,]*),"))
        high_bid.info <- str_dup(high_bid.info, n_row)
        high_bid <- as.data.table(do.call(rbind, high_bid))[, -1]
        names(high_bid)[c(1:3)] <- c("Code", "Seq", "HIGH_BID")


        high_bid.info <- str_match_all(high_bid.info, "75=([^,]*),52=([^,]*),60=([^,]*),")
        high_bid.info <- as.data.table(do.call(rbind, high_bid.info))[, -1]
        names(high_bid.info)[c(1:3)] <- c("Date", "SendingTime", "TransactTime")

        high_bid$Seq <- as.numeric(high_bid$Seq)
        high_bid$HIGH_BID <- as.numeric(high_bid$HIGH_BID)

        high_bid <- cbind(high_bid.info, high_bid)
        rm(HIGH_BID, high_bid.info, n_row)

        setkey(high_bid, Code, Seq)

      }else{
        high_bid <- NA
      }
      ############# SESSION LOW OFFER ###############################
      if (length(LOW_OFFER) != 0) {
        low_offer <- str_match_all(LOW_OFFER,
                                   "269=O,55=([^,]*),83=([^,]*),270=([^,]*),")

        n_row <- sapply(low_offer, nrow)
        low_offer.info <- unlist(str_extract_all(LOW_OFFER, "75=([^,]*),52=([^,]*),60=([^,]*),"))
        low_offer.info <- str_dup(low_offer.info, n_row)
        low_offer <- as.data.table(do.call(rbind, low_offer))[, -1]
        names(low_offer)[c(1:3)] <- c("Code", "Seq", "LOW_OFFER")


        low_offer.info <- str_match_all(low_offer.info, "75=([^,]*),52=([^,]*),60=([^,]*),")
        low_offer.info <- as.data.table(do.call(rbind, low_offer.info))[, -1]
        names(low_offer.info)[c(1:3)] <- c("Date", "SendingTime", "TransactTime")

        low_offer$Seq <- as.numeric(low_offer$Seq)
        low_offer$LOW_OFFER <- as.numeric(low_offer$LOW_OFFER)

        low_offer <- cbind(low_offer.info, low_offer)
        rm(LOW_OFFER, low_offer.info, n_row)

        setkey(low_offer, Code, Seq)

      }else{
        low_offer <- NA
      }
      ############# VOLUME  ###############################
      if (length(VOLUME) != 0) {
        volume <- str_match_all(VOLUME, "269=B,55=([^,]*),83=([^,]*),271=([^,]*),")

        n_row <- sapply(volume, nrow)
        volume.info <- unlist(str_extract_all(VOLUME, "75=([^,]*),52=([^,]*),60=([^,]*),"))
        volume.info <- str_dup(volume.info, n_row)
        volume <- as.data.table(do.call(rbind, volume))[, -1]
        names(volume)[c(1:3)] <- c("Code", "Seq", "VOLUME")


        volume.info <- str_match_all(volume.info, "75=([^,]*),52=([^,]*),60=([^,]*),")
        volume.info <- as.data.table(do.call(rbind, volume.info))[, -1]
        names(volume.info)[c(1:3)] <- c("Date", "SendingTime", "TransactTime")

        volume$Seq <- as.numeric(volume$Seq)
        volume$VOLUME <- as.numeric(volume$VOLUME)

        volume <- cbind(volume.info, volume)
        rm(VOLUME, volume.info, n_row)

        setkey(volume, Code, Seq)


      }else{
        volume <- NA
      }
      ############# OPEN INTEREST  ###############################
      if (length(OPEN_INT) != 0) {
        open_int <- str_match_all(OPEN_INT,
                                  "269=C,55=([^,]*),83=([^,]*),271=([^,]*),")

        n_row <- sapply(open_int, nrow)
        open_int.info <- unlist(str_extract_all(OPEN_INT, "75=([^,]*),52=([^,]*),60=([^,]*),"))
        open_int.info <- str_dup(open_int.info, n_row)
        open_int <- as.data.table(do.call(rbind, open_int))[, -1]
        names(open_int)[c(1:3)] <- c("Code", "Seq", "OPEN_INT")


        open_int.info <- str_match_all(open_int.info, "75=([^,]*),52=([^,]*),60=([^,]*),")
        open_int.info <- as.data.table(do.call(rbind, open_int.info))[, -1]
        names(open_int.info)[c(1:3)] <- c("Date", "SendingTime", "TransactTime")

        open_int$Seq <- as.numeric(open_int$Seq)
        open_int$OPEN_INT <- as.numeric(open_int$OPEN_INT)

        open_int <- cbind(open_int.info, open_int)
        rm(OPEN_INT, open_int.info, n_row)

        setkey(open_int, Code, Seq)


      }else{
        open_int <- NA
      }
      ############# ELECTRONIC VOLUME  ###############################
      if (length(ELEC_VOLUME) != 0) {
        elec_volume <- str_match_all(ELEC_VOLUME,
                                     "269=e,55=([^,]*),83=([^,]*),271=([^,]*),")

        n_row <- sapply(elec_volume, nrow)
        elec_volume.info <- unlist(str_extract_all(ELEC_VOLUME, "75=([^,]*),52=([^,]*),60=([^,]*),"))
        elec_volume.info <- str_dup(elec_volume.info, n_row)
        elec_volume <- as.data.table(do.call(rbind, elec_volume))[, -1]
        names(elec_volume)[c(1:3)] <- c("Code", "Seq", "ELEC_VOLUME")


        elec_volume.info <- str_match_all(elec_volume.info,
                                          "75=([^,]*),52=([^,]*),60=([^,]*),")
        elec_volume.info <- as.data.table(do.call(rbind, elec_volume.info))[, -1]
        names(elec_volume.info)[c(1:3)] <- c("Date", "SendingTime", "TransactTime")

        elec_volume$Seq <- as.numeric(elec_volume$Seq)
        elec_volume$ELEC_VOLUME <- as.numeric(elec_volume$ELEC_VOLUME)

        elec_volume <- cbind(elec_volume.info, elec_volume)
        rm(ELEC_VOLUME, elec_volume.info, n_row)

        setkey(elec_volume, Code, Seq)

      }else{
        elec_volume <- NA
      }
      ############# LIMIT  ###############################
      if (length(LIMIT) != 0) {
        ## 1148
        ## Allowable low limit price for the trading day.  orders submitted with px below this will be rejected
        ##  1149
        ## Allowable high limit price for the trading day.  orders submitted with px above this will be rejected
        ## 1143 	MaxPriceVariation for price banding


        limit <- str_match_all(
          LIMIT,
          "269=g,55=([^,]*),83=([^,]*),1149=([^,]*),1148=([^,]*),1143=([^,]*),"
        )

        n_row <- sapply(limit, nrow)
        limit.info <- unlist(str_extract_all(LIMIT, "75=([^,]*),52=([^,]*),60=([^,]*),"))
        limit.info <- str_dup(limit.info, n_row)
        limit <- as.data.table(do.call(rbind, limit))[, -1]
        names(limit)[c(1:5)] <- c("Code", "Seq", "high_limit", "low_limit", "variation")


        limit.info <- str_match_all(limit.info, "75=([^,]*),52=([^,]*),60=([^,]*),")
        limit.info <- as.data.table(do.call(rbind, limit.info))[, -1]
        names(limit.info)[c(1:3)] <- c("Date", "SendingTime", "TransactTime")

        limit$Seq <- as.numeric(limit$Seq)
        limit$high_limit <- as.numeric(limit$high_limit)
        limit$low_limit <- as.numeric(limit$low_limit)
        limit$variation <- as.numeric(limit$variation)

        limit <- cbind(limit.info, limit)
        rm(LIMIT, limit.info, n_row)

        setkey(limit, Code, Seq)


      }else{
        limit <- NA
      }

      info_list <- list(
        open = open,
        settle = settle,
        high_px = high_px,
        low_px = low_px,
        high_bid = high_bid,
        low_offer = low_offer,
        volume = volume,
        open_int = open_int,
        elec_volume = elec_volume,
        limit = limit
      )

    }

    return(info_list)

  }

  file <-  fread(input,
                 header = F,
                 sep = "\\",
                 fill = TRUE)[[1L]]
  stat <- stat_main(file, date = date)
}
