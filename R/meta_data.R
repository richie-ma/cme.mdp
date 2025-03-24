

#' Extract necessary metadata from the CME Market by Price and/or Market by Order data
#'
#' `meta_data()` extracts necessary security definition of a specific contract, or
#' all tradeable contract of a financial security, traded in the CME, including security
#' symbol, maturity, security type (e.g,. futures/options),
#' exchange (e.g., CME, NYMEX), matching algorithm, minimum price increment (tick size),
#' price display format, number of market depth supported, implied eligibility, etc.
#'
#' @param sunday_input A Sunday CME MBO/MBP raw data.
#' @param date Trading date associated with the raw data, which can be found in
#' the file name easily in \code{YYYYMMDD} format. This argument is required to select
#' the parsing format.
#' @param security A symbol of a specific contract of a financial security, should be
#' in the format like "CLK9" (crude oil), "ESH9" (E-mini S&P 500), etc. These can
#' be obtained from CME website for a contract specification.
#'
#' @returns a data.table contains the meta data of either a specific contract, or
#' all tradeable contracts of a financial security traded in the CME.
#' @export
#'
#' @import data.table stringr
#'
#' @examples
#' # This function requires a CME data license to run
#' # Example showing how to extract meta data
#' # extract meta data
#' # Know the specific security (e,g., ESH9)
#' \dontrun{
#' es.meta.data <- meta_data(file, "2019-01-07", 'ESH9')
#'
#' # For all tradable contracts in E-mini S&P 500 futures
#' es.meta.data <- meta_data(file, "2019-01-07")
#' }
meta_data <- function(sunday_input, date, security = NULL) {
  DisplayFactor <- NULL

  date <- as.Date(date)

  if (inherits(date, "Date") == FALSE) {
    stop("date should be in the format as YYYY-MM-DD.")

  }
  main <- function(data, date, security = NULL) {
    if (date < "2015-11-20") {
      if (is.null(security) == FALSE) {
        data <- str_subset(data,
                           paste0("\00155=", security, "(?!([:space:]|-))"))
        data <- str_subset(data, "\00135=d")
        data <- str_replace_all(data, "\001", ",")
      } else{
        data <- str_subset(data, "\00135=d")
        data <- str_replace_all(data, "\001", ",")

      }




      ## delete the tags that are not necessary

      data <-  str_replace_all(
        data,
        "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),34=([^,]*),52=([^,]*),",
        ""
      )
      data <-  str_replace_all(data, ",22=([^,]*),", ",")
      data <-  str_replace_all(data, ",461=([^,]*),", ",")
      data <-  str_replace_all(data, ",731=([^,]*),", ",")
      data <-  str_replace_all(data, ",1150=([^,]*),", ",")
      data <-  str_replace_all(data, ",864=([^,]*),", ",")
      data <-  str_replace_all(data, "865=([^,]*)", ",")
      data <-  str_replace_all(data, "866=([^,]*)", ",")
      data <-  str_replace_all(data, "1145=([^,]*)", ",")
      data <-  str_replace_all(data, "870=([^,]*)", ",")
      data <-  str_replace_all(data, "871=([^,]*)", ",")
      data <-  str_replace_all(data, "872=([^,]*)", ",")
      data <-  str_replace_all(data, ",1141=([^,]*),", ",")
      data <-  str_replace_all(data, ",1146=([^,]*),", ",")
      data <-  str_replace_all(data, ",1180=([^,]*),", ",")
      data <-  str_replace_all(data, ",1300=([^,]*),", ",")
      data <-  str_replace_all(data, ",5796=([^,]*),", ",")
      data <-  str_replace_all(data, ",9850=([^,]*),", ",")
      data <-  str_replace_all(data, ",15=([^,]*),", ",")
      data <-  str_replace_all(data, ",48=([^,]*),", ",")
      data <-  str_replace_all(data, ",202=([^,]*),", ",")
      data <-  str_replace_all(data, ",462=([^,]*),", ",")
      data <-  str_replace_all(data, ",827=([^,]*),", ",")
      data <-  str_replace_all(data, ",947=([^,]*),", ",")
      data <-  str_replace_all(data, ",1143=([^,]*),", ",")
      data <-  str_replace_all(data, ",1144=([^,]*),", ",")
      data <-  str_replace_all(data, ",1147=([^,]*),", ",")
      data <-  str_replace_all(data, ",1151=([^,]*),", ",")
      data <-  str_replace_all(data, ",55=([^,]*),", ",")
      data <-  str_replace_all(data, ",1148=([^,]*),", ",")
      data <-  str_replace_all(data, ",1149=([^,]*),", ",")
      data <-  str_replace_all(data, ",555=([^,]*),", ",")
      data <-  str_replace_all(data,
                               "600=([^,]*),602=([^,]*),603=([^,]*),623=([^,]*),624=([^,]*)",
                               ",")
      data <-  str_replace_all(data, ",762=([^,]*),", ",")
      data <-  str_replace_all(data, ",10=([^,]*),", ",")
      data <-  str_replace_all(data, ",+", ",")

      ## searching for the following tags,
      ## tag52-time stamp
      ## tag75-date
      #  tag83-seq#
      #  tag107-contract
      #  tag269-type: 2-trade
      #  tag270-px
      #  tag271-qty

      ## generate the trade data (with order flow)
      ## first find the tag269=2
      ## Trade summary pattern
      ## 75 60 269=2 55 270 271 346 5797 37711-trade id 37705 37 32

      if (length(data) != 0) {
        if (TRUE %in% unique(str_detect(data, "1022=GBI,"))) {
          data1 <- str_subset(data, "1022=GBI,")

          if (length(data1) != 0) {
            index1 <- str_match_all(
              data1,
              "15=([^,]*),107=([^,]*),200=([^,]*),207=([^,]*),562=([^,]*),969=([^,]*),996=([^,]*),1140=([^,]*),1022=([^,]*),264=([^,]*),1022=([^,]*),264=([^,]*),1142=([^,]*),9787=([^,]*),"
            )
            meta_data1 <- as.data.table(do.call(rbind, index1))[, -1]
            names(meta_data1)[c(1:14)] <- c(
              "Currency",
              "Symbol",
              "MaturityMonthYear",
              "SecurityExchange",
              "MinTradeVol",
              "MinPriceIncrement",
              "UnitOfMeasure",
              "MaxTradeVol",
              "MDFeedType",
              "MarketDepth",
              "MDFeedTypeimplied",
              "MarketDepthimplied",
              "MatchAlgorithm",
              "DisplayFactor"
            )


          }
          data2 <- data[-which(data %in% data1)]
          if (length(data2) != 0) {
            index2 <- str_match_all(
              data2,
              "15=([^,]*),107=([^,]*),200=([^,]*),207=([^,]*),562=([^,]*),969=([^,]*),996=([^,]*),1140=([^,]*),1022=([^,]*),264=([^,]*),1142=([^,]*),9787=([^,]*),"
            )
            meta_data2 <- as.data.table(do.call(rbind, index2))[, -1]
            names(meta_data2)[c(1:12)] <- c(
              "Currency",
              "Symbol",
              "MaturityMonthYear",
              "SecurityExchange",
              "MinTradeVol",
              "MinPriceIncrement",
              "UnitOfMeasure",
              "MaxTradeVol",
              "MDFeedType",
              "MarketDepth",
              "MatchAlgorithm",
              "DisplayFactor"
            )
            meta_data <- rbind(meta_data1, meta_data2, fill = TRUE)
          } else{
            meta_data <- meta_data1
          }
        } else{
          index2 <- str_match_all(
            data,
            "15=([^,]*),107=([^,]*),200=([^,]*),207=([^,]*),562=([^,]*),969=([^,]*),996=([^,]*),1140=([^,]*),1022=([^,]*),264=([^,]*),1142=([^,]*),9787=([^,]*),"
          )
          meta_data2 <- as.data.table(do.call(rbind, index2))[, -1]
          names(meta_data2)[c(1:12)] <- c(
            "Currency",
            "Symbol",
            "MaturityMonthYear",
            "SecurityExchange",
            "MinTradeVol",
            "MinPriceIncrement",
            "UnitOfMeasure",
            "MaxTradeVol",
            "MDFeedType",
            "MarketDepth",
            "MatchAlgorithm",
            "DisplayFactor"
          )
          meta_data <- meta_data2
        }
      }
    } else{
      if (is.null(security) == FALSE) {
        data <- str_subset(data,
                           paste0("\00155=", security, "(?!([:space:]|-))"))
        data <- str_subset(data, "\00135=d")
        data <- str_replace_all(data, "\001", ",")
      } else{
        data <- str_subset(data, "\00135=d")
        data <- str_replace_all(data, "\001", ",")

      }

      ## delete the tags that are not necessary


      data <- str_replace_all(data,
                              "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),",
                              "")
      data <- str_replace_all(data, ",34=([^,]*),", ",")
      data <- str_replace_all(data, ",201=([^,]*),", ",")
      data <- str_replace_all(data, ",52=([^,]*),", ",")
      data <- str_replace_all(data, ",5799=([^,]*),", ",")
      data <- str_replace_all(data, ",980=([^,]*),", ",")
      data <- str_replace_all(data, ",779=([^,]*),", ",")
      data <- str_replace_all(data, ",1180=([^,]*),", ",")
      data <- str_replace_all(data, ",1300=([^,]*),", ",")
      data <- str_replace_all(data, ",48=([^,]*),", ",")
      data <- str_replace_all(data, ",22=([^,]*),", ",")
      data <- str_replace_all(data, ",1151=([^,]*),", ",")
      data <- str_replace_all(data, ",6937=([^,]*),", ",")
      data <- str_replace_all(data, ",461=([^,]*),", ",")
      data <- str_replace_all(data, ",9779=([^,]*),", ",")
      data <- str_replace_all(data, ",462=([^,]*),", ",")
      data <- str_replace_all(data, ",37702=([^,]*),", ",")
      data <- str_replace_all(data, ",9800=([^,]*),", ",")
      data <- str_replace_all(data, ",1141=([^,]*),", ",")
      data <- str_replace_all(data, ",864=([^,]*),", ",")
      data <- str_replace_all(data, "865=([^,]*),1145=([^,]*)", ",")
      data <- str_replace_all(data, ",870=([^,]*),", ",")
      data <- str_replace_all(data, ",871=([^,]*),", ",")
      data <- str_replace_all(data, ",872=([^,]*),", ",")
      data <- str_replace_all(data, ",731=([^,]*),", ",")
      data <- str_replace_all(data, ",10=([^,]*),", ",")
      data <- str_replace_all(data,
                              "602=([^,]*),603=([^,]*),624=([^,]*),623=([^,]*)",
                              ",")
      data <- str_replace_all(data, ",555=([^,]*),", ",")
      data <- str_replace_all(data, ",947=([^,]*),", ",")
      data <- str_replace_all(data, ",9850=([^,]*),", ",")
      data <- str_replace_all(data, ",711=([^,]*),", ",")
      data <- str_replace_all(data, ",311=([^,]*),", ",")
      data <- str_replace_all(data, ",309=([^,]*),", ",")
      data <- str_replace_all(data, ",1146=([^,]*),", ",")
      data <- str_replace_all(data, ",305=([^,]*),", ",")
      data <- str_replace_all(data, ",1150=([^,]*),", ",")
      data <- str_replace_all(data, ",1147=([^,]*),", ",")
      data <- str_replace_all(data, ",1149=([^,]*),", ",")
      data <- str_replace_all(data, ",1148=([^,]*),", ",")
      data <- str_replace_all(data, ",1143=([^,]*),", ",")
      data <- str_replace_all(data, ",762=([^,]*),", ",")
      data <- str_replace_all(data, ",202=([^,]*),", ",")
      data <- str_replace_all(data, ",1234=([^,]*),", ",")
      data <- str_replace_all(data, ",+", ",")




      ## searching for the following tags,
      ## tag52-time stamp
      ## tag75-date
      #  tag83-seq#
      #  tag55-contract
      #  tag269-type: 2-trade
      #  tag270-px
      #  tag271-qty
      #  tag346-# of orders in a given px level
      #  tag5797-buyer/seller initiated
      #  tag33705-order entries

      ## generate the trade data (with order flow)
      ## consider the tradings with both specific defined aggressors or non-defined aggressors (implied order or sides are not defined)
      ## tag5797=0 or tag5797=1 or tag5797=2
      ## first find the tag269=2
      ## Trade summary pattern
      ## 75 60 269=2 55 270 271 346 5797 37711-trade id 37705 37 32


      if (length(data) != 0) {
        if (TRUE %in% unique(str_detect(data, "1022=GBI,"))) {
          data1 <- str_subset(data, "1022=GBI,")

          if (length(data1) != 0) {
            index1 <- str_match_all(
              data1,
              "75=([^,]*),55=([^,]*),200=([^,]*),167=([^,]*),207=([^,]*),15=([^,]*),1142=([^,]*),562=([^,]*),1140=([^,]*),969=([^,]*),9787=([^,]*),1022=([^,]*),264=([^,]*),1022=([^,]*),264=([^,]*),996=([^,]*),"
            )
            meta_data1 <- as.data.table(do.call(rbind, index1))[, -1]
            names(meta_data1)[c(1:16)] <- c(
              "Date",
              "Symbol",
              "MaturityMonthYear",
              "SecurityType",
              "SecurityExchange",
              "Currency",
              "MatchAlgorithm",
              "MinTradeVol",
              "MaxTradeVol",
              "MinPriceIncrement",
              "DisplayFactor",
              "MDFeedType",
              "MarketDepth",
              "MDFeedTypeImplied",
              "MarketDepthImplied",
              "UnitOfMeasure"
            )

          }


          data2 <- data[-which(data %in% data1)]

          if (length(data2) != 0) {
            index2 <- str_match_all(
              data2,
              "75=([^,]*),55=([^,]*),200=([^,]*),167=([^,]*),207=([^,]*),15=([^,]*),1142=([^,]*),562=([^,]*),1140=([^,]*),969=([^,]*),9787=([^,]*),1022=([^,]*),264=([^,]*),996=([^,]*),"
            )
            meta_data2 <- as.data.table(do.call(rbind, index2))[, -1]
            names(meta_data2)[c(1:14)] <- c(
              "Date",
              "Symbol",
              "MaturityMonthYear",
              "SecurityType",
              "SecurityExchange",
              "Currency",
              "MatchAlgorithm",
              "MinTradeVol",
              "MaxTradeVol",
              "MinPriceIncrement",
              "DisplayFactor",
              "MDFeedType",
              "MarketDepth",
              "UnitOfMeasure"
            )

            meta_data <- rbind(meta_data1, meta_data2, fill = TRUE)
          } else{
            meta_data <- meta_data1
          }
        } else{
          index2 <- str_match_all(
            data,
            "75=([^,]*),55=([^,]*),200=([^,]*),167=([^,]*),207=([^,]*),15=([^,]*),1142=([^,]*),562=([^,]*),1140=([^,]*),969=([^,]*),9787=([^,]*),1022=([^,]*),264=([^,]*),996=([^,]*),"
          )
          meta_data2 <- as.data.table(do.call(rbind, index2))[, -1]
          names(meta_data2)[c(1:14)] <- c(
            "Date",
            "Symbol",
            "MaturityMonthYear",
            "SecurityType",
            "SecurityExchange",
            "Currency",
            "MatchAlgorithm",
            "MinTradeVol",
            "MaxTradeVol",
            "MinPriceIncrement",
            "DisplayFactor",
            "MDFeedType",
            "MarketDepth",
            "UnitOfMeasure"
          )
          meta_data <- meta_data2
        }

      }





    }

    if (meta_data[, unique(as.numeric(DisplayFactor))] != 1) {
      meta_data[, grep("Price", colnames(meta_data)) := lapply(.SD, function(x)
        as.numeric(x) * as.numeric(DisplayFactor)), .SDcols = patterns("Price")]

    }

    cat("CME MDP 3.0 Securitity Definition (Meta data)", "\n")
    return(meta_data)
  }


  file <-  fread(sunday_input,
                 header = F,
                 sep = "\\",
                 fill = TRUE)[[1L]]
  meta_data <- main(file, date = date, security = NULL)




}
