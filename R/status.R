

#' Extract trading status from the CME Market by Price and/or Market by Order data
#'
#' `status()` extracts the trading status from the CME Market by Price and/or Market
#' by Order data. This function indicates the sequence numbers
#' of pre-open session, opening session, continuous trading session, etc.
#'
#' @param input CME MBO/MBP data
#' @param date Trading date associated with the raw data, which can be found in
#' the file name easily in \code{YYYYMMDD} format. This argument is required to select
#' the parsing format.
#'
#' @returns A data.table contains sequence numbers, trading session indicator, etc.
#'
#' @export
#' @import data.table stringr
#'
#'
#' @examples
#' # This function requires a CME data license to run
#' # Example showing how to extract trading status
#' # Know the specific security (e,g., ESH9)
#' \dontrun{
#' es.status <- status(file, "2019-01-07")
#'
#' # For all tradable contracts in E-mini S&P 500 futures
#' eg.status <- status(file, "2019-01-07")
#' }
#'
status <- function(input, date) {
  SessionID <- Code <- Seq <- MsgSeq <- session <- TradingStatus <- TradingEvent <- NULL
  date <- as.Date(date)

  if (inherits(date, "Date") == FALSE) {
    stop("date should be in the format as YYYY-MM-DD.")

  }

  data <-  fread(input,
                 header = F,
                 sep = "\\",
                 fill = TRUE)[[1L]]


  if (date < "2015-11-20") {
    ### One need to find the continuous trading session and the opening match sessions by different securities


    Index <- str_subset(data, "\001336=([^,]*)")

    rm(data)

    Index <- str_replace_all(Index, "\001", ",")


    Index <- str_replace_all(Index,
                             "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),",
                             "")


    Index <- str_replace_all(Index,
                             ",268=([^,]*),279=([^,]*),22=([^,]*),48=([^,]*),",
                             ",")
    Index <- str_replace_all(Index, ",10=([^,]*),", "")
    Index <- str_replace_all(Index,
                             ",269=([^,]*),270=([^,]*),271=([^,]*),273=([^,]*)",
                             "")
    Index <- str_replace_all(Index, ",276=([^,]*)", "")

    if (length(Index) != 0) {
      Index1 <- str_match_all(Index, "83=([^,]*),107=([^,]*),336=([^,]*),")
      n_row <- sapply(Index1, nrow)


      Index1 <- as.data.table(do.call(rbind, Index1))[, -1]

      names(Index1)[c(1:3)] <- c("Seq", "Code", "SessionID")

      Index1$Seq <- as.numeric(Index1$Seq)
      Index1$SessionID <- as.numeric(Index1$SessionID)


      Index1.info <- unlist(str_extract_all(Index, "34=([^,]*),52=([^,]*),"))
      Index1.info <- str_dup(Index1.info, n_row)

      Index1.info <- str_match_all(Index1.info, "34=([^,]*),52=([^,]*)")
      Index1.info <- as.data.table(do.call(rbind, Index1.info))[, -1]
      names(Index1.info)[c(1:2)] <- c("MsgSeq", "Time")

      Index1 <- cbind(Index1.info, Index1)
      rm(Index1.info)

      Index1 <- Index1[SessionID == 0 | SessionID == 1 | SessionID == 2]
      Index1[, SessionID := fifelse(SessionID == 0, "preopen",
                                    fifelse(SessionID ==1, "opening", "continuous"))]
      Index1$MsgSeq <- as.numeric(Index1$MsgSeq)

      setkey(Index1, Code, SessionID, Seq)

      Session_info <- Index1
      Session_info <-Session_info[, .SD[1], by=.(Code, SessionID)]


    }


  } else{
    Session_info <- list()

    Index <- str_subset(data, "\00135=f")
    rm(data)
    Index <- str_replace_all(Index, "\001", ",")

    Index <- str_replace_all(Index, ",5799=([^,]*),", ",")
    Index <- str_replace_all(Index,
                             "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),",
                             "")
    Index <- str_replace_all(Index, "1151=([^,]*),", "")
    Index <- str_replace_all(Index, "6937=([^,]*),", "")
    Index <- str_replace_all(Index, "10=([^,]*),", "")
    Index <- str_replace_all(Index, "327=([^,]*),", "")

    Index1 <- str_subset(Index, "326=21")

    if (length(Index1) != 0) {
      Index1 <- str_match_all(
        Index1,
        "34=([^,]*),52=([^,]*),60=([^,]*),75=([^,]*),326=([^,]*),1174=([^,]*),"
      )



      Index1 <- as.data.table(do.call(rbind, Index1))[, -1]

      names(Index1)[c(1:6)] <- c(
        "MsgSeq",
        "SendingTime",
        "TransactTime",
        "Date",
        "TradingStatus",
        "TradingEvent"
      )
      Session_info[[1]] <- Index1
    }



    Index2 <- str_subset(Index, "326=15")

    if (length(Index2) != 0) {
      Index2 <- str_match_all(
        Index2,
        "34=([^,]*),52=([^,]*),60=([^,]*),75=([^,]*),326=([^,]*),1174=([^,]*),"
      )
      Index2 <- as.data.table(do.call(rbind, Index2))[, -1]
      names(Index2)[c(1:6)] <- c(
        "MsgSeq",
        "SendingTime",
        "TransactTime",
        "Date",
        "TradingStatus",
        "TradingEvent"
      )
      Session_info[[2]] <- Index2
    }

    Index3 <- str_subset(Index, "326=17")

    if (length(Index3) != 0) {
      Index3 <- str_match_all(
        Index3,
        "34=([^,]*),52=([^,]*),60=([^,]*),75=([^,]*),326=([^,]*),1174=([^,]*),"
      )
      Index3 <- as.data.table(do.call(rbind, Index3))[, -1]
      names(Index3)[c(1:6)] <- c(
        "MsgSeq",
        "SendingTime",
        "TransactTime",
        "Date",
        "TradingStatus",
        "TradingEvent"
      )
      Session_info[[3]] <- Index3
    }


    Session_info <- rbindlist(Session_info)

    if (dim(Session_info)[1] != 0) {
      setkey(Session_info, MsgSeq)

      Session_info[, session := fifelse(
        TradingStatus == 21 &
          (TradingEvent == 0 |
             TradingEvent == 4),
        "preopen",
        fifelse(
          TradingStatus == 21 & TradingEvent == 1,
          "preopen_nocancel",
          fifelse(
            TradingStatus == 15,
            "opening",
            fifelse(TradingStatus == 17, "open", "none")
          )
        )
      )]


    }

  }

  return(Session_info)
}
