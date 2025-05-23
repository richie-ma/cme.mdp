% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trade_summary.R
\name{trade_summary}
\alias{trade_summary}
\title{Extract trade summary from CME Market by Price data}
\usage{
trade_summary(mbp_input, date, price_displayformat = NULL, sunday_input = NULL)
}
\arguments{
\item{mbp_input}{Market by Price raw data.}

\item{date}{Trading date associated with the raw data, which can be found in
the file name easily in \code{YYYYMMDD} format. This argument is required to select
the parsing format.}

\item{price_displayformat}{Display format of trade price, which could be 1,
0.01, or 0.001, etc. If it is unknown, the nearest Sunday's (not a CME trading
holiday) MBP/MBO data need to be provided to extract the display format.}

\item{sunday_input}{When \code{price_displayformat} is not provided, the nearest
Sunday's (not a CME trading holiday)
MBP/MBO data need to be provided to extract the price display format.}
}
\value{
A list of data.table that contains trade summary for all tradeable
contracts in a trading day.
}
\description{
`trade_summary()` extracts the trade summary messages from the whole CME
Market by Price (MBP) datasets, including timestamps, trade price, trade
quantity, sequence number, number of orders involved in a trade, etc.
}
\section{Trade summary}{

In terms of trade summary, all trades are recorded with specific message tags.
Each trade is timestamped to nanosecond and a unique sequence number is also assigned if multiple
trades occur with the same timestamps. Trade price and quantity executed are recorded
for each trade. The number of orders include all matching (limit) orders and an incoming (market) order.
When implied orders are matched, the number of orders do not contain these implied orders as the CME
does not stipulate it. CME disseminates the trade aggressor for each trade, with 1 indicating buyer-initiated
trades, and 2 seller-initiated trades. A trade aggressor might not be defined when a trade involves implied trades, which is
coded as 0 by the CME.
More details can refer to \url{https://cmegroupclientsite.atlassian.net/wiki/spaces/EPICSANDBOX/pages/457418925/MDP+3.0+-+Trade+Summary}.
}

\section{Fields of trade summary}{

\itemize{
\item Date: Trading date. The night trading session on Sundays is assigned
the trading date as the next Monday.
\item MsgSeq: Message sequence number for each packaet. This is unique for every
message, however, it is not security unique.
\item SendingTime: The timestamp (in UTC) indicates the time when outbound messages to clients
leave the segment gateway (Tag 60).
\item TransactTime: The timestamp (in UTC) indicates the time when incoming messages hit the segment gateway (Tag 52).
\item Code: Contract symbol.
\item Seq: Sequence number, which is security unique. A single packaet may contain information
for multiple tradeable contracts with the single message sequence number, while every
tradeable contract has its unique sequence number.
\item PX: Trade price.
\item Size: Trade size displayed as the number of contracts.
\item Ord: Number of orders involved in a trade, including both matching (limit) order and an incoming (market) order.
\item agg: Trade aggressor generated by the CME. 1 indicates buyer-initiated trades,
2 indicates seller-initiated trades, while 0 indicates no trade aggressor is defined.
}
}

\examples{
# This function requires a CME data license to run
# Example showing how to extract trade summary
# extract trade summary
# Know the display format of trade price
\dontrun{
trades <- trade_summary(file, "2019-01-07", price_displayformat = 1)

# Unknown price_displayformat
trades <- trade_summary(file, "2019-01-07", sunday_input = sunday_file)
}
}
\seealso{
[meta_data()]
}
