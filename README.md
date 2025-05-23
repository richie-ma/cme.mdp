
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cme.mdp: Clean and Analyze Chicago Mercantile Exchange Market Data in R

<!-- badges: start -->

<!-- badges: end -->

Authors: Richie R. Ma and Brian G. Peterson

The goal of `cme.mdp` is to clean Chicago Mercantile Exchange (CME)
market data with FIX protocol more easily (pretty user-friendly) in the
R environment, including but not limited to trade summaries, quote
updates, and limit order book reconstruction.

## Updates

Version 0.1.1 substantially updates the `order_book` function and the
previous one is deprecated. The new function does not have any explicit
loops, which can improve the speed of processing.

## Introduction

Financial markets have become more transparent and exchanges can provide
high-frequency data for traders to better monitor markets, which creates
more demand about the high-frequency data usage both in the academia and
industry. Most exchanges do not disseminate tabulated complete market
data to non-member market participants, and almost all market data are
specially coded to enhance the communication efficiency. Thus, financial
economists need to know how to clean these untabulated data at first,
which is a substantially time-consuming task. This project will closely
focus on how to parse and clean the market data of Chicago Mercantile
Exchange (CME) under the FIX and MDP protocols and provide other
statistical procedures related to market liquidity (in later version).

## CME market data overview

So far, there have been Market by Price (MBP) data which aggregates all
individual order information (e.g., size) at every price level, and
Market by Order (MBO) data that can show all individual order details
(e.g., order priority) at each price level. The MBO data also provide
more information about trade summaries than the MBP, so that traders are
able to know which limit orders are matched in each trade and their
corresponding matching quantities. The detailed trade summaries also
assign the trade direction more precisely than the MBP and no quote
merge is required for almost all trades. In general, CME will
disseminate the MBP incremental updates followed by the order-level
details (e.g., submission, cancellation) that describes the reason for
MBP updates. Our package considers the above characters and can process
both the MBP and MBO data including quote messages and trade summaries.

## Installation

You can install the development version of cme.mdp from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("richie-ma/cme.mdp")
library(cme.mdp)
```

## Trade summary: MBP and MBO

### MBP trade summary

This is a basic example which shows you how to extract trade summary
messages from the raw MBP data. CME MBP data disseminate trade summary
messages with trade price, trade size, number of orders, and trade
aggressor (direction) for most of trades. Trades that are matched with
implied orders might not have an explicit aggressor from the CME.

CME does not necessarily display the price as what we see from the
Bloomberg Terminal or others and users need to know the display format
of the trade price prior to using `trade_summary()` function. Otherwise,
a Sunday’s input needs to be given to extract this information.

``` r
## The sample data is COMEX gold futures on January 2, 2020
## Sample data can be downloaded at CME website
library(cme.mdp)

gold_trades <- trade_summary(
  mbp_input = "R:/xcec_md_gc_fut_20200102-r-00390.gz",
  date = '2020-01-02',
  price_displayformat = 0.1
)
#> CME MDP 3.0 Trade Summary 
#>  contracts: GCF0 GCG0 GCG1 GCH0 GCJ0 GCM0 GCQ0 GCV0 GCZ0

## Showing the trade summary messages of GCF0 contract

head(gold_trades[["GCF0"]])
#> Key: <Code, Seq>
#>        Date  MsgSeq             SendingTime            TransactTime   Code
#>      <char>   <num>                  <char>                  <char> <char>
#> 1: 20200102 7672654 20200102020146175426919 20200102020146174657851   GCF0
#> 2: 20200102 7676350 20200102020218681738774 20200102020218678765327   GCF0
#> 3: 20200102 7676361 20200102020218682147202 20200102020218678773403   GCF0
#> 4: 20200102 7676364 20200102020218682531825 20200102020218678775243   GCF0
#> 5: 20200102 7676366 20200102020218682721907 20200102020218678777285   GCF0
#> 6: 20200102 7676370 20200102020218682975735 20200102020218678781159   GCF0
#>        Seq     PX  Size   Ord   agg
#>      <num>  <num> <num> <num> <num>
#> 1: 2275838 1518.1     5     1     0
#> 2: 2276877 1518.0     2     1     0
#> 3: 2276886 1518.0     2     1     0
#> 4: 2276890 1518.0     1     1     0
#> 5: 2276894 1518.0     1     1     0
#> 6: 2276898 1518.0     1     1     0
```

### MBO trade summary with order details

CME MBO data further provide the order details for each trade, including
both incoming (market) order and matching (limit) orders. For each trade
summary, the first row shows the information of the incoming (market)
order and the rest rows show the information of matching (limit) orders.

For each trade, the MBO data assign a unique trade ID and users can know
the matched quantity of each matching (limit) order. Generally, the
matched quantity of the incoming (market) order equals to the sum of
matched quantity of all matching (limit) orders when implied orders are
not involved in each trade.

``` r
## The sample data is COMEX gold futures on October 25, 2017
## Sample data can be downloaded at CME website
gold_trades_orders <- mbo_match_details(
  mbo_input = "R:/xcec_md_gc_fut.gz",
  price_displayformat = 0.1
)
#> CME MDP 3.0 Trade Summary with Matching Details 
#>  contracts: GCZ7

## Showing the trade summary messages of GCZ7 contract

head(gold_trades_orders[["GCZ7"]])
#> Key: <MsgSeq>
#>        Date   MsgSeq             SendingTime            TransactTime   Code
#>      <char>    <num>                  <char>                  <char> <char>
#> 1: 20171025 14899367 20171025123000003425726 20171025123000000215181   GCZ7
#> 2: 20171025 14899367 20171025123000003425726 20171025123000000215181   GCZ7
#> 3: 20171025 14899504 20171025123000007263052 20171025123000007035151   GCZ7
#> 4: 20171025 14899504 20171025123000007263052 20171025123000007035151   GCZ7
#> 5: 20171025 14899513 20171025123000008425450 20171025123000008189723   GCZ7
#> 6: 20171025 14899513 20171025123000008425450 20171025123000008189723   GCZ7
#>        Seq     PX   Qty   Ord   agg trade_id     order_id matched_qty
#>      <num>  <num> <num> <num> <num>    <num>       <char>       <num>
#> 1: 5207583 1273.6     1     2     1 15508475 762981983336           1
#> 2: 5207583 1273.6     1     2     1 15508475 762981983304           1
#> 3: 5207611 1273.5     1     2     1 15508484 762981983442           1
#> 4: 5207611 1273.5     1     2     1 15508484 762981983435           1
#> 5: 5207616 1273.6     3     3     1 15508485 762981983447           3
#> 6: 5207616 1273.6     3     3     1 15508485 762981983367           2
```

## Quote messages: MBP and MBO

### MBP quote messages

CME MBP quote messages show quote updates at each book depth and all
relevant order management is based on the limit order book. The updates
include submissions, modifications, and cancellations. CME MBP data
disseminate quoted price, quoted quantity, and the number of orders in
the queue for each book depth, etc. The MBP quote messages can be used
directly to reconstruct the limit order book.

``` r

## Example: corn futures on March 12, 2020
## We only use a part of the original data for illustrative purposes

corn_quotes <- quote_message(mbp_input = "R:/xcbt_md_zc_fut_20200312-r-00348",
                             date = '2020-03-12',
                             price_displayformat = 1)
#> CME MDP 3.0 Quote Messages 
#>  contracts: ZCH0 ZCH1 ZCH2 ZCK0 ZCK1 ZCK2 ZCN0 ZCN1 ZCN2 ZCN3 ZCU0 ZCU1 ZCU2 ZCZ0 ZCZ1 ZCZ2 ZCZ3

## Showing the trade summary messages of ZCK0 contract

head(corn_quotes[["ZCK0"]])
#> Key: <Code, Seq>
#>        Date   MsgSeq             SendingTime            TransactTime   Code
#>      <char>    <num>                  <char>                  <char> <char>
#> 1: 20200312 10307729 20200311214500086980411 20200311214500086711869   ZCK0
#> 2: 20200312 10307731 20200311214500087011116 20200311214500086719357   ZCK0
#> 3: 20200312 10307733 20200311214500087054348 20200311214500086722709   ZCK0
#> 4: 20200312 10307735 20200311214500087078250 20200311214500086725593   ZCK0
#> 5: 20200312 10307738 20200311214500087143791 20200311214500086739517   ZCK0
#> 6: 20200312 10307740 20200311214500087165169 20200311214500086743317   ZCK0
#>        Seq Update   Side     PX   Qty    Ord Implied PX_depth
#>      <num>  <num> <char>  <num> <num> <char>  <char>    <num>
#> 1: 1204314      1      0 373.50     8      3       N        3
#> 2: 1204315      1      0 373.25     1      1       N        4
#> 3: 1204316      1      0 373.00    22      5       N        5
#> 4: 1204317      1      1 374.75     1      1       N        3
#> 5: 1204318      1      1 375.00    24      9       N        4
#> 6: 1204319      1      1 375.25     1      1       N        5
```

### MBO quote messages with queues

CME MBO data can further provide order queues for each quote message
included in the MBP. The MBO data can also show all limit order
information, including order ID, order size, and order priority, etc.

``` r
## The sample data is COMEX gold futures on October 25, 2017
## Sample data can be downloaded at CME website

sp500_quotes_orders <- mbo_quote_queue(
  mbo_input = "R:/xcme_md_es_fut.gz",
  price_displayformat = 0.01
)
#> CME MDP 3.0 Quote Messages with Queue Information 
#>  contracts: ESZ7 ESH8 ESM8

## Showing the trade summary messages of ESZ7 contract

head(sp500_quotes_orders[["ESZ7"]])
#>        Date  MsgSeq             SendingTime            TransactTime Update
#>      <char>  <char>                  <char>                  <char>  <num>
#> 1: 20171025 4504335 20171025123000001200661 20171025123000001087823      0
#> 2: 20171025 4504336 20171025123000001208956 20171025123000001096167      2
#> 3: 20171025 4504355 20171025123000089527420 20171025123000089296549      2
#> 4: 20171025 4504355 20171025123000089527420 20171025123000089296549      2
#> 5: 20171025 4504356 20171025123000089681329 20171025123000089555777      1
#> 6: 20171025 4504357 20171025123000089698822 20171025123000089558125      1
#>      Side   Code     PX   Qty Order_priority     Order_id   Seq
#>    <char> <char>  <num> <num>          <num>       <char> <num>
#> 1:      1   ESZ7 2579.5     1     5535661496 644490560830    NA
#> 2:      0   ESZ7 2549.5     1     5535661193 644490560657    NA
#> 3:      0   ESZ7 2564.5     2     5535661466 644490560815    NA
#> 4:      0   ESZ7 2564.5     2     5535661495 644490560829    NA
#> 5:      0   ESZ7 2523.5    41     5535661510 644490542185    NA
#> 6:      0   ESZ7 2528.0    22     5535661511 644490429553    NA
```

## Order book

### Reconstruction: Pseudo code

The limit order book reconstruction is based on the quote messages for
each single tradeable contract. When the quote update is addition, one
needs to move the existing book depths backward and insert information
(e.g., price, quantity, number of orders). When the quote update is
modification, one just needs to revise the information of that book
depth. Finally, when the quote update is cancellation, one needs to move
the existing book depths forward and nullify the last depth in the book.
Motivated by Christensen and Woodmansey (2013), a pseudocode is shown as
follows:

![](man/figures/algo1.png)

In terms of consolidated limit order book, the function makes each book
contains all records in both outright and implied books, fill missing
values with last observation carrying forward (locf). Then, the initial
consolidated book is set to be the same of the outright limit order
book. For each record in the implied book, we compare the prices of each
implied book record to those in the consolidated book. When the implied
prices are in the existing prices in the consolidated book, we add the
implied quantity to the corresponding depth in the consolidated book.
Otherwise, the implied prices will be sorted with the existing prices in
the consolidated book, with possible book moving if applicable. A
pseudocode is shown as follows:

![](man/figures/algo2.png)

### Feature: Anmiated order book visualization

This package also provides an animated order book application for users
and now only supports `book_bar()` function. Users can simply set
`animation` to `TRUE` and the function will return the animated order
book. Users can also adjust the FPS (frame per second) to control the
animation playing speed. One example is given as below.

![](man/figures/book_bar.gif)

## Acknowledgements

Ma acknowledges the financial support from the [Bielfeldt Office for
Futures and Options Research](https://ofor.illinois.edu/) at the
University of Illinois at Urbana-Champaign. Ma also acknowledges prior
practice from former OFOR members, including but not limited to Anabelle
Couleau and Siyu Bian. Some codes are heavily inspired by their work.
The OFOR has signed non-disclosure agreement with the CME and only
sample data are used here for illustration purposes.

## Help, Feature Requests and Bug Reports

Please post an issue on the [GitHub
repository](https://github.com/richie-ma/cme.mdp/issues).

## References

Christensen, Hugh, R.Woodmansey. “Prediction of Hidden Liquidity in the
Limit Order Book of GLOBEX Futures.” Journal of Trading, Summer 2013,
pp. 68–95.
