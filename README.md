
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cme.mdp

<!-- badges: start -->
<!-- badges: end -->

The goal of cme.mdp is to clean Chicago Mercantile Exchange (CME) market
data with FIX protocol more easily in the R environment, including but
not limited to trade summaries, quote updates, and limit order book
reconstruction.

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
# install.packages("devtools") #Install the 'devtools' package if you haven't
devtools::install_github("richie-ma/cme.mdp")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cme.mdp)
#> Loading required package: data.table
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
