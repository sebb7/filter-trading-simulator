# filter-trading-simulator
Scripts for examining Filter Rule

Assumptions:
  * project analyzes data from 2007-03-16,
  * stocks are not divisible,
  * transaction cost is the percent of the whole amount of the transaction,
  * currency: PLN.

ISINs of companies which were included in WIG20 index at any time are available in `index_comapnies.Rda`. They were obtained from [www.gpw.pl/historical-index-portfolios](URL) and transformed to R-data with `pdf-tables-to-R-data.R` script from [github.com/sebb7/portfolio-analyzer](URL).

`wse_listed_stocks_till_2017.csv` includes all Polish stock companies till 2017 with ISINs and tickers.
