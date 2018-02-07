library("xts")

# Add functions
source("script-funs.R")

# Get all comapnies which were included in index
load("R_data/index_company_isins.Rda")

# Change company names to tickers
listed_stocks_data <- read.csv("wse_listed_stocks_till_2017.csv",
                               stringsAsFactors = FALSE)
tickers <- character()
for(i in 1:length(index_company_isins)){
  tickers[i] <- 
    listed_stocks_data[listed_stocks_data$ISIN == index_company_isins[i], 2]
}

selected_comapnies <- c("PKO", "KGH", "BZW")
initial_date <- as.Date("2007-03-16")
filter <- 0.01
funds <- 10000 #PLN
transaction_cost <- 0.02

# Download only non-existing files from stooq.com
DownloadHistData(selected_comapnies)

# To refresh your data delete your quotation files from 
# companies_and_index_historical_data directory
#file.remove(list.files("companies_and_index_historical_data",full.names=TRUE))
#DownloadHistData(tickers_to_download)

# Create combined xts for both close and open prices; consider initial_date
open_prices <- TransformDataToCombinedXts('Open')[paste(initial_date, "/")]
close_prices <- TransformDataToCombinedXts('Close')[paste(initial_date, "/")]

# Create combined xts table with cash section for each company;
# Number in comapny data indicates number of bought stocks for given price;
# Cash column consists of multiplying number of bought stocks by price
# for each comapny
invested_funds <- data.frame(matrix(0, nrow = 1, ncol =
                                      (length(tickers_to_download) + 3)))
colnames(invested_funds) <- c("Date", tickers_to_download, "cash", "value")
invested_funds$Date <- initial_date
invested_funds$cash <- funds
invested_funds$value <- funds
invested_funds <- xts(invested_funds[,-1], order.by =
                        as.Date(invested_funds$Date))
