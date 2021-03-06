library("xts")
library("parallel")
library("ggplot2")

# Add functions
source("script-funs.R")

# Get all comapnies which were included in index
# Obtained from www.gpw.pl/historical-index-portfolios and 
# transformed to R-data with pdf-tables-to-R-data.R script from
# github.com/sebb7/portfolio-analyzer.
load("R_data/index_company_isins.Rda")

# Change company names to tickers
listed_stocks_data <- read.csv("wse_listed_stocks_till_2017.csv",
                               stringsAsFactors = FALSE)
index_tickers <- sapply(index_company_isins, function(isin)
  listed_stocks_data[listed_stocks_data$ISIN == isin, 2])

# Select companies for simulation (max = 36)
selected_comapnies <- sample(unname(index_tickers), 6)

# User configuration
filter <- 0.005
funds <- 10000 #PLN
transaction_cost <- 0.000
# Make simulation more realistic:
# price rises + handicap while buying
# price falls - handicap while selling
handicap <- 0.000

initial_date <- as.Date("2007-03-16")

# Download only non-existing files from stooq.com
DownloadHistData(selected_comapnies)

# To refresh your data delete your quotation files from 
# companies_and_index_historical_data directory
# file.remove(list.files("companies_and_index_historical_data",full.names=TRUE))
# DownloadHistData(selected_comapnies)

# Create combined xts for both close and open prices; consider initial_date
open_prices <- TransformDataToCombinedXts('Open')[paste(initial_date, "/")]
close_prices <- TransformDataToCombinedXts('Close')[paste(initial_date, "/")]

# Create combined xts tables with cash section for each company;
# Number in comapny 'shares' colum indicates number of bought stocks 
# for given open price after taking decision; transaction cost included
# 'end_day_position' column consists of multiplying number of bought stocks by 
# close price
for(company in selected_comapnies){
  assign(paste(company, "_table", sep = ""),
         CreateTableForCompany(company, initial_date,
                               funds/length(selected_comapnies)))
  rm(company)
}

# Start trading 

# For each company - parallel version with time of execution
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type="FORK")
start.time <- Sys.time()
results <- parLapply(cl, selected_comapnies, function(company)
  Trade(get(paste(company, "_table", sep = "")), open_prices, close_prices,
        filter, transaction_cost, handicap))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
stopCluster(cl)

# For each company - no-parallel version with time of execution
start.time <- Sys.time()
results <- lapply(selected_comapnies, function(company)
  Trade(get(paste(company, "_table", sep = "")), open_prices, close_prices,
        filter, transaction_cost, handicap))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

names(results) <- selected_comapnies

# Get stats and info for results

# Print results of trading for each company
for(company_name in names(results)){
  print(kable(tail(results[[company_name]])))
}

# Print plot of end_day position for each company
for(company_name in names(results)){
  ggplot(data = results[[company_name]],
         aes(x = Date, y = end_day_position)) +geom_line() +
    ggtitle(paste("End day position for",
                  (listed_stocks_data[listed_stocks_data$Ticker ==
                                        company_name, 1]),
                  sep = " "))
}
