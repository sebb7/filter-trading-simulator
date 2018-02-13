library("xts")
library("parallel")
no_cores <- detectCores() - 1

# Add functions
source("script-funs.R")

# Get all comapnies which were included in index
load("R_data/index_company_isins.Rda")

# Change company names to tickers
listed_stocks_data <- read.csv("wse_listed_stocks_till_2017.csv",
                               stringsAsFactors = FALSE)
tickers <- sapply(index_company_isins, function(isin)
  listed_stocks_data[listed_stocks_data$ISIN == isin, 2])

initial_date <- as.Date("2007-03-16")

# User configuration
selected_comapnies <- c("PKO", "KGH", "PZU", "MBK")
filter <- 0.01
funds <- 10000 #PLN
transaction_cost <- 0.003

# Make simulation more realistic:
# price rises + handicap while buying
# price falls - handicap while selling
handicap <- 0.005

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

# Get stats for results
View(results[[1]])
View(results[[4]])
View(results[[3]])
View(results[[2]])
