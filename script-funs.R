DownloadHistData <- function(ticker_vector){
  # Downloads only non-existing files from stooq.com
  # Returns nothing
  # Saves the files in companies_and_index_historical_data directory
  companies_having_data <- sapply(list.files(
    "companies_and_index_historical_data"), gsub, pattern = ".csv", 
    replacement = "")
  
  for(ticker in ticker_vector){
    if(!(ticker %in% companies_having_data)){
      download.file(paste("https://stooq.com/q/d/l/?s=", ticker,"&i=d",
                          sep=""),
                    destfile = paste("./companies_and_index_historical_data/", 
                                     ticker,".csv", sep = ""))
    }
  }
}

TransformDataToCombinedXts <- function(price_type){
  # Transforms all downloaded files from companies_and_index_historical_data 
  # folder
  # Takes as argument string which indicates which prices should be returned
  # Returns xts with open or close prices
  # List files
  exstg_files_tickers <- unlist(lapply(list.files(
    "companies_and_index_historical_data"), gsub, pattern = ".csv",
    replacement = ""))
  
  file_list <- list()
  for(i in 1:length(exstg_files_tickers)){
    file_list[[i]] <- read.csv(paste("./companies_and_index_historical_data/", 
                                     exstg_files_tickers[i], ".csv", sep=""),
                               header=TRUE)
    # Consider lack of data
    if(colnames(file_list[[i]])[1] != "No.data"){
      file_list[[i]] <- xts(file_list[[i]][c(price_type)], 
                            order.by = as.Date(file_list[[i]]$Date))
      names(file_list[[i]]) <- c(exstg_files_tickers[i])
    }else{
      print(paste("no data for: ", exstg_files_tickers[i]))
    }
  }
  
  all_prices <- Reduce(function(x, y) merge(x, y, all=TRUE), file_list)
  return(all_prices)
}

CreateTableForCompany <- function(company, date, cash){
  # Create table with all information and specification needed for each
  # company
  # Returns data.frame
  temp_df <- data.frame(matrix(0, nrow = 1, ncol = 7))
  colnames(temp_df) <- c("Date", "company_name","decision", "shares",
                         "invested", "cash", "end_day_position")
  temp_df$company_name <- company
  temp_df$Date <- date
  temp_df$decision <- NA
  temp_df$cash <- cash
  return(temp_df)
}

Trade <- function(table, open_prices, close_prices, filter, transaction_cost){
  # Simulates trading for given period
  company_name <- table$company_name[1]
  for(day in 2:length(index(open_prices))){
    # Add another row for given day
    table <- rbind(table, table[day - 1, ])
    rownames(table) <- NULL
    table$Date[day] <- index(open_prices)[day]
      
    # Check buy/sell action for given day and add it to new row
    action <- CheckAction(day, company_name, filter)
    table$decision[day] <- action
  
    # Buy or sell and calculate position according to the taken action
    open_price <- open_prices[(day), company_name][[1]]
    close_price <- close_prices[(day), company_name][[1]]
    if(action == "buy"){
      # Buy as many shares as possible for open price and for given transaction
      # cost
      shares_to_buy <- CalcNumberOfSharesToBuy(table$cash[day],
                                               open_price, transaction_cost)
      table$shares[day] <- table$shares[day] + shares_to_buy
        
      # Calculate whole transaction cost
      whole_transaction <- (shares_to_buy * open_price)
      table$invested[day] <- table$invested[day] + whole_transaction
      
      # Include transaction cost
      table$cash[day] <-
        table$cash[day] - (whole_transaction * (1 + transaction_cost))
      
    }else if(action == "sell"){
      # Calculate whole transaction cost
      whole_transaction <- (table$shares[day] * open_price)
      table$invested[day] <- 0
      table$shares[day] <- 0
      
      # Include transaction cost
      table$cash[day] <-
        table$cash[day] + (whole_transaction * (1 - transaction_cost))
      
    }else if(action == "delisted"){
      whole_transaction <-
        (table$shares[day] * close_prices[(day - 1), company_name][[1]])
      table$invested[day] <- 0
      table$shares[day] <- 0
      
      # Include transaction cost
      table$cash[day] <-
        table$cash[day] + (whole_transaction * (1 - transaction_cost))
    }
    # Calculate end day position with close price and cash
    table$end_day_position[day] <-
      table$shares[day] * close_price + table$cash[day]
  }
  return(table)
}

CheckAction <- function(day, company, filter){
  # Calculates the percentage change between open and close price
  # Returns string 'buy' or 'sell'
  # Returns 'delisted' when company was delisted
  # Returns "wait" when nothing changes
  if(is.na(close_prices[(day - 1), company][[1]])){
    return("wait")
  }else if(is.na(open_prices[(day), company][[1]])){
    return("delisted")
  }
  percentage_change <-
    (close_prices[(day - 1), company][[1]] - open_prices[day, company][[1]])/
    (close_prices[(day - 1), company][[1]])
  if(percentage_change >= filter){
    return("buy")
  }else if(percentage_change <= -filter){
    return("sell")
  }
  return("wait")
}

CalcNumberOfSharesToBuy <- function(available_cash, price, cost){
  # Calculates number of shares which can be buy with given transaction cost
  # Return number of shares
  number_of_shares <- floor(available_cash/(price * (1 + cost)))
  return(number_of_shares)
}
