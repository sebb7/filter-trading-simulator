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

CreateTableForCompany <- function(company){
  # Create table with all information and specification needed for each company
  # Returns xts
  temp_df <- data.frame(matrix(0, nrow = 1, ncol = 6))
  colnames(temp_df) <- c("Date", "decision", "shares", "invested", "cash",
                         "end_day_position")
  temp_df$Date <- initial_date
  temp_df$cash <- funds/length(selected_comapnies)
  temp_df <- xts(temp_df[,-1], order.by =
                   as.Date(temp_df$Date))
  return(temp_df)
}

CheckAction <- function(day, company, filter){
  # Calculates the percentage change between open and close price
  # Returns string 'buy' or 'sell'
  # Returns 'delisted' when company was delisted
  # Returns NA when nothing changes
  if(is.na(close_prices[(day - 1), company][[1]])){
    return(NA)
  }else if(is.na(open_prices[(day), company][[1]])){
    return('delisted')
  }
  percentage_change <-
    (close_prices[(day - 1), company][[1]] - open_prices[day, company][[1]])/
      (close_prices[(day - 1), company][[1]])
  if(percentage_change >= filter){
    return('buy')
  }
  return('sell')
}

Trade <- function(x){

}
