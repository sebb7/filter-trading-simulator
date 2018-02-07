DownloadHistData <- function(ticker_vector){
  # Downloads only non-existing files from stooq.com
  # Returns nothing
  # Saves the files in companies_and_index_historical_data directory
  companies_having_data <- unlist(lapply(list.files(
    "companies_and_index_historical_data"), gsub, pattern = ".csv", 
    replacement = ""))
  
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
