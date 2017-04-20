options(warn=-1)
require("rvest")
require("stringr")
require("RSelenium")

remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4445L
                      , browserName = "firefox"
)
remDr$open()

user_dir <- readline(prompt="Enter working directory: ")
setwd(user_dir)   #"/Users/kshitijap/Desktop/R_project1/ProjectWork-master"
curr_dir <- getwd()
base_url <- "https://www.zillow.com/homedetails/"
print(paste0(curr_dir, "/Zillow_IDs.txt"))
con = file(paste0(curr_dir, "/Zillow_IDs.txt"))

zillow_ids <- readLines(con, warn = FALSE)
zillow_ids <- strsplit(x = zillow_ids, split = ",")
my_paste <- function(x){
  if(length(x) > 0){
    curr_url <- gsub(x = paste("https://www.zillow.com/homedetails/", x, "_zpid/"), pattern = "[ ]*", replacement = "")
    return( curr_url)
  }
}

zillow_urls <- sapply(X = as.list(zillow_ids[[1]]), my_paste)
zillow_data_df <- data.frame("URL" = zillow_urls)
colnames(zillow_data_df) <- c("URL")
row.names(zillow_data_df) <- NULL

write.csv(file = paste0(curr_dir, "/zillow_data.csv"), x = zillow_data_df)
close(con)