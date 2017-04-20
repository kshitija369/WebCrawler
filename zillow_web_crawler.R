
#Function definitions
getFacts <- function(){
  facts_html <- html_nodes(htmlpage, "#hdp-neighborhood h2, .zsg-h4+ .zsg-sm-1-1 li, .top-facts, .addr_bbs , #region-zipcode .track-ga-event, .addr .notranslate, .main-row span, .z-moreless-content+ .z-moreless-content~ .z-moreless-content+ .z-moreless-content li")
  return( html_text(facts_html))
}

getSchoolInfo <- function(){
  school_html <- html_nodes(htmlpage, "#nearbySchools .clearfix")
  return( html_text(school_html))
}


getPriceHistory <- function(curr_url){
  remDr$navigate(curr_url)
  webElem <- try(remDr$findElement('xpath', '//*[@id="hdp-price-history"]'), silent = TRUE)
  if(class(webElem) == "try-error"){
    webElem <- "Error"
  }else{
    webElem <- webElem$getElementText()
  }
  return (webElem)
}


getTaxHistory <- function(curr_url){
  remDr$navigate(curr_url)
  webElem <- try(remDr$findElement('xpath', '//*[@id="hdp-tax-history"]'), silent = TRUE)
  if(class(webElem) == "try-error"){
    webElem <- "Error"
  }else{
    webElem <- webElem$getElementText()
  }
  return (webElem)
}

# Program code
zillow_pgs <- read.csv(paste0(curr_dir, "/zillow_data.csv"), stringsAsFactors = F)
colnames(zillow_pgs) <- c("No","URL")

final_df <- NULL

for(i in 1:nrow(zillow_pgs)){
  
  listing <- NULL
  htmlpage <- read_html(zillow_pgs$URL[i])
  listing$pg_url <- zillow_pgs$URL[i]
  
  facts <- getFacts()
  #print(facts)
  #Clean facts fields
  listing$zip <- facts[1]
  listing$addr <-  facts[2]
  listing$city <- "Jacksonville; Florida"
  
  listing$beds <- facts[grep(pattern = "beds", x = facts)]
  listing$beds <- gsub(pattern = " beds", x = listing$beds, replacement = "")  
  listing$baths <- facts[grep(pattern = "baths", x = facts)]
  listing$baths <- gsub(pattern = " baths", x = listing$baths, replacement = "")  
  listing$room_counts <- facts[grep(pattern = "Room count: ", x = facts)]
  listing$room_counts <- gsub(pattern = "Room count: ", x = listing$room_counts, replacement = "") 
  listing$lot_size <- facts[grep(pattern = "^Lot: ", x = facts)]
  listing$lot_size <- gsub(pattern = "[a-zA-Z :]", x = listing$lot_size, replacement = "") 
  
  listing$Build_in_yr <- facts[grep(pattern = "^Built in ", x = facts)]
  listing$Build_in_yr <- gsub(pattern = "Built in ", x = listing$Build_in_yr, replacement = "") 
  
  listing$house_type <- facts[grep(pattern = "^Built in ", x = facts) - 1]
  
  listing$neighbor <- facts[grep(pattern = "^Neighborhood: ", x = facts)]
  listing$neighbor <- gsub(pattern = "Neighborhood: ", x = listing$neighbor, replacement = "")   
  listing$neighbor <- gsub(pattern = "[0-9]", x = listing$neighbor, replacement = "")
  
  listing$price_sqft <- facts[grep(pattern = "Price/sqft: ", x = facts)]
  matched_idx <- regexpr(pattern = "Price/sqft: ", text = listing$price_sqft)
  listing$price_sqft <- str_sub(listing$price_sqft, start = (matched_idx[1] + 12))
  listing$price_sqft <- gsub(pattern = "MLS #: .*", x = listing$price_sqft, replacement = "")  
  listing$price_sqft <- gsub(pattern = "\\$", x = listing$price_sqft, replacement = "")
  listing$price_sqft <- gsub(pattern = "\\$", x = listing$price_sqft, replacement = "")
  listing$price_sqft <- gsub(pattern = "[A-Z a-z]+.*", x = listing$price_sqft, replacement = "")
  
  listing$mls_no <- facts[grep(pattern = "MLS #: ", x = facts)]
  matched_idx <- regexpr(pattern = "MLS #: ", text = listing$mls_no)
  listing$mls_no <- str_sub(listing$mls_no, start = (matched_idx[1] + 7), end = (matched_idx[1] + 7 + 6))
  listing$mls_no <- gsub(pattern = "[A-za-z ]", x = listing$mls_no, replacement = "")  
  
  listing$zillow_id <- facts[grep(pattern = "Zillow Home ID: ", x = facts)]
  listing$zillow_id <- gsub(pattern = "Zillow Home ID: ", x = listing$zillow_id, replacement = "")

  listing$current_price <- facts[grep(pattern = "\\$", x = facts)]
  listing$current_price <- gsub(pattern = "[a-zA-Z \\.]",x = listing$current_price, replacement = "")
  listing$current_price <- gsub(pattern = "\\$", x = listing$current_price, replacement = "")[1]
  listing$current_price <- gsub(pattern = "/", x = listing$current_price, replacement = "")

  listing$parcel_no <- facts[grep(pattern = "Parcel #: ", x = facts)]
  listing$parcel_no <- gsub(pattern = "Parcel #: ", x = listing$parcel_no, replacement = "")
  
  listing$last_model_yr <- facts[grep(pattern = "Last remodel year: ", x = facts)]
  listing$last_model_yr <- gsub(pattern = "[A-za-z ]*: [A-za-z ]*", x = listing$last_model_yr, replacement = "")

  listing$floor_sz <- facts[grep(pattern = "Floor size: ", x = facts)]
  listing$floor_sz <- gsub(pattern = "[A-za-z ]*: ", x = listing$floor_sz, replacement = "")
  listing$floor_sz <- gsub(pattern = "[A-za-z ]*", x = listing$floor_sz, replacement = "")
  
  listing$stories <- facts[grep(pattern = "Stories: ", x = facts)]
  listing$stories <- gsub(pattern = "[A-za-z ]*: ", x = listing$stories, replacement = "")
  listing$stories <- gsub(pattern = " .*$", x = listing$stories, replacement = "")
  
  listing$structure_type <- facts[grep(pattern = "Structure type: ", x = facts)]
  listing$structure_type <- gsub(pattern = "[A-za-z ]*: ", x = listing$structure_type, replacement = "")
    
  listing$school_info <- getSchoolInfo()
  #Clean school field
  listing$school_info <- gsub(pattern = " ",x = listing$school_info, replacement = "")
  listing$school_info <- gsub(pattern = "\\n\\n",x = listing$school_info, replacement = "")
  listing$school_info <- gsub(pattern = "\\n",x = listing$school_info, replacement = "_")
  listing$school_info <- gsub(pattern = "outof10",x = listing$school_info, replacement = "")
  listing$school_info <- paste(listing$school_info, collapse = ";")
  
  #Price History
  #listing$price_hist <- ""
  listing$price_hist <- getPriceHistory(listing$pg_url)
  if(str_detect(listing$price_hist, "[E|e]rror") == FALSE)
  {
    listing$price_hist <- getPriceHistory(listing$pg_url)
    listing$price_hist <- gsub(pattern = "Price History\\nDATE EVENT PRICE \\$/SQFT SOURCE\\n|More",x = listing$price_hist, replacement = "")
    
    tmp_price_hist <- strsplit(listing$price_hist, "\\n")
    categorize_price_hist <- function(x){
      tmp_str <- gsub(pattern = "[-|+][0-9].[0-9]%",x = x, replacement = "")
      tmp_str <- gsub(pattern = "\\.|\\$",x = tmp_str, replacement = "")
      tmp_str <- gsub(pattern = " +",x = tmp_str, replacement = "_")
      tmp_str <- gsub(pattern = "_$",x = tmp_str, replacement = "")
      return(tmp_str)
    }
    tmp_price_hist <- sapply(tmp_price_hist, categorize_price_hist)
    listing$price_hist <- paste0(tmp_price_hist, collapse = ";")
  }else{
    listing$price_hist <- NA
  }
  
  #Tax History
  #listing$tax_hist <- ""
  listing$tax_hist <- getTaxHistory(listing$pg_url)
  if(str_detect(listing$tax_hist, "[E|e]rror") == FALSE)
  {
    listing$tax_hist <- getTaxHistory(listing$pg_url)
    listing$tax_hist <- gsub(pattern = "Tax History\nFind assessor information on the county website\nYEAR PROPERTY TAXES CHANGE TAX ASSESSMENT CHANGE\nMore\n",x = listing$tax_hist, replacement = "")
    
    tmp_tax_hist <- strsplit(listing$tax_hist, "\\n")
    categorize_tax_hist <- function(x){
      tmp_str <- gsub(pattern = "[-|+][0-9]+.?[0-9]+%",x = x, replacement = "")
      tmp_str <- gsub(pattern = "\\.|\\$",x = tmp_str, replacement = "")
      tmp_str <- gsub(pattern = " +",x = tmp_str, replacement = "_")
      tmp_str <- gsub(pattern = "_$",x = tmp_str, replacement = "")
      return(tmp_str)
    }
    tmp_tax_hist <- sapply(tmp_tax_hist, categorize_tax_hist)
    listing$tax_hist <- paste0(tmp_tax_hist, collapse = ";")
  }else{
    listing$tax_hist <- NA
  }
  fun <- function(x){
    x <- gsub(pattern = "\\s+",x = x, replacement = "")
    if(length(x) == 0){
      x = NA 
    }
    return (x)
  }
  listing <- lapply(listing,fun)
  curr_df <- c(listing$zillow_id, listing$current_price, listing$price_sqft, listing$price_hist, listing$tax_hist,listing$pg_url,listing$addr, listing$zip, 
               listing$neighbor, listing$school_info, listing$lot_size, listing$floor_sz, listing$mls_no, listing$parcel_no, listing$stories, listing$Build_in_yr, 
               listing$last_model_yr, listing$house_type, listing$structure_type, listing$baths, listing$beds, listing$room_counts)
  
  final_df <- rbind(final_df, curr_df)
  print(paste("Completed : ", (as.double(i/nrow(zillow_pgs)*100)), "%"))
}

colnames(final_df) <- c("Zillow_Home_ID",	"Current_Price",	"Price_Each_SQFT",	"Price_History",	"Tax_History",	"URL",	
                        "Address",	"Zip_Code",	"Neighborhood",	"School",	"Lot_Size",	"Floor_Size",	"MLS_Number",	
                        "Parcel_Number",	"Stories",	"Built_Year",	"Last_Remodel_Year",	"House_Type",	"Structure_Type",	
                        "Bath_Num",	"Bed_Num",	"Room_Num")

rownames(final_df) <- NULL
write.csv(file = paste0(curr_dir, "/jacksonville.csv"), x = final_df)
write.table(file = paste0(curr_dir, "/jacksonville.txt"), x = final_df, sep = "\t")
close(con1,con2)
#remDr$close()
#print(final_df)


