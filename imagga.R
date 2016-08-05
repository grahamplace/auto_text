#test url
url <- "https://hd.unsplash.com/photo-1465232377925-cce9a9d87843"

#function to return df of tags w/confidence based on image recognition performed on image at passed in url 
tag_image <- function(url) {
  
  #load package
  library(httr)
  
  #store auth keys
  api_key <- "acc_d4e484d32c6754e"
  api_secret <- "39331edd5fafb67536beacda2e8f81a2"
  
  #build url with api endpoint
  comb_url <- paste("https://api.imagga.com/v1/tagging?url=", url, sep = "")

  #get request to immaga api 
  imagga_resp = GET(comb_url, authenticate(api_key, api_secret))
  
  #if get request is successful:
  if(http_status(imagga_resp)$category == "Success") {
    
    #get/parse response content 
    resp_content <- content(imagga_resp, as = "parsed")
    
    #get only results from content
    results <- resp_content$results
    
    #get tags as list from results
    tags <- results[[1]]$tags
    
    #unlist tags to vector
    unlistTags <- unlist(tags)
  
    #index vector properly to build dataframe
    tagIndex <- seq(from = 2, to = length(unlistTags), by = 2)
  
    #seperate tags and confidence scores 
    tags <- unlistTags[tagIndex]
    confidence <- unlistTags[-tagIndex]
  
    #build dataframe fom results
    df <- data.frame("Tag" = tags, "Confidence" = confidence)
    
    #return data frame with tags + confidences
    return(df)
  }
  
  #if get request is unsuccessful
  else {
    print("Error processing image.")
    return(FALSE)
  }
  
}