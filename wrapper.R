get_quote <- function(image_url) {
  
  library(readr)
  
  #source all other function dependencies
  wd <- getwd()
  
  #source imagga tagging
  imagga_path <- paste(wd, "/imagga.R", sep = "")
  source(imagga_path)
  
  #source quote_top_words
  quote_path <- paste(wd, "/quote_top_words.R", sep = "")
  source(quote_path)
  
  #source imagga tagging
  pair_path <- paste(wd, "/pair_tag.R", sep = "")
  source(pair_path)
  
  #get tags of image at passed in url
  tags <- tag_image(image_url)
  
  #get quote csv with top words for every quote
  all_quotes <- read_csv("Content.csv")
  
  #find best quote for the image 
  quote <- pair_tag(all_quotes, tags)
  
  return(quote)
  
}