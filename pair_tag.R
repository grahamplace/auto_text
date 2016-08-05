#' @param content_table A dataframe of content, where the last three columns correspond to the top 3 words in each piece of content.
#' @param image A dataframe of image tags and their confidence intervals for one picture. 
#' @return A string that is the recommended quote for an image. 
pair_tag <- function(content_table, image_tags) {
  #compare top tag from images to tags from content 
  top_tag <- image_tags[1,which(colnames(image_tags)== "Tag")]
  
  index_favorites <- which(colnames(content_table)=="Favorites")
  index_text <- which(colnames(content_table)=="Text")
  for (i in 1:nrow(content_table)) {
    for (j in 1:3) {
      if (is.na(content_table[i, index_favorites+j])) {
        break
      }
      else if (top_tag == content_table[i, index_favorites+j]) {
        return(content_table[i, index_text])
      }
    }
  }
  
  return("Hope anchors the soul.")
}

