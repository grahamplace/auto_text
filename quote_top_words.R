
#'        - TIDF = total number of documents / number of documents which contain the word
#'          - indicates how common a term is across all documents 


#' @param t A dataframe of quotes. 
#' @return A dataframe of quotes, with the top three words for each quote appended at the end as columns. 
#' If a quote does not have three top words, it returns NA. 
top_words <- function(t) {
  library(tm)
  library(magrittr)
  
  #get text of content 
  content <- sapply(t$Text, as.character)
  
  #convert content into a corpus
  corpus <- VectorSource(content) %>% Corpus
  
  #pre-processing text 
  corpus <- corpus %>% tm_map(removePunctuation) %>% 
                      tm_map(content_transformer(tolower)) %>% 
                      tm_map(removeNumbers) %>% 
                      tm_map(removeWords, stopwords("english")) %>% 
                      tm_map(stripWhitespace) %>% 
                      tm_map(stemDocument)
  
  #generate document term matrix 
  dtm <- DocumentTermMatrix(corpus)
  
  #calculate term frequency-inverse document frequency 
  tfidf <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf)) %>% as.matrix() %>% as.data.frame  
  
  t <- cbind(t, first_word = rep(NA, nrow(t)), second_word = rep(NA, nrow(t)), third_word = rep(NA, nrow(t)))
  
  for (i in 1:nrow(t)) {
    temp <- sort(tfidf[i,], decreasing = TRUE)
    index_favorites <- which(colnames(t)=="Favorites")
    for (j in 1:3) {
      if (temp[j]!= 0) {
        t[i, index_favorites +j] <- colnames(temp[j])
      }
    }
  }
  
  return(t)
}
