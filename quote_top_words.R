#' Text Recognition
#' - Stemming - words with the same roots (needs to be done for images and quotes)
#'    - SnowballC
#' - Document Term Matrix 
#'    - DocumentTermMatrix
#'      - maps the occurences of words to a corpus
#'      - can isolate if any word in a given quote is significant relative to the rest of the words in the corpus
#'      - if it is significant, it can then be used as a meaningful and unique tag 
#'      
#'      
#'      
#'      Steps: 
#'      1. convert quotes into a corpus 
#'        
#'        - for a vector of strings, use VectorSource
#'        - to extract a quote from the corpus: corpus[[i]]
#'        - to get the text of the quote: as.character(corpus[[i]])
#'      2. pre-processing
#'        - tm_map(corpus, removePunctuation)
#'        - tm_map(corpus, content_transformer(tolower))
#'        - tm_map(corpus, removeNumbers) 
#'        - tm_map(corpus, removeWords, stopwords("english")) 
#'             - next use tm_map(corpus, stripWhitespace) #to remove multiple spaces left after removing text 
#'        - 
#'      3. stemming 
#'        - tm_map(corpus, stemDocument )
#'      4. document term matrix
#'        - the columns corresponds to words, and the rows correspond to documents 
#'        - the numbers in the matrix correspond to the number of times that a given word occurs in a specific document 
#'      5. find the inverse document frequency of each word in a quote 
#'        - TIDF = total number of documents / number of documents which contain the word
#'          - indicates how common a term is across all documents 

setwd("C:/Users/maggie.lou/Dropbox/MyDocuments/NU 2019/Data Mining/Projects/Over/twitter")

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

quotes <- c("All I love are flowers", "I love mountains", "I love men", "of mice and men!!!")

quotes <- read.csv("Quotes.csv")
quotes <- sapply(quotes$Text, as.character)


as.character(corpus[[1]])
tfidf
