library(rtweet)        # Used for extracting the tweets
library(tm)            # Text mining cleaning
library(stringr)       # Removing characters
library(qdapRegex)     # Removing URLs 
library(wordcloud2)    # Creating the wordcloud
library(tidyverse)

tweets<- get_timelines(c("WomenInStat"), n = 1000) #WomenInStat
tweets%>%dim
tweets<-tweets%>% filter(created_at>="2022-07-04")

text <- str_c(tweets$text, collapse = "")
text <- 
  text %>%
  str_remove("\\n") %>%                   # remove linebreaks
  rm_twitter_url() %>%                    # Remove URLS
  rm_url() %>%
  str_remove_all("#\\S+") %>%             # Remove any hashtags
  str_remove_all("@\\S+") %>%             # Remove any @ mentions
  removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("amp"))


textCorpus <- 
  Corpus(VectorSource(text)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
textCorpus <- data.frame(word = names(textCorpus), 
                         freq=textCorpus, 
                         row.names = NULL)


# build wordcloud 
wordcloud <- wordcloud2(data = textCorpus, 
                        minRotation = 0, 
                        maxRotation = 0, 
                        ellipticity = 0.6)
wordcloud


