# Author: Federica Gazzelloni
library(rtweet)
library(tidyverse)
library(extrafont)
# loadfonts()

library(webshot)
library(htmlwidgets)

# Recent Tweets where "WomenInStat" is in
wis_recent_mentions<- rtweet::search_tweets("WomenInStat",
                                #n=2000,
                                retryonratelimit=TRUE,
                                type = "mixed",
                                include_rts = F)

# saveRDS(wis_recent_mentions,"data/wis_recent_mentions.rds")
# wis_recent_mentions<-readRDS("data/wis_recent_mentions.rds")

features<- wis_recent_mentions %>%
  #count(created_at) %>%
  arrange(created_at) %>% DataExplorer::profile_missing() %>%
  arrange(-pct_missing)%>%
  filter(pct_missing<0.5) %>% count(feature)%>%select(-n)%>%unlist()


idx <- match(features, names(wis_recent_mentions))

df <- wis_recent_mentions %>%
  arrange(created_at) %>% 
  select(idx) %>% 
  select(!contains(c("url","source","coords","id","is","lang","protected","verified","account"))) %>% # View()
  mutate(location=case_when(location==""~"Unknown",TRUE~location)) 


# retweet_count vs favorite_count
  df %>%
    ggplot(aes(favorite_count,retweet_count)) + 
    geom_smooth(fill="pink",color="grey15",alpha=0.2) +
    geom_jitter(shape=21,stroke=0.5,fill="grey80",color="grey50")+
    labs(title="@WomenInStat Twitter account",
         subtitle="Counts of retweet vs favorite 2022 July 04-10",
         caption="DataSource: Twitter API @WomenInStat\nGraphics: Federica Gazzelloni (@fgazzelloni)",
         x="Favorite",y="Retweet") +
    ggthemes::theme_fivethirtyeight()+
    theme(text = element_text(family = "Roboto Condensed"),
      axis.title = element_text())
  
  # ggsave("images/rtvsfav.png")
  
  
  
  
  #-------get_timelines---------
  
  library(tm)            # Text mining cleaning
  library(stringr)       # Removing characters
  library(qdapRegex)     # Removing URLs 
  library(wordcloud2)    # Creating the wordcloud
 
  
tweets<- get_timelines(c("WomenInStat"), n = 1000) #WomenInStat

tweets2 <-tweets %>% 
  filter(created_at>="2022-07-04") %>%
  mutate(date=lubridate::day(created_at)) #%>%
  #count(date) 

features <-tweets2 %>% DataExplorer::profile_missing() %>%
  arrange(-pct_missing)%>%
  filter(pct_missing<=0) %>%
  count(feature) %>%select(-n) %>% unlist()
  

text <- str_c(tweets2$text, collapse = "")
  

text2 <- 
    text %>%
    str_remove("\\n") %>%                   # remove linebreaks
    rm_twitter_url() %>%                    # Remove URLS
    rm_url() %>%
    rm_non_words() %>%
    str_remove_all("#\\S+") %>%             # Remove any hashtags
    str_remove_all("@\\S+") %>%             # Remove any @ mentions
    removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
    removeNumbers() %>%
    stripWhitespace() %>%
    removeWords(c("amp"))


textCorpus <- 
  Corpus(VectorSource(text2)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
textCorpus <- data.frame(word = names(textCorpus), 
                         freq=textCorpus, 
                         row.names = NULL)


# build wordcloud -------
wordcloud <- wordcloud2(data = textCorpus, 
                        minRotation = 0, 
                        maxRotation = 0, 
                        ellipticity = 0.6)
wordcloud 



saveWidget(wordcloud, "tmp.html", selfcontained = F)
webshot("tmp.html", "images/wordcloud3.png")
unlink()  


#-----------

idx <- match(features, names(tweets2))

df <- tweets2 %>%#count(name)
  arrange(created_at) %>% 
  select(idx) %>% 
select(text,display_text_width,is_quote:retweet_count,date)


df%>%head
df %>%
  ggplot(aes(x=factor(date),y=favorite_count))+
  geom_col(aes(fill=retweet_count))
  coord_polar()
  
  df %>%
    ggplot(aes(x=factor(date),y=display_text_width))+
    geom_col(aes(fill=is_retweet)) +
    ggthemes::scale_fill_fivethirtyeight()+
    labs(title="@WomenInStat Twitter account",
         subtitle="Counts of Text width for tweets and retweets 2022 July 04-10",
         caption="DataSource: Twitter API @WomenInStat\nGraphics: Federica Gazzelloni (@fgazzelloni)",
         fill="Is retweet ?",
         x="July day",y="Text width") +
    ggthemes::theme_fivethirtyeight()+
    theme(text = element_text(family = "Roboto Condensed"),
          axis.title = element_text())

  ggsave("images/tweets_retweets.png",
         width = 8,
         height = 6)    
  
  
  
  
  
  
  
