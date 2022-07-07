
library(rtweet)
library(tidyverse)

wrl_200<- rtweet::search_tweets("WomenInStat",
                        n=200,
                        include_rts = F)

# saveRDS(wrl_200,"data/wrl_200.rds")
wrl_200<-readRDS("data/wrl_200.rds")

names(wrl_200)
View(wrl_200)

df <- wrl_200%>%dplyr::select(created_at,
                              location,
                              quoted_favorite_count,
                              text,
                              hashtags,
                              retweet_location,
                              reply_to_status_id)


# Location

df%>%
  mutate(location=case_when(location==""~"Unknown",TRUE~location)) %>% 
  filter(created_at>="2022-07-04") %>% # DataExplorer::profile_missing()
  ggplot(aes(created_at,fct_reorder(location,created_at),fill=location))+
  geom_col(show.legend = F) +
  scale_fill_brewer() +
  labs(title="@WomenInStat tweets interaction by location",
       subtitle = "Period: 2022 July 04 to 07",
       caption="DataSource: Twitter API @WomenInStat | Graphics: @fgazzelloni")+
  ggthemes::theme_fivethirtyeight()+
  theme(plot.title.position = "plot")

# ggsave("images/location.png")
  

# Tweets

df%>%
  mutate(location=case_when(location==""~"Unknown",TRUE~location),
         date=as.Date(created_at)) %>%
  filter(created_at>="2022-07-04") %>% #DataExplorer::profile_missing()
  count(date) %>%
  ggplot(aes(date,n,fill=n))+
  geom_col()+
  guides(color="none")+
  viridis::scale_fill_viridis() +
  labs(title="Number of Tweets",
       subtitle="Jul 04 to Jul 07",
       x="Created at",y="N.",
       caption="DataSource: Twitter API @WomenInStat | Graphics: @fgazzelloni")+
  ggthemes::theme_fivethirtyeight()+
  theme(plot.title = element_text(size=24),
        plot.title.position = "plot")

# ggsave("images/n_tweets.png")
