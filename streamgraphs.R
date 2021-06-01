library("readr")
library("dplyr")
library("lubridate")
library("streamgraph")
library("htmlwidgets")
library("tm")
library("stringr")
library(RColorBrewer)


removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}


load("~/vienna0211/data/vienna_data_cleaned_with_coordinates.RData")

#remove RT, keep only original tweets
otweets <- tweets[tweets$isretweet==0,]

# select only important information 
tweets_df <- otweets %>%
  select(date, hashtag, text) %>%
  mutate(text = tolower(text), hashtag=tolower(hashtag)) %>% 
 
#select only a period of time
tweets_df <- tweets_df[tweets_df$date >= "2020-11-02" & tweets_df$date <= "2020-11-06",]


#create a list of hashtags and a dataframe 
hashtags_list <- regmatches(tweets_df$hashtag, gregexpr("#[[:alnum:]]+", tweets_df$hashtag))

hashtags_df <- data.frame()
for (i in which(sapply(hashtags_list, length) > 0)) {
  hashtags_df <- bind_rows(hashtags_df, data_frame(date = tweets_df$date[i],
                                                   hashtag = hashtags_list[[i]]))
}


topx <- 30

hashtags_df_sort <- hashtags_df %>%
  filter(hashtag %in% names(sort(table(hashtag), decreasing=TRUE))[1:topx]) %>%
  mutate(yearmonthday =format(as.Date(date), format="%Y-%m-%d")) %>%
  group_by(yearmonthday, hashtag) %>%
  summarise(value = n())


#streamgraph
hsg <- streamgraph(data = hashtags_df_sort, 
                  key = "hashtag", 
                  value = "value", 
                  date = "yearmonthday",
                  offset = "zero",
                  width = "700", height = "400",
                  interpolate = "cardinal") %>%
  sg_legend(TRUE, "hashtag:") %>%
  sg_fill_manual(values = colorRampPalette(brewer.pal(topx, "Spectral"))(topx)) %>%
  sg_axis_x(tick_interval = 1, tick_units = "day", tick_format = "%d-%m") 

show(hsg)


#########################################################################

mentions_list <- regmatches(tweets_df$text, gregexpr("@[[:alnum:]]+", tweets_df$text))


mentions_df <- data.frame()
for (i in which(sapply(mentions_list, length) > 0)) {
  mentions_df <- bind_rows(mentions_df, data_frame(date = tweets_df$date[i],
                                                   mention = mentions_list[[i]]))
}

topx <- 30

mentions_df_sort <- mentions_df %>%
  filter(mention %in% names(sort(table(mention), decreasing=TRUE))[1:topx]) %>%
  mutate(yearmonthday =format(as.Date(date), format="%Y-%m-%d")) %>%
  group_by(yearmonthday, mention) %>%
  summarise(value = n())


msg <- streamgraph(data = mentions_df_sort, 
                   key = "mention", 
                   value = "value", 
                   date = "yearmonthday",
                   offset = "zero",
                   width = "1000", height = "500",
                   interpolate = "cardinal") %>%
  sg_legend(TRUE, "mention:") %>%
  sg_fill_manual(values = colorRampPalette(brewer.pal(topx, "Spectral"))(topx)) %>%
  sg_axis_x(tick_interval = 1, tick_units = "day", tick_format = "%d-%m") 


show(msg)


