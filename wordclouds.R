library(tm)            
library(stringr)       
library(qdapRegex)    
library(wordcloud2)    

removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  print(x)
  print(x[!x %in% stopwords])
  paste(x[!x %in% stopwords], collapse = " ")
}


load("~/vienna0211/data/vienna_data_cleaned_with_coordinates.RData")
sort(table(tweets$language),decreasing=TRUE)

#create several subset for each language
en_tweets <- tweets[tweets$language=="en" & tweets$isretweet==0,]
de_tweets <- tweets[tweets$language=="de" & tweets$isretweet==0,]
fr_tweets <- tweets[tweets$language=="fr" & tweets$isretweet==0,]
it_tweets <- tweets[tweets$language=="it" & tweets$isretweet==0,]
es_tweets <- tweets[tweets$language=="es" & tweets$isretweet==0,]
hi_tweets <- tweets[tweets$language=="hi" & tweets$isretweet==0,]
tr_tweets <- tweets[tweets$language=="tr" & tweets$isretweet==0,]

#choose which language represent
tw <- en_tweets

#clean the text
cleantext <- str_c(tw$text, collapse = "") %>%
  str_remove("\\n") %>%              
  removeNumbers() %>%                
  rm_twitter_url() %>%               
  rm_url() %>%
  str_remove_all("#\\S+") %>%        
  str_remove_all("@\\S+") %>%       
  stripWhitespace() %>%
  removeWords(c("amp")) %>%              
  str_replace_all("[[:punct:]]", " ")

#list of most frequent words
fw <- 
  Corpus(VectorSource(cleantext)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

fw <- sort(rowSums(fw), decreasing=TRUE)
wc_df <- data.frame(word = names(fw), freq=fw, row.names = NULL)

#ENGLISH
wc_df <- wc_df[!(wc_df$word %in% stopwords("en")),]

#GERMAN
wc_df <- wc_df[!(wc_df$word %in% stopwords("de")),]

#FRENCH
wc_df <- wc_df[!(wc_df$word %in% stopwords("fr")),]

#ITALIAN
wc_df <- wc_df[!(wc_df$word %in% stopwords("it")),]

#SPANISH
wc_df <- wc_df[!(wc_df$word %in% stopwords("es")),]


wc_df2 <- wc_df[wc_df$freq>10,]

wc_df <- wc_df[order(wc_df$freq, decreasing = TRUE),]
wordcloud <- wordcloud2(data = wc_df2, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
show(wordcloud)

