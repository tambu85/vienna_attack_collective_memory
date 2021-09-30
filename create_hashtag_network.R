library(ggplot2)
library(igraph)
library(dplyr)
library(stringr)


# 
load("~/data/vienna_data_cleaned_with_coordinates.RData")
load("~/data/hashtags_df.RData")


tweets$user_id <- tweets$author_id
otweets <- tweets[tweets$isretweet==0,]
ctweets <- left_join(otweets,users,by="user_id")
numTweets <- length((ctweets$tweet_id))
numUsers <- length(users$user_id)


tweets_df <- ctweets %>%
  mutate(text = tolower(text), hashtag=tolower(hashtag))

hashtags_list <- regmatches(tweets_df$hashtag, gregexpr("#[[:alnum:]]+", tweets_df$hashtag))

hashtag_grouped <- hashtags_df %>%
  group_by(hashtag) %>%
  count()  

numHashtags <- length(hashtag_grouped$hashtag)

h_g <- make_empty_graph(n=numHashtags, directed = FALSE)
V(h_g)$label <- hashtag_grouped$hashtag
V(h_g)$magnitude <- hashtag_grouped$n
summary(h_g)


for(i in 1:numTweets){
  if(i%%1000==0){print(i)}
  hashtag_vec <- hashtags_list[[i]]

  if(length(hashtag_vec)>1){
    for(h1 in 1:(length(hashtag_vec)-1)){
      for(h2 in (h1+1):length(hashtag_vec)){
        n1 <- V(h_g)[V(h_g)$label==hashtag_vec[h1]]
        n2 <- V(h_g)[V(h_g)$label==hashtag_vec[h2]]
        
        if(length(n1)>0){
          if(length(n2)>0){
          
            if(are.connected(h_g, n1, n2)){
              E(h_g)[get.edge.ids(h_g,c(n1,n2))]$weight <- E(h_g)[get.edge.ids(h_g,c(n1,n2))]$weight +1
            }
            else{
              h_g <- add_edges(h_g, c(n1,n2),weight=1)
            }
          }else{print(paste("it:",i,"node n2",hashtag_vec[h2],"does not exist"))}
        }else{print(paste("it:",i,"node n1",hashtag_vec[h1],"does not exist"))}
      }
    }
  }
}

V(h_g)$deg=degree(h_g)
summary(h_g)

filename_hashtag_graph="~/data/hashtag_cooccurrences_graph_noRT.RData"
save(h_g,file=filename_hashtag_graph,envir = parent.frame())
