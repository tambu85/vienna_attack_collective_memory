library(ggplot2)
library(igraph)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(twitteR)
library(visNetwork)

load("~/data/vienna_data_cleaned_with_coordinates.RData")

all_tweets <- rbind(tweets,other_tweets)
all_tweets <- all_tweets[!duplicated(all_tweets$tweet_id),]

numTweets <- length((all_tweets$tweet_id))
numUsers <- length(users$user_id)


tweets$user_id <- tweets$author_id

m_tweets <- left_join(tweets,users,by="user_id")
m_tweets <- m_tweets[!is.na(m_tweets$mentionedUsers),]

#eventually select by hashtags (uncomment the line)
#m_tweets <- m_tweets[grep("#viennaattack",m_tweets$hashtag),]
#m_tweets <- m_tweets[grep("#viennaterrorattack",m_tweets$hashtag),]
#m_tweets <- m_tweets[grep("#wienattack",m_tweets$hashtag),]
#m_tweets <- m_tweets[grep("#schleichdiduoaschloch",m_tweets$hashtag),]
#m_tweets <- m_tweets[grep("#prayforvienna",m_tweets$hashtag),]
#m_tweets <- m_tweets[grep("#0211w",m_tweets$hashtag),]


m_users <- unique(m_tweets$user_id)
usersM <- users[users$user_id %in% m_users,]

#create the network
numMtweets <- length((m_tweets$tweet_id))
m_g <- make_empty_graph(n=length(m_users))
V(m_g)$user_id <- usersM$user_id
V(m_g)$label <- usersM$user_name
V(m_g)$followers <- usersM$followers
V(m_g)$friends <- usersM$friends
V(m_g)$verified <- usersM$verified
V(m_g)$online_since <-usersM$online_since


for(i in 1:numMtweets){
  if(i%%100==0){print(i)}
  
  from <- m_tweets[i,]$author_id
  from_g_index <-  which(V(m_g)$user_id==from)
  mentioned <- str_split(m_tweets[i,]$mentionedUsers, " ")[[1]]
  
  for(j in 2:length(mentioned)){

    if(!(mentioned[j] %in% V(m_g)$label)){
      if(mentioned[j] %in% users$user_name){
        to=users[users$user_name==mentioned[j],]
        m_g <- add_vertices(m_g, 1,user_id=to$user_id, 
                            label=to$user_name,followers=to$followers,friends=to$friends,
                            verified=to$verified, online_since=to$online_since)
        #to_g_index <- which(V(m_g)$user_id==to$user_id) #index of the mentioned 
      }else{
        print(paste("user:",mentioned[j],"NOT FOUND,added only by username"))
        m_g <- add_vertices(m_g, 1,user_id=NA, 
                            label=mentioned[j],followers=NA,friends=NA,
                            verified=NA, online_since=NA)
        #to_g_index <- which(V(m_g)$label==mentioned[j]) #index of the mentioned 
      }
    }
    
    to_g_index <-which(V(m_g)$label==mentioned[j]) #index of the mentioned 
    m_g<- add_edges(m_g,c(from_g_index,to_g_index), 
                     weigth = 1, 
                     date=m_tweets[i,]$date, 
                     hashtag=m_tweets[i,]$hashtag, 
                     hasmedia=m_tweets[i,]$hasmedia, 
                     tweet_id=m_tweets[i,]$tweet_id, 
                     retweetcount=m_tweets[i,]$retweet,
                     language=m_tweets[i,]$language)
  }
  
}

summary(m_g)
V(m_g)$membership <- clusters(m_g)$membership
head(clusters(m_g)$csize)
gm <- delete.vertices(m_g,V(m_g)[V(m_g)$membership!=which.max(clusters(m_g)$csize)])

summary(gm)
V(gm)$btw<- betweenness(gm, directed = TRUE)

filename_data="~/data/MENTION_network.RData"
save(m_g,file=filename_data,envir = parent.frame())
save.image()

####### visualisation

data <- toVisNetworkData(gm,idToLabel = FALSE)
data$edges$from <- as.integer(data$edges$from)
data$edges$to <- as.integer(data$edges$to)

visNetwork(nodes = data$nodes, edges = data$edges)%>%
  visOptions(highlightNearest = TRUE) %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visNodes(label = V(gm)$label)%>%
  visInteraction(hover = TRUE)
