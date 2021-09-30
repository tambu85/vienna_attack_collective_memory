library(ggplot2)
library(igraph)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(visNetwork)


load("~/data/vienna_data_cleaned_with_coordinates.RData")

#consider tweets and retweets (RT)
all_tweets <- rbind(tweets,other_tweets)
#remove duplicates
all_tweets <- all_tweets[!duplicated(all_tweets$tweet_id),]

numTweets <- length((all_tweets$tweet_id))
numUsers <- length(users$user_id)


tweets$user_id <- tweets$author_id
#join the two dataset by user_id and select only users who are involved in RT activity
rt_tweets <- left_join(tweets,users,by="user_id")
rt_tweets <- rt_tweets[rt_tweets$isretweet==1,]
rt_users <- unique(rt_tweets$user_id)
usersRT <- users[users$user_id %in% rt_users,]

#build the network
#step 1: create an empty network where the nodes are the RT users 
numRTweets <- length((rt_tweets$tweet_id))
rt_g <- make_empty_graph(n=length(rt_users))
V(rt_g)$user_id <- usersRT$user_id
V(rt_g)$label <- usersRT$user_name
V(rt_g)$followers <- usersRT$followers
V(rt_g)$friends <- usersRT$friends
V(rt_g)$verified <- usersRT$verified
V(rt_g)$online_since <-usersRT$online_since
V(rt_g)$other <- FALSE 

#step 2: for each RT ...
for(i in 1:numRTweets){
    if(i%%100==0){print(i)}
    rtwid <- rt_tweets[i,]$retweeted_tweet_id #id original tweet
    
    to <- rt_tweets[i,]$author_id #author of the retweet
    to_g_index <- which(V(rt_g)$user_id==to) #index of the author in the graph
    
    #...check if the involved users have already a correspondent node, otherwise create one...
    if (rtwid %in% tweets$tweet_id){
      original_tw <- tweets[tweets$tweet_id==rtwid,] 
      from <- original_tw$author_id 
      from_u <- users[users$user_id==from,]
      if(!(from %in% V(rt_g)$user_id)){ 
        rt_g <- add_vertices(rt_g, 1,user_id=from, 
                             label=from_u$user_name,
                             followers=from_u$followers,friends=from_u$friends,
                             verified=from_u$verified, online_since=from_u$online_since, 
                             other=FALSE)
      }
    }else{
      from <- other_tweets[other_tweets$tweet_id==rtwid,]$author_id # #author original tweet
      #if it does not exist yet in the graph, let's create the correspondent node 
      #(in this case, we do not have complete infos) 
      if(!(from %in% V(rt_g)$user_id)){ 
        rt_g <- add_vertices(rt_g, 1,user_id=from, 
                             label=NA,followers=NA,friends=NA,
                             verified=NA, online_since=NA, other=TRUE)
      }
    }
    
    #... and finally create a link among retweeted and retweeting users
    from_g_index <-  which(V(rt_g)$user_id==from)
    rt_g<- add_edges(rt_g,c(from_g_index,to_g_index), 
                     weigth = 1, 
                     date=rt_tweets[i,]$date, 
                     hashtag=rt_tweets[i,]$hashtag, 
                     hasmedia=rt_tweets[i,]$hasmedia, 
                     tweet_id=rt_tweets[i,]$tweet_id, 
                     retweetcount=rt_tweets[i,]$retweet)
    
}

#save network data
filename_data="~/data/RT_network.RData"
save(rt_g,file=filename_data,envir = parent.frame())
save.image()


#extract giant component and simplify 
V(rt_g)$membership <- clusters(rt_g)$membership
g1 <- delete.vertices(rt_g,V(rt_g)[V(rt_g)$membership!=1])
summary(g1)
has.multiple(g1)
gs <- simplify(g1, remove.multiple = TRUE,
               edge.attr.comb=list(weight="sum",
                                   date="min",
                                   hashtag="concat",
                                   tweet_id="ignore",
                                   hasmedia="ignore",
                                   retweetcount="ignore"))

gs <- as.undirected(gs)
V(gs)$comm <- membership(cluster_louvain(gs, weights=E(gs)$weigth))

data <- toVisNetworkData(gs,idToLabel = FALSE)
data$edges$from <- as.integer(data$edges$from)
data$edges$to <- as.integer(data$edges$to)

visNetwork(nodes = data$nodes, edges = data$edges)%>%
  visOptions(highlightNearest = TRUE) %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visNodes(label = V(gs)$label)%>%
  visInteraction(hover = TRUE)


