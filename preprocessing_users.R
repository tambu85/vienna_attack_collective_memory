library(rjson)
library(stringr)
library(geojsonR)
library(dplyr) 

path <- "~/vienna0211/data/users"
setwd(path)



vienna_other_tweets <- data.frame(text=character(),
                                  hashtag=character(),
                                  date=character(),
                                  url=character(),
                                  tweet_id=double(),
                                  conversation_id=double(),
                                  author_id=double(),
                                  reply=double(),
                                  retweet=double(),
                                  like=double(),
                                  quote=double(),
                                  isretweet=logical(),
                                  isquote=logical(),
                                  isreply=logical(),
                                  retweeted_tweet_id=character(),
                                  quoted_tweet_id=character(),
                                  replied_tweet_id=character(),
                                  language=character(),
                                  mentionedUsers=character(),
                                  hasmedia=logical(),
                                  stringsAsFactors=FALSE) 
vienna_users <- data.frame(user_id=double(),
                           user_name=character(),
                           online_since=character(),
                           followers=double(),
                           friends=double(),
                           tweetcount=double(),
                           verified=logical(),
                           location=character(),
                           stringsAsFactors=FALSE) 

temp <- list.files(pattern="*.json", full.names=TRUE)

df_index <- 1
u_index <-1
part_sum <- 0
part_sum_usr <- 0

for(f in temp){
  json_data <- fromJSON(file=f)
  print(f)
  
  ##TWEETS
  numTweets <- length(json_data$tweets)
  part_sum <- part_sum + numTweets
  print(paste("numTweets:",numTweets))
  vienna_other_tweets[df_index:numTweets+df_index-1,] <- NA
  
  for (i in 1:numTweets) { #df_index:numTweets+df_index-1
    #if(i%%10==0){print(i)}
    vienna_other_tweets[df_index,]["text"] = json_data$tweets[[i]]$text
    vienna_other_tweets[df_index,]["hashtag"]=paste(unlist(str_extract_all(json_data$tweets[[i]]$text, "#\\S+")),collapse = " ")
    vienna_other_tweets[df_index,]["date"]= json_data$tweets[[i]]$created_at
    vienna_other_tweets[df_index,]["language"] = json_data$tweets[[i]]$lang
    
    if(length(json_data$tweets[[i]]$entities$urls)>0){
      urls_list=""
      for(ui in 1:length(json_data$tweets[[i]]$entities$urls)){
        urls_list <- paste(urls_list,as.character(json_data$tweets[[i]]$entities$urls[[ui]]$url))
      }
      vienna_other_tweets[df_index,]["url"] <- urls_list
    }
    
    vienna_other_tweets[df_index,]["tweet_id"] = json_data$tweets[[i]]$id
    vienna_other_tweets[df_index,]["conversation_id"] = json_data$tweets[[i]]$conversation_id
    vienna_other_tweets[df_index,]["author_id"] = json_data$tweets[[i]]$author_id
    vienna_other_tweets[df_index,]["reply"] = json_data$tweets[[i]]$public_metrics$reply_count
    vienna_other_tweets[df_index,]["retweet"] = json_data$tweets[[i]]$public_metrics$retweet_count
    vienna_other_tweets[df_index,]["like"] = json_data$tweets[[i]]$public_metrics$like_count
    vienna_other_tweets[df_index,]["quote"] = json_data$tweets[[i]]$public_metrics$quote_count
    vienna_other_tweets[df_index,]["isreply"] = 0
    vienna_other_tweets[df_index,]["isretweet"] = 0
    vienna_other_tweets[df_index,]["isquote"] = 0
    
    if(length(json_data$tweets[[i]]$referenced_tweets)>0){
      for(rti in 1:length(json_data$tweets[[i]]$referenced_tweets)){
        if(json_data$tweets[[i]]$referenced_tweets[[rti]]$type=="replied_to"){
          vienna_other_tweets[df_index,]["isreply"] = 1
          vienna_other_tweets[df_index,]["replied_tweet_id"] = json_data$tweets[[i]]$referenced_tweets[[rti]]$id
        }
        else if(json_data$tweets[[i]]$referenced_tweets[[rti]]$type=="retweeted"){
          vienna_other_tweets[df_index,]["isretweet"] = 1
          vienna_other_tweets[df_index,]["retweeted_tweet_id"] = json_data$tweets[[i]]$referenced_tweets[[rti]]$id
        }
        else if(json_data$tweets[[i]]$referenced_tweets[[rti]]$type=="quoted"){
          vienna_other_tweets[df_index,]["isquote"] = 1
          vienna_other_tweets[df_index,]["quoted_tweet_id"] = json_data$tweets[[i]]$referenced_tweets[[rti]]$id
        }
      }
    }
    
    if(length(json_data$tweets[[i]]$entities$mentions )>0){
      mentions_list=""
      for(mi in 1:length(json_data$tweets[[i]]$entities$mentions)){
        mentions_list <- paste(mentions_list,as.character(json_data$tweets[[i]]$entities$mentions[[mi]]$username))
      }
      vienna_other_tweets[df_index,]["mentionedUsers"] <- mentions_list
    }
    
    if(length(json_data$tweets[[i]]$attachments$media_keys)>0){
      vienna_other_tweets[df_index,]["hasmedia"] = 1
    }
    else{
      vienna_other_tweets[df_index,]["hasmedia"] = 0
    }
    
    df_index <- df_index+1
  }
  print(paste("fine ciclo TWEET, df_index=",df_index))
  
  ##USERS
  numUsers <- length(json_data$users)
  part_sum_usr <- part_sum_usr + numUsers
  print(paste("numUsers:",numUsers))
  vienna_users[u_index:numUsers+u_index-1,] <- NA
  
  for (i in 1:numUsers) { #df_index:numTweets+df_index-1
    #if(i%%10==0){print(i)}
    vienna_users[u_index,]["user_id"] = json_data$users[[i]]$id
    vienna_users[u_index,]["user_name"] = json_data$users[[i]]$username
    vienna_users[u_index,]["online_since"] = json_data$users[[i]]$created_at
    vienna_users[u_index,]["followers"] = json_data$users[[i]]$public_metrics$followers_count
    vienna_users[u_index,]["friends"] = json_data$users[[i]]$public_metrics$following_count
    vienna_users[u_index,]["tweetcount"] = json_data$users[[i]]$public_metrics$tweet_count
    vienna_users[u_index,]["verified"] = json_data$users[[i]]$verified
    
    if(!is.null(json_data$users[[i]]$location)){
      vienna_users[u_index,]["location"] = json_data$users[[i]]$location
    }
    else{ vienna_users[u_index,]["location"] = NA }
    
    u_index <- u_index+1
  }
  print(paste("fine ciclo USERS, u_index=",u_index))
  
  
}

#remove NA and duplicates
vienna_users2  <- distinct(filter(vienna_users, rowSums(is.na(vienna_users)) != ncol(vienna_users))) 
vienna_other_tweets2 <- distinct(filter(vienna_other_tweets, rowSums(is.na(vienna_other_tweets)) != ncol(vienna_other_tweets))) 

#remove duplicates by id (there are some accounts repeated with different num followers, for instance)
users <- vienna_users2[!duplicated(vienna_users2$user_id), ] 
other_tweets <- vienna_other_tweets2[!duplicated(vienna_other_tweets2$tweet_id), ] 




write.csv(users,"~/vienna0211/data/vienna_users.csv", row.names = TRUE)
write.csv(other_tweets,"~/vienna0211/data/vienna_other_tweets.csv", row.names = TRUE)
filename_data="~/vienna0211/data/vienna_users_cleaned.RData"
save(users,other_tweets,file=filename_data,envir = parent.frame())
save.image()


locations <- unique(users[!is.na(users$location),]$location)
write.csv(locations,"~/vienna0211/data/locations.csv", row.names = TRUE)

# ---> treat locations with python TagME/Wikipedia API to retrieve the geo coordinates

#add location geographic coordinates when possible 
locations_coord <- read.csv("~/vienna0211/data/locations_coord.csv")
locations_coord <- locations_coord[-1,]


users$loc_clean <- NA
users$lon <- NA
users$lat <- NA

numUsers <- length(users$user_id)

for(i in 1:numUsers){
  if(i%%100==0){print(i)}
  loc <- users[i,]$location
  if(!is.na(loc) && loc!="null" && loc!="n/a" && loc!="NA"){
    loc_cleaned <- locations_coord[locations_coord$raw==loc,]
    if(!is.na(loc_cleaned$lon)){
      users[i,]$loc_clean <- as.character(loc_cleaned$clean)
      users[i,]$lon <- loc_cleaned$lon
      users[i,]$lat <- loc_cleaned$lat
    }
  }
}

head(users)
write.csv(users,"~/vienna0211/data/vienna_users_with_coordinates.csv", row.names = TRUE)
filename_data="~/vienna0211/data/vienna_users_cleaned_with_coordinates.RData"
save(users,other_tweets,file=filename_data,envir = parent.frame())
save.image()

filename_data="~/vienna0211/data/vienna_data_cleaned_with_coordinates.RData"
save(tweets,users,other_tweets,file=filename_data,envir = parent.frame())
save.image()


