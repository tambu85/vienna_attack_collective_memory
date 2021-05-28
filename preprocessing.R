library(rjson)
library(stringr)
library(geojsonR)

path <- "~/vienna0211/data/tweets"
setwd(path)

#create an empty dataframe to select only some information

vienna_tweets <- data.frame(text=character(),
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

temp <- list.files(pattern="*.json", full.names=TRUE)

df_index <- 1
part_sum <- 0

for(f in temp){
  json_data <- fromJSON(file=f)
  print(f)
  numTweets <- length(json_data)
  part_sum <- part_sum + numTweets
  print(paste("numTweets:",numTweets))
  vienna_tweets[df_index:numTweets+df_index-1,] <- NA
  
  for (i in 1:numTweets) { #df_index:numTweets+df_index-1
    #if(i%%10==0){print(i)}
    vienna_tweets[df_index,]["text"] = json_data[[i]]$text
    vienna_tweets[df_index,]["hashtag"]=paste(unlist(str_extract_all(json_data[[i]]$text, "#\\S+")),collapse = " ")
    vienna_tweets[df_index,]["date"]= json_data[[i]]$created_at
    vienna_tweets[df_index,]["language"] = json_data[[i]]$lang
    
    if(length(json_data[[i]]$entities$urls)>0){
      urls_list=""
      for(ui in 1:length(json_data[[i]]$entities$urls)){
        urls_list <- paste(urls_list,as.character(json_data[[i]]$entities$urls[[ui]]$url))
      }
      vienna_tweets[df_index,]["url"] <- urls_list
    }
    
    vienna_tweets[df_index,]["tweet_id"] = json_data[[i]]$id
    vienna_tweets[df_index,]["conversation_id"] = json_data[[i]]$conversation_id
    vienna_tweets[df_index,]["author_id"] = json_data[[i]]$author_id
    vienna_tweets[df_index,]["reply"] = json_data[[i]]$public_metrics$reply_count
    vienna_tweets[df_index,]["retweet"] = json_data[[i]]$public_metrics$retweet_count
    vienna_tweets[df_index,]["like"] = json_data[[i]]$public_metrics$like_count
    vienna_tweets[df_index,]["quote"] = json_data[[i]]$public_metrics$quote_count
    vienna_tweets[df_index,]["isreply"] = 0
    vienna_tweets[df_index,]["isretweet"] = 0
    vienna_tweets[df_index,]["isquote"] = 0
    
    if(length(json_data[[i]]$referenced_tweets)>0){
     for(rti in 1:length(json_data[[i]]$referenced_tweets)){
       if(json_data[[i]]$referenced_tweets[[rti]]$type=="replied_to"){
         vienna_tweets[df_index,]["isreply"] = 1
         vienna_tweets[df_index,]["replied_tweet_id"] = json_data[[i]]$referenced_tweets[[rti]]$id
       }
       else if(json_data[[i]]$referenced_tweets[[rti]]$type=="retweeted"){
         vienna_tweets[df_index,]["isretweet"] = 1
         vienna_tweets[df_index,]["retweeted_tweet_id"] = json_data[[i]]$referenced_tweets[[rti]]$id
       }
       else if(json_data[[i]]$referenced_tweets[[rti]]$type=="quoted"){
         vienna_tweets[df_index,]["isquote"] = 1
         vienna_tweets[df_index,]["quoted_tweet_id"] = json_data[[i]]$referenced_tweets[[rti]]$id
       }
     }
    }
  
    if(length(json_data[[i]]$entities$mentions )>0){
      mentions_list=""
      for(mi in 1:length(json_data[[i]]$entities$mentions)){
        mentions_list <- paste(mentions_list,as.character(json_data[[i]]$entities$mentions[[mi]]$username))
      }
      vienna_tweets[df_index,]["mentionedUsers"] <- mentions_list
    }
    
    if(length(json_data[[i]]$attachments$media_keys)>0){
      vienna_tweets[df_index,]["hasmedia"] = 1
    }
    else{
      vienna_tweets[df_index,]["hasmedia"] = 0
    }
    
    df_index <- df_index+1
  }
  print(paste("fine ciclo for, df_index=",df_index))
}

head(vienna_tweets)
length(unique(vienna_tweets$tweet_id))
sum(na.omit(vienna_tweets$isretweet))

#remove NA and duplicates
vienna_tweets  <- distinct(filter(vienna_tweets, rowSums(is.na(vienna_tweets)) != ncol(vienna_tweets))) 


#check there are no duplicates by tweetid (dup should have length 0, if not, remove the dup)
dup <- vienna_tweets[duplicated(vienna_tweets$tweet_id), ] 


write.csv(wien_tweets2,"~/vienna0211/data/vienna_tweets.csv", row.names = TRUE)
filename_data="~/vienna0211/data/vienna_tweets_cleaned.RData"
save(vienna_tweets,file=filename_data,envir = parent.frame())
save.image()






