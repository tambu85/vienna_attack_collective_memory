#not running! 

library(rtweet)
library(dplyr)
library(academictwitteR)

#insert credentials given by Twitter API
mytoken= 
mytoken <- create_token(
  app = "name_app",
  consumer_key = "cons_key",
  consumer_secret = "cons_secret",
  access_token = "access_token",
  access_secret = "access_secret",
)

query <- "#wienterror OR #prayforvienna OR #viennaterrorattack OR #viennaattack OR #0211w OR #wienattack OR #viennashooting OR #wirsindwien OR #wienliebe OR #austriaterrorattack OR #schleichdiduoaschloch OR #schleichdiduoaschloch OR #schleichdichduoaschloch OR #schleichdiduorschloch"

#premium account search
rt <- search_fullarchive(query, n = 500, env_name = "research0211vienna",
                         fromDate = "202011020000", toDate = "202011220000",
                         parse = TRUE, token=mytoken)
filename_data="~/vienna0211/TWcrawling_0211.RData"
save(rt,file=filename_data,envir = parent.frame())


#academic twitter
bearer_token <- "b_tokem" # Insert bearer token
get_all_tweets(query, "2020-11-02T00:00:00Z", "2020-11-22T00:00:00Z", bearer_token, data_path = "data/")



