# Pull and return the top 10 trending tags from the trends website 
pull_trending_tags <- function(url){
selectors <- c()
for (i in 1:10){
  selector <- paste0('#trend-list > div:nth-child(1) > ol > li:nth-child(', i, ')')
  selectors <- c(selectors, selector)
}

trending_tags <- c()
for (selector in selectors){
  trending_tag  <- url %>% read_html %>% html_node(css=selector) %>% html_text
  trending_tags <- c(trending_tags, trending_tag)
}
trending_tags
}

# Function that takes in the trending tags and returns the top tweets for each tag
 pull_top_tweets <- function(trending_tags){
   twitter_info <- read.delim("twitter-authentication.txt")
   consumer_key <- as.character(twitter_info[[1]][1])
   consumer_secret <- as.character(twitter_info[[1]][2])
   access_token <- as.character(twitter_info[[1]][3])
   access_secret <- as.character(twitter_info[[1]][4])
   setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
   tweet_list = c()
   for (hashtag in trending_tags) {  
     tweets <- twitteR::searchTwitter(hashtag, n = 10)
     tweet_list <- c(tweet_list, tweets)
   }
   tweet_list
 }

# Fuction that takes top tweets then returns which if any are actually disasters
# detect_disaster <-  function(top_tweets){}

# Function that notifies administrator if a disaster has taken place
notify_admin <-  function(disaster){
  text = paste0('Disaster indicated by trending: ', disaster)
  mime() %>%
    to("disasterdonationappp@gmail.com") %>%
    from("disasterdonationappp@gmail.com") %>%
    subject('DISASTER WARNING') %>%
    text_body(text) -> text_msg
  send_message(text_msg)
}
# notify_admin <-  function(disaster){}

# Function that takes in input from administrator and then notifies users to make a donation
# get_money <- function(disaster){}

# Function to run the program
run_program <- function(x){
  library(rvest)
  library(twitteR)
  library(gmailr)
  url <- "https://trends24.in/united-states/"
  trending_tags <- pull_trending_tags(url)
  top_tweets    <- pull_top_tweets(trending_tags)
  #disasters     <- detect_disaster(top_tweets)
  #for (disaster in disasters){}
  notify_admin('#PrayForThisProject')
  # for (disaster in disasters){}
  #get_money('PrayForThisProject')
}

print(run_program())