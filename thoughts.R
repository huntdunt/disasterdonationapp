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
     tweets <- twitteR::searchTwitter(hashtag, n = 1)
     tweet_list <- c(tweet_list, tweets)
   }
   tweet_list
 }

# Fuction that takes top tweets then returns which if any are actually disasters
 detect_disaster <-  function(top_tweets){
   
   words <- "flood crisis, victims, flood victims, flood powerful  
   powerful storms, hoisted flood, storms amazing, explosion, amazing rescue,
   rescue women, flood cost, counts flood, toll rises, braces river, river peaks,
   crisis deepens, prayers, thoughts prayers, affected tornado, affected, death toll,
   tornado relief, photos flood, water rises, toll, flood waters, flood appeal, victims explosion,
   bombing suspect, massive explosion, affected areas, praying victims, injured, please join, join praying,
   prayers people, redcross, text redcross, visiting flood, lurches fire, video explosion, deepens death,
   opposed flood, help flood, died explosions, marathon explosions, flood relief, donate, first responders,
   flood affected, donate cross, braces, tornado victims, deadly, prayers affected, explosions running,
   evacuated, relief, flood death, deaths confirmed, affected flooding, people killed, dozens, footage,
   survivor finds, worsens eastern, flood worsens, flood damage, people dead, girl died, flood, donation help,
   major flood, rubble, another explosion, confirmed dead, rescue, send prayers, flood warnings,
   tornado survivor, damage, devastating, flood toll, affected hurricane, prayers families,
   releases photos, hundreds injured, inundated, crisis, text donation, redcross give, recede,
   bombing, massive, bombing victims, explosion ripped, gets donated, donated victims, relief efforts,
   news flood, flood emergency, give online, fire flood, huge explosion, bushfire, torrential rains,
   residents, breaking news, redcross donate, affected explosion, disaster, someone captured, tragedy,
   enforcement, people injured, twister, blast, crisis deepens, injuries reported, fatalities,
   donated million, donations assist, dead explosion, survivor, death, suspect dead, peaks deaths,
   love prayers, explosion fertiliser, explosion reported, return home, evacuees, large explosion,
   firefighters, morning flood, praying, public safety, txting redcross, destroyed, displaced,
   fertilizer explosion, unknown number, donate tornado, retweet donate, flood tornado, casualties,
   climate change, financial donations, stay strong, dead hundreds, major explosion, bodies recovered,
   waters recede, response disasters, victims donate, unaccounted, fire fighters, explosion victims,
   prayers city, accepting financial, torrential, bomber, disasters txting, explosion registered,
   missing flood, volunteers, brought hurricane, relief fund, help tornado, explosion fire, ravaged,
   prayers tonight, tragic, enforcement official, saddened, dealing hurricane, impacted, flood recovery,
   stream, dead torrential, flood years, nursing, recover, responders, massive tornado, buried alive,
   alive rubble, crisis rises, flood peak, homes inundated, flood ravaged, explosion video, killed injured,
   killed people, people died, missing explosion, make donation, floods kill, tornado damage, entire crowd,
   cross tornado, terrifying, need terrifying, even scary, cost deaths, facing flood, deadly explosion,
   dead missing, floods force, flood disaster, tornado disaster, medical examiner, help victims,
   hundreds homes, severe flooding, shocking video, bombing witnesses, magnitude, firefighters police,
   fire explosion, storm, flood hits, floodwaters, emergency, flash flood, flood alerts, crisis unfolds,
   daring rescue, tragic events, medical office, deadly tornado, people trapped, police officer,
   explosion voted, lives hurricane, bombings reports, breaking suspect, bombing investigation,
   praying affected, reels surging, surging floods, teenager floods, rescue teenager, appeal launched,
   explosion injured, injured explosion, responders killed, explosion caught, city tornado, help text,
   name hurricane, damaged hurricane, breaking arrest, suspect bombing, massive manhunt, releases images,
   shot killed, rains severely, house flood, live coverage, devastating tornado, lost lives, reportedly dead,
   following explosion, remember lives, tornado flood, want help, seconds bombing, reported dead, imminent,
   rebuild, safe hurricane, surviving, injuries, prayers victims, police suspect, warning, help affected,
   kills forces, dead floods, flood threat, military, flood situation, thousands homes, risk running,
   dead injured, dying hurricane, loss life, thoughts victims, bombing shot, breaking enforcement,
   police people, video capturing, feared dead, terrible explosion, prayers involved, reported injured,
   seismic, victims waters, flood homeowners, flood claims, homeowners reconnect, reconnect power,
   power supplies, rescuers help, free hotline, hotline help, please stay, investigation, saddened loss,
   identified suspect, bombings saddened, killed police, dead, praying community,
   registered magnitude, leave town, reported explosion, heart praying, life heart, prepare hurricane,
   landfall, crisis worsens, arrest, bombing case, suspect run, communities damaged, destruction, levy,
   tornado, hurricane coming, toxins flood, release toxins, toxins, supplies waters, crisis found,
   braces major, government negligent, attack, hurricane, rebuilt communities, help rebuilt, rebuilt,
   rescuers, buried, heart prayers, flood levy, watch hurricane, victims lost, soldier, waiting hurricane,
   run massive, high river, terror, memorial service, terror attack, coast hurricane, terrified hurricane,
   aftermath, suspect killed, suspect pinned, lost legs, hurricane category, names terrified, authorities,
   assist people, hurricane black, unknown soldier, events, safety, troops, disaster relief, cleanup,
   troops lend, effected hurricane, time hurricane, saying hurricane, praying families, dramatic,
   path hurricane"
   words <- str_replace_all(words, "\\n", " ")
   words <- strsplit(words, ', ')
   words <- unlist(words, recursive = TRUE, use.names = TRUE)
   words <- data_frame(words=words, stringsAsFactors=FALSE)
   
   disasters <- c()
   for (i in 0:9){
        count_words <- 0
        for (tweet in tweet_list[i+1]){
            tweet_l <- data.frame(words=tweet, stringsAsFactors = FALSE)
            colnames(tweet_l) <- 'words'
            joined <- tweet_l %>% inner_join(words)
            count_words <- count_words + length(nchar(joined$words)) 
       if (count_words >= 1){
           disasters <- c(disasters, i+1)
        }
        }
   }
   return(disasters)
   
 } 
 

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

# Function that takes in input from administrator and then notifies users to make a donation
 get_money <- function(disaster){
   text = readline('Enter email body: ')
   mime() %>%
     to("disasterdonationappp@gmail.com") %>%
     from("disasterdonationappp@gmail.com") %>%
     subject(paste0('Please donate to: ', disaster)) %>%
     text_body(text) -> text_msg
   send_message(text_msg)
 }

# Function to run the program
run_program <- function(x){
  library(rvest)
  library(twitteR)
  library(gmailr)
  library(stringr)
  library(dplyr)
  url <- "https://trends24.in/united-states/"
  trending_tags <- pull_trending_tags(url)
  top_tweets    <- pull_top_tweets(trending_tags)
  disasters     <- detect_disaster(top_tweets)
  if (!is.null(disasters)){
      disasters <- trendings_tags[disasters]
      for (disaster in disasters){
          notify_admin('#PrayForThisProject')
      }
      for (disaster in disasters){
          get_money('PrayForThisProject')
  }}

 
# Running the program
while (TRUE){
  run_program()
  Sys.sleep(3600)
}

