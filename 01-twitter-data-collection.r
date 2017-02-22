################################################################
## Workshop: Collecting and Analyzing Social Media Data with R
## February 2nd, 2015
## Script 1: Collecting Twitter data
## Author: Pablo Barbera, NYU, @p_barbera
################################################################

## INSTALLING PACKAGES THAT WE WILL USE TODAY
toInstall <- c("ROAuth", "twitteR", "streamR", "ggplot2", "stringr",
	"tm", "RCurl", "maps", "Rfacebook", "topicmodels", "devtools")

# Uncomment to install the packages
#lapply(packages, install.packages(toInstall), character.only = TRUE)

## Set Working Directory, set to where you exported the Repository files
setwd("~/Dropbox (UNC Charlotte)/social-media-workshop")

#####################################
### CREATING YOUR OWN OAUTH TOKEN ###
#####################################

## Step 1: go to apps.twitter.com and sign in
## Step 2: click on "Create New App"
## Step 3: fill name, description, and website (it can be anything, even google.com)
##			(make sure you leave 'Callback URL' empty)
## Step 4: Agree to user conditions
## Step 5: copy consumer key and consumer secret and paste below

library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "xxx"
consumerSecret <- "yyy"

my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
  consumerSecret=consumerSecret, requestURL=requestURL,
  accessURL=accessURL, authURL=authURL)

## run this line and go to the URL that appears on screen
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

## now you can save oauth token for use in future sessions with twitteR or streamR
save(my_oauth, file="./oauth_token.Rdata")
#load("~/Downloads/credentials/oauth_token.Rdata")

### NOTE (added February 17, 2015)
### The twitteR package just changed its authentication method
### (streamR remains the same)
### New code to authenticate with twitteR now requires access token and access secret,
### which can be found in 'Keys and Access Tokens' tab in apps.twitter.com

accessToken = 'zzz'
accessSecret = 'xxx'

library(twitteR)
setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret,
	access_token=accessToken, access_secret=accessSecret)

## testing that it works
searchTwitter('trump', n=5)

## from a Windows machine:
# searchTwitter("obama", cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

#####################################
### COLLECTING USER INFORMATION   ###
#####################################

library(twitteR)

# profile information
user <- getUser('realDonaldTrump')
# from a Windows machine
# user <- getUser('barackobama', cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

user$toDataFrame()

# followers
user$getFollowers(n=10)
# (10 most recent followers)

# from a Windows machine
# user$getFollowers(n=10, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

# friends (who they follow)
user$getFriends(n=10)

# from a Windows machine
# user$getFriends(n=10, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

#####################################
### SEARCH RECENT TWEETS		  ###
#####################################

# basic searches by keywords
tweets <- searchTwitter("#beer", n=20)

# wrapper to find #rstats tweets
Rtweets(n=5)

## Search between two dates
tweets <- searchTwitter('beer', since='2017-01-20', until='2017-02-20')

# convert to data frame
tweets <- twListToDF(tweets)

## geocoded results
searchTwitter('patriots', geocode='42.375,-71.1061111,10mi')

## using resultType
searchTwitter('world cup+brazil', resultType="popular", n=15)
searchTwitter('from:hadleywickham', resultType="recent", n=10)

## language
searchTwitter('trump', lang = "es", n=20)

# from a Windows machine
# tweets <- searchTwitter("obama", n=20, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

# but NOTE: limited to most recent ~3000 tweets in the past few days!
tweets <- searchTwitter("#BuzzCity", n = 3200)
tweets <- twListToDF(tweets)
tweets$created

# from a Windows machine
# tweets <- searchTwitter("#APSA2014", cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# tweets <- searchTwitter("#PoliSciNSF", cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# tweets <- twListToDF(tweets)
# tweets$created

#############################################
### DOWNLOADING RECENT TWEETS FROM A USER ###
#############################################

## Here's how you can capture the most recent tweets (up to 3,200)
## of any given user (in this case, @realDonaldTrump)

## you can do this with twitteR
timeline <- userTimeline('realDonaldTrump', n=3200)

# from a Windows machine
# timeline <- userTimeline('realDonaldTrump', n=20, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

timeline <- twListToDF(timeline)

## but I have written my own function so that I can store the raw JSON data
source("functions.r")

getTimeline(filename="tweets_nytimes.json", screen_name="nytimes", 
    n=1000, oauth=my_oauth, trim_user="false")

# it's stored in disk and I can read it with the 'parseTweets' function in
# the streamR package
library(streamR)
tweets <- parseTweets("tweets_nytimes.json")

###############################################
### STREAMING TWEETS FILTERING BY KEYWORDS  ###
###############################################

library(streamR)

filterStream(file.name="obama_tweets.json", track="obama", 
    timeout=60, oauth=my_oauth)

## Note the options:
## FILE.NAME indicates the file in your disk where the tweets will be downloaded
## TRACK is the keyword(s) mentioned in the tweets we want to capture.
## TIMEOUT is the number of seconds that the connection will remain open
## OAUTH is the OAuth token we are using

## Once it has finished, we can open it in R as a data frame with the
## "parseTweets" function
tweets <- parseTweets("obama_tweets.json")

## This is how we would capture tweets mentioning multiple keywords:
filterStream(file.name="political_tweets.json", 
	track=c("obama", "bush", "clinton"),
    tweets=100, oauth=my_oauth)

###############################################
### COLLECTING TWEETS FILTERING BY LOCATION ###
###############################################

## This second example shows how to collect tweets filtering by location
## instead. In other words, we can set a geographical box and collect
## only the tweets that are coming from that area.

## For example, imagine we want to collect tweets from the United States.
## The way to do it is to find two pairs of coordinates (longitude and latitude)
## that indicate the southwest corner AND the northeast corner.
## (NOTE THE REVERSE ORDER, IT'S NOT LAT, LONG BUT LONG, LAT)
## In the case of the US, it would be approx. (-125,25) and (-66,50)
## How to find the coordinates? I use: http://itouchmap.com/latlong.html

filterStream(file.name="tweets_geo.json", locations=c(-125, 25, -66, 50), 
    timeout=60, oauth=my_oauth)

## Note that now we choose a different option, "TIMEOUT", which indicates for
## how many seconds we're going to keep open the connection to Twitter.

## But we could have chosen also tweets=100 instead

## We can do as before and open the tweets in R
tweets <- parseTweets("tweets_geo.json")

############################################
### COLLECTING A RANDOM SAMPLE OF TWEETS ###
############################################

## It's also possible to collect a random sample of tweets. That's what
## the "sampleStream" function does:

sampleStream(file.name="tweets_random.json", timeout=30, oauth=my_oauth)

## Here I'm collecting 30 seconds of tweets
## And once again, to open the tweets in R...
tweets <- parseTweets("tweets_random.json")

## What are the most common hashtags right now?
getCommonHashtags(tweets$text)

## What is the most retweeted tweet?
tweets[which.max(tweets$retweet_count),]

############################################
### PULL FOLLOWER INFORMATION            ###
############################################

# See post: https://wesslen.github.io/twitter/twitter-get-followers/

#devtools::install_github("pablobarbera/twitter_ideology/pkg/tweetscores")
library(tweetscores)
oa_folder_location <- "~/Downloads/credentials"

names <- c("HeistBrewery","WoodenRobotAle")

#initialize our dataset
beer.followers <- data.frame(
  id_str=character(),
  screen_name=character(),
  name=character(),
  description=character(),
  followers_count=integer(),
  statuses_count=integer(),
  friends_count=integer(),
  created_at=character(),
  location=character()
)

for (i in names){
  print(i)
  
  # get followers
  followers <- getFollowers(screen_name = names[1],
                            oauth_folder= oa_folder_location,
                            cursor = -1, user_id = NULL, verbose = TRUE, sleep = 1)
  
  # Batch mode - get User ID info (ID, name, description, etc)
  userdata <- getUsersBatch(ids=followers,
                            oauth_folder= oa_folder_location)
  
  # Id and timestamp lists
  userdata$brewery <- i
  userdata$time_stamp <- format(Sys.time(), "%a %b %d %X %Y")
  beer.followers <- rbind(beer.followers, userdata)
}