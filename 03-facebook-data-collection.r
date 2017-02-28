################################################################
## Workshop: Collecting and Analyzing Social Media Data with R
## February 2nd, 2015
## Script 3: Collecting Facebook data
## Author: Pablo Barbera, NYU, @p_barbera
################################################################

setwd("~/Dropbox (UNC Charlotte)/social-media-workshop")

# Loading the Rfacebook package
library(Rfacebook)

## To get access to the Facebook API, you need an OAuth code.
## You can get yours going to the following URL:

## https://developers.facebook.com/tools/explorer

## Once you're there:
## 1) Click on "Get Access Token"
## 2) Copy the long code ("Access Token") and paste it here below, substituting
## the fake one I wrote:

fb_oauth = 'EAACEdEose0cBADIxuP6HTxQXXbGO3K9lQJNc0kik0udkYmvfBfS35E6DWHGS4ZB6VrfO04NpN7X5KSshKY3QsbmrhYZCNBnT7eAVRoJPE5yo3HqXTj0G7dVs8CKj5gckilatPifPK5gfg2uAoh9HC5W0DduQr8b5xDSlgSWmzA5AFM7uxEoP6KJGj01msZD'

## Now try running the following line:
getUsers("me", token=fb_oauth, private_info=TRUE)

## Does it return your Facebook public information? Yes? Then we're ready to go

## See also ?fbOAuth for information on how to get a long-lived OAuth token

################################################
### SCRAPING INFORMATION FROM FACEBOOK PAGES ###
################################################

### Let's first search public pages for the term Charlotte
library(dplyr)

cltPages <- searchPages("Charlotte", token = fb_oauth , n = 500)
cltPages <- subset(cltPages, city == "Charlotte") %>% arrange(desc(likes))


# How can I get a list of posts from a Facebook page?
# The following line downloads the ~100 most recent posts on the facebook
# page of Barack Obama
page <- getPage("thecharlotteobserver", token=fb_oauth, n=500) 

# Which post got more likes?
page[which.max(page$likes_count),]

# Which post got more comments?
page[which.max(page$comments_count),]

# Which post was shared the most?
page[which.max(page$shares_count),]

# We can also subset by date
# For example, let's try to get all the posts from the Charlotte Observer around the Protests
ProtPage <- getPage("thecharlotteobserver", token=fb_oauth, n=1000,
	since='2016/09/20', until='2016/09/30') 

####################################
### COLLECTING PAGES' LIKES DATA ###
####################################

# How can I get a list of users who liked a specific post?
# The following line downloads more information about the first post
# (note that it uses the ID of the post as main option), as well
# as a list of 1,000 people who "liked" it
post <- getPost(page$id[183], token=fb_oauth, n.likes=1000, comments=FALSE)

# This is how you can view that list of people:
likes <- post$likes
head(likes)

# What information is available for these users?
# The first step is to use again "getUsers" to gather their public Facebook
# information, with their IDs as main option.
users <- getUsers(likes$from_id, token=fb_oauth)

# What are the most common first names?
head(sort(table(users$first_name), decreasing=TRUE), n=10)

# This gives us an idea about the gender distribution of the people 
# interacting with this page.

##################################
### COLLECTING PAGES' COMMENTS ###
##################################

# How can I get the text of the comments on a specific post?
post <- getPost(page$id[183], token=fb_oauth, n.comments=1000, likes=FALSE)

# This is how you can view those comments:
comments <- post$comments
head(comments)

# Also, note that users can like comments!
# What is the comment that got the most likes?
comments[which.max(comments$likes_count),]


