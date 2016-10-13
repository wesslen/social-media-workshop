#==============================================================================
# functions.R
# Purpose: additional functions used in paper
# Author: Pablo Barbera
#==============================================================================

#' @rdname getFollowers
#' @export
#'
#' @title 
#' Returns the list of user IDs that correspond to a given user's follower
#'
#' @description
#' \code{getFollowers} connects to the REST API of Twitter and returns the
#' list of followers of a given user. Note that this function allows the
#' use of multiple OAuth token to make the process more efficient.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param screen_name user name of the Twitter user for which their followers
#' will be downloaded
#'
#' @param oauth_folder folder where OAuth tokens are stored.
#'
#' @param cursor See \url{https://dev.twitter.com/docs/api/1.1/get/followers/ids}
#'
#' @param user_id user id of the Twitter user for which their friends will be
#' downloaded
#'
#' @param verbose If \code{TRUE}, prints information about API calls on console
#'
#' @param sleep Number of seconds to sleep between API calls.
#'
#' @examples \dontrun{
#' ## Download list of followers of user "p_barbera"
#'  followers <- getFollowers(screen_name="p_barbera", oauth_folder="oauth")
#' }
#'

getFollowers <- function(screen_name=NULL, oauth_folder, cursor=-1, user_id=NULL, verbose=TRUE, sleep=1){

    require(rjson); require(ROAuth)

    ## create list of credentials
    creds <- list.files(oauth_folder, full.names=T)
    ## open a random credential
    cr <- sample(creds, 1)
    if (verbose) {cat(cr, "\n")}
    load(cr)
    ## while rate limit is 0, open a new one
    limit <- getLimitFollowers(my_oauth)
    if (verbose) {cat(limit, " API calls left\n")}
    while (limit==0){
        cr <- sample(creds, 1)
        if (verbose){cat(cr, "\n")}
        load(cr)
        Sys.sleep(sleep)
        # sleep for 5 minutes if limit rate is less than 100
        rate.limit <- getLimitRate(my_oauth)
        if (rate.limit<100){
            Sys.sleep(300)
        }
        limit <- getLimitFollowers(my_oauth)
        if (verbose){cat(limit, " API calls left\n")}
    }
    ## url to call
    url <- "https://api.twitter.com/1.1/followers/ids.json"
    ## empty list for followers
    followers <- c()
    ## while there's more data to download...
    while (cursor!=0){
        ## making API call
        if (!is.null(screen_name)){
            params <- list(screen_name = screen_name, cursor = cursor)
        }
        if (!is.null(user_id)){
            params <- list(user_id = user_id, cursor = cursor)
        }
        url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
        Sys.sleep(sleep)
        ## one API call less
        limit <- limit - 1
        ## trying to parse JSON data
        json.data <- fromJSON(url.data, unexpected.escape = "skip")
        if (length(json.data$error)!=0){
            if(verbose){cat(url.data)}
            stop("error! Last cursor: ", cursor)
        }
        ## adding new IDS
        followers <- c(followers, as.character(json.data$ids))

        ## previous cursor
        prev_cursor <- json.data$previous_cursor_str
        ## next cursor
        cursor <- json.data$next_cursor_str
        ## giving info
        cat(length(followers), "followers. Next cursor: ", cursor, "\n")

        ## changing oauth token if we hit the limit
        if (verbose){cat(limit, " API calls left\n")}
        while (limit==0){
            cr <- sample(creds, 1)
            if (verbose){cat(cr, "\n")}
            load(cr)
            Sys.sleep(sleep)
            # sleep for 5 minutes if limit rate is less than 100
            rate.limit <- getLimitRate(my_oauth)
            if (rate.limit<100){
                Sys.sleep(300)
            }
            limit <- getLimitFollowers(my_oauth)
            if (verbose){cat(limit, " API calls left\n")}
        }
    }
    return(followers)
}

getLimitFollowers <- function(my_oauth){
    require(rjson); require(ROAuth)
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "followers,application")
    response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    return(unlist(fromJSON(response)$resources$followers$`/followers/ids`[['remaining']]))
}

getLimitRate <- function(my_oauth){
    require(rjson); require(ROAuth)
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "followers,application")
    response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    return(unlist(fromJSON(response)$resources$application$`/application/rate_limit_status`[['remaining']]))
}

#' @rdname getUsers
#' @export
#'
#' @title 
#' Returns user data for up to 100 Twitter users
#'
#' @description
#' \code{getUsers} connects to the REST API of Twitter and returns user
#' objects (user information) for up to 100 Twitter users, based on their
#' screen names or user IDs
#' 
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param screen_names user names of the Twitter users
#' 
#' @param id ids of Twitter users
#'
#' @param include_entities if "true", returned data will include most
#' recent tweet
#'
#' @param oauth_folder folder where OAuth tokens are stored.
#'
#'
#' @examples \dontrun{
#' ## Download user data for user "p_barbera"
#'  userdata <- getUsers(screen_names="p_barbera", oauth_folder="oauth")
#' }
#'

getUsers <- function(oauth_folder="~/credentials", screen_names=NULL, 
    id=NULL, include_entities="true"){

    require(rjson); require(ROAuth)

    ## create list of credentials
    creds <- list.files(oauth_folder, full.names=T)
    ## open a random credential
    cr <- sample(creds, 1)
    cat(cr, "\n")
    load(cr)
    ## while rate limit is 0, open a new one
    limit <- getLimitUsers(my_oauth)
    cat(limit, " hits left\n")
    while (limit==0){
        cr <- sample(creds, 1)
        cat(cr, "\n")
        load(cr)
        Sys.sleep(1)
        # sleep for 5 minutes if limit rate is less than 100
        rate.limit <- getLimitRate(my_oauth)
        if (rate.limit<100){
            Sys.sleep(300)
        }
        limit <- getLimitUsers(my_oauth)
        cat(limit, " hits left\n")
    }
    ## url to call
    url <- "https://api.twitter.com/1.1/users/lookup.json"

    ## first API call
    if (!is.null(screen_names)){
        screen_names <- paste(screen_names, collapse=",")
        params <- list(screen_name = screen_names, include_entities=include_entities)
    }
    if (!is.null(id)){
        ids <- paste(id, collapse=",")
        params <- list(user_id=ids, include_entities=include_entities)   
    }
    
    url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl")) 
    Sys.sleep(.5)
    return(RJSONIO::fromJSON(url.data))
}


getLimitUsers <- function(my_oauth){
    require(rjson); require(ROAuth)
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "users,application")
    response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    return(unlist(rjson::fromJSON(response)$resources$users$`/users/lookup`[['remaining']]))

}


getDoneIDs <- function(mongo, ns){
    n <- mongo.count(mongo, ns)
    out <- rep(NA, n)
    cursor <- mongo.find(mongo, ns, fields=list("_id"))
    pb <- txtProgressBar(min=1,max=n, style=3)
    i <- 1
    while (mongo.cursor.next(cursor)){
        out[i] <- list(mongo.bson.to.list(mongo.cursor.value(cursor)))
        i <- i + 1
        setTxtProgressBar(pb, i)
    }
    return(unlist(out))
}

prepareForMongo <- function(userlist){

    # changing 'id_str' to '_id' (mongoDB index)
    names(userlist)[names(userlist)=="id_str"] <- "_id"
    # adding field: last status update
    if (!is.null(userlist[['status']])){
        last.d <- format.twitter.date(userlist[['status']][['created_at']])
        userlist[['date_last_tweet']] <- as.numeric(last.d)        
    }
    if (is.null(userlist[['status']])){
        userlist[['date_last_tweet']] <- 0    
    }
    return(userlist)
}

unlistWithNA <- function(lst, field){
    if (length(field)==1){
        notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field]])))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], '[[', field))
    }
    if (length(field)==2){
        notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]]))
    }
    return(vect)
}

parseMongo <- function(tweets, fields){
    fields.list <- strsplit(fields, "\\.")
    tweets.out <- matrix(NA, 
        nrow=length(tweets), ncol=length(fields))
    for (i in 1:length(fields)){
        tweets.out[,i] <- unlistWithNA(tweets, fields.list[[i]])
    }
    tweets.out <- data.frame(tweets.out, stringsAsFactors=F)
    names(tweets.out) <- fields
    return(tweets.out)
}

getSubsetIDs <- function(mongo, ns, min_followers, min_lasttweet,
	min_statuses, fields=c("_id", "screen_name", "location")){
	fields.arg <- fields
	query <- list(
		followers_count=list('$gte'=min_followers),
		date_last_tweet=list('$gte'=min_lasttweet),
		statuses_count=list('$gte'=min_statuses)
		)
	n <- mongo.count(mongo, ns, query)

    new.fields <- list()
    for (f in fields){ new.fields[f] <- 1L}
    fields <- new.fields

    out <- rep(NA, n)
    cursor <- mongo.find(mongo, ns, query=query, fields=fields)
    pb <- txtProgressBar(min=1,max=n, style=3)
    i <- 1
    while (mongo.cursor.next(cursor)){
        out[i] <- list(mongo.bson.to.list(mongo.cursor.value(cursor)))
        i <- i + 1
        setTxtProgressBar(pb, i)
    }
    out <- parseMongo(out, fields=fields.arg)
    return(out)
}

#' @rdname getGeo
#' @export
#'
#' @title 
#' Returns geographic information about a location string
#'
#' @description
#' \code{getGeo} connects to the Data Science Tool Kit and converts the
#' location string into a pair of coordinates, and then into information
#' (city, state, country, congressional district...) for those coordinates
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param location location string for which information is desired
#'
#' @param verbose If TRUE, provides additional information on console.
#'
#' @examples \dontrun{
#' ## Download geographic information for "New York"
#'  getGeo("New York")
#' }
#'

getGeo <- function(location, verbose=FALSE, rdstk="http://www.datasciencetoolkit.org"){
    require(httr); require(rjson)
    # empty list for results
    result <- list()
    ## check if location is set of coordinates, then scrape
    coord <- grepl("-?[0-9]+\\.[0-9]+.*-?[0-9]+\\.[0-9]+", location)
    if (coord){
            coordinates.clean <- sub(x=location, 
                pattern='.* (-?[0-9]{1,3}\\.[0-9]+,-?[0-9]{1,3}\\.[0-9]+).*', 
                replacement="\\1")
            result[['lat']] <-  sub(x=coordinates.clean, 
                pattern='.*(^-?[0-9]+\\.[0-9]+).*', 
                replacement="\\1")
            result[['lng']] <- sub(x=coordinates.clean, 
                pattern='.*,[[:blank:]]?(-?[0-9]{1,3}\\.[0-9]+).*', 
                replacement="\\1")
            Encoding(result[['lng']]) <- "UTF-8"; Encoding(result[['lat']]) <- "UTF-8"
            geo.info <- coordinates2politics(result[['lat']], result[['lng']], rdstk=rdstk)
            if (length(geo.info[[1]]$politics)>0){
                for (p in 1:length(geo.info[[1]]$politics)){
                    result[[(geo.info[[1]]$politics[[p]][['friendly_type']])]] <-
                        geo.info[[1]]$politics[[p]][['name']]
                }
            }
            if (length(geo.info[[1]]$politics)==0){
                result <- NULL
            }

    }
    # if not, try to extract coordinates from location
    if (!coord){
        geo <- getCoordinates(location, rdstk=rdstk)    
        if (length(geo$interpretations)>0){
            result[['lat']] <- geo$interpretations[[1]]$feature$geometry$center[[1]]
            result[['lng']] <- geo$interpretations[[1]]$feature$geometry$center[[2]]
            geo.info <- coordinates2politics(result[['lat']], result[['lng']], rdstk=rdstk)
            if (length(geo.info[[1]]$politics)>0){
                for (p in 1:length(geo.info[[1]]$politics)){
                    result[[(geo.info[[1]]$politics[[p]][['friendly_type']])]] <-
                        geo.info[[1]]$politics[[p]][['name']]
                }
            }
        }
    }
    if (verbose){
        print(location)
        print(unlist(result))
    }
    return(result)
}

getCoordinates <- function (address, rdstk) 
{
    api <- paste(rdstk, "/twofishes?query=", sep = "")
    result <- content(GET(paste(api, URLencode(address), sep = "")))
    return(result)
}

coordinates2politics <- function (latitude, longitude, 
    rdstk="http://www.datasciencetoolkit.org") 
{
    api <- paste(rdstk, "/coordinates2politics/", 
        sep = "")
    result <- rawToChar(GET(paste(api, latitude, "%2c", longitude, sep = ""))$content)
    if (nchar(result)>0){
        return(fromJSON(result))
    }
    if (nchar(result==0)){
        return(NULL)
    } 
}


