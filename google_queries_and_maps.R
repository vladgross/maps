#                                                                         #
#                             Querying Google                             #
#                             and makin' maps                             #
#                                                                         #

# Load Packages -----------------------------------------------------------

rm(list=ls())

# install.packages("magrittr")
library(magrittr)

# install.packages("ggmap")
library(ggmap)

# install.packages("ggplot2")
library(ggplot2)

# install.packages("RJSONIO")
library(RJSONIO)

# install.packages("RCurl")
library(RCurl)

# Step 0: Generate Some Fake Data -----------------------------------------

# set.seed(1111)

N <- 20

data <- data.frame(
     ID     = 1:N,
     number = round(rnorm(n = N,mean = 15,sd = 5),0),
     street = sample(c(
                     "Picton St", 
                     "Lichfield Road", 
                     "Karangahape Road",
                     "Tamaki Drive",
                     "Norfolk Street",
                     "Victoria Ave",
                     "Mount Eden Road",
                     "Morningside Drive",
                     "Balmoral Road",
                     "Shore Road",
                     "Parnell Road",
                     "Pitt Street",
                     "Williamson Ave",
                     "Quay Street",
                     "Jervois Road",
                     "Anzac Ave",
                     "Queen Street",
                     "Wellesly Street",
                     "Richmond Road",
                     "Wellington Street"),
                     size    = N,
                     replace = T),
     city    = rep("Auckland",N),
     cntry   = rep("New Zealand",N),
     treated = rbinom(n = N, prob = .5, size = 1),
     fancy   = sample(c(0,1,2,3,4), size = N, replace = T)
     )

head(data)

# Step 1: Get Coordinates -------------------------------------------------

# Google Geocoding API:
# - activate Geocoding API in the google developers console 
# - 2,500 requests per 24 hour period
# - 5 requests per second
# - API Key required

# 1.1. Generate Coordinate Query ------------------------------------------

# Define the API key to be used in the queries:

api.key   <- "AIzaSyAT4e6SnwwNoMOHQmB_qY7-9jNn9qytjvo"

# Backup keys in case we exhaust the first one:
# api.key   <- "AIzaSyBesm4qXqRF4d8J2bzu6exihF1RDBcJehQ"
# api.key   <- "AIzaSyDPVxfCwSNukdPrT5bESjg-QVfGJxP0b-Y"

# paste our address elements together to get a list of addresses:

addresses <- with(data,paste0(number," ",street,", ",city,", ",cntry))

# generate a function to make queries for the google geocode API

geocode.query <- function(address,api.key){

     # The URL queries take + signs as spaces:
     
     address  <- gsub(pattern     = " ",
                      replacement = "+",
                      x           = address)
     
     # Put it all together:
     
     paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",
            address, "&key=", api.key)
}

# Now we are going sapply through the address vector, 
# and 'pipe' the results into a vector transform: 

sapply(X = addresses,

       # We apply our geocode.query function through the 
       # addresses vector while holding the api.key constant:
       
       FUN = function(address){
       
            geocode.query(address = address,
                          api.key = api.key)
            
            # This little operator %>% is a 'pipe': it spits
            # out the results into the next function
            
            }) %>%
            
            # We put the sapply results into as.vector
     
                    as.vector(.) -> 
     
                                   # And spit these into an object, queries:
                                   queries

# The queries match each observation:

head(cbind(data[2:5],queries))

# 1.2. Grab Coordinates ---------------------------------------------------

# sapply through our queries, grabbing the lat and lon as we go:
sapply(X = queries,
       function(query){
            
            # Send the query to google:
            
            req      <- getURLContent(query)
            
            # Translate the JSON into an R object:
            
            JSON.req <- fromJSON(req,asText=TRUE)
            
            # Grab the latitude and longitude: 
            
            latlon   <- JSON.req$results[[1]]$geometry$location
            
            return(latlon)
            
            }) %>%
     
          # Pipe the sapply output into a vector transformation
          as.vector(.) %>%
     
               # Pipe the vector into a matrix transformation
               matrix(data = .,
                      
                      ncol = 2,
                      
                      byrow = T) %>% 
     
                    # Pipe the matrix into a data.frame transformation
     
                    as.data.frame(.)  ->  
     
                         # Pipe the data.frame into an object:
                                        latlons

names(latlons) <- c("lat","lon")

head(latlons)

# Add the coordinates back to the data

data <- cbind(data,latlons)

# 1.3. Plot Locations -----------------------------------------------------

# With ggplot: 
base <- ggplot(data)

base + geom_point(aes(x = lon,
                      y = lat))

# With ggmap:

# Grab a map from google
map <- get_map(location = "Auckland, New Zealand",
               zoom = 13)

# Set up the "map base" using ggmap() (analogous to ggplot())
map.base <- ggmap(ggmap = map) 

# Basic plot
map.base +
     geom_point(data  = data,
                aes(x = lon,
                    y = lat))

# Adding transparency to see clustering
map.base +
     geom_point(data  = data,
                aes(x = lon,
                    y = lat),
                size  = 4,
                alpha = .5
                )

# Adding some jitter and gradient for fanciness
map.base +
     geom_point(data     = data,
                size     = 4,
                position = "jitter",
                aes(x     = lon,
                    y     = lat,
                    color = fancy)
     )

# Size for fanciness and discrete color for treatment assignment
map.base +
     geom_point(data     = data,
                position = "jitter",
                aes(x     = lon,
                    y     = lat,
                    color = as.factor(treated),
                    size  = fancy
                    )
     )

# Step 2: Generate Distance Matrix ----------------------------------------

# Google Distance Matrix API
# - 100 elements per query
#    - where N(origins) x N(destinations) = N(elements)
# - 100 elements per 10 seconds
# - 2,500 elements per 24 hour period

# 2.1. Generate Distance Matrix Queries -----------------------------------

dist.mat.queries <- function( # start of function
     
# Arguments:     
     
     lat,    # Latitude vector
     
     lon,    # Longitude vector     
     
     walk=T, # Do you want walking or driving distances?
     
     api.key # Your API key from the google developer console
     
     ){
     
     # These will be our origins and destinations:
     
     latlons <- paste(lat,lon,sep = ",")
     
     # We will use this function to put origins with destinations
     
     pastr <- function(origin,destinations){
          paste0(origin,
                 "&destinations=",
                 paste(destinations,
                       collapse = "|")) # The | sign is used to separate groups of destination elements
                                        # in the googlemaps query
     }
     
     # The first part of the query to the distmat API
     part.1 <- "https://maps.googleapis.com/maps/api/distancematrix/json?origins="
     
     # The second part, which uses our pastr function to create a bunch of strings
     # in which each subject is an "origin" and all others are "destinations"
     part.2 <- 
          sapply(X = latlons,
                 FUN = function(origin){
                      pastr(origin = origin,
                            destinations = latlons)
                 })
     
     # The third part puts in the mode option, based on the user's specification:
     part.3 <- paste0("&mode=",ifelse(test = walk,
                                      yes = "walking",
                                      no = "driving"))
     
     # Then we add in our API key
     part.4 <- paste0("&sensor=false&key=",api.key)
     
     # We apply over the part.2 vector we made above, putting the other constant
     # terms with it
     queries <- 
          sapply(X   = part.2,
                 FUN = function(X){
                      paste0(part.1,
                             X,     # Here each part.2 gets added
                             part.3,
                             part.4)
                 })
     
     return(as.vector(queries))
     
} # end of function

# Now let's use the function we just made, 
# taking the latitudes and longitudes we 
# grabbed from google's geocoding API

dist.queries <- 
     dist.mat.queries(lat     = data$lat,
                      lon     = data$lon,
                      walk    = T,
                      api.key = api.key)

head(dist.queries) # lovely

# 2.2 Generate distance matrix --------------------------------------------

# This function takes a query, grabs the content from google, and then 
# returns the distances 
get.dist <- 
     function(query){
          # Send the request:
          req      <- getURLContent(query)
          
          # Translate request into R object
          JSON.req <- fromJSON(req,asText=TRUE)
          
          # Figure out how many distances to grab
          N.dists  <- length(JSON.req$rows[[1]]$elements)
          
          # Go through and grab 'em
          sapply(X = 1:N.dists,
                 FUN = function(i){
                      JSON.req$rows[[1]]$elements[[i]]$distance$value})     
     }

# DEMO
# Using the first query of the vector we created:
get.dist(dist.queries[1])

# Now we are going to do get.dist to all of our dist.queries, and pipe
# the results through various transformations in order to produce 
# the distance matrix: 

# Start a pipe, in which we sapply our get.dist
sapply(X   = dist.queries,
       
       FUN = get.dist) %>%
     
          # Now pipe the results into a vector transform
     
          as.vector(.) %>%
     
               # Now pipe the vector into an N by N matrix, filling by row
     
               matrix(data  = .,
                      nrow  = N,
                      ncol  = N,
                      byrow = T) %>%
     
                    # Now pipe the matrix into a data.frame transform
     
                    as.data.frame(.)  ->
     
                         # ... and pipe the results into our distmat object:
     
                         distmat 

# We name each column after the subject
names(distmat) <- paste0("S",1:N)

# Add it to our data:
data <- cbind(data,distmat)

# Step 3: Estimate Spillovers ---------------------------------------------

# Now let's make a function for defining units as adjacent, given 
# some hypothesized threshold

adjacent <- 
     function(dist,threshold=threshold){
          # Is the distance from S_j to S_k below the threshold?
          within.range <- dist<=threshold
          
          # Give me a one if it is and a 0 if not
          ifelse(test = within.range,
                 yes  = 1,
                 no   = 0
               )
     }

# DEMO:
adjacent(dist = 300,threshold = 400)
adjacent(dist = 500,threshold = 400)

# This defines the adjacencies in our experiment
adj.mat <- 
     adjacent(dist      = distmat,
              threshold = 500)

# This is A Coppock's function for defining subjects 
# according to their spillover status:
get.condition <- function(adj.mat, Z){
     
     # Matrix multiply the adjacency by the treatment vector:
     exposure <- adj.mat %*% Z
     
     # Then use the results to define types:
     condition <- rep(NA, N)
     condition[Z==1 & exposure  >0] <- "11"
     condition[Z==0 & exposure  >0] <- "10"
     condition[Z==1 & exposure ==0] <- "01"
     condition[Z==0 & exposure ==0] <- "00"
     return(condition)
}

# Let's add the condition to our data:
data$condition <- get.condition(adj.mat = adj.mat,
                                Z       = data$treated)

# We can also define spillovers 'cumulatively':
data$cumulative <- adj.mat%*%data$treated-data$treated

# 3.1. Plot Spillovers ----------------------------------------------------

map <- get_map(location = "Auckland, New Zealand",
               zoom = 13,
               color = "bw")

map.base <- ggmap(ggmap = map) 

# Plot conditions as labels: 
map.base +
     geom_point(data     = data,
                size     = 10,
                color    = "white",
                position = "jitter",
                aes(x = lon,
                    y = lat)) +
     geom_text( data = data,
                aes(x     = lon,
                    y     = lat,
                    label = condition)) 

# Plot cumulative spillover as size, with 
# color as degree of fanciness and 
# shape as treatment status
map.base +
     geom_point(data     = data,
                position = "jitter",
                aes(x     = lon,
                    y     = lat,
                    size  = cumulative,
                    shape = as.factor(treated),
                    color = fancy)) 

# ... etc.

#FIN