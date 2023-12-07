# This script 
# 1) Extracts the objects from Inkscape, where every photo is a sample,
#     with every layer being an unique group, and every object within each layer
#      is an individual animal. 
# 2) Combines these with each photo to create a long dataframe 

# Load Libraries ----------------------------------------------------------

library(XML)
library(dplyr)

# Load the Data and Folder ------------------------------------------------

load(file = "data/profileMetadata.RData")

# DEX function ------------------------------------------------------------
#returns a dataframe of every individual in the photo, along with taxonomic
#name, object ID, and length+width of ellipse 

dex <- function(filepath) {
  #read in SVG
  p <- xmlParse(filepath) #reads SVG
  d <- xmlRoot(p)  
  g <- d[3:10000] # Don't have more layers than this number
  g <- g[-which(sapply(g, is.null))] #removes null layers, 
  # because previous step adds room for many many taxa 
  
  
  #extract all specimens (ellipses) and get specimen ids (at the moment not 
  # particularly important for Ming's markup style)
  f <- lapply(g, function(x) {m <- x["ellipse"]})
  #runs through the list of layers, only keeps ellipse ID
  
  get_id <- function(x){
    ids <- lapply(x[[1]], function(x){id <- xmlGetAttr(x, "id")})
  }
  
  #extracts info for dex
  
  m <- list(seq(length(f)))
  for(i in 1: length(f)){
    m[[i]] <- get_id(f[i])
  }
  
  ids <- unname(unlist(m)) #gets the object ID for every object
  
  #number of specimens for each taxa
  n <- lapply(f, function(x){length(x)})
  
  
  # Assigning Taxon Names to ObjectIDs --------------------------------------
  
  
  #get taxon name from the layer names
  taxa <- lapply(g, function(x){xmlGetAttr(x, "inkscape:label")})
  
  #replicate taxa for number of specimens
  m <- list(seq(length(f)))
  for(i in seq_len(length(g))){
    m[[i]]<- rep(taxa[[i]], n[[i]])
  }
  taxa <- unlist(m)  ##THIS NUMBER GIVES YOU NUMBER OF TOTAL INDIVIDUALS IN PHOTO
  
  
  # Gets Size of the Object, Can be modified to get Area --------------------
  
  #extract rx width of ellipse  
  get_rx <- function(x){
    lapply(x[[1]], function(x){xmlGetAttr(x, "rx")})
  }
  
  m <- list(seq(length(f)))
  for(i in 1: length(f)){
    m[[i]] <- get_rx(f[i])
  }
  
  rx <- as.numeric(unname(unlist(m))) ## width of each ellipse
  
  #extract ry - length of ellipse 
  get_ry <- function(x){
    lapply(x[[1]], function(x){xmlGetAttr(x, "ry")})
  }
  m <- list(seq(length(f)))
  for(i in 1: length(f)){
    m[[i]] <- get_ry(f[i])
  }
  
  ry <- as.numeric(unname(unlist(m))) ##Length of each Ellipse 
  
  A <- pi*rx*ry*0.5*0.5 #area of each ellipse
  
  #combine to data frame
  df <- data.frame(cbind(taxa, ids, rx, ry, A))
  df$psid <- NA
  
  return(df) 
}

# Assign the Folder -------------------------------------------------------


#### REMEMBER TO CHANGE THIS WD

setwd("SVGs/AP")
transect <- list.files()
temp <- list(length(transect))

for (i in 1:length(transect)){
  file <- transect[i]
  temp[[i]] <- dex(file)
  temp[[i]][6] <- file
}

df <- do.call(rbind, temp)

setwd(system("git rev-parse --show-toplevel", intern=T))

write.csv(df, row.names = FALSE, file = "data/AP_dexOutput.csv") 


# TAKE THIS DEX OUTPUT, and CLEAN TYPOS etc IN EXCEL with FIND AND REPLACE 

setwd("SVGs/PB")
transect <- list.files()
temp <- list(length(transect))

for (i in 1:length(transect)){
  file <- transect[i]
  temp[[i]] <- dex(file)
  temp[[i]][6] <- file
}

df <- do.call(rbind, temp)
setwd(system("git rev-parse --show-toplevel", intern=T))

write.csv(df, row.names = FALSE, file = "data/PB_dexOutput.csv") 
