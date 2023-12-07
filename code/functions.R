# This file contains all the functions used, minus the Dex function

get.color <- function(n){
  # get a list of unique, discrete colors for ggplot
  # n is the total number of colors required 
  library(RColorBrewer)
  qual_col_pals <- RColorBrewer::brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  col <- sample(col_vector, n)
  pie(rep(1,n), col=col)
  return(col)
}


depth.quartile <-function(x) {
  # categorize each photo into a quartile 
  #x <- marked the marked dataframe
  quartile <- rep(NA, nrow(x)) 
  q <- quantile(x$depth)
  quartile[x$depth <= q[2]] <- 1
  quartile[x$depth > q[2] & x$depth <= q[3]] <- 2
  quartile[x$depth > q[3] & x$depth <= q[4]] <- 3
  quartile[x$depth > q[4]] <- 4
  x <- cbind(x, quartile)
  return(x)
}

get.quartile <- function(x){
  quartile <- rep(NA, nrow(x))
  q <- quantile(x$encrustPercent)
  quartile[x$encrustPercent <= q[2]] <- 1
  quartile[x$encrustPercent > q[2] & x$encrustPercent <= q[3]] <- 2
  quartile[x$encrustPercent > q[3] & x$encrustPercent <= q[4]] <- 3
  quartile[x$encrustPercent > q[4]] <- 4
  x <- cbind(x, quartile)
  
}

get.hilo.m <-function(x) {
  # categorize each column into zero, low, high counts
  x[x==0] <- NA
  med<-median(x, na.rm = TRUE)
  is.na(x) <- 0 
  hilo<-rep(0,length(x))
  hilo[x>med] <- 2
  hilo[x<=med & x != 0] <- 1
  hilo[x==0] <- 0
  return(hilo)
}

pres.abs.m <-function(x){
  # categorize each column into presence/absence data
  pres <- rep(0, length(x))
  pres[x==0] <- 0
  pres[x==1] <- 1 
  pres[x>1] <- 1
  return(pres)
}

plot.group <- function(x){
  #plot the distribution of the discretized data 
  library(tidyr)
  gg_melt <- pivot_longer(x, 1:ncol(x), names_to="species")
  gg_melt$value <- factor(gg_melt$value) #MUST BE IN FACTORS 
  #windows()
  ggplot(gg_melt, aes(value)) +
    geom_bar(stat="count") +
    scale_x_discrete(labels = c("0" = "0", "1"= "1", "2" = "2")) +  
    facet_wrap(~species) 
}

