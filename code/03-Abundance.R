## Ming Khan, PhD Chapter 1 

# This script 
# 1) Takes the dex Output and creates an abundance table 

## After first compute, load from the cleaned abundance table 
## and skip section 1 of this script 

# 2) Visualizes abundance as a bar plot 

# After grouping and more cleaning, load from CLEANER abundance table
# and skip section 2 of this script 


library(dplyr)
library(magrittr)
library(ggplot2)
library(vegan)
source("scripts/functions.R")

# Load CLEANED dex output -------------------------------------------------

# REMEMBER TO CHANGE THESE 
df <- read.csv(file = "data/PS118/PS118_69-1/2023-08-09_dexOutput_CLEANED.csv")
load(file = "data/PS118/PS118_69-1/2023-08-08_ProfileMetadata.RData")


# Compute Abundance -------------------------------------------------------

#first test the uniqueness, when sorted, move on
taxa.list <- as.data.frame(sort(unique(df$taxa)))
taxa.list <- cbind(taxa.list, NA)
names(taxa.list)[names(taxa.list) == "sort(unique(df$taxa))"] <- "Var1"
names(taxa.list)[names(taxa.list) == "NA"] <- "Freq"
test.abu <- list()

df$psid <- gsub(".svg","",df$psid) #remove SVG
df$psid <- gsub(" ","_",df$psid) #remove spaces

for (i in 1:length(transect)){
  temp <- df[df$psid == transect[i],]
  tempAbu <- table(temp$taxa)
  tempAbu <- as.data.frame(tempAbu, stringsAsFactors=FALSE)
  #### MUST ADD the strings argument!!!!!!!!!!!
  tempAbu2 <- rbind(tempAbu, taxa.list) #adding species list
  tempAbu2 <- tempAbu2 %>% distinct(Var1, .keep_all = TRUE) #removing duplicates
  tempAbu2[is.na(tempAbu2)] <- 0


  tempAbu2 <- tempAbu2[order(tempAbu2$Var1),] #alphabetizing in order
  # to make rbind later work

  #get it in correct format
  tempAbu2 <- as.data.frame(t(as.matrix(tempAbu2)))
  names(tempAbu2) <- tempAbu2[1,]
  tempAbu2 <- tempAbu2[-1,]

  rownames(tempAbu2) <- transect[i]
  test.abu[[i]] <- tempAbu2
}

imgs <- stringr::str_split_fixed(marked$name, "IMG_", 2)
imgs <- imgs[,2]
imgs <- paste(rep("IMG", times = 40), imgs, sep = "_")


abundance <- do.call(rbind, test.abu)
colnames(abundance)
abundance <- lapply(abundance, as.numeric)
abundance <- as.data.frame(abundance)
tax <- colnames(abundance)
#transect <- gsub(".SVG","",transect)
rownames(abundance) <- transect
#need to remove the SVGs 

#NOW THESE WILL BE UNIQUE TO EACH TRANSECT 

#remove the brittle star bodies, keep arms only 



abundance <- abundance[,-c(5, 9, 14, 36)] #brittle stars

abundance <- abundance[,-21] #encrusters


substrate <- read.csv("data/PS118/PS118_69-1/2023-08-08_Encrusting.csv")

encrust <- df[df$taxa == "Encrusters",]
#encrust$psid <- gsub(".svg","",encrust$psid) #remove SVG
#encrust$psid <- gsub(" ","_",encrust$psid) #remove spaces

sampA <- substrate$Photo_Area

encrustA <- rep(0, times = 40)

for (i in 1:length(transect)){
  #identify the encrusters in each photo 
  ind <- encrust$psid == transect[i]
  temp <- encrust[ind,]
  tempP <- (sum(temp$A)/(substrate$Photo_Area[i]))*100
  encrustA[i] <- tempP
}

substrate <- cbind(substrate, encrustA)
substrate$encrustPercent <- substrate$Encrusting_percent + substrate$encrustA

encrustPercent <- substrate$encrustPercent

abundance <- cbind(abundance, encrustPercent)

save(abundance, transect, df, imgs, 
     file = file.path("data/PS118/PS118_69-1/", 
                                           paste0(Sys.Date(),
                                                  "_abundance_cleanest.RData")))
write.csv(abundance, row.names = FALSE, 
          file = file.path("data/PS118/PS118_69-1/", 
                                      paste0(Sys.Date(),
                                             "_abundance_cleanest.csv")))


# Abundance metrics -- LOAD DATA FROM HERE --------------------------------

load(file = "data/PS118/PS118_69-1/2023-08-11_abundance_cleanest.RData")
load(file = "data/PS118/PS118_69-1/2023-08-08_ProfileMetadata.RData")

#clean a bit more

# pink_cc <- rowSums(abundance[c(16, 18)])
# del <- c(11, 20, 12, 16, 18, 20, 27, 45, 51)
# #abundance <- abundance[-pink_cc]
# abundance <- abundance[-del]
# abundance <- cbind(abundance, pink_cc)
# 

abu <- abundance

del <- c(9, 19) #boulders, dropstones 

abu <- abu[,-del]

#merge the fish 

fish <- rowSums(abu[,c(8, 14, 19, 41, 30)])
abu <- abu[,-c(8, 14, 19, 41, 30)]
abu <- cbind(abu, fish)
nm <- as.data.frame(colnames(abu))
colnames(nm) <- "original"
#write.csv(nm, row.names = FALSE,
#       "supplements/hard-names2.csv") #correct the col-names in excel 

nm <- read.csv("supplements/hard-names2.csv")
#nm <- nm[,-1]
colnames(abu) <- nm$for_supplement

#write.csv(abu, row.names = TRUE, col.names = TRUE, 
#          "supplements/PowellBasin_Abundance.csv")


# Holuthurians abundance check --------------------------------------------

# holo <- rowSums(abu[,c(13, 42)])
# glass <- rowSums(abu[,c(18, 22)]) #glass + donut
# starfish <- rowSums(abu[,c(45, 46, 47, 48, 49, 50, 52, 53, 54, 55)]) #minus hymenaster
# 
# holoTest <- abu[,-c(13, 42, 8, 9, 15, 19, 21, 32, 61,
#                     18, 22, 45, 46, 47, 48, 49, 50, 52, 53, 54, 55)]
# holoTest <- cbind(holo, holoTest, glass, starfish)
# 
# 
# holoTestBin <- as.data.frame(apply(holoTest, 2, pres.abs.m))
# 
# cor.test(holo, rowSums(holoTest))
# cor.test(holo, rowSums(holoTestBin))
# 
# holothurians <- as.data.frame(cbind(holo, rowSums(holoTestBin)))
# write.csv(holothurians, "holothurians.csv")
# 
# #holothurian abundance vs species richness per photo 
# 
# windows()
# par(mfrow = c(1,2))
# plot(holo, rowSums(holoTest), 
#      xlab = "Holothurian abundance", ylab = "Total Abundance", 
#      main = "Holothurian v/s Abundance relationship", pch = 16)
# plot(holo, rowSums(holoTestBin),
#      xlab = "Holothurian abundance", ylab = "Species Richness", 
#      main = "Holothurian v/s Diversity relationship", pch = 16)
# windows()
# par(mfrow = c(3, 2))
# plot(holo, abu$Ophiocantha.vivipara.arms, col = "red", pch = 16,
#      xlab = "Holothurian abundance",
#      ylab = "Abundance", main = "Vivipara")
# plot(holo, glass, col = "blue", pch = 16,
#      xlab = "Holothurian abundance",
#      ylab = "Abundance", main = "Glass Sponges")
# plot(holo, starfish, col = "orange", pch = 16,
#      xlab = "Holothurian abundance",
#      ylab = "Abundance", main = "Starfish")
# plot(holo, abu$Astrochlamys.arms, col = "purple", pch = 16,
#      xlab = "Holothurian abundance",
#      ylab = "Abundance", main = "Astrochlamys")
# plot(holo, abu$dead.cup.corals, col = "brown", pch = 16,
#      xlab = "Holothurian abundance",
#      ylab = "Abundance", main = "Dead Cup corals")
# 
# 
# #NMDS on NMDS script, 
# 
# save(holo, holoTestBin, transect, holoTest, 
#      marked, profile, imgs, 
#      file = file.path("data/PS118/PS118_69-1/", 
#                       paste0(Sys.Date(), "_holoThuriansTest.RData")))
# 




# abundance bar plots -----------------------------------------------------



#taxa.list.clean <- colnames(abundance)
#write.csv(taxa.list.clean, row.names = FALSE, 
#          file = file.path("data/PS118/PS118_69-1", 
#                           paste0(Sys.Date(),
#                                  "_superGroup-list.csv")))

#Reload with grouped data THIS WILL BE DIFFERENT 

#INITIAL GROUP 
#taxa.list.clean <- read.csv("data/PS118/PS118_69-1/2023-08-09_taxa.list3.csv")


#BNA supergroup 
#supergroup <- read.csv("data/PS118/PS118_69-1/2023-08-11_superGroup-list.csv")


# Bar plots supergroup, NOT BN --------------------------------------------


supergroup <- read.csv("supplements/hard-names2.csv")
tax <- supergroup

#tax <- supergroup

tax$comb <- paste(tax$group, tax$for_supplement, sep = ">>")
colnames(abu) <- tax$comb

temp <- vector("list", nrow(abu))

for (i in 1:nrow(abu)){
  ind <- which(!is.na(abu[i,])) #find which ones have observations
  temp2 <- abu[i, ind] #extract the counts 
  temp2 <- as.matrix(t(temp2)) #transpose the matrix
  colnames(temp2) <- "count" #get the frequency 
  taxon <- tax$comb
  
  photo <- rep(transect[i], times = nrow(temp2))
  img <- imgs[i]
  temp2 <- cbind(photo, img, taxon, temp2)
  rownames(temp2) <- NULL
  temp2 <- as.data.frame(temp2)
  temp[[i]] <- temp2
  
}

abuBar <- do.call(rbind, temp)
abuBar$count <- as.numeric(abuBar$count)
temp<- strsplit(abuBar$taxon, split = ">>")
temp <- as.data.frame(do.call(rbind, temp))
abuBar$group <- temp$V1
abuBar$sp <- temp$V2
#test <- which(abuBar$group == "delete")
#abuBar <- abuBar[-test,]
abuBarH <- abuBar
colnames(abu) <- nm$for_supplement
abuH <- abu




# HARD PLOT  -------------------------------------------

#n <- length(unique(abuBarH$group)) #to get the number of colors 

#windows()
#set.seed(124)
#x <- get.color(n)



# Powell Basin ---------------------------------------------------------

sort(unique(abuBarH$group))
cols <- supergroup[,c(3,4)]
cols <- cols[order(cols$group, decreasing = FALSE),]
cols <- unique(cols)

xH <- cols$col
supergroupH <- supergroup

save(abuH, supergroupH, abuBarH, xH, 
     file = "results/PS118/PS118_69-1/Abundance_Bars_Powell.RData")




##### LOAD POWELL BASIN HERE #### 

load("results/PS118/PS118_69-1/Abundance_Bars_Powell.RData")

#regular bar graph 
windows()
img<- ggplot(abuBarH, aes(fill = group, y = count, x = img)) + 
  geom_bar(position = "stack", 
              stat = "identity") +  scale_fill_manual(values = xH)

qH <- img + theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot(qH)


ggsave(file = "figures/PS118/PS118_69-1/superGroupAbundanceBarWide_Nov_14.svg", plot = qH, width = 25, height = 18)
ggsave(file = "figures/PS118/PS118_69-1/superGroupAbundanceBarWide_Nov_14.png", plot = qH, width = 25, height = 18)

# 100% stacked bar graph 

windows()
img<- ggplot(abuBarH, aes(fill = group, y = count, x = img)) + 
  geom_col(position = "fill") +  scale_fill_manual(values = xH)

qHS <- img + theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot(qHS)

ggsave(file = "figures/PS118/PS118_69-1/superGroupAbundanceBarStacked_Nov_14.svg", plot = qHS, width = 20, height = 18)
ggsave(file = "figures/PS118/PS118_69-1/superGroupAbundanceBarStacked_Nov_14.png", plot = qHS, width = 25, height = 18)



# Rank Abundance ----------------------------------------------------------

library(goeveg)

windows()
racurve(abu,
  main = "Rank-abundance diagram",
  nlab = 15,
  ylog = FALSE,
  frequency = FALSE,
  ylim = NULL,
  xlim = NULL
)



# Co-occur ----------------------------------------------------------

# do co occur on Huw assigned type C communities only 

df_no_sing <- abu[-c(26, 27,1, 24, 28, 23, 25, 40, 38, 7, 37, 39),1:60]


#remove anything that doesn't occur in 10% of photos, so greater than 3 
#df_no_sing <- abu[,1:60]

tly <- function(x){
  length(which(x != 0)) #tallies the number of non zero rows 
}

# #counting how many photos contain each group 

abu <- abundance
test <- as.matrix(apply(abu, MARGIN = 2, tly))

nonsing <- which(test > 3) #indices of non singletons 

abu <- abu[,nonsing]
#df_no_sing <- df_no_sing[,-6] #remove boulders
colnames(df_no_sing)

abu.coo <- df_no_sing
nm <- colnames(abu.coo)
abu.coo <- as.data.frame(t(abu.coo)) #transpose the matrix 
abu.coo[abu.coo >= 1 ] <- 1 

dat.mat <- cooccur::cooccur(abu.coo, type = "spp_site", spp_names = TRUE, 
                            thresh = TRUE)
summary(dat.mat)
windows(h = 7, w = 12)
plot(dat.mat)


# Barplot of abundance for soft subs --------------------------------------

load("data/PS118/PS118_6-9/2022-06-21_PS118_6_9b_abundance.RData")

abu <- df[8:64]
abu <- abu[,-c(21, 36, 37, 46)]
#merge unknown sponges 
sponges <- rowSums(abu[,c(46, 50, 51, 52, 53)])
abu <- abu[,-c(46, 50, 51, 52, 53)]
abu <- cbind(abu, sponges)

#taxa.list <- colnames(df[8:64])
#taxa.list <- taxa.list[-c(21, 36, 37, 46)]

#write.csv(taxa.list, row.names = FALSE, 
#          file = file.path("data/PS118/PS118_6-9/", 
#                           paste0(Sys.Date(),
#                                  "_taxa.list.groups.csv")) )

#nm <- colnames(abu)
#write.csv(nm, row.names = FALSE, file = "supplements/soft-names2.csv")


#supergroup <- read.csv("data/PS118/PS118_6-9/2023-08-24_taxa.list.groups.csv")
supergroup <- read.csv("supplements/soft-names2.csv")
nm <- supergroup$for_supplement
colnames(abu) <- nm
#abundance <- cbind(df$name, abu)
#write.csv(abundance, row.names = FALSE, file = "supplements/AntarcticPeninsula-abundance.csv")
tax <- supergroup


tax$comb <- paste(tax$group, tax$for_supplement, sep = ">>")
colnames(abu) <- tax$comb

temp <- vector("list", nrow(df))

imgs <- stringr::str_split_fixed(df$name, "IMG_", 2)
imgs <- imgs[,2]
imgs <- paste(rep("IMG", times = 61), imgs, sep = "_")



for (i in 1:nrow(abu)){
  ind <- which(!is.na(abu[i,])) #find which ones have observations
  temp2 <- abu[i, ind] #extract the counts 
  temp2 <- as.matrix(t(temp2)) #transpose the matrix
  colnames(temp2) <- "count" #get the frequency 
  taxon <- tax$comb
  
  photo <- rep(imgs[i], times = nrow(temp2))
  img <- imgs[i]
  temp2 <- cbind(photo, img, taxon, temp2)
  rownames(temp2) <- NULL
  temp2 <- as.data.frame(temp2)
  temp[[i]] <- temp2
  
}

abuBar <- do.call(rbind, temp)
abuBar$count <- as.numeric(abuBar$count)
temp<- strsplit(abuBar$taxon, split = ">>")
temp <- as.data.frame(do.call(rbind, temp))
abuBar$group <- temp$V1
abuBar$sp <- temp$V2
test <- which(abuBar$group == "delete")
abuBar <- abuBar[-test,]
abuBarS <- abuBar
colnames(abu) <- nm
abuS <- abu

sort(unique(abuBarS$group))
cols <- supergroup[,c(3,4)]
cols <- cols[-42,]
cols <- cols[order(cols$group, decreasing = FALSE),]
cols <- unique(cols)
supergroupS <- supergroup

#x <- c("#DECBE4", "#FCCDE5", "#E78AC3","#808080","#FB9A99", "#BC80BD", "#CCCCCC",
#       "#E5C494" , "#E7298A",
#       "#FDCDAC", "#A6761D", "#CC99FF",  "#8DD3C7")

xS <- cols$col

#save(abuS, abuBarS, xS,  supergroupS, 
#     file = "results/PS118/PS118_6-9/Abundance_Bars_AP.RData")



# AP LOAD FROM HERE ---------------------------------------------------------

load("results/PS118/PS118_6-9/Abundance_Bars_AP.RData")


windows()
img<- ggplot(abuBarS, aes(fill = group, y = count, x = img)) + 
  geom_bar(position = "stack", 
  stat = "identity") +  scale_fill_manual(values = xS)


qS <- img + theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot(qS)

img<- ggplot(abuBarS, aes(fill = group, y = count, x = img)) + 
  geom_col(position = "fill") +  scale_fill_manual(values = xS)

qSS <- img + theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot(qSS)


#save(abuBarS, qS, x, qSS, file = 
#       file.path("data/PS118/PS118_6-9/", 
#                 paste0(Sys.Date(), "_abuBarS.RData")))



ggsave(file = "figures/PS118/PS118_6-9/superGroupAbundanceBar.svg", plot = qS, width = 25, height = 18)
ggsave(file = "figures/PS118/PS118_6-9/superGroupAbundanceBar.png", plot = qS, width = 25, height = 18)
ggsave(file = "figures/PS118/PS118_6-9/superGroupAbundanceBarStacked.svg", plot = qSS, width = 25, height = 18)
ggsave(file = "figures/PS118/PS118_6-9/superGroupAbundanceBarStacked.png", plot = qSS, width = 25, height = 18)


# Barplot hard soft side by side ------------------------------------------

library(gridExtra)


all4 <- grid.arrange(qS, qSS, qH, qHS, nrow = 2) #better with legend on right
ggsave(file = "figures/all4_Nov_15.png", plot = all4, 
       width = 25, height = 18)
ggsave(file = "figures/all4_Nov_15.svg", 
       plot = all4, width = 25, height = 18)


PB_both <- grid.arrange(qH, qHS, nrow = 1) #better with legend on bottom
ggsave(file = "figures/PB_both_Nov_15-test2.png", plot = PB_both, 
       width = 12.5, height = 9, limitsize = FALSE)
ggsave(file = "figures/PB_both_Nov_15.svg", 
       plot = PB_both, width = 25, height = 18)


AP_both <- grid.arrange(qS, qSS, nrow = 1) #better with legend on bottom
ggsave(file = "figures/AP_both_Nov_15.png", plot = AP_both, 
       width = 25, height = 18)
ggsave(file = "figures/AP_both_Nov_15.svg", 
       plot = AP_both, width = 25, height = 18)


raw_PB_AP <- grid.arrange(qS, qH, nrow = 1) #better with legend on bottom
ggsave(file = "figures/raw_PB_AP_Nov_15.png", plot = raw_PB_AP, 
       width = 25, height = 18)
ggsave(file = "figures/raw_PB_AP_Nov_15.svg", 
       plot = raw_PB_AP, width = 25, height = 18)


stacked_PB_AP <- grid.arrange(qSS, qHS, nrow = 1)
ggsave(file = "figures/stacked_PB_AP_Nov_15.png", plot = stacked_PB_AP, 
       width = 25, height = 18)
ggsave(file = "figures/stacked_PB_AP_Nov_15.svg", 
       plot = stacked_PB_AP, width = 25, height = 18)

