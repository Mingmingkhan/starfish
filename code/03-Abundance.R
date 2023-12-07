# This script 
# 1) Takes the dex Output and creates an abundance table, deletes unknowns and
#   merges duplicates 
# 2) Creates supplementary abundance files (Table S1 and S2)
# 3) Creates community composition figure (Figure 4)


library(dplyr)
library(magrittr)
library(ggplot2)
library(vegan)
source("code/functions.R")

load(file = "data/profileMetadata.RData")

# ANTARCTIC PENINSULA -------------------------------------------------


# load dex output 
df <- read.csv(file = "data/AP_dexOutput_cleaned.csv")


# Create abundance table --------------------------------------------------


#first test the uniqueness, when sorted, move on
taxa.list <- as.data.frame(sort(unique(df$taxa)))
taxa.list <- cbind(taxa.list, NA)
names(taxa.list)[names(taxa.list) == "sort(unique(df$taxa))"] <- "Var1"
names(taxa.list)[names(taxa.list) == "NA"] <- "Freq"
test.abu <- list()

df$psid <- gsub(".svg","",df$psid) #remove SVG
df$psid <- gsub(" ","_",df$psid) #remove spaces
transect <- AP_transect

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

imgs <- stringr::str_split_fixed(transect, "IMG_", 2)
imgs <- imgs[,2]
imgs <- paste(rep("IMG", times = 40), imgs, sep = "_")


abundance <- do.call(rbind, test.abu)
colnames(abundance)
abundance <- lapply(abundance, as.numeric)
abundance <- as.data.frame(abundance)
tax <- colnames(abundance)
#transect <- gsub(".SVG","",transect)
rownames(abundance) <- transect

abu <- abundance
abu <- abu[,-c(36, 45, 57, 58)] #remove krill, pelagic fish, two super unknowns

#merge unknown sponges 
colnames(abu)
sponges <- rowSums(abu[,c(47, 51, 52, 53, 54)])
abu <- abu[,-c(47, 51, 52, 53, 54)]
abu <- cbind(abu, sponges)

save(abu, file = "data/AP_abundance_plus_dropstones.RData")

#remove dropstones
abu <- abu[,-21]

taxa.list <- colnames(abu)

write.csv(taxa.list, row.names = FALSE, 
          file = "data/AP_taxa_list_groups.csv")
## NOW ADD TO THIS CSV FILE EXTRA COLUMNS: 
## 1) name used for supplement
## 2) larger morphogroup
## 3) colour used for plot  


# Create Table S1 ---------------------------------------------------------

#reload the CSV file after adding the info 

supergroup <- read.csv("data/AP_taxa_list_groups.csv")
nm <- supergroup$for_supplement
colnames(abu) <- nm
abundance <- abu
write.csv(abundance, file = "supplements/Table_S01_AntarcticPeninsula-abundance.csv")



# Create Community Composition Figure  ------------------------------------

tax <- supergroup
tax$comb <- paste(tax$group, tax$for_supplement, sep = ">>")
colnames(abu) <- tax$comb

temp <- vector("list", nrow(abu))

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

xS <- cols$col

windows()
img<- ggplot(abuBarS, aes(fill = group, y = count, x = img)) + 
  geom_bar(position = "stack", 
           stat = "identity") +  scale_fill_manual(values = xS)
qS <- img + theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
plot(qS)


# POWELL BASIN ------------------------------------------------------------

# load dex output 
df <- read.csv(file = "data/PB_dexOutput_cleaned.csv")


# Create abundance table --------------------------------------------------


#first test the uniqueness, when sorted, move on
taxa.list <- as.data.frame(sort(unique(df$taxa)))
taxa.list <- cbind(taxa.list, NA)
names(taxa.list)[names(taxa.list) == "sort(unique(df$taxa))"] <- "Var1"
names(taxa.list)[names(taxa.list) == "NA"] <- "Freq"
test.abu <- list()

df$psid <- gsub(".svg","",df$psid) #remove SVG
df$psid <- gsub(" ","_",df$psid) #remove spaces
transect <- PB_transect

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

imgs <- stringr::str_split_fixed(transect, "IMG_", 2)
imgs <- imgs[,2]
imgs <- paste(rep("IMG", times = 40), imgs, sep = "_")


abundance <- do.call(rbind, test.abu)
colnames(abundance)
abundance <- lapply(abundance, as.numeric)
abundance <- as.data.frame(abundance)
tax <- colnames(abundance)
#transect <- gsub(".SVG","",transect)
rownames(abundance) <- transect

#remove the brittle star bodies, keep arms only 

abundance <- abundance[,-c(5, 9, 14, 36)] #brittle stars

abundance <- abundance[,-24] #encrusters


encrustPercent <- read.csv("data/encrustPercent.csv")


abundance <- cbind(abundance, encrustPercent$encrustPercent)
abu <- abundance

del <- c(9, 19) #boulders, dropstones 

abu <- abu[,-del]

#merge the fish 

fish <- rowSums(abu[,c(8, 14, 20, 41, 30)])
abu <- abu[,-c(8, 14, 20, 41, 30)]
abu <- cbind(abu, fish)
nm <- as.data.frame(colnames(abu))
colnames(nm) <- "original"

write.csv(nm, row.names = FALSE, file = "data/PB_taxa_list_groups.csv") 

## NOW ADD TO THIS CSV FILE EXTRA COLUMNS: 
## 1) name used for supplement
## 2) larger morphogroup
## 3) colour used for plot  


# Create Table S2 ---------------------------------------------------------

#reload the CSV file after adding the info 

supergroup <- read.csv("data/PB_taxa_list_groups.csv")

save(abundance, file = "data/PowellBasin-abundance.RData")

write.csv(abundance, file = "supplements/Table_S02_PowellBasin-abundance.csv")


# Create Community Composition Figure -----------------------------------------------------

tax <- supergroup
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
colnames(abu) <- supergroup$for_supplement
abuH <- abu

sort(unique(abuBarH$group))
cols <- supergroup[,c(3,4)]
cols <- cols[order(cols$group, decreasing = FALSE),]
cols <- unique(cols)

xH <- cols$col
supergroupH <- supergroup


windows()
img<- ggplot(abuBarH, aes(fill = group, y = count, x = img)) + 
  geom_bar(position = "stack", 
              stat = "identity") +  scale_fill_manual(values = xH)

qH <- img + theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot(qH)

# CREATE FIGURE 4 ------------------------------------------

library(gridExtra)

Figure4 <- grid.arrange(qS, qH, nrow = 1) #better with legend on bottom

#this plot was further modified in Inkscape 

