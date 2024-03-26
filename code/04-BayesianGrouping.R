## This script discretizes the abundance data and creates Supplementary Tables 
## S3-5. 

source("code/functions.R")


# ANTARCTIC PENINSULA -----------------------------------------------------

load("data/AP_abundance_plus_dropstones.RData")


# group the data ----------------------------------------------------------


actiniarians <- rowSums(abu[,c(1, 2, 3, 17)])

#ascidians minus pyuvera 
ascidians <- rowSums(abu[,c(6, 7, 8, 9)])

bryozoans <- rowSums(abu[,c(14, 15, 16)])

#amphipods + mysids + isopods 
malacostraca <- rowSums(abu[,c(4, 5, 36, 35)]) 

#clumps 
clump <- rowSums(abu[,c(19, 33, 34)])

echinoderms <- rowSums(abu[,c(22, 23)])

soft_corals <- rowSums(abu[,c(24, 25, 26, 27, 28, 29, 39, 44)])
#gorgonians + octocoral + pennatularia 

holothurians <- rowSums(abu[,c(30, 31, 32)])

worms <- rowSums(abu[,c(37, 45, 46)])

sponges <- rowSums(abu[,c(47, 48, 49, 50)])

ophiuroids <- rowSums(abu[,c(41, 18, 42)])

#those in the groups
included <- c(1, 2, 3, 17, 6, 7, 8, 9, 14, 15, 16, 4, 5, 36, 35, 19, 33, 34,
              22, 23, 24, 25, 26, 27, 28, 29, 39, 44, 30, 31, 32, 37, 45, 46,
              47, 48, 49, 50, 41, 18, 42)

#the things we ignore (singletons)

toDelete <- c(11, 13, 20, 40, 43) 
#asteroida, brachiopoda, cup coral, octopus, ophiuroid traces

dat <- abu[,-c(included, toDelete)]

dat <- cbind(dat, actiniarians, ascidians, malacostraca,
             clump, echinoderms, soft_corals, holothurians,
             worms, sponges, ophiuroids, bryozoans)


# Discretize the data -----------------------------------------------------

dat.hi.lo <- as.data.frame(apply(dat, 2, get.hilo.m))

dat.pres <- as.data.frame(apply(dat, 2, pres.abs.m))

windows()
plot.group(dat.pres)

windows()
plot.group(dat.hi.lo)

gr01 <- dat[,c(1, 2, 3, 6, 7, 9, 11,
                12, 13, 15)]
gr01 <- as.data.frame(apply(gr01, 2, pres.abs.m))

gr012 <- dat[,c(4, 5, 8, 10, 14  )]
gr012 <- as.data.frame(apply(gr012, 2, get.hilo.m))

mixed <- cbind(gr01, gr012)

nm2 <- c("Pyura_bouvetensis", "Benthic_fish", "Dropstones",
         "Ascidians", "Malacostraca", "Echinoderms", "Holothurians",
         "Worms", "Sponges", "Bryozoans", "Notocrangon", 
         "Actiniarians", "Clumps", "Soft_corals", "Ophiuroids")

colnames(mixed) <- nm2

mixed <- mixed[,-c(6, 7, 8, 10)]
rownames(mixed) <- rownames(abu)

windows()
plot.group(mixed)

write.csv(mixed, file = "supplements/Table_S03_AntarcticPeninsula_BN_inputs.csv")


# POWELL BASIN ------------------------------------------------------------

load("data/PowellBasin-abundance.RData")


# Grouping variables -----------------------------------------------------

encrusters <- get.quartile(abu)
encrusters <- encrusters$quartile
dat <- abu

actiniarians <- rowSums(dat[,c(1, 2, 5, 34, 28)])

glass <- rowSums(dat[,c(15, 17)]) #glass + donut

demo <- rowSums(dat[,c(13, 14, 37)]) #demosponges

starfish <- rowSums(dat[,c(38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)]) 

cucumbers <- rowSums(dat[,c(11, 36)])

cup_corals <- rowSums(dat[,c(29, 35)])

octocorals <- rowSums(dat[,c(3, 24, 6)]) #plus anthomastus

gorgonians <- rowSums(dat[,c(18, 30, 31, 50, 51, 52)])

included <- c(1, 2, 5, 34, 28, 15, 17, 13, 14, 37, 
              38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 
              11, 36, 29, 35, 3, 24, 6, 18, 30, 31, 50, 51, 52)

#singletons
toDelete <-  c(8, 9, 10, 16, 20, 21, 22, 23, 25, 
               32, 33, 50, 53, 54, 55) #fish and dropstones, bryozoans, octopus, 
#mollusk, sterechinus, worms, pycnogonids, octocorals, brittle general + brown,
# gorgonians, crinoids


dat2 <- dat[,-c(included, toDelete)]

dat2 <- cbind(dat2, actiniarians, starfish, glass,
              demo, cucumbers, octocorals, cup_corals, gorgonians)
dat2 <- dat2[,-3] #remove dead


# Discretize --------------------------------------------------------------


dat.hi.lo <- as.data.frame(apply(dat2, 2, get.hilo.m))

dat.pres <- as.data.frame(apply(dat2, 2, pres.abs.m))

windows()
plot.group(dat.hi.lo)

windows()
plot.group(dat.pres)


group01 <- cbind(actiniarians)
group01.d <- as.data.frame(apply(group01, 2, pres.abs.m)) #discrete
colnames(group01.d) <- ("Actiniarians")

hi.lo.gr <- dat.hi.lo[,-7] #all except actiniarians
pres.abs.gr <- dat.pres$actiniarians

mixed.group <- cbind(hi.lo.gr, pres.abs.gr, encrusters)
colnames(mixed.group) <- c("Amphiura_Ophioperla", "Astrochlamys_sol",
                           "Lace_corals", "Ophiacantha_vivipara", 
                           "Pencil_urchins", "Stylasterids", "starfish",
                           "Glass_sponges", "Demosponges", "Holothurians",
                           "Octocorals", "Cup_corals", "Gorgonians", 
                           "Actiniarians", "Encrusters")

mixed.group$Astrochlamys_sol[mixed.group$Astrochlamys == 0] <- 1
windows()
plot.group(mixed.group)
nodes <- colnames(mixed.group)

write.table(mixed.group, file = "PB_inputs_V2.txt", row.names = FALSE,
            col.names = FALSE)
write.table(nodes, file = "nodes2.txt", row.names = FALSE,
            col.names = FALSE)


group012.d$Astrochlamys[group012.d$Astrochlamys == 0] <- 1

mixed <- cbind(group01.d, group012.d, encrusters)
nodes <- colnames(mixed)
windows()
plot.group(mixed)
rownames(mixed) <- rownames(abu)


write.csv(mixed, file = "supplements/Table_S04_PowellBasin_BN_inputs.csv")




# POWELL BASIN SENSITIVITY ------------------------------------------------

# Test a network with only the nodes that can be discretized uniformly at the 
# raw collection level, i.e., no groups 

dat.hi.lo <- as.data.frame(apply(dat, 2, get.hilo.m))

dat.pres <- as.data.frame(apply(dat, 2, pres.abs.m))

windows()
plot.group(dat.pres)

windows()
plot.group(dat.hi.lo)

group01 <- dat.pres$Starfish_Hymenaster

group012 <- cbind(dat.hi.lo$Amphiura_Ophioperla, dat.hi.lo$Anthomastus, 
                  dat.hi.lo$Astrochlamys_sol, dat.hi.lo$Holothurians, 
                  dat.hi.lo$Cup_corals_dead,
                  dat.hi.lo$Demosponges, dat.hi.lo$Glass_Sponges, 
                  dat.hi.lo$Hard_corals,
                  dat.hi.lo$Ophiacantha_vivipara, dat.hi.lo$Pencil_urchins,
                  dat.hi.lo$Cup_corals_red, dat.hi.lo$Stylasterids)

noGroup <- as.data.frame(cbind(group01, group012, encrusters))
colnames(noGroup) <- c("Hymenaster", "Amphiura_Ophioperla", "Anthomastus",
                       "Astrochlamys", "Holothurians", "dead_cup_corals",
                       "Demosponges", "Glass_sponges",
                       "Hard_corals", "Ophiocantha_vivipara", "Pencil_urchins",
                       "red_cup_corals", "Stylasterids", "Encrusters")
noGroup$Astrochlamys[noGroup$Astrochlamys == 0] <- 1
noGroup$dead_cup_corals[noGroup$dead_cup_corals == 0] <- 1

nodes <- colnames(noGroup)
rownames(noGroup) <- rownames(abu)


windows()
plot.group(noGroup)

write.csv(noGroup, file = "supplements/Table_S05_PowellBasin_Sensitivity_BN_inputs.csv" )
