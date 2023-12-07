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

encrusters <- get.quartile(abundance)
encrusters <- encrusters$quartile
dat <- abundance

actiniarians <- rowSums(dat[,c(1, 2, 5, 34, 28)])

glass <- rowSums(dat[,c(15, 17)]) #glass + donut

demo <- rowSums(dat[,c(13, 14, 37)]) #demosponges

starfish <- rowSums(dat[,c(38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)]) 

cucumbers <- rowSums(dat[,c(11, 36)])

cup_corals <- rowSums(dat[,c(29, 35)])

octocorals <- rowSums(dat[,c(3, 24, 6)]) #plus anthomastus


included <- c(1, 2, 5, 34, 28, 15, 17, 13, 14, 37, 
              38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 
              11, 36, 29, 35, 3, 24, 6)

#singletons
toDelete <-  c(8, 9, 10, 16, 20, 21, 22, 23, 25, 18, 30, 31, 
               32, 33, 50, 51, 52, 53, 54, 55) #fish and dropstones, bryozoans, octopus, 
#mollusk, sterechinus, worms, pycnogonids, octocorals, brittle general + brown,
# gorgonians, crinoids


dat2 <- dat[,-c(included, toDelete)]

dat2 <- cbind(dat2, actiniarians, starfish, glass,
              demo, cucumbers, octocorals, cup_corals)
dat2 <- dat2[,-3]


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

group012 <- cbind(dat2$Astrochlamys_sol, dat2$Amphiura_Ophioperla, cucumbers, 
                  cup_corals, demo, glass, dat2$Hard_corals, octocorals, 
                  dat2$Ophiacantha_vivipara, dat2$Pencil_urchins, starfish, 
                  dat2$Stylasterids)

group012.d <- as.data.frame(apply(group012, 2, get.hilo.m))
colnames(group012.d) <- c("Astrochlamys", "Holothurians", "Demosponges", "Glass_sponges",
                          "Hard_corals", "Octocorals", "Ophiacantha_vivipara", 
                          "Pencil_urchins", "Cup_corals", "Starfish", "Stylasterids",
                          "Amphiura_Ophioperla")


group012.d$Astrochlamys[group012.d$Astrochlamys == 0] <- 1
#group012.d$Glass_sponges[group012.d$Glass_sponges == 0] <- 1

mixed <- cbind(group01.d, group012.d, encrusters)
nodes <- colnames(mixed)
windows()
plot.group(mixed)

write.table(mixed, row.names = FALSE, col.names = FALSE, 
            file = file.path("banjo_try3/", paste0(Sys.Date(), "_yesDeadStarfishGroup.txt")))

write.table(nodes, row.names = FALSE, col.names = FALSE, 
            file = file.path("banjo_try3/", paste0(Sys.Date(), "_yesDeadStarfishGroupNodes.txt")))

save(mixed, dat.hi.lo, dat.pres, dat2, nodes, 
     file = file.path("data/PS118/PS118_69-1/", paste0(Sys.Date(), "yesDeadStarfishGroup.RData")))


# test one without the dead cup corals 

mixed <- mixed[,-14]
nodes <- colnames(mixed)
plot.group(mixed)


