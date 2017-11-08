library("zoo")


##############################
# load sapling and tree data #
##############################
tree_psp <- read.csv("data/Tree_list.csv", colClasses = "character")
sapling_psp <- read.csv("data/Yos_result2.csv",
                        colClasses = "character",
                        header = FALSE)
Tr <- subset(tree_psp, select = c("ID_PEP_MES", "ESSENCE", "DBH"))
length(1:dim(Tr)[1])

names(sapling_psp)[names(sapling_psp) == "V2"] <- "ID_PEP_MES"
names(sapling_psp)[names(sapling_psp) == "V3"] <- "ESSENCE"
names(sapling_psp)[names(sapling_psp) == "V4"] <- "DBH"
sapling_psp$V5 <- NULL
sapling_psp$V1 <- NULL

length(1:dim(sapling_psp)[1])
ps <- rbind(sapling_psp, Tr)



#######################
# load temporal plots #
#######################
tree_tmp <- read.table("data/TemporalTrees.txt",
                    colClasses = "character",
                    sep = ",",
                    quote = "\"")

length(1:dim(tree_tmp)[1])
head(tree_tmp)
tree_tmp$DBH <- as.numeric(tree_tmp$DBH)
tree_tmp <- subset(tree_tmp, select = c("ID_PET_MES", "ESSENCE", "DBH"))
names(tree_tmp)[names(tree_tmp) == "ID_PET_MES"] <- "ID_PEP_MES"

#######################################
# merge PSP trees with Temporal trees #
#######################################
Tree <- rbind(ps, tree_tmp)
head(tree_tmp)
length(1:dim(Tree)[1])
Tree$DBH <- as.numeric(Tree$DBH)
Tree$ESSENCE <- as.factor(Tree$ESSENCE)
Tree <- Tree[-which(Tree$DBH > 90), ]
Tree <- na.omit(Tree)



#################################################
## loading permanent and temporal sample plots ##
#################################################
y <- read.table("data/union_plots_export.txt",
                colClasses = "character",
                header = FALSE,
                sep = ",",
                quote = "\"")

colnames(y)[1] <- "ID_PEP_MES"
colnames(y)[2] <- "LATITUDE"
colnames(y)[3] <- "LONGITUDE"
colnames(y)[4] <- "SREG_ECO"
colnames(y)[5] <- "FIRE_CODE"
colnames(y)[6] <- "DOM_BIO"
colnames(y)[7] <- "ALTITUDE"
colnames(y)[8] <- "DEP_SUR"
colnames(y)[9] <- "CL_DRAI"
colnames(y)[10] <- "TYPE_ECO"
colnames(y)[11] <- "Sur_Dep"
colnames(y)[12] <- "Deposit"
colnames(y)[13] <- "Deposito"
colnames(y)[14] <- "Total_BA"
colnames(y)[15] <- "Spruce_BA"
colnames(y)[16] <- "JackPine_BA"
colnames(y)[17] <- "stands"
colnames(y)[18] <- "shannon"
all_plots <- y
all_plots <- all_plots[- which(all_plots$stands == "0"), ]
all_plots <- all_plots[- which(all_plots$stands == "RxEn"), ]
all_plots <- all_plots[- which(all_plots$stands == "RxRx"), ]
all_plots <- all_plots[- which(all_plots$stands == "EnRx"), ]
all_plots$stands <- factor(all_plots$stands)
all_plots$ID_PEP_MES <- as.factor(all_plots$ID_PEP_MES)
all_plots$stands <- as.factor(all_plots$stands)
all_plots$FIRE_CODE <- as.factor(all_plots$FIRE_CODE)
all_plots$FIRE_CODE_  <- all_plots$FIRE_CODE



######################################################################
## Create new deposit classes differentiating among 1_Till.Thick,  ##
## 1_Till.Thin, M, R etc..  Differentiating the substrate thickness ##
## only for the Till group                                          ##
######################################################################
all_plots$Dep <- ifelse(all_plots$Deposito == "1A"  |
                       all_plots$Deposito == "1",
                       "1_Till.Thick",
                ifelse(all_plots$Deposito == "1AY"  |
                       all_plots$Deposito == "1AM"  |
                       all_plots$Deposito == "M1A",
                       "1_Till.Thin",
                ifelse(all_plots$Deposito == "2",
                       "2_Fluvio_glaciar",
                ifelse(all_plots$Deposito == "3",
                       "3_Fluvial",
                ifelse( all_plots$Deposito == "4",
                       "4_Lacustre",
                ifelse(all_plots$Deposito == "5",
                       "5_Marins",
                ifelse(all_plots$Deposito == "6",
                       "6_Littoraux_marins",
                ifelse(all_plots$Deposito == "7",
                       "7_Organique",
                ifelse(all_plots$Deposito == "8",
                       "8_Depots_pentes",
                ifelse(all_plots$Deposito == "R" | all_plots$Deposito == "R1A",
                       "Rocheux",
                       "Other"))))))))))



#################################################
## Merge drainage classes 0 and 1  and 5 and 6 ##
#################################################
all_plots$Drenaje <- ifelse(all_plots$CL_DRAI == "0" | all_plots$CL_DRAI == "1",
                           "1_DRY",
                    ifelse(all_plots$CL_DRAI == "2",
                           "2_Bon",
                    ifelse(all_plots$CL_DRAI == "3",
                           "3_Modere",
                    ifelse(all_plots$CL_DRAI == "4",
                           "4_Imperfait",
                    ifelse(all_plots$CL_DRAI == "5" | all_plots$CL_DRAI == "6",
                           "5_WET",
                           "Other")))))



##########################################
## subset plots based on drainage class ##
##########################################
subsetPlot <- subset(all_plots, (Drenaje  == "2_Bon"  |
                                Drenaje == "3_Modere"  |
                                Drenaje == "4_Imperfait")
                     & (Dep == "1_Till.Thin"  |
                        Dep == "1_Till.Thick"  |
                        Dep == "2_Fluvio_glaciar"),
                     select = c(1:length(all_plots)))

Plots <- subsetPlot



##############################################
## Get intensity file for the C-2 fuel type ##
##############################################
Intensity <- read.csv("data/Sopfeu.csv",
                      colClasses = "character",
                      header = TRUE,
                      sep = ",")
Intensity <- Intensity[which(Intensity$JA == "1"), ]
Intensity <- Intensity[which(Intensity$INT > 1), ]
Intensity <- Intensity[which(Intensity$SupFin > 0), ]
IntensityC2 <- Intensity[which(Intensity$Comb == "C2"), ]
colnames(IntensityC2)[21] <- "FIRE_CODE"

IntensityC2 <- subset(IntensityC2, (FIRE_CODE == "A2"  |
                                    FIRE_CODE == "B3" |
                                    FIRE_CODE == "C3" |
                                    FIRE_CODE == "D4" |
                                    FIRE_CODE == "E1" |
                                    FIRE_CODE == "E3"),
                      select = c(1:length(IntensityC2)))

IntensityC2$Concatanate <- paste(IntensityC2$Annee,
                                 IntensityC2$Numero,
                                 IntensityC2$INT,
                                 sep = "_")



#####################
## Add seasonality ##
#####################
IntensitySS <- read.csv("data/IntensityData.csv",
                        colClasses = "character",
                        header = TRUE, sep = ",")
IntensitySS <- na.omit(IntensitySS)
IntensitySS <- IntensitySS[- which(IntensitySS$INT == "0"), ]
IntensitySS <- IntensitySS[which(IntensitySS$JA == "1"), ]
IntensitySS <- IntensitySS[which(IntensitySS$Comb == "C2"), ]
IntensitySS <- IntensitySS[which(IntensitySS$Domaine == "6"), ]

strDates <- as.character(IntensitySS$Dates)
IntensitySS$Dates <- as.Date(IntensitySS$Dates, "%Y/%m/%d")
IntensitySS$date1 <- as.yearmon(IntensitySS$Dates)
IntensitySS$Season <- as.numeric(format(IntensitySS$date1, "%m"))
IntensitySS$Concatanate <- paste(IntensitySS$Annee,
                                 IntensitySS$Numero,
                                 IntensitySS$INT,
                                 sep = "_")


##########################
## merge intensity data ##
##########################
Intensities <- merge(IntensitySS,
                      IntensityC2,
                      by = c("Concatanate"))
Intensities$Annee.y <- NULL
Intensities$Comb.y <- NULL
Intensities$DateRap <- NULL
Intensities$Domaine.y <- NULL
Intensities$iLat.y <- NULL
Intensities$iLon.y <- NULL
Intensities$INT.y <- NULL
Intensities$JA.y <- NULL
Intensities$Numero.y <- NULL
names(Intensities)[c(2, 3, 4, 5, 6, 7, 8, 12)] <- c("Annee", "JA", "Comb",
                                                    "iLat", "iLon", "INT",
                                                    "Domaine", "Numero")
Intensities$SprSummer <- ifelse(Intensities$Season == "4"  |
                                Intensities$Season == "5"  |
                                Intensities$Season == "6",
                                "Spring", "Summer")
as.factor(Intensities$SprSummer)




############################################################
## Apply Catchpole function and size weighted distibution ##
############################################################
knownpoints <- data.frame(x <- c(0.04, 0.1, 0.13, 0.16, 0.21, 0.28, 0.37, 0.48,
                                 0.63, 0.7, 0.8, 0.9, 1),
                          y <- c(1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2,
                                 0.18, 0.15, 0.1, 0.05))
aim <- runif(24000, 0, 1)
yos <- approx(knownpoints$x, knownpoints$y, xout = aim, rule = 1)
ProportionalIntensities <- yos$y
ProportionalIntensities <- na.omit(ProportionalIntensities)
PI <- sample(ProportionalIntensities, 24000, replace = T, prob = NULL)
SpringFires <- Intensities[which(Intensities$SprSummer == "Spring"), ]

SpringFires$SupFin <- as.numeric(SpringFires$SupFin)
TotalFireSizeSpring <- sum(SpringFires$SupFin)
SpringFires$Weight <- SpringFires$SupFin / TotalFireSizeSpring
SpringIntensitiesWeighted <- sample(SpringFires$INT, size = 24000, replace = T,
                                    prob = SpringFires$Weight)
SpringIntensitiesWeighted <- as.numeric(SpringIntensitiesWeighted)

SummerFires <- Intensities[which(Intensities$SprSummer == "Summer"), ]

SummerFires$SupFin <- as.numeric(SummerFires$SupFin)
TotalFireSizeSpring <- sum(SummerFires$SupFin)
SummerFires$Weight <- SummerFires$SupFin / TotalFireSizeSpring
SummerIntensitiesWeighted <- sample(SummerFires$INT, size = 24000,
                                    replace = T, prob = SummerFires$Weight)
SummerIntensitiesWeighted <- as.numeric(SummerIntensitiesWeighted)
SpringWeightedCatch <- as.numeric(SpringIntensitiesWeighted * PI)
SummerWeightedCatch <- as.numeric(SummerIntensitiesWeighted * PI)

########################
## save prepared data ##
########################
plots_file <- file("data/Plots.Rdata", "wb")
save(Plots, file = plots_file)
close(plots_file)

tree_file <- file("data/Tree.Rdata", "wb")
save(Tree, file = tree_file)
close(tree_file)

intensities_file <- file("data/Intensities.Rdata", "wb")
save(Intensities, file = intensities_file)
close(intensities_file)

spring_file <- file("data/spring_weighted_catch.Rdata", "wb")
save(SpringWeightedCatch, file = spring_file)
close(spring_file)

summer_file = file("data/summer_weighted_catch.Rdata", "wb")
save(SummerWeightedCatch, file = summer_file)
close(summer_file)


########################
## Verification plots ##
########################
Intensities$INT <- as.numeric(Intensities$INT)
boxplot(INT~SprSummer, data = Intensities,
        ylab = "Fire intensity", ylab = "Fire season",
        main = "Raw Intensities")
dev.copy(png, "plots/Fire_intensities_by_fire_season.png")
dev.off()

###########################################################################
## Compare distribution of sampled intensities weighted/unweighted cases ##
###########################################################################
IUnWeighted <- sample(Intensities$INT, size = 3000, replace = T)
IWeighted <- sample(Intensities$INT, size = 3000, replace = T,
                    prob = Intensities$Weight)
par(mfrow = c(1, 2))
hist(IUnWeighted, main = "Unweighted Intensities") #unweighted
hist(IWeighted, main = "Weighted Intensities")
dev.copy(png, "plots/Histograms_size_weighted.png")
dev.off()


#################################################
## Correct head fire intensity using Catchpole ##
#################################################
IUnWeightedCatch <- as.numeric(IUnWeighted * PI)
IWeightedCatch <- as.numeric(IWeighted * PI)
par(mfrow = c(1, 2))
hist(IUnWeightedCatch, main = "Unweighted w/catchpole") #unweighted
hist(IWeightedCatch, main = "Weighted w/catchpole")
dev.copy(png, "plots/Histograms_size_weighted_catchpole.png")
dev.off()
