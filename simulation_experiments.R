set.seed(10)
n.iter <- 1000      # plots to check
Y <- 4800
FRI <- 60    # change according to the FRI to evaluate




Ba_s <- matrix(0, n.iter, Y, byrow = T)
Size_list <- vector("list", n.iter)
PreFireStand_s <- vector("list", n.iter)
Transition_list <- vector("list", n.iter)
Structure_s <- matrix(0, n.iter, Y, byrow = T)
Recruits_s <- matrix(0, n.iter, Y, byrow = T)
Heights_list <- vector("list", n.iter)
CR_list <- vector("list", n.iter)
DOM_Pool_list <-  vector("list", n.iter)
DOM_Inputs_list <-  vector("list", n.iter)
DOM_Flux_s <- matrix(0, n.iter, Y, byrow = T)
PrimaryProductivity_s  <- matrix(0, n.iter, Y, byrow = T)
NetEcosystemProduction_s  <- matrix(0, n.iter, Y, byrow = T)
Rh_s  <- matrix(0, n.iter, Y, byrow = T)
Turnover_s  <- matrix(0, n.iter, Y, byrow = T)
TotalLiveBiomass_s <- matrix(0, n.iter, Y, byrow = T)
SnagCProduction_s <- matrix(0, n.iter, Y, byrow = T)
Severity_s <- matrix(0, n.iter, Y, byrow = T)
InitialIntensity_s <- matrix(0, n.iter, Y, byrow = T)
NF_s <- numeric(n.iter)

for (i in 1:n.iter) {
    Size_list[[i]] <- matrix(0, Y, 15, byrow = T)
    PreFireStand_s[[i]] <- matrix(0, Y, 15, byrow = T)
    Transition_list[[i]] <- matrix(0, Y, 15, byrow = T)
    Heights_list[[i]] <- matrix(0, Y, 15, byrow = T)
    CR_list[[i]] <- matrix(0, Y, 15, byrow = T)
    DOM_Pool_list[[i]] <-  matrix(0, Y, 9, byrow = T)
    DOM_Inputs_list[[i]] <-  matrix(0, Y, 9, byrow = T)
}

do.call(rbind, Size_list)
do.call(rbind, Transition_list)
do.call(rbind, Heights_list)
do.call(rbind, CR_list)
do.call(rbind, DOM_Pool_list)
do.call(rbind, DOM_Inputs_list)


for (i in 1:n.iter) {
    CarbonModel <- exe(stand, Y, FRI, SpringWeightedCatch)
    Ba_s[i, ] <- CarbonModel$BA
    Structure_s[i, ] <- CarbonModel$Structure
    Recruits_s[i, ] <- CarbonModel$Recruits
    Size_list[[i]] <- CarbonModel$Size
    PreFireStand_s[[i]]  <- CarbonModel$PreFireStand
    Transition_list[[i]] <- CarbonModel$Transition
    Heights_list[[i]] <- CarbonModel$Heights
    CR_list[[i]]  <- CarbonModel$CR
    SnagCProduction_s [i, ] <-  CarbonModel$SnagCProduction
    TotalLiveBiomass_s[i, ] <- CarbonModel$TotalLiveBiomass
    PrimaryProductivity_s[i, ] <- CarbonModel$NetPrimaryProductivity
    NetEcosystemProduction_s[i, ] <- CarbonModel$NetEcosystemProduction
    Rh_s[i, ] <- CarbonModel$Rh
    Turnover_s[i, ] <- CarbonModel$Turnover
    DOM_Pool_list[[i]] <- CarbonModel$DOMC_Pool
    DOM_Flux_s[i, ]  <- CarbonModel$DOM_Flux
    DOM_Inputs_list[[i]]  <- CarbonModel$DOM_Inputs
    Severity_s[i, ] <- CarbonModel$BALost
    InitialIntensity_s[i, ] <- CarbonModel$InitialIntensity
    NF_s[i] <- CarbonModel$NF
    print(i)
}

Year <- 4800
plots <- 1000


#Season 1 Spring #2 Summer
BasalArea <- Ba_s[, Year]
Recruitment <- Recruits_s[, Year]
BiomassTurnover <- Turnover_s[1:plots, Year]
Inputs <- sapply(DOM_Inputs_list, rowSums)
Litterfall <- Inputs[Year, ]
BiomassLiveCStock <- TotalLiveBiomass_s[1:plots, Year]
BiomassLiveCStock1 <- t(BiomassLiveCStock)
NPP <- PrimaryProductivity_s[1:plots, Year]
SoilRespiration <- Rh_s[1:plots, Year]
NEP <- NetEcosystemProduction_s[1:plots, Year]
StandStructure <- Structure_s[1:plots, Year]
Intensities <- InitialIntensity_s[1:plots, 1:Year]
Snags <- sapply(DOM_Pool_list, function(m) m[Year, 1])
SnagBranch <- sapply(DOM_Pool_list, function(m) m[Year, 2])
AGMedium <- sapply(DOM_Pool_list, function(m) m[Year, 3])
AGfast <- sapply(DOM_Pool_list, function(m) m[Year, 4])
AGveryfast <- sapply(DOM_Pool_list, function(m) m[Year, 5])
AGslow <- sapply(DOM_Pool_list, function(m) m[Year, 6])
BGveryfast <- sapply(DOM_Pool_list, function(m) m[Year, 7])
BGfast <- sapply(DOM_Pool_list, function(m) m[Year, 8])
BGslow <- sapply(DOM_Pool_list, function(m) m[Year, 9])
SoilCStock <- Snags + SnagBranch + AGMedium + AGfast + AGveryfast + AGslow + BGveryfast + BGfast + BGslow
EcosystemCStock <- BiomassLiveCStock + SoilCStock
Season <- rep(1, plots)
FireReturnInterval <- rep(FRI, plots)
NumberFires <- mean(NF_s)


# Save results
FRI60Summer <- cbind (Season, FireReturnInterval, StandStructure, BasalArea, BiomassLiveCStock, Recruitment,
                      NPP, NEP, SoilRespiration, BiomassTurnover, Snags, SnagBranch,
                      AGMedium, AGfast, AGveryfast, AGslow, BGveryfast, BGfast, BGslow, SoilCStock,
                      EcosystemCStock)
save("FRI60Summer", file =  "FRI60_Summer_Newest.RData")

FRI100Summer <- cbind (Season, FireReturnInterval, StandStructure, BasalArea, BiomassLiveCStock, Recruitment,
                       NPP, NEP, SoilRespiration, BiomassTurnover, Snags, SnagBranch,
                       AGMedium, AGfast, AGveryfast, AGslow, BGveryfast, BGfast, BGslow, SoilCStock,
                       EcosystemCStock)
save("FRI100Summer", file =  "FRI100_Summer_Newest.RData")

FRI150Summer <- cbind (Season, FireReturnInterval, StandStructure, BasalArea, BiomassLiveCStock, Recruitment,
                       NPP, NEP, SoilRespiration, BiomassTurnover, Snags, SnagBranch,
                       AGMedium, AGfast, AGveryfast, AGslow, BGveryfast, BGfast, BGslow, SoilCStock,
                       EcosystemCStock)
save("FRI150Summer", file =  "FRI150_Summer_Newest.RData")

FRI300Summer <- cbind (Season, FireReturnInterval, StandStructure, BasalArea, BiomassLiveCStock, Recruitment,
                       NPP, NEP, SoilRespiration, BiomassTurnover, Snags, SnagBranch,
                       AGMedium, AGfast, AGveryfast, AGslow, BGveryfast, BGfast, BGslow, SoilCStock,
                       EcosystemCStock)
save("FRI300Summer", file =  "FRI300_Summer_Newest.RData")

FRI700Summer <- cbind (Season, FireReturnInterval, StandStructure, BasalArea, BiomassLiveCStock, Recruitment,
                       NPP, NEP, SoilRespiration, BiomassTurnover, Snags, SnagBranch,
                       AGMedium, AGfast, AGveryfast, AGslow, BGveryfast, BGfast, BGslow, SoilCStock,
                       EcosystemCStock)
save("FRI700Summer", file =  "FRI700_Summer_Newest.RData")

FRI1200Summer <- cbind (Season, FireReturnInterval, StandStructure, BasalArea, BiomassLiveCStock, Recruitment,
                        NPP, NEP, SoilRespiration, BiomassTurnover, Snags, SnagBranch,
                        AGMedium, AGfast, AGveryfast, AGslow, BGveryfast, BGfast, BGslow, SoilCStock,
                        EcosystemCStock)
save("FRI1200Summer", file =  "FRI1200_Summer_Newest.RData")

FRINFSummer <- cbind (Season, FireReturnInterval, StandStructure, BasalArea, BiomassLiveCStock, Recruitment,
                      NPP, NEP, SoilRespiration, BiomassTurnover, Snags, SnagBranch,
                      AGMedium, AGfast, AGveryfast, AGslow, BGveryfast, BGfast, BGslow, SoilCStock,
                      EcosystemCStock)
save("FRINFSummer", file =  "FRINF_Summer_Newest.RData")
