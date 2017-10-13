library("reshape")
library("plotrix")
library("relaimpo")
library("MASS")
library("car")
library("ggplot2")
library("multcomp")
library("class")
library("Hmisc")
library("scatterplot3d")
library("reshape")
library("fitdistrplus")
library("compare")
library("truncreg")
library("truncdist")
library("msm") ## for dtnorm
library("actuar")
library("moments")
library("vegan")
library("stats")
library("utils")



################
## full model ##
################

exe <- function(stand, Y, FRI, Season) {Sampled <- subpop (reg = c("A2", "D4", "B3", "C3", "E1", "E3"), st = c("EnEn"), d = Plots)
    Sampled <- Sampled[sample(1:dim(Sampled)[1], size = 1, replace = T), ]
    Tree.List <- GetTrees (Tree, Plots = Sampled)
    as.character(Tree.List$DBH) as.factor(Tree.List$ESSENCE)
    Tree.List <- as.data.frame(lapply(Tree.List[, ], function(x)rep(x, 25)))
    range.DBH <- c(seq(1, 30, by = 2), 100)
    Tree.List$DBH <- as.numeric(Tree.List$DBH)

    basal_big_class <- 0.0707905544
    BAB <- rep(0, 100)
    TBA <- 3.142 * (Tree.List[Tree.List[, 3]>31, 3]/200)^2
    BAB <- round(TBA/basal_big_class, digits = 0)
    y <- sum(BAB)
    stand[15] <- stand[15] + y
    n <- length(stand)
    N1s <- rep(1, n)
    N0s <- rep(0, n)
    BurnMortP <- numeric(n)
    BurnMortPpar <- numeric(n)
    shannon <- 0
    ScH <- 0
    cl <- 0
    stand <- rep(0, 15)


    dbhl  <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29)  # dbh lower limit
    dbhu <- c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31)  # dbh upper limit
    dbhq  <- sqrt((dbhu^3-dbhl^3)/((dbhu-dbhl) * 3))  # assuming a uniform dbh distribution over interval
    baq  <- (dbhq/2)^2 * pi/1e4

    # Initialise variables
    Top <- ceiling(Height(dbhq))
    Top <- ifelse(Top<2, rep(2, length(Top)), Top)
    HeightUpdated <- Top
    PCR <- crownratio(dbhq, sum(baq * stand))
    CCR <- PCR
    Base <- floor(HeightUpdated * (1-CCR))
    Bark <- 0.032 * 2.54 * dbhq ##inches to cm!! Black spruce Equation taken from Behave plus fire modelling system  (thickness in cm)
    hatom2 <- 1e4
    b <- 3
    DenCrit <- 0.11
    iw <- dbhu-dbhl
    BarkThickness <- 0.032 * 2.54 * dbhq
    n <- length(stand)
    N1s <- rep(1, n)
    N0s <- rep(0, n)
    n  <- length(stand)
    m2Ha  <- 1e4
    biocar_factor <- 0.5
    MgKg <- 1000
    SaplingSurvival <- 0.98  # sapling survival of different initial sizes (Matthias et al. 2003)
    pBurn <- 1/FRI
    NF <- 0
    fire.year <- NULL
    RegenLagTime <- 27   # assume it takes a 3-yr old natural seedling 27 yearss to grow to
                         # 214 cm (DBH 1.0 cm), VanBoagaert et al. 2015)
    RegenRegular <- 60   # natural regeneration
    RegenIrregular <- 55 # natural regeneration
    shannon <- diversity(stand[5:15], index = "shannon", MARGIN = 1, base = exp(1))  # shannon index
    RegenCohorts <- rpois(RegenLagTime, ifelse(shannon<1.7, RegenRegular, RegenIrregular))
    CPool <- c(5.8, 2.1, 24, 9.89, 9.0, 36, 1.69, 3.75, 74) * MgKg
    Snags <- 0
    Snagbranches <- 0
    SnagFoliage <- 0
    SnagCoarse <- 0
    SnagFine <- 0
    CC <- 0


    # Initialize object variables to save simulation results
    Size <- matrix(0, Y, n, byrow = TRUE)
    PreFireStand <- matrix(0, Y, n, byrow = TRUE)
    Recruits <- numeric(Y)
    BA <- numeric(Y)
    FireDeaths <- matrix(0, Y, n, byrow = TRUE)
    Senescence <- matrix(0, Y, n, byrow = TRUE)
    Transition <- matrix(0, Y, n, byrow = TRUE)  # transition probability
    Crecimiento <- matrix(0, Y, n, byrow = TRUE)
    Parcela <- matrix(0, Y, n, byrow = TRUE)
    Muertos <- matrix(0, Y, n, byrow = TRUE)
    Mortality <- matrix(0, Y, n, byrow = TRUE)  # probability of mortality
    Structure <- numeric(Y)
    InitialIntensity <- numeric(Y)
    FireSeason <- numeric(Y)
    BALost <- numeric(Y)
    Delta_BA <- numeric(Y)
    DeltaN <- matrix(0, Y, n, byrow = TRUE)
    CR <- matrix(0, Y, n, byrow = TRUE)
    Heights <- matrix(0, Y, n, byrow = TRUE)
    Heights <- matrix(0, Y, n, byrow = TRUE)
    ShiftCrownratio <- matrix(0, Y, n, byrow = TRUE)
    ShiftHeights <- matrix(0, Y, n, byrow = TRUE)
    DiameterGrowth <- matrix(0, Y, n, byrow = TRUE)
    SnagCProduction <- numeric(Y)
    Turnover <- numeric(Y)
    DOMC_Pool <- matrix(0, Y, length(CPool), byrow = TRUE)
    DOM_Flux <- numeric(Y)
    DOM_Inputs <- matrix(0, Y, 9, byrow = TRUE)
    BioMass <- matrix(c(Stemwood(dbhq), Bark(dbhq), Branches(dbhq), Needles(dbhq), Coarse(dbhq), Fineroots(dbhq)), nrow = 6,
                      ncol = length(dbhq), byrow = TRUE)
    BioMassCarbon <- BioMass * biocar_factor  # Biomass C per diameter class
    InitialCBiomass <- sum(BioMassCarbon% * %as.matrix(stand))  # initial biomass for the site
    ICB <- InitialCBiomass  # initial biomass for the site
    NetPrimaryProductivity <- numeric(Y)
    TotalLiveBiomass <- numeric(Y)
    AppliedDecayRates <- matrix(0, Y, 9, byrow = TRUE)
    EmpiricalTemperature <- numeric(Y)
    NetEcosystemProduction <- numeric(Y)
    Rh <- numeric(Y)
    CarbonCombusted <- numeric(Y)
    AnnualBiomassRecruits <- numeric(Y)

    # main loop
    for (y in 1:Y) {

        HeadIntensity <- sample(Season, size = 1, replace = F)
        I <- HeadIntensity
        Istart  <- I
        shannon <- diversity(stand[5:15], index = "shannon", MARGIN = 1, base = exp(1)) ##updated shannon
        Structure[y] <- shannon
        Firedeaths <- rep(0, 15)
        BALost[y] <- 0
        bay <- sum(stand * baq)
        MAT <- 0.36
        EmpiricalTemperature[y] <- MAT
        AppDecayRates <- Decayrates(MAT)
        AppliedDecayRates[y, ] <- AppDecayRates
        Decayrate <- rep(0, 9)
        Decayrate[1] <- AppDecayRates[1]
        Decayrate[2] <- AppDecayRates[2]
        Decayrate[3] <- AppDecayRates[3]
        Decayrate[4] <- AppDecayRates[4]
        Decayrate[5] <- AppDecayRates[5]
        Decayrate[6] <- AppDecayRates[6]
        Decayrate[7] <- AppDecayRates[7]
        Decayrate[8] <- AppDecayRates[8]
        Decayrate[9] <- AppDecayRates[9]

        # Apply decay rates to CPools
        CarbonPoolTransferMatrix <- matrix(c(
            Decayrate[1] * 0.83, (1-Decayrate[1]-0.08), 0, 0.08, 0, 0, Decayrate[1] * (1-0.83), 0, 0, 0,
            Decayrate[2] * 0.83, 0, (1-Decayrate[2]-0.10), 0, 0.10, 0, Decayrate[2] * (1-0.83), 0, 0, 0,
            Decayrate[3] * 0.83, 0, 0, 1-Decayrate[3], 0, 0, Decayrate[3] * (1-0.83), 0, 0, 0,
            Decayrate[4] * 0.83, 0, 0, 0, (1-Decayrate[4]), 0, Decayrate[4] * (1-0.83), 0, 0, 0,
            Decayrate[5] * 0.815, 0, 0, 0, 0, (1-Decayrate[5]), Decayrate[5] * (1-0.815), 0, 0, 0,
            Decayrate[6] * 1, 0, 0, 0, 0, 0, (1-Decayrate[6]-0.006), 0, 0, 0.006,
            Decayrate[7] * 0.83, 0, 0, 0, 0, 0, 0, (1-Decayrate[7]), 0, Decayrate[7] * (1-0.83),
            Decayrate[8] * 0.83, 0, 0, 0, 0, 0, 0, 0, (1-Decayrate[8]), Decayrate[8] * (1-0.83),
            Decayrate[9] * 1, 0, 0, 0, 0, 0, 0, 0, 0, 1-Decayrate[9]
        ), nrow = 9, ncol = 10, byrow = TRUE)
        colnames(CarbonPoolTransferMatrix) <- c("Atm", "Snags", "Snagbranch", "Medium",
                                                "AGfast", "AGveryfast", "AGslow", "BGveryfast", "BGfast", "BGslow")
        rownames(CarbonPoolTransferMatrix) <- c("Snags", "Snagbranch", "Medium", "AGfast", "AGveryfast", "AGslow",
                                                "BGveryfast", "BGfast", "BGslow")

        tmp <- as.vector(t(CPool)% * %CarbonPoolTransferMatrix)
        SoilCAtmFlux <- tmp[1]


        # Stand dynamics
        annual_diam_incre <- diameter_growth(stand, dbhq, baq, u_i, u_ik)
        annual_diam_incre_small <- diametersmall(stand, dbhq, baq)
        annual_diam_incre[1:4] <- annual_diam_incre_small[1:4]
        adi <- annual_diam_incre
        graduating <- 1 / (iw / adi) #Transition probabilities
        growth <- rbinom(N1s, stand, graduating) #stochastically
        stand <- stand - growth
        stand <- stand + c(0, growth[1:n - 1]) #after growth
        LastDCT <- growth[15]  # prevents loosing trees in last diameter class
        stand [15] <- stand[15] + LastDCT
        BAIncrement <- sum(growth * baq)    # patch BA increment due to growth
        CCRgrowth <- growth * CCR           # the trees that grow bring their crown ratio
        Heightgrowth <- growth * HeightUpdated

        # Calculate Biomass due to growth
        GrowthCBiomass <- sum(BioMassCarbon % * % as.matrix(stand))  # calculate biomass due to growth
                                                                     # Biomass that has not been lost to turnover or mortality
        Parcela[y, ] <- stand  # stand after regeneration, captures regeneration pulses
        delta <- (GrowthCBiomass-ICB)

        # Apply turnover
        # match IPCC Good Practice Guidance
        Stemwoodsmall <- BioMassCarbon[1, 1:4] % * % (stand[1:4])
        Barkmerchantable <- BioMassCarbon[2, 5:15] % * % (stand[5:15])
        LiveBiomassCPools <- BioMassCarbon % * % (stand) # biomass C kg/ha
        LiveBiomassCPoolsCorrected  <- matrix(0, nrow = 5, ncol = 1)
        LiveBiomassCPoolsCorrected[1, 1] <- LiveBiomassCPools[1, 1] - Stemwoodsmall + Barkmerchantable
        LiveBiomassCPoolsCorrected[2, 1] <- LiveBiomassCPools[2, 1] - Barkmerchantable + LiveBiomassCPools[3, 1] +
            Stemwoodsmall   #Otherwood + Bark
        LiveBiomassCPoolsCorrected[3, 1] <- LiveBiomassCPools[4, 1]  # Foliage
        LiveBiomassCPoolsCorrected[4, 1] <- LiveBiomassCPools[5, 1]  # Coarse
        LiveBiomassCPoolsCorrected[5, 1] <- LiveBiomassCPools[6, 1]  # Fine


        # Apply transfer rates
        Input_Matrix2 <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, (1 - 0.04), 0, 0, 0, 0, (0.04 * 0.25), 0, (0.04 * 0.75), 0, 0, 0, 0, 0,
                                  0, 0, (1 - 0.16), 0, 0, 0, 0, 0, 0, (0.16 * 1), 0, 0, 0, 0,
                                  0, 0, 0, (1 - 0.02), 0, 0, 0, 0, (0.02 * 0.5), 0, 0, 0, (0.02 * 0.5), 0,
                                  0, 0, 0, 0, (1 - 0.64), 0, 0, 0, 0, (0.64 * 0.5), 0, (0.64 * 0.5), 0, 0
                                  ), nrow = 5, ncol = 14, byrow = TRUE)

        colnames(Input_Matrix2) <- c("Merchantable", "Otherwood", "Needles",
                                     "Coarse", "Fine", "Snags", "Snagbranch",
                                     "Medium", "AGfast", "AGveryfast", "AGslow",
                                     "BGveryfast", "BGfast", "BGslow")
        rownames(Input_Matrix2) <- c("Merchantable", "Otherwood",
                                     "Needles", "Coarse", "Fine")


        ctmp <- as.vector(t(LiveBiomassCPoolsCorrected)% * %Input_Matrix2)  # turnover
        BiomassLostTurnover <- sum(ctmp[6:14])

        # Carbon fluxes
        NPP <- delta  +  BiomassLostTurnover
        NEP <- (NPP-SoilCAtmFlux)


        # Mortality (senescence and fire)
        surviving <- mortality(stand, dbhq, baq)
        surviving[1:4] <- SaplingSurvival
        Senescencedeaths <- rbinom(N1s, stand, 1 - surviving)
        deaths <- Senescencedeaths + Firedeaths
        Muertos[y, ] <- deaths
        Senescence[y, ] <- Senescencedeaths
        SnagCpools <- SnagsCarbon(deaths, BioMassCarbon)
        Snags <- SnagCpools$SnagC
        Snagbranches <- SnagCpools$SnagbranchC
        SnagFoliage <- SnagCpools$SnagFoliage
        SnagCoarse <- SnagCpools$SnagCoarse
        SnagFine <- SnagCpools$SnagFine
        stand <- stand-Senescencedeaths        # fire deaths are taken care in the fire module
        DeltaBA <- BAIncrement                 # net basal area increment after mortality (fire and senescence) and growth
        DeltaStand <- sum(stand)               # net change in density after mortality (fire and senescence) and growth
        if(sum(stand < 0) > 0)
            stop(message = "neg count 3")
        Delta_BA[y] <- DeltaBA
        DeltaN[y, ] <- deaths + growth
        CMortality <- Snags + Snagbranches + SnagFoliage + SnagCoarse + SnagFine

        # Fire module
        # Evaluates if a fire arrives, its intensity, crowning, and post-fire tree mortality and regeneration.
        if(runif(1) < pBurn) {  # Determine if a fire happens
            fire.year <- c(fire.year, y)
            NF <- NF + 1  # Update number of fires that occurred during the simulation
            RegenCohorts <- rep(0, RegenLagTime)  # KILL ALL regenerating trees
            prefirestand <- stand
            PreFireStand[y, ] <- prefirestand
            NewRegen <- SeedProd(prefirestand, baq)
            Fuel <- as.numeric(FuelClass(stand, dbhq))  # Kg/ha per dbh class
            VF <- VerticalFuelProfile(Fuel, HeightUpdated, Base)
            cl <- Crowning(I, HeightUpdated, Base, VF, b, DenCrit) #if there is crowning, update the crown layer affected by fire
            cl <- ifelse (cl>0, UpdateCrownLayer(cl, HeightUpdated, Base, VF, b, DenCrit),
                          Crowning(I, HeightUpdated, Base, VF, b, DenCrit))
            u <- UpdateIntensity(I, HeightUpdated[cl])
            I <- ifelse(cl > 0, max(UpdateIntensity(I, HeightUpdated[cl])), Istart)
            ScH <- ScorchHeight(I)
            CK <- CrownKill(I, HeightUpdated, CCR)
            BurnMortP <- ScorchMortality(BarkThickness, CrownKill(I, HeightUpdated, CCR))
            Firedeaths <- rbinom(N1s, stand, BurnMortP)
            Firedeaths <- ifelse(is.na(Firedeaths), N0s, Firedeaths)
            FireDeaths[y, ] <- Firedeaths
            newstand <- stand-Firedeaths
            severity <- Basalost(stand, baq, newstand)
            stand <- newstand # update stand after a fire
            InitialIntensity[y] <- I # adjusted intensity using Catchpole et.al 1992
            #FireSeason[y] <- Season
            BALost[y] <- severity
            tmp3 <- CPool[3] * 0.392140        # carbon consumed in the medium carbon pool
            tmp4 <- CPool[4] * 0.6415          # carbon consumed in the Ag fast carbon pool
            tmp5 <- CPool[5] * 0.968533        # carbon consumed in the Ag very fast pool
            tmp6 <- CPool[6] * 0.09001         # carbon consumed in the Ag slowpool
            CPool[3] <- CPool[3]-tmp3
            CPool[4] <- CPool[4]-tmp4
            CPool[5] <- CPool[5]-tmp5
            CPool[6] <- CPool[6]-tmp6
            CC <- tmp3 + tmp4 + tmp5 + tmp6
        } else {
            NewRegen <- rpois(1, ifelse(shannon < 1.7, RegenRegular, RegenIrregular))
        }

        # Add recruitment
        Recruits[y] <- RegenCohorts[RegenLagTime]
        RegenCohorts <- c(NewRegen, RegenCohorts[1:RegenLagTime - 1])
        stand[1] <- stand[1] + Recruits[y]
        CCRRecruits  <- Recruits[y] * PCR[1]
        HeightRecruits  <- Recruits[y] * Top[1]


       # Update biomass
        Biomass <- sum(BioMassCarbon % * % as.matrix(stand))
        ICB <- Biomass

        # Dynamically updating crown ratios and heights of recruits (natural regenerated and fire derived)
        xH <- Top + adi  # Heightgrowth
        dH <- xH-Top     # deltaheight
        MaximumCR <- (CCR * Top + dH) / xH
        TotalN <- stand + c(0, growth[1:n - 1])
        TotalN[1] <- TotalN[1] + Recruits[y]
        CCR <- updateCR(PCR, CCR, dbhq, Top, adi, bay, DeltaBA, stand)  # CCR after recruitment, growth, mortality
        CCRnow <- stand * CCR
        ShiftCR <- CCRnow + c(0, CCRgrowth[1:n - 1])
        ShiftCR[1] <- ShiftCR[1] + CCRRecruits
        CCR <- ifelse(stand > 0, pmin(MaximumCR, ShiftCR / TotalN), PCR)

        # Updating heights
        Top <- Height(dbhq)
        HeightUpdated <- Top + adi  # Heightgrowth
        Heightnow <- stand * HeightUpdated
        ShiftHeight <- Heightnow + c(0, Heightgrowth[1:n-1])
        ShiftHeight[1] <- ShiftHeight[1] + HeightRecruits
        HeightUpdated <- ifelse(stand>0, ShiftHeight/TotalN, Top)
        Base <- HeightUpdated * (1-CCR)


        # Distribute turnover to carbon pools
        ctmp[6] <- ctmp[6] + Snags             # adding C from snags
        ctmp[7] <- ctmp[7] + Snagbranches      # adding C from small trees
        ctmp[9] <- ctmp[9] + SnagCoarse * (0.5) # adding C from coarse roots to AG fast
        ctmp[10] <- ctmp[10] + SnagFine * (0.5) + SnagFoliage * (0.85)
        ctmp[12] <- ctmp[12] + SnagFine * (0.5)
        ctmp[13] <- ctmp[13] + SnagCoarse * (0.5)
        Inputs <- ctmp[6:14]  # how much C is incorporated into the DOMCpools including snags (mortality)
        CPool <- tmp[2:10] + Inputs # update the pools


        # Save results into objects
        CarbonCombusted[y] <- CC
        SnagCProduction[y] <- CMortality
        TotalLiveBiomass[y] <-   Biomass
        DOMC_Pool[y, ] <- CPool # Nine DOM carbon pools
        DOM_Flux[y] <- SoilCAtmFlux
        DOM_Inputs[y, ] <- Inputs
        Turnover[y] <- BiomassLostTurnover
        NetPrimaryProductivity[y] <- NPP
        NetEcosystemProduction[y] <- NEP
        Rh[y] <- SoilCAtmFlux
        BA[y] <- bay
        CR[y, ] <- CCR
        Heights[y, ] <- HeightUpdated
        ShiftCrownratio[y, ] <- ShiftCR
        ShiftHeights[y, ] <- ShiftHeight
        DiameterGrowth[y, ] <- adi
        Transition[y, ] <- graduating
        Mortality[y, ] <- 1-surviving
        Crecimiento[y, ] <- growth
        Muertos[y, ] <- deaths
        Size[y, ] <- stand # final size after all processes
    }


    res <- list(Parcela = Parcela, Size = Size, DeltaN = DeltaN, BA = BA,
                PreFireStand = PreFireStand, Senescence = Senescence,
                AppliedDecayRates = AppliedDecayRates,
                EmpiricalTemperature = EmpiricalTemperature, FireDeaths = FireDeaths,
                Muertos = Muertos, Structure = Structure, Crecimiento = Crecimiento,
                Mortality = Mortality, Transition = Transition, Structure = Structure,
                DiameterGrowth = DiameterGrowth, CR = CR, CMortality = CMortality,
                ShiftCrownratio = ShiftCrownratio, ShiftHeights = ShiftHeights,
                Heights = Heights, BALost = BALost, NF = NF, InitialIntensity = InitialIntensity,
                fire.year = fire.year, Recruits = Recruits, TotalLiveBiomass = TotalLiveBiomass,
                CarbonCombusted = CarbonCombusted, Turnover = Turnover, SnagCProduction = SnagCProduction,
                DOMC_Pool = DOMC_Pool, DOM_Flux = DOM_Flux, DOM_Inputs = DOM_Inputs,
                NetPrimaryProductivity = NetPrimaryProductivity, Rh = Rh,
                NetEcosystemProduction = NetEcosystemProduction)

    return(res)
}
