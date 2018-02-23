
 library("reshape")
 library("ggplot2")


#Spring60<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\New_Simulation_AfterSylvie\\60\\Spring\\FRI60_Spring2400.Rdata", .GlobalEnv)
#Spring60<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\Simulation_FireSize_Crownratio\\60\\Spring\\FRI60_Spring2400_C.Rdata", .GlobalEnv)  
 Spring60<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations2017\\AfterDefense\\FRI60_Spring_Newest.Rdata", .GlobalEnv)  
 Summer60<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations2017\\AfterDefense\\FRI60_Summer_Newest.Rdata", .GlobalEnv)
#Summer60<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\##\Simulation_FireSize_Crownratio\\60\\Summer\\FRI60_Summer2400_C.Rdata", .GlobalEnv)
#Summer60<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\New_Simulation_AfterSylvie\\60\\Summer\\FRI60_Summer2400.Rdata", .GlobalEnv)
FRI60<-data.frame(rbind(FRI60Spring, FRI60Summer))
length(FRI60[,1])
str(FRI60)
FRI60$Season

#Spring100<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\New_Simulation_AfterSylvie\\100\\Spring\\FRI100_Spring2400.Rdata", .GlobalEnv)
Spring100<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations2017\\AfterDefense\\FRI100_Spring_Newest.Rdata", .GlobalEnv)
Summer100<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations2017\\AfterDefense\\FRI100_Summer_Newest.Rdata", .GlobalEnv)
#Spring100<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\Simulation_FireSize_Crownratio\\100\\Spring\\FRI100_Spring2400_C.Rdata", .GlobalEnv)
#Summer100<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\Simulation_FireSize_Crownratio\\100\\Summer\\FRI100_Summer2400_C.Rdata", .GlobalEnv)
#Summer100<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\New_Simulation_AfterSylvie\\100\\Summer\\FRI100_Summer2400.Rdata", .GlobalEnv)
FRI100<-data.frame(rbind(FRI100Spring, FRI100Summer))
length(FRI100[,1])
FRI100$Season

#Spring150<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\New_Simulation_AfterSylvie\\150\\Spring\\FRI150_Spring2400.Rdata", .GlobalEnv)
Spring150<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations2017\\AfterDefense\\FRI150_Spring_Newest.Rdata", .GlobalEnv)
Summer150<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations2017\\AfterDefense\\FRI150_Summer_Newest.Rdata", .GlobalEnv)
#Spring150<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\Simulation_FireSize_Crownratio\\150\\Spring\\FRI150_Spring2400_C.Rdata", .GlobalEnv)
#Summer150<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\Simulation_FireSize_Crownratio\\150\\Summer\\FRI150_Summer2400_C.Rdata", .GlobalEnv)
#Summer150<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\New_Simulation_AfterSylvie\\150\\Summer\\FRI150_Summer2400.Rdata", .GlobalEnv)
FRI150<-data.frame(rbind(FRI150Spring,FRI150Summer))
str(FRI150)
FRI150$Season


#Spring300<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\New_Simulation_AfterSylvie\\300\\Spring\\FRI300_Spring2400.Rdata", .GlobalEnv)
#Spring300<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\Simulation_FireSize_Crownratio\\300\\Spring\\FRI300_Spring2400_C.Rdata", .GlobalEnv)
Spring300<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations2017\\AfterDefense\\FRI300_Spring_Newest.Rdata", .GlobalEnv)
Summer300<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations2017\\AfterDefense\\FRI300_Summer_Newest.Rdata", .GlobalEnv)
#Summer300<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\Simulation_FireSize_Crownratio\\300\\Summer\\FRI300_Summer2400_C.Rdata", .GlobalEnv)
#Summer300<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\New_Simulation_AfterSylvie\\300\\Summer\\FRI300_Summer2400.Rdata", .GlobalEnv)
FRI300<-data.frame(rbind(FRI300Spring,FRI300Summer))
FRI300$Season


#Spring700<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\New_Simulation_AfterSylvie\\700\\Spring\\FRI700_Spring2400.Rdata", .GlobalEnv)
#Spring700<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\Simulation_FireSize_Crownratio\\700\\Spring\\FRI700_Spring2400_C.Rdata", .GlobalEnv)
Spring700<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations2017\\AfterDefense\\FRI700_Spring_Newest.Rdata", .GlobalEnv)
Summer700<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations2017\\AfterDefense\\FRI700_Summer_Newest.Rdata", .GlobalEnv)
#Summer700<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\Simulation_FireSize_Crownratio\\700\\Summer\\FRI700_Summer2400_C.Rdata", .GlobalEnv)
#Summer700<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\New_Simulation_AfterSylvie\\700\\Summer\\FRI700_Summer2400.Rdata", .GlobalEnv)
FRI700<-data.frame(rbind(FRI700Spring,FRI700Summer))
FRI700$Season

#Spring1200<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\Simulation_FireSize_Crownratio\\1200\\Spring\\FRI1200_Spring2400_C.Rdata", .GlobalEnv)
Spring1200<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations2017\\AfterDefense\\FRI1200_Spring_Newest.Rdata", .GlobalEnv)
Summer1200<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations2017\\AfterDefense\\FRI1200_Summer_Newest.Rdata", .GlobalEnv)
#Summer1200<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\Simulation_FireSize_Crownratio\\1200\\Summer\\FRI1200_Summer2400_C.Rdata", .GlobalEnv)
FRI1200<-data.frame(rbind(FRI1200Spring,FRI1200Summer))

#SpringNF<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\Simulation_FireSize_Crownratio\\NoFire\\Spring\\FRINF_Spring2400_C.Rdata", .GlobalEnv)
SpringNF<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations2017\\AfterDefense\\FRINF_Spring_Newest.Rdata", .GlobalEnv)
SummerNF<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations2017\\AfterDefense\\FRINF_Summer_Newest.Rdata", .GlobalEnv)
#SummerNF<-load("C:\\Users\\psladmin\\Documents\\ModelOutput2\\Simulations\\Simulation_FireSize_Crownratio\\NoFire\\Summer\\FRINF_Summer2400_C.Rdata", .GlobalEnv)
FRINF<-data.frame(rbind(FRINFSpring,FRINFSummer))
str(FRINF)
FRINF$Season


All<-rbind(FRI60,FRI100, FRI150,FRI300,FRI700,FRI1200,FRINF)
All$FireReturnInterval <- factor(All$FireReturnInterval)
levels(All$FireReturnInterval)[levels(All$FireReturnInterval)== "2.4e+13"] <- "NF"
levels(All$FireReturnInterval)[levels(All$FireReturnInterval)== "2.4e+09"] <- "NF"
length(All[,1])
head(All)
All$FireReturnInterval
as.factor(All$FireReturnInterval)
All$Season<-as.factor(All$Season)
 as.factor(All$FireReturnInterval)
KgMg<-(0.001)
kghatogm2<-0.1
All[,5] <- with(All, All[,5]*KgMg)
All[,7:21] <- with(All, All[,7:21]*KgMg)
#All[,5:27] <-with(All, All[,5:27]*kghatogm2)
All$FireReturnInterval <- factor(All$FireReturnInterval,
levels=c('60','100','150','300','700','1200','NF'), ordered=TRUE)
All$Organic<-(All$AGveryfast+All$AGslow)
All$Mineral<-(All$BGveryfast+All$BGslow)
#All$Soil<-sum(All[,17:25]*KgMg)
All$WoodyDebris<-(All$Snags+All$SnagBranch+All$AGfast+All$AGMedium)

#########
#tapply(All$NPP,list(FireReturnInterval=All$FireReturnInterval,
#  Season=All$Season), mean, na.rm=TRUE)
##Stand Structure after 500 years
All$Structure<-ifelse(All$StandStructure>=1.7,'Irregular','Regular')
All$Structure<-as.factor(All$Structure)

##calculate C balance for each structure FireReturn Interval and season


Fire60Springregular <- All[ which(All$FireReturnInterval==60 
                                & All$Season == 1& All$Structure =="Regular"), ]
length(Fire60Springregular[,1]) ##344                             
                                
mean(Fire60Springregular$NPP)#2.53
mean(Fire60Springregular$SoilRespiration)#1.89
mean(Fire60Springregular$NEP)#0.644
mean(Fire60Springregular$ EcosystemCStock)#169.17
mean(Fire60Springregular$SoilCStock) #143.91
mean(Fire60Springregular$BiomassLiveCStock)  #25.26
mean(Fire60Springregular$Organic)  #32.7
mean(Fire60Springregular$Mineral)  #87.4
mean(Fire60Springregular$WoodyDebris)  #22.3

sd(Fire60Springregular$NPP)
sd(Fire60Springregular$SoilRespiration)
sd(Fire60Springregular$NEP)
sd(Fire60Springregular$ EcosystemCStock)
sd(Fire60Springregular$SoilCStock) 
sd(Fire60Springregular$BiomassLiveCStock)  
sd(Fire60Springregular$Organic)  
sd(Fire60Springregular$Mineral)  
sd(Fire60Springregular$WoodyDebris)  




Fire60Springirre <-  All[ which(All$FireReturnInterval==60 
                                & All$Season == 1& All$Structure =="Irregular"), ]

length(Fire60Springirre[,1]) ##656  

head(Fire60Springirre)
mean(Fire60Springirre$NPP)  #3.38
mean(Fire60Springirre$SoilRespiration) #3.08
mean(Fire60Springirre$NEP) #0.29
mean(Fire60Springirre$ EcosystemCStock)  #223.47
mean(Fire60Springirre$SoilCStock) ##174.46
mean(Fire60Springirre$BiomassLiveCStock)  ##49.00
mean(Fire60Springirre$Organic)  #32.7
mean(Fire60Springirre$Mineral)  #87.4
mean(Fire60Springirre$WoodyDebris)  #22.3


sd(Fire60Springirre$NPP)  
sd(Fire60Springirre$SoilRespiration) 
sd(Fire60Springirre$NEP) 
sd(Fire60Springirre$ EcosystemCStock)  
sd(Fire60Springirre$SoilCStock) 
sd(Fire60Springirre$BiomassLiveCStock)  
sd(Fire60Springirre$Organic)  
sd(Fire60Springirre$Mineral)
sd(Fire60Springirre$WoodyDebris) 


Fire60Summerirre <-  All[ which(All$FireReturnInterval==60 
                                & All$Season == 2& All$Structure =="Irregular"), ]

length(Fire60Summerirre[,1]) ##624  

head(Fire60Summerirre)
mean(Fire60Summerirre$NPP)  #3.21
mean(Fire60Summerirre$SoilRespiration) #2.99
mean(Fire60Summerirre$NEP) #0.22
mean(Fire60Summerirre$ EcosystemCStock)  #216.86
mean(Fire60Summerirre$SoilCStock) ##169.97
mean(Fire60Summerirre$BiomassLiveCStock)  ##46.88
mean(Fire60Summerirre$Organic)  #32.7
mean(Fire60Summerirre$Mineral)  #87.4
mean(Fire60Summerirre$WoodyDebris)  #22.3   


sd(Fire60Summerirre$NPP) 
sd(Fire60Summerirre$SoilRespiration) 
sd(Fire60Summerirre$NEP) 
sd(Fire60Summerirre$ EcosystemCStock)  
sd(Fire60Summerirre$SoilCStock) 
sd(Fire60Summerirre$BiomassLiveCStock)  
sd(Fire60Summerirre$Organic)  
sd(Fire60Summerirre$Mineral)  
sd(Fire60Summerirre$WoodyDebris)    

                                
Fire60Summerregular <-  All[ which(All$FireReturnInterval==60 
& All$Season == 2& All$Structure =="Regular"), ]
length(Fire60Summerregular[,1]) ##376 

head(Fire60Summerregular)
mean(Fire60Summerregular$NPP)  #2.85
mean(Fire60Summerregular$SoilRespiration) #1.94
mean(Fire60Summerregular$NEP) #0.90
mean(Fire60Summerregular$ EcosystemCStock)  #165.43
mean(Fire60Summerregular$SoilCStock) ##137.29
mean(Fire60Summerregular$BiomassLiveCStock)  ##28.14
mean(Fire60Summerregular$Organic)  #32.7
mean(Fire60Summerregular$Mineral)  #87.4
mean(Fire60Summerregular$WoodyDebris)  #22.3  



sd(Fire60Summerregular$NPP)  
sd(Fire60Summerregular$SoilRespiration) 
sd(Fire60Summerregular$NEP) 
sd(Fire60Summerregular$ EcosystemCStock)  
sd(Fire60Summerregular$SoilCStock) 
sd(Fire60Summerregular$BiomassLiveCStock)  
sd(Fire60Summerregular$Organic)  
sd(Fire60Summerregular$Mineral)  
sd(Fire60Summerregular$WoodyDebris)    


......
Fire100Springregular <- All[ which(All$FireReturnInterval==100 
                                & All$Season == 1& All$Structure =="Regular"), ]
length(Fire100Springregular[,1]) ##228                              
                                
mean(Fire100Springregular$NPP)#3.86
mean(Fire100Springregular$SoilRespiration)#2.54
mean(Fire100Springregular$NEP)#1.31
mean(Fire100Springregular$ EcosystemCStock)#217.07
mean(Fire100Springregular$SoilCStock) #179.51
mean(Fire100Springregular$BiomassLiveCStock)  #37.56
mean(Fire100Springregular$Organic)   #41.06
mean(Fire100Springregular$Mineral)   #106.6
mean(Fire100Springregular$WoodyDebris) #30.04


sd(Fire100Springregular$NPP)
sd(Fire100Springregular$SoilRespiration)
sd(Fire100Springregular$NEP)
sd(Fire100Springregular$ EcosystemCStock)
sd(Fire100Springregular$SoilCStock) 
sd(Fire100Springregular$BiomassLiveCStock)  
sd(Fire100Springregular$Organic)   
sd(Fire100Springregular$Mineral)   
sd(Fire100Springregular$WoodyDebris) 



  

Fire100Springirre <-  All[ which(All$FireReturnInterval==100 
                                & All$Season == 1& All$Structure =="Irregular"), ]

length(Fire100Springirre[,1]) ##772    

head(Fire100Springirre)
mean(Fire100Springirre$NPP)  #3.41
mean(Fire100Springirre$SoilRespiration) #3.49
mean(Fire100Springirre$NEP) #-0.0791
mean(Fire100Springirre$BiomassLiveCStock)  ##54.85
mean(Fire100Springirre$SoilCStock) ##211.17
mean(Fire100Springirre$ EcosystemCStock)  #266.03
mean(Fire100Springirre$Organic)   #50.66
mean(Fire100Springirre$WoodyDebris) # 45.5
mean(Fire100Springirre$Mineral)   # 110.9

sd(Fire100Springirre$NPP)  #3.41
sd(Fire100Springirre$SoilRespiration) #3.49
sd(Fire100Springirre$NEP) #-0.0791
sd(Fire100Springirre$BiomassLiveCStock)  ##54.85
sd(Fire100Springirre$SoilCStock) ##211.17
sd(Fire100Springirre$ EcosystemCStock)  #266.03
sd(Fire100Springirre$Organic)   #50.66
sd(Fire100Springirre$WoodyDebris) # 45.5
sd(Fire100Springirre$Mineral)   # 110.9




Fire100Summerirre <-  All[ which(All$FireReturnInterval==100 
                                & All$Season == 2& All$Structure =="Irregular"), ]

length(Fire100Summerirre[,1]) ##745   

head(Fire100Summerirre)
mean(Fire100Summerirre$NPP)  #3.48
mean(Fire100Summerirre$SoilRespiration) #3.38
mean(Fire100Summerirre$NEP) #0.100
mean(Fire100Summerirre$ EcosystemCStock)  #257.16
mean(Fire100Summerirre$SoilCStock) ##204.16
mean(Fire100Summerirre$BiomassLiveCStock)  ##52.99   
mean(Fire100Summerirre$Organic)   #49.11
mean(Fire100Summerirre$WoodyDebris) #43.1 
mean(Fire100Summerirre$Mineral)   # 107.9


sd(Fire100Summerirre$NPP)  #3.48
sd(Fire100Summerirre$SoilRespiration) #3.38
sd(Fire100Summerirre$NEP) #0.100
sd(Fire100Summerirre$ EcosystemCStock)  #257.16
sd(Fire100Summerirre$SoilCStock) ##204.16
sd(Fire100Summerirre$BiomassLiveCStock)  ##52.99   
sd(Fire100Summerirre$Organic)   #49.11
sd(Fire100Summerirre$WoodyDebris) #43.1 
sd(Fire100Summerirre$Mineral)   # 107.9                           
                                
Fire100Summerregular <-  All[ which(All$FireReturnInterval==100 
& All$Season == 2& All$Structure =="Regular"), ]
length(Fire100Summerregular[,1]) ##255  

head(Fire100Summerregular)
mean(Fire100Summerregular$NPP)  #3.85
mean(Fire100Summerregular$SoilRespiration) #2.65
mean(Fire100Summerregular$NEP) #1.19
mean(Fire100Summerregular$ EcosystemCStock)  #214.93
mean(Fire100Summerregular$SoilCStock) ##176.44
mean(Fire100Summerregular$BiomassLiveCStock)  ##38.48 
mean(Fire100Summerregular$Organic)   #
mean(Fire100Summerregular$WoodyDebris) # 
mean(Fire100Summerregular$Mineral) 


sd(Fire100Summerregular$NPP)  #3.85
sd(Fire100Summerregular$SoilRespiration) #2.65
sd(Fire100Summerregular$NEP) #1.19
sd(Fire100Summerregular$ EcosystemCStock)  #214.93
sd(Fire100Summerregular$SoilCStock) ##176.44
sd(Fire100Summerregular$BiomassLiveCStock)  ##38.48 
sd(Fire100Summerregular$Organic)   #
sd(Fire100Summerregular$WoodyDebris) # 
sd(Fire100Summerregular$Mineral) 



   
Fire150Springregular <-  All[ which(All$FireReturnInterval==150 
                                & All$Season == 1& All$Structure =="Regular"), ]
length(Fire150Springregular[,1]) ##150  
head(Fire150Springregular)
mean(Fire150Springregular$NPP)  #4.09
mean(Fire150Springregular$SoilRespiration) #2.92
mean(Fire150Springregular$NEP) #1.16
mean(Fire150Springregular$ EcosystemCStock)  #239.32
mean(Fire150Springregular$SoilCStock) ##197.23
mean(Fire150Springregular$BiomassLiveCStock)  ##42.08 
mean(Fire150Springregular$Organic)   # 45.6
mean(Fire150Springregular$WoodyDebris) #  34.9
mean(Fire150Springregular$Mineral)  #  114.4

sd(Fire150Springregular$NPP)  #4.09
sd(Fire150Springregular$SoilRespiration) #2.92
sd(Fire150Springregular$NEP) #1.16
sd(Fire150Springregular$ EcosystemCStock)  #239.32
sd(Fire150Springregular$SoilCStock) ##197.23
sd(Fire150Springregular$BiomassLiveCStock)  ##42.08 
sd(Fire150Springregular$Organic)   # 45.6
sd(Fire150Springregular$WoodyDebris) #  34.9
sd(Fire150Springregular$Mineral)  #  114.4




Fire150Springirregular <-  All[ which(All$FireReturnInterval==150 
                                & All$Season == 1& All$Structure =="Irregular"), ]
length(Fire150Springirregular[,1]) ##863
head(Fire150Springirregular)
mean(Fire150Springirregular$NPP)  #3.64
mean(Fire150Springirregular$SoilRespiration) #3.65
mean(Fire150Springirregular$NEP) #-0.012
mean(Fire150Springirregular$ EcosystemCStock)  #282.74
mean(Fire150Springirregular$SoilCStock) ##225.10
mean(Fire150Springirregular$BiomassLiveCStock)  ##57.64
mean(Fire150Springirregular$Organic)   # 53.4
mean(Fire150Springirregular$WoodyDebris) #  48.7
mean(Fire150Springirregular$Mineral)  # 118.6 


sd(Fire150Springirregular$NPP)  #3.64
sd(Fire150Springirregular$SoilRespiration) #3.65
sd(Fire150Springirregular$NEP) #-0.012
sd(Fire150Springirregular$ EcosystemCStock)  #282.74
sd(Fire150Springirregular$SoilCStock) ##225.10
sd(Fire150Springirregular$BiomassLiveCStock)  ##57.64
sd(Fire150Springirregular$Organic)   # 53.4
sd(Fire150Springirregular$WoodyDebris) #  48.7
sd(Fire150Springirregular$Mineral)  # 118.6 





Fire150Summerregular <-  All[ which(All$FireReturnInterval==150 
                                & All$Season == 2& All$Structure =="Regular"), ]
length(Fire150Summerregular[,1]) ##172

head(Fire150Summerregular)
mean(Fire150Summerregular$NPP)  #4.29
mean(Fire150Summerregular$SoilRespiration) #3.00
mean(Fire150Summerregular$NEP) #1.28
mean(Fire150Summerregular$ EcosystemCStock)  #236.74
mean(Fire150Summerregular$SoilCStock) ##191.80
mean(Fire150Summerregular$BiomassLiveCStock)  ##44.93
mean(Fire150Summerregular$Organic)   # 53.4
mean(Fire150Summerregular$WoodyDebris) #  48.7
mean(Fire150Summerregular$Mineral)

sd(Fire150Summerregular$NPP)  #4.29
sd(Fire150Summerregular$SoilRespiration) #3.00
sd(Fire150Summerregular$NEP) #1.28
sd(Fire150Summerregular$ EcosystemCStock)  #236.74
sd(Fire150Summerregular$SoilCStock) ##191.80
sd(Fire150Summerregular$BiomassLiveCStock)  ##44.93
sd(Fire150Summerregular$Organic)   # 53.4
sd(Fire150Summerregular$WoodyDebris) #  48.7
sd(Fire150Summerregular$Mineral)





Fire150Summerirregular <-  All[ which(All$FireReturnInterval==150 
                                & All$Season == 2& All$Structure =="Irregular"), ]
length(Fire150Summerirregular[,1]) ##828

head(Fire150Summerirregular)
mean(Fire150Summerirregular$NPP)  #3.58
mean(Fire150Summerirregular$SoilRespiration) #3.65
mean(Fire150Summerirregular$NEP) #-0.07
mean(Fire150Summerirregular$ EcosystemCStock)  #280.52                        
mean(Fire150Summerirregular$SoilCStock) ##223.00
mean(Fire150Summerirregular$BiomassLiveCStock)  ##57.51
mean(Fire150Summerirregular$Organic)   # 
mean(Fire150Summerirregular$WoodyDebris) #  
mean(Fire150Summerirregular$Mineral)



sd(Fire150Summerirregular$NPP)  #3.58
sd(Fire150Summerirregular$SoilRespiration) #3.65
sd(Fire150Summerirregular$NEP) #-0.07
sd(Fire150Summerirregular$ EcosystemCStock)  #280.52                        
sd(Fire150Summerirregular$SoilCStock) ##223.00
sd(Fire150Summerirregular$BiomassLiveCStock)  ##57.51
sd(Fire150Summerirregular$Organic)   # 
sd(Fire150Summerirregular$WoodyDebris) #  
sd(Fire150Summerirregular$Mineral)

Fire300Springregular <-  All[ which(All$FireReturnInterval==300 
                                & All$Season == 1& All$Structure =="Regular"), ]
length(Fire300Springregular[,1]) ##86
head(Fire300Springregular)
mean(Fire300Springregular$NPP)  #4.68
mean(Fire300Springregular$SoilRespiration) #3.08
mean(Fire300Springregular$NEP) #1.59
mean(Fire300Springregular$ EcosystemCStock)  #248.48
mean(Fire300Springregular$SoilCStock) ##201.97
mean(Fire300Springregular$BiomassLiveCStock)  ##46.50
mean(Fire300Springregular$Organic)   # 
mean(Fire300Springregular$WoodyDebris) #  
mean(Fire300Springregular$Mineral)

sd(Fire300Springregular$NPP)  #4.68
sd(Fire300Springregular$SoilRespiration) #3.08
sd(Fire300Springregular$NEP) #1.59
sd(Fire300Springregular$ EcosystemCStock)  #248.48
sd(Fire300Springregular$SoilCStock) ##201.97
sd(Fire300Springregular$BiomassLiveCStock)  ##46.50
sd(Fire300Springregular$Organic)   # 
sd(Fire300Springregular$WoodyDebris) #  
sd(Fire300Springregular$Mineral)

Fire300Springirregular <-  All[ which(All$FireReturnInterval==300 
                                & All$Season == 1& All$Structure =="Irregular"), ]
length(Fire300Springirregular[,1]) ##914
head(Fire300Springirregular)
mean(Fire300Springirregular$NPP)  #3.68
mean(Fire300Springirregular$SoilRespiration) #3.71
mean(Fire300Springirregular$NEP) #-0.03
mean(Fire300Springirregular$ EcosystemCStock)  #289.24
mean(Fire300Springirregular$SoilCStock) ##229.65
mean(Fire300Springirregular$BiomassLiveCStock)  ##59.58
mean(Fire300Springirregular$Organic)   # 
mean(Fire300Springirregular$WoodyDebris) #  
mean(Fire300Springirregular$Mineral)


sd(Fire300Springirregular$NPP)  #3.68
sd(Fire300Springirregular$SoilRespiration) #3.71
sd(Fire300Springirregular$NEP) #-0.03
sd(Fire300Springirregular$ EcosystemCStock)  #289.24
sd(Fire300Springirregular$SoilCStock) ##229.65
sd(Fire300Springirregular$BiomassLiveCStock)  ##59.58
sd(Fire300Springirregular$Organic)   # 
sd(Fire300Springirregular$WoodyDebris) #  
sd(Fire300Springirregular$Mineral)

Fire300Summerregular <-  All[ which(All$FireReturnInterval==300 
                                & All$Season == 2& All$Structure =="Regular"), ]
length(Fire300Summerregular[,1]) ##84

head(Fire300Summerregular)
mean(Fire300Summerregular$NPP)  #4.99
mean(Fire300Summerregular$SoilRespiration) #3.31
mean(Fire300Summerregular$NEP) #1.68
mean(Fire300Summerregular$ EcosystemCStock)  #254.97
mean(Fire300Summerregular$SoilCStock) ##203.95
mean(Fire300Summerregular$BiomassLiveCStock)  ##51.01
mean(Fire300Summerregular$Organic)   # 
mean(Fire300Summerregular$WoodyDebris) #  
mean(Fire300Summerregular$Mineral)

sd(Fire300Summerregular$NPP)  #4.99
sd(Fire300Summerregular$SoilRespiration) #3.31
sd(Fire300Summerregular$NEP) #1.68
sd(Fire300Summerregular$ EcosystemCStock)  #254.97
sd(Fire300Summerregular$SoilCStock) ##203.95
sd(Fire300Summerregular$BiomassLiveCStock)  ##51.01
sd(Fire300Summerregular$Organic)   # 
sd(Fire300Summerregular$WoodyDebris) #  
sd(Fire300Summerregular$Mineral)


Fire300Summerirregular <-  All[ which(All$FireReturnInterval==300 
                                & All$Season == 2& All$Structure =="Irregular"), ]
length(Fire300Summerirregular[,1]) ##916
head(Fire300Summerirregular)
mean(Fire300Summerirregular$NPP)  #3.61
mean(Fire300Summerirregular$SoilRespiration) #3.67
mean(Fire300Summerirregular$NEP) #-0.06
mean(Fire300Summerirregular$ EcosystemCStock)  #287.01
mean(Fire300Summerirregular$SoilCStock) ##228.52
mean(Fire300Summerirregular$BiomassLiveCStock)  ##58.48
mean(Fire300Summerirregular$Organic)   # 
mean(Fire300Summerirregular$WoodyDebris) #  
mean(Fire300Summerirregular$Mineral)

sd(Fire300Summerirregular$NPP)  #3.61
sd(Fire300Summerirregular$SoilRespiration) #3.67
sd(Fire300Summerirregular$NEP) #-0.06
sd(Fire300Summerirregular$ EcosystemCStock)  #287.01
sd(Fire300Summerirregular$SoilCStock) ##228.52
sd(Fire300Summerirregular$BiomassLiveCStock)  ##58.48
sd(Fire300Summerirregular$Organic)   # 
sd(Fire300Summerirregular$WoodyDebris) #  
sd(Fire300Summerirregular$Mineral)




...

Fire700 <-  All[ which(All$FireReturnInterval==700), ]
mean(Fire700$NPP)

Fire700Springregular <-  All[ which(All$FireReturnInterval==700 
                                & All$Season == 1& All$Structure =="Regular"), ]
length(Fire700Springregular[,1]) ##33
head(Fire700Springregular)
mean(Fire700Springregular$NPP)  #5.24
mean(Fire700Springregular$SoilRespiration) #3.41
mean(Fire700Springregular$NEP) #1.83
mean(Fire700Springregular$ EcosystemCStock)  #260.09
mean(Fire700Springregular$SoilCStock) ##207.07
mean(Fire700Springregular$BiomassLiveCStock)  ##53.01
mean(Fire700Springregular$Organic)   # 
mean(Fire700Springregular$WoodyDebris) #  
mean(Fire700Springregular$Mineral)

sd(Fire700Springregular$NPP)  #5.24
sd(Fire700Springregular$SoilRespiration) #3.41
sd(Fire700Springregular$NEP) #1.83
sd(Fire700Springregular$ EcosystemCStock)  #260.09
sd(Fire700Springregular$SoilCStock) ##207.07
sd(Fire700Springregular$BiomassLiveCStock)  ##53.01
sd(Fire700Springregular$Organic)   # 
sd(Fire700Springregular$WoodyDebris) #  
sd(Fire700Springregular$Mineral)








Fire700Springirregular <-  All[ which(All$FireReturnInterval==700 
                                & All$Season == 1& All$Structure =="Irregular"), ]
length(Fire700Springirregular[,1]) ##967
head(Fire700Springirregular)
mean(Fire700Springirregular$NPP)  #3.64
mean(Fire700Springirregular$SoilRespiration) #3.63
mean(Fire700Springirregular$NEP) #-0.03
mean(Fire700Springirregular$ EcosystemCStock)  #286.88
mean(Fire700Springirregular$SoilCStock) ##227.48
mean(Fire700Springirregular$BiomassLiveCStock)  ##59.39
mean(Fire700Springirregular$Organic)   # 
mean(Fire700Springirregular$WoodyDebris) #  
mean(Fire700Springirregular$Mineral)


sd(Fire700Springirregular$NPP)  #3.64
sd(Fire700Springirregular$SoilRespiration) #3.63
sd(Fire700Springirregular$NEP) #-0.03
sd(Fire700Springirregular$ EcosystemCStock)  #286.88
sd(Fire700Springirregular$SoilCStock) ##227.48
sd(Fire700Springirregular$BiomassLiveCStock)  ##59.39
sd(Fire700Springirregular$Organic)   # 
sd(Fire700Springirregular$WoodyDebris) #  
sd(Fire700Springirregular$Mineral)

Fire700Summerregular <-  All[ which(All$FireReturnInterval==700 
                                & All$Season == 2& All$Structure =="Regular"), ]
length(Fire700Summerregular[,1]) ##32
head(Fire700Summerregular)
mean(Fire700Summerregular$NPP)  #5.36
mean(Fire700Summerregular$SoilRespiration) #3.69
mean(Fire700Summerregular$NEP) #1.67
mean(Fire700Summerregular$ EcosystemCStock)  #267.36
mean(Fire700Summerregular$SoilCStock) ##209.49
mean(Fire700Summerregular$BiomassLiveCStock)  ##57.86
mean(Fire700Summerregular$Organic)   # 
mean(Fire700Summerregular$WoodyDebris) #  
mean(Fire700Summerregular$Mineral)


sd(Fire700Summerregular$NPP)  #5.36
sd(Fire700Summerregular$SoilRespiration) #3.69
sd(Fire700Summerregular$NEP) #1.67
sd(Fire700Summerregular$ EcosystemCStock)  #267.36
sd(Fire700Summerregular$SoilCStock) ##209.49
sd(Fire700Summerregular$BiomassLiveCStock)  ##57.86
sd(Fire700Summerregular$Organic)   # 
sd(Fire700Summerregular$WoodyDebris) #  
sd(Fire700Summerregular$Mineral)


Fire700Summerirregular <-  All[ which(All$FireReturnInterval==700 
                                & All$Season == 2& All$Structure =="Irregular"), ]
length(Fire700Summerirregular[,1]) ##968
head(Fire700Summerirregular)
mean(Fire700Summerirregular$NPP)  #3.57
mean(Fire700Summerirregular$SoilRespiration) #3.58
mean(Fire700Summerirregular$NEP) #-0.01
mean(Fire700Summerirregular$ EcosystemCStock)  #285.04
mean(Fire700Summerirregular$SoilCStock) ##226.89
mean(Fire700Summerirregular$BiomassLiveCStock)  ##58.14
mean(Fire700Summerirregular$Organic)   # 
mean(Fire700Summerirregular$WoodyDebris) #  
mean(Fire700Summerirregular$Mineral)

sd(Fire700Summerirregular$NPP)  #3.57
sd(Fire700Summerirregular$SoilRespiration) #3.58
sd(Fire700Summerirregular$NEP) #-0.01
sd(Fire700Summerirregular$ EcosystemCStock)  #285.04
sd(Fire700Summerirregular$SoilCStock) ##226.89
sd(Fire700Summerirregular$BiomassLiveCStock)  ##58.14
sd(Fire700Summerirregular$Organic)   # 
sd(Fire700Summerirregular$WoodyDebris) #  
sd(Fire700Summerirregular$Mineral)



..........

Fire1200Springregular <-  All[ which(All$FireReturnInterval==1200 
                                & All$Season == 1& All$Structure =="Regular"), ]
length(Fire1200Springregular[,1]) ##22
head(Fire1200Springregular)
mean(Fire1200Springregular$NPP)  #5.48
mean(Fire1200Springregular$SoilRespiration) #3.37
mean(Fire1200Springregular$NEP) #2.10
mean(Fire1200Springregular$ EcosystemCStock)  #257.40
mean(Fire1200Springregular$SoilCStock) ##203.63
mean(Fire1200Springregular$BiomassLiveCStock)  ##53.76
mean(Fire1200Springregular$Organic)   # 
mean(Fire1200Springregular$WoodyDebris) #  
mean(Fire1200Springregular$Mineral)


sd(Fire1200Springregular$NPP)  #5.48
sd(Fire1200Springregular$SoilRespiration) #3.37
sd(Fire1200Springregular$NEP) #2.10
sd(Fire1200Springregular$ EcosystemCStock)  #257.40
sd(Fire1200Springregular$SoilCStock) ##203.63
sd(Fire1200Springregular$BiomassLiveCStock)  ##53.76
sd(Fire1200Springregular$Organic)   # 
sd(Fire1200Springregular$WoodyDebris) #  
sd(Fire1200Springregular$Mineral)

Fire1200Springirregular <-  All[ which(All$FireReturnInterval==1200 
                                & All$Season == 1& All$Structure =="Irregular"), ]
length(Fire1200Springirregular[,1]) ##978
head(Fire1200Springirregular)
mean(Fire1200Springirregular$NPP)  #3.64
mean(Fire1200Springirregular$SoilRespiration) #3.63
mean(Fire1200Springirregular$NEP) #-0.03
mean(Fire1200Springirregular$ EcosystemCStock)  #286.88
mean(Fire1200Springirregular$SoilCStock) ##227.48
mean(Fire1200Springirregular$BiomassLiveCStock)  ##59.39
mean(Fire1200Springirregular$Organic)   # 
mean(Fire1200Springirregular$WoodyDebris) #  
mean(Fire1200Springirregular$Mineral)



sd(Fire1200Springirregular$NPP)  #3.64
sd(Fire1200Springirregular$SoilRespiration) #3.63
sd(Fire1200Springirregular$NEP) #-0.03
sd(Fire1200Springirregular$ EcosystemCStock)  #286.88
sd(Fire1200Springirregular$SoilCStock) ##227.48
sd(Fire1200Springirregular$BiomassLiveCStock)  ##59.39
sd(Fire1200Springirregular$Organic)   # 
sd(Fire1200Springirregular$WoodyDebris) #  
sd(Fire1200Springirregular$Mineral)

Fire1200Summerregular <-  All[ which(All$FireReturnInterval==1200 
                                & All$Season == 2& All$Structure =="Regular"), ]
length(Fire1200Summerregular[,1]) ##25
head(Fire1200Summerregular)
mean(Fire1200Summerregular$NPP)  #5.36
mean(Fire1200Summerregular$SoilRespiration) #3.69
mean(Fire1200Summerregular$NEP) #1.67
mean(Fire1200Summerregular$ EcosystemCStock)  #267.36
mean(Fire1200Summerregular$SoilCStock) ##209.49
mean(Fire1200Summerregular$BiomassLiveCStock)  ##57.86
mean(Fire1200Summerregular$Organic)   # 
mean(Fire1200Summerregular$WoodyDebris) #  
mean(Fire1200Summerregular$Mineral)

sd(Fire1200Summerregular$NPP)  #5.36
sd(Fire1200Summerregular$SoilRespiration) #3.69
sd(Fire1200Summerregular$NEP) #1.67
sd(Fire1200Summerregular$ EcosystemCStock)  #267.36
sd(Fire1200Summerregular$SoilCStock) ##209.49
sd(Fire1200Summerregular$BiomassLiveCStock)  ##57.86
sd(Fire1200Summerregular$Organic)   # 
sd(Fire1200Summerregular$WoodyDebris) #  
sd(Fire1200Summerregular$Mineral)





Fire1200Summerirregular <-  All[ which(All$FireReturnInterval==1200 
                                & All$Season == 2& All$Structure =="Irregular"), ]
length(Fire1200Summerirregular[,1]) ##975
head(Fire1200Summerirregular)
mean(Fire1200Summerirregular$NPP)  #3.57
mean(Fire1200Summerirregular$SoilRespiration) #3.58
mean(Fire1200Summerirregular$NEP) #-0.009
mean(Fire1200Summerirregular$ EcosystemCStock)  #284.46
mean(Fire1200Summerirregular$SoilCStock) ##225.74
mean(Fire1200Summerirregular$BiomassLiveCStock)  ##58.72
mean(Fire1200Summerirregular$Organic)   # 
mean(Fire1200Summerirregular$WoodyDebris) #  
mean(Fire1200Summerirregular$Mineral)

sd(Fire1200Summerirregular$NPP)  #3.57
sd(Fire1200Summerirregular$SoilRespiration) #3.58
sd(Fire1200Summerirregular$NEP) #-0.009
sd(Fire1200Summerirregular$ EcosystemCStock)  #284.46
sd(Fire1200Summerirregular$SoilCStock) ##225.74
sd(Fire1200Summerirregular$BiomassLiveCStock)  ##58.72
sd(Fire1200Summerirregular$Organic)   # 
sd(Fire1200Summerirregular$WoodyDebris) #  
sd(Fire1200Summerirregular$Mineral)



................

FireNFSpringirregular <-  All[ which(All$FireReturnInterval=="NF"
                                & All$Season == 1& All$Structure =="Irregular"), ]
length(FireNFSpringirregular[,1]) ##1000
head(FireNFSpringirregular)
mean(FireNFSpringirregular$NPP)  #3.52
mean(FireNFSpringirregular$SoilRespiration) #3.52
mean(FireNFSpringirregular$NEP) #0
mean(FireNFSpringirregular$ EcosystemCStock)  #280.46
mean(FireNFSpringirregular$SoilCStock) ##222.44
mean(FireNFSpringirregular$BiomassLiveCStock)  ##58.01
mean(FireNFSpringirregular$Organic)   # 
mean(FireNFSpringirregular$WoodyDebris) #  
mean(FireNFSpringirregular$Mineral)

sd(FireNFSpringirregular$NPP)  #3.52
sd(FireNFSpringirregular$SoilRespiration) #3.52
sd(FireNFSpringirregular$NEP) #0
sd(FireNFSpringirregular$ EcosystemCStock)  #280.46
sd(FireNFSpringirregular$SoilCStock) ##222.44
sd(FireNFSpringirregular$BiomassLiveCStock)  ##58.01
sd(FireNFSpringirregular$Organic)   # 
sd(FireNFSpringirregular$WoodyDebris) #  
sd(FireNFSpringirregular$Mineral)

FireNFSummerirregular <-  All[ which(All$FireReturnInterval=="NF"
                                & All$Season == 2& All$Structure =="Irregular"), ]
length(FireNFSummerirregular[,1]) ##1000
head(FireNFSummerirregular)
mean(FireNFSummerirregular$NPP)  #3.52
mean(FireNFSummerirregular$SoilRespiration) #3.51
mean(FireNFSummerirregular$NEP) #0.01
mean(FireNFSummerirregular$ EcosystemCStock)  #280.1
mean(FireNFSummerirregular$SoilCStock) ##222.08
mean(FireNFSummerirregular$BiomassLiveCStock)  ##58.01
mean(FireNFSummerirregular$Organic)   # 
mean(FireNFSummerirregular$WoodyDebris) #  
mean(FireNFSummerirregular$Mineral)


sd(FireNFSummerirregular$NPP)  #3.52
sd(FireNFSummerirregular$SoilRespiration) #3.51
sd(FireNFSummerirregular$NEP) #0.01
sd(FireNFSummerirregular$ EcosystemCStock)  #280.1
sd(FireNFSummerirregular$SoilCStock) ##222.08
sd(FireNFSummerirregular$BiomassLiveCStock)  ##58.01
sd(FireNFSummerirregular$Organic)   # 
sd(FireNFSummerirregular$WoodyDebris) #  
sd(FireNFSummerirregular$Mineral)

##nested ANOVA

bb<-lm(NPP~FireReturnInterval*Season, data=All)

plot(bb)
par(mfcol=c(1,2))
plot(bb, 1:2) # Plot diagnostics for the model
 Anova(bb)
Anova Table (Type II test)
interactionMeans(bb)




####Function to do ANOVA to each variable (each of 12 columns)
###FireReturn
aov.fct<-function(x){
aov1<-summary(aov(x~FireReturnInterval*Season,data=All))
FR<-as.character(aov1[[1]][1,5])
Seas<-as.character(aov1[[1]][2,5] )
Int<-as.character(aov1[[1]][3,5])
return(list(FR=FR,Seas=Seas,Int=Int))
}


res<-do.call(rbind,lapply(as.list((All[,5:30])),aov.fct))
as.character(res)


lapply(as.list(All[,5:27]),aov.fct)

aa<-tapply(All$NEP,list(All$FireReturnInterval),mean)

means<-function(x){
promedio<-tapply(x,
list(FireReturnInterval=All$FireReturnInterval,
Season=All$Season), mean, na.rm=TRUE)
return(promedio)}

lista<-lapply(as.list(All[,5:30]),means)



detach(package:nlme)
##attach lme4 package which contains lmer( ) function - YOU MUST DETACH nlme BEFORE ATTACHING lme4 BECAUSE THEY CONFLICT WITH EACH OTHER
library(lme4)
lmm.1 <- lmer(formula = EcosystemCStock ~ FireReturnInterval+Season + FireReturnInterval*Season
+ (1|Structure), data= All)
summary(lmm.1)

##look at deviance slot
lmm.1@deviance

lmm1.ML <-lmer(Radon~Floor+County_uranium + (1 | County), data=radon, REML = FALSE)
lmm1.ML@deviance["pwrss"]/lmm1.ML@dims["n"] #gives ML estimate of the residual variance
summary(lmm1.ML)




detach(package:lme4)
library(nlme)
###mixed model
#Analize the residual effect of structure after the effect of
#fire return interval and season was accounted for.
mod<-gls(EcosystemCStock~FireReturnInterval+Season+FireReturnInterval:Season,data=All)
varId<-varIdent(form=~1 | FireReturnInterval)
moda<-gls(EcosystemCStock~FireReturnInterval+Season+FireReturnInterval:Season, weights=varId, data=All)
mod1<-lme(EcosystemCStock~FireReturnInterval+Season+FireReturnInterval:Season, random=~1+FireReturnInterval|Structure, method="REML",data=All)
mod2<-lme(EcosystemCStock~FireReturnInterval+Season+FireReturnInterval:Season, random=~1|Structure,method="REML",weights=varIdent(form=~1 | FireReturnInterval), data=All)
plot(residuals(mod2, type="pearson")~All$FireReturnInterval, ylab="Pearson residuals", xlab="FireReturnInterval")
anova(moda, mod1, mod2)  # better fit with mod2

summary(mod2)

#mod2<-lme(EcosystemCStock~FireReturnInterval+Season +
#FireReturnInterval:Season, random=~1|Structure,weights=varIdent(form=~1
#| FireReturnInterval), data=All)
##Basic diagnostioc plots
par(2,1)
boxplot(NPP~FireReturnInterval, data=All)
boxplot(NPP~Season,data=All)
###################

## Set the proportions of interest.
p = Sratio
N = length(p)
value = critical.range = c()
## Compute critical values.
for (i in 1:(N-1))
   { for (j in (i+1):N)
    {
     value = c(value,(abs(p[i]-p[j])))
     critical.range = c(critical.range,
      sqrt(qchisq(.95,4))*sqrt(p[i]*(1-p[i])/300 + p[j]*(1-p[j])/300))
    }
   }

round(cbind(value,critical.range),3)




LiveCtoEcosystem<-function(liveC) {
b<- as.vector(tapply(All$BiomassLiveCStock,list(FireReturnInterval=All$FireReturnInterval), mean, na.rm=TRUE))
so<- as.vector(tapply(All$Soil,list(FireReturnInterval=All$FireReturnInterval), mean, na.rm=TRUE))
e<- as.vector(tapply(All$EcosystemCStock,list(FireReturnInterval=All$FireReturnInterval), mean, na.rm=TRUE))
Lratio<-as.vector(b/e)
Sratio<-as.vector(so/e*100)
CContribution<-cbind(b,so)
rownames(CContribution)<-c("60","150","300","700","1200")
six<-CContribution[1,1:2]
onefif<-CContribution[2,1:2]
trois<- CContribution[3,1:2]
sept<- CContribution[4,1:2]
on12<- CContribution[5,1:2]
prop.test(six,onefif, correct=FALSE)
chisq.test(CContribution, correct=FALSE)
.Table <- matrix(c(43,150,54,186,56,189,55,187,55,185), 5, 2, byrow=TRUE)
rownames(.Table) <- c('1', '2', '3', '4', '5')
colnames(.Table) <- c('1', '2')
.Test <- chisq.test(.Table, correct=FALSE)

##Stocks
#plotmeans(All$EcosystemCStock~All$FireReturnInterval,xlab="Fire Return Inteval", ylab="Stocks MgC/ha",connect=FALSE)

BioStock<-tapply(All$BiomassLiveCStock,All$FireReturnInterval,mean)
SoilStock<-tapply(All$SoilCStock,All$FireReturnInterval,mean)
TousStocks<-rbind(c(245.83,267.43,268.77,267.30,266.27),
c(43.74,57.22,55.76,55.13,55.2))

sixty<-cbind(TousStocks[,1],1, c(1:2))
one50<- cbind(TousStocks[,2],2, c(1:2))
trois00<- cbind(TousStocks[,3],3, c(1:2))
seventy00<- cbind(TousStocks[,4],4, c(1:2))
one200<- cbind(TousStocks[,5],5, c(1:2))
NF<-cbind(TousStocks[,6],6, c(1:2))
data<-as.data.frame(rbind(sixty,one50,trois00,seventy00,one200))
colnames(data)<-c("CStock","FireReturnInterval","Group")
data$FireReturnInterval<-factor(data$FireReturnInterval,labels=c("60","150","300","700","1200"))
data$Group<-factor(data$Group,labels=c("DOM C","Biomass C"))
ggplot(data,aes(FireReturnInterval,CStock,fill=factor(Group))) +
geom_bar(stat="identity")


##BiomassCpools
#plotmeans(All$EcosystemCStock~All$FireReturnInterval,xlab="Fire Return Inteval", ylab="Stocks MgC/ha",connect=FALSE)

BioCPool1<-sapply(All[,5:9],mean)
BioCPool1<-tapply(All[,5:9],All$FireRetunInterval,mean)
head(All[,5:9])
BioCPool1<-tapply(All[,5],All$FireReturnInterval,mean)
BioCPool2<-tapply(All[,6],All$FireReturnInterval,mean)
BioCPool3<-tapply(All[,7],All$FireReturnInterval,mean)
BioCPool4<-tapply(All[,8],All$FireReturnInterval,mean)
BioCPool5<-tapply(All[,9],All$FireReturnInterval,mean)

TousBio<-rbind(c(19.5,24.9,26.5,26.5,26.6),
c(8.2,9.7,9.5,8.7,8.6),
c(4.4,5.1,5.0,4.7,4.6),
c(10.3,13.3,14.0,13.9,13.9),
c(1.1,1.4,1.4,1.3,1.3))

sixty<-cbind(TousBio[,1],1, c(1:5))
one50<- cbind(TousBio[,2],2, c(1:5))
trois00<- cbind(TousBio[,3],3, c(1:5))
seventy00<- cbind(TousBio[,4],4, c(1:5))
one200<- cbind(TousBio[,5],5, c(1:5))
data<-as.data.frame(rbind(sixty,one50,trois00,seventy00,one200))
colnames(data)<-c("BiomassC","FireReturnInterval","Group")
data$FireReturnInterval<-factor(data$FireReturnInterval,labels=c("60","150","300","700","1200"))
data$Group<-factor(data$Group,labels=c("Merchantable","Otherwood","Foliage","CoarseRoots","FineRoots"))
ggplot(data,aes(FireReturnInterval,BiomassC,fill=factor(Group))) +
geom_bar(stat="identity") +
ylim(0,60)







interaction.plot(All$FireReturnInterval,All$Season,All$NetPrimaryProduction, fun=mean, type="b")

pc1<-ggplot(All,aes(FireReturnInterval,NetPrimaryProduction,color=factor(Season)))
pc1<-pc1+geom_point(shape=1)+ylim(0,4)+scale_x_discrete("FireReturnInterval")
pc2<-pc1+theme_bw()
pc2<-pc2+
pc2




Subset60<-All[which(All$FireReturnInterval=="60"),]
Subset100<-All[which(All$FireReturnInterval=="100"),]
Subset150<-All[which(All$FireReturnInterval=="150"),]
Subset300<-All[which(All$FireReturnInterval=="300"),]
Subset700<-All[which(All$FireReturnInterval=="700"),]
Subset1200<-All[which(All$FireReturnInterval=="1200"),]
SubsetNF<-All[which(All$FireReturnInterval=="NF"),]

table(SubsetNF$Structure)


.Table <- matrix(c(1469,531,1668,332,1753,247,1907,93,1944,56,1961,39,2000,0), 7, 2, byrow=TRUE)
rownames(.Table) <- c('60','100', '150', '300', '700', '1200', 'NoFire')
colnames(.Table) <- c('Uneven', 'Even')
chisq.test(.Table)




#####################Statistical Analysis
##Carbon storage
#All$EcosystemCStocklog<-log (All$EcosystemCStock)
#
#mod.ecs <- lme(EcosystemCStocklog~FireReturnInterval+Season +
#FireReturnInterval:Season, random=~1|Structure, data=All)

##Total ecosystem C
mod.ecs1 <- lme(EcosystemCStock~FireReturnInterval+Season+
FireReturnInterval:Season, random=~1|Structure,
weights=varIdent(form=~1 | FireReturnInterval), data=All)
mod.ecs <- lme(EcosystemCStock~FireReturnInterval+Season+
FireReturnInterval:Season, random=~1|Structure,
, data=All)
plot(residuals(mod.ecs, type="pearson")~All$FireReturnInterval, ylab="Pearson residuals", xlab="FireReturnInterval")
plot(residuals(mod.ecs, level=1, type="pearson")~fitted(mod.ecs, level=1), ylab="Résidus de Pearson", xlab="Valeurs prédites", main="Résidus vs valeurs prédites")
abline(h=0, lty=2)
anova(mod.ecs1)
summary(mod.ecs1)
##check relative variability of intercept with respect to the corresponding fixed effect (Pinheiro and Bates 2001, Section 4.3.2, p. 191)
(17.9)/abs(fixef(mod.ecs)[1])*100
##relative variability of intercept is 6.96% - a random effect that varies very little (e.g., 0.006%) could be deleted from model
#mod.ecs <- lme(EcosystemCStock~FireReturnInterval+Season +
#FireReturnInterval:Season, random=~1|Structure,
#weights = varPower(), data=All)
#plot(residuals(mod.ecs, type="pearson")~All$FireReturnInterval, ylab="Pearson residuals", xlab="FireReturnInterval")
#anova(mod.ecs)
#summary(mod.ecs)
ranef(mod.ecs)
fixef(mod.ecs)
coef(mod.ecs1)[2,]
coef(mod.ecs1)
Var_Random_effect <- as.numeric(VarCorr(mod.ecs))
plot(fitted(mod.ecs),
varests <- as.numeric(VarCorr(mod.ecs1)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)




##Total soil C

mod.scs <- lme(SoilCStock~FireReturnInterval+Season +
FireReturnInterval:Season, random=~1|Structure,
weights=varIdent(form=~1 | FireReturnInterval), data=All)
anova(mod.scs)
summary(mod.scs)
varests <- as.numeric(VarCorr(mod.scs)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)
 coef(mod.scs)

###BiomassL
mod.bcs <- lme(BiomassLiveCStock~FireReturnInterval+Season +
FireReturnInterval:Season, random=~1|Structure,
weights=varIdent(form=~1 | FireReturnInterval), data=All)
anova(mod.bcs)
summary(mod.bcs)
varests <- as.numeric(VarCorr(mod.bcs)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)
coef(mod.bcs)
###Organic
mod.org <- lme(Organic~FireReturnInterval+Season +
FireReturnInterval:Season, random=~1|Structure,
weights=varIdent(form=~1 | FireReturnInterval), data=All)
anova(mod.org)
summary(mod.org)
coef(mod.org)
varests <- as.numeric(VarCorr(mod.org)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)
coef(mod.org)

ctrl <- lmeControl(opt='optim')
mod.min<- lme(Mineral~FireReturnInterval+Season+FireReturnInterval*Season, random=~1|Structure,control=ctrl,weights=varIdent(form=~1 |FireReturnInterval), data=All)
anova(mod.min)
summary(mod.min)
varests <- as.numeric(VarCorr(mod.min)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)
coef(mod.min)


mod.wood<- lme(WoodyDebris~FireReturnInterval+Season +
FireReturnInterval:Season, random=~1|Structure,,control=ctrl,
weights=varIdent(form=~1 |FireReturnInterval), data=All)
mod.wood<- lme(WoodyDebris~FireReturnInterval+Season +
FireReturnInterval:Season, random=~1|Structure,
 data=All)
anova(mod.wood)
summary(mod.wood)
varests <- as.numeric(VarCorr(mod.wood)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)
coef(mod.wood)

mod.snagbranch<-lme(SnagBranch~FireReturnInterval+Season +
FireReturnInterval:Season, random=~1|Structure,control=ctrl,
weights=varIdent(form=~1 | FireReturnInterval), data=All)
anova(mod.snagbranch)
summary(mod.snagbranch)
varests <- as.numeric(VarCorr(mod.snagbranch)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)
coef(mod.snagbranch)



mod.snags<-lme(Snags~FireReturnInterval+Season, random=~1|Structure,control=ctrl,
weights=varIdent(form=~1 | FireReturnInterval), data=All)
anova(mod.snags)
summary(mod.snags)
varests <- as.numeric(VarCorr(mod.snags)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)
coef(mod.snags)

#####FLUXES

##Fluxes
#mod.npp<-lme(NPP~FireReturnInterval+Season +
#FireReturnInterval:Season, random=~1|Structure,
#weights = varPower(), data=All)
#anova(mod.npp)
ctrl <- lmeControl(opt='optim')
mod.npp<- lme(NPP~FireReturnInterval+Season +
FireReturnInterval:Season, random=~1|Structure,control=ctrl,
weights=varIdent(form=~1 | FireReturnInterval), data=All)
anova(mod.npp)
summary(mod.npp)
coef(mod.npp)
(1.07)/abs(fixef(mod.npp)[1])*100
summary(mod.npp)$tTable
VarCorr(mod.npp)
varests <- as.numeric(VarCorr(mod.npp)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)

#F0 <- fitted(mod.npp, level = 0)
#F1 <- fitted(mod.npp, level = 1)
#I <- order(All$FireReturnInterval); FireReturnIntervals <- sort(All$FireReturnInterval)
#plot(FireReturnIntervals, F0[I], lwd = 4, type = "l",
#ylim = c(0, 22), ylab = "Richness", xlab = "NAP")
#



mod.rh<- lme(SoilRespiration~FireReturnInterval+Season +
FireReturnInterval:Season, random=~1|Structure,control=ctrl,
weights=varIdent(form=~1 | FireReturnInterval), data=All)
anova(mod.rh)
summary(mod.rh)
coef(mod.rh)
varests <- as.numeric(VarCorr(mod.rh)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)

mod.nep<- lme(NEP~FireReturnInterval+Season +
FireReturnInterval:Season, random=~1|Structure, control=ctrl,
weights=varIdent(form=~1 | FireReturnInterval), data=All)
anova(mod.nep)
summary(mod.nep)
coef(mod.nep)
varests <- as.numeric(VarCorr(mod.nep)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)
mod.nep1<- lm(NEP~FireReturnInterval+Season +
FireReturnInterval:Season,data=All)
anova(mod.nep1)

mod.nep<- lme(NEP~FireReturnInterval+Season +
FireReturnInterval:Season, random=~1|Structure, data=All)

library(plyr)
library(ggplot2)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {


    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

####Function to do ANOVA to each variable (each of 12 columns)
###FireReturn
aov.fct<-function(x){
aov1<-summary(aov(x~FireReturnInterval*Scenario,data=AllT))
FR<-as.character(aov1[[1]][1,5])
Seas<-as.character(aov1[[1]][2,5] )
Int<-as.character(aov1[[1]][3,5])
return(list(FR=FR,Seas=Seas,Int=Int))
}

res<-do.call(rbind,lapply(as.list((All[,5:27])),aov.fct))
as.character(res)




head(All)

npp <- summarySE(All, measurevar="NPP", groupvars=c("FireReturnInterval"))
nep <- summarySE(All, measurevar="NEP", groupvars=c("FireReturnInterval"))
rh<- summarySE(All, measurevar="SoilRespiration", groupvars=c("FireReturnInterval"))
#ecs <- summarySE(All, measurevar="EcosystemCStock", groupvars=c("FireReturnInterval","Season#"))
ecs <- summarySE(All, measurevar="EcosystemCStock", groupvars=c("FireReturnInterval"))
scs<- summarySE(All, measurevar="SoilCStock", groupvars=c("FireReturnInterval","Season"))
bcs<-summarySE(All, measurevar="BiomassLiveCStock", groupvars=c("FireReturnInterval"))
mscs<-summarySE(All, measurevar="Mineral", groupvars=c("FireReturnInterval","Season"))
mscs<-summarySE(All, measurevar="Mineral", groupvars=c("FireReturnInterval"))
ol<-summarySE(All, measurevar="Organic", groupvars=c("FireReturnInterval"))
wd<-summarySE(All, measurevar="WoodyDebris", groupvars=c("FireReturnInterval"))

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


library(grid)

my_grobA = grobTree(textGrob("A)", x=0.01,  y=0.95, hjust=0,
  gp=gpar(col="black", fontsize=10, fontface="bold")))
my_grobB = grobTree(textGrob("B)", x=0.01,  y=0.95, hjust=0,
  gp=gpar(col="black", fontsize=10, fontface="bold")))
my_grobC = grobTree(textGrob("C)", x=0.01,  y=0.95, hjust=0,
  gp=gpar(col="black", fontsize=10, fontface="bold")))
my_grobD = grobTree(textGrob("D)", x=0.01,  y=0.95, hjust=0,
  gp=gpar(col="black", fontsize=10, fontface="bold")))
my_grobE = grobTree(textGrob("E)", x=0.01,  y=0.95, hjust=0,
  gp=gpar(col="black", fontsize=10, fontface="bold")))
my_grobF = grobTree(textGrob("F)", x=0.01,  y=0.95, hjust=0,
  gp=gpar(col="black", fontsize=10, fontface="bold")))




pd <- position_dodge(0.1)
pn<-ggplot(npp, aes(x=FireReturnInterval, y=NPP, colour=FireReturnInterval),ylim=c(2.9,3.80)) + labs(x="Fire Return Interval",y="NPP (MgC/ha*yr)")+
geom_errorbar(aes(ymin=NPP-ci, ymax=NPP+ci), colour="black", width=.1, position=pd)
pnp<-pn+geom_line(position=pd) + geom_point(size=2, colour="black") 
pnpp<-pnp + theme(axis.text = element_text(colour = "black", size = 12),
axis.title = element_text(size = 12, color = "black"))
pnpp1<-pnpp + theme_bw()
pnpp2<-pnpp1+theme(legend.position="none")
pnpp2<-pnpp2+annotation_custom(my_grobA)
pdf("C:/Users/psladmin/Documents/ModelOutput2/Defense_graphs/NPP.pdf", width=6,height=4, paper='special')
pnpp2
dev.off()


pne<-ggplot(nep, aes(x=FireReturnInterval, y=NEP, colour=FireReturnInterval)) + labs(x="Fire Return Interval",y="NEP (MgC/ha*yr)")+
geom_errorbar(aes(ymin=NEP-ci, ymax=NEP+ci), colour="black", width=.1, position=pd)
pnep<-pne+geom_line(position=pd) + geom_point(size=2, colour="black")+ geom_hline(yintercept = 0)
pnep1<-pnep + theme(axis.text = element_text(colour = "black", size = 12),
axis.title = element_text(size = 12, color = "black"))
pnep1<-pnep1 + theme_bw()
pnep2<-pnep1+theme(legend.position="none")
pnep2<-pnep2+annotation_custom(my_grobC)

pdf("C:/Users/psladmin/Documents/ModelOutput2/Defense_graphs/NEP.pdf", width=6,height=4, paper='special')
pnep2
dev.off()


pso<-ggplot(rh, aes(x=FireReturnInterval, y=SoilRespiration, colour=FireReturnInterval))  +
labs(x="Fire Return Interval",y="Heterotrophic respiration (MgC/ha*yr)")+
geom_errorbar(aes(ymin=SoilRespiration-ci, ymax=SoilRespiration+ci), colour="black", width=.1)
psoi<-pso+geom_line() + geom_point(size=2, colour="black") 
psoil<-psoi +theme(axis.text = element_text(colour = "black", size = 12),
axis.title = element_text(size = 12, color = "black"))
psoil1<-psoil+theme_bw()
psoil1<-psoil1+theme(legend.position="none")
psoil1<-psoil1+annotation_custom(my_grobB)

pdf("C:/Users/psladmin/Documents/ModelOutput2/Defense_graphs/Rh.pdf", width=6,height=4, paper='special')
psoil1
dev.off()


#pso<-ggplot(rh, aes(x=FireReturnInterval, y=SoilRespiration, group=Season))  +
#labs(x="Fire Return Interval",y="Heterotrophic respiration (Mg C ha-1 year-1)")+
#geom_errorbar(aes(ymin=SoilRespiration-se, ymax=SoilRespiration+se), colour="black", width=.1)
#psoi<-pso+geom_line(aes(linetype=Season),position=pd, size=0.5) +
#geom_point(size=2)+
#annotate("text", x=0.8, y=2.63, label= "1", size=4) +
#annotate("text", x = 0.8, y=2.81, label = "2", size=4)
#psoil<-psoi + theme_bw()
#psoil1<-psoil+theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
#psoil1<-psoil1+annotation_custom(my_grobC)

tiff("C:/Users/psladmin/Documents/ModelOutput2/Defense_graphs/NPP.tiff", units="in", width=7.5,height=10, res=400, compression="lzw")
b<-multiplot(pnpp2,pnep2,psoil1, cols=2, rows=2)
dev.off()

pdf("C:/Users/psladmin/Documents/ModelOutput2/Defense_graphs/NPPNEP.pdf", width=6,height=4,paper='special')
b<-multiplot(pnpp2,pnep2,psoil1, cols=2, rows=2)
dev.off()


black.12.text <- element_text(face="plain",color = "black", size =12)
black.bold.text <- element_text(face = "plain", color = "black", size=14)


##Ecosystem C stock
pd <- position_dodge(0.1)
#p<-ggplot(ecs, aes(x=FireReturnInterval, y=EcosystemCStock, group=Season)) + ylim(180, 260) +
p<-ggplot(ecs, aes(x=FireReturnInterval, y=EcosystemCStock)) + ylim(190, 290) +
labs(x="Fire Return Interval",y="Total ecosystem C Stock (MgC/ha)")+
geom_errorbar(aes(ymin=EcosystemCStock-ci, ymax=EcosystemCStock+ci), colour="black", width=.1, position=pd)
#p1<-p+geom_line(aes(linetype=Season),position=pd, size=0.5) +
p1<-p+geom_point(position=pd,size=2) 
#annotate("text", x=0.8, y=190, label= "1", size=4) +
#annotate("text", x = 0.8, y=200, label = "2", size=4)
p2<-p1+ theme_bw()
p2a<-p2+theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p3<-p2a+annotation_custom(my_grobA)
pdf("C:/Users/psladmin/Documents/ModelOutput2/Defense_graphs/Eco.pdf", width=6,height=4, paper='special')
p3
dev.off()


pd <- position_dodge(0.1)
p<-ggplot(bcs, aes(x=FireReturnInterval, y=BiomassLiveCStock)) + ylim(38,60) +
labs(x="Fire Return Interval",y="Live Biomass C Stock (MgC/ha)")+
geom_errorbar(aes(ymin=BiomassLiveCStock-ci, ymax=BiomassLiveCStock+ci), colour="black", width=.1, position=pd)
p1<-p+geom_point(position=pd,size=2) 
p6a<-p1 + theme_bw()
p6ab<-p6a+theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p6<-p6ab+annotation_custom(my_grobB)
pdf("C:/Users/psladmin/Documents/ModelOutput2/Defense_graphs/Bio.pdf", width=6,height=4, paper='special')
p6
dev.off()

pd <- position_dodge(0.1)
p<-ggplot(ol, aes(x=FireReturnInterval, y=Organic)) + ylim(38,54) +
labs(x="Fire Return Interval",y="Organic Layer C Stock (MgC/ha)")+
geom_errorbar(aes(ymin=Organic-ci, ymax=Organic+ci), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p7<-p7a+annotation_custom(my_grobC)
pdf("C:/Users/psladmin/Documents/ModelOutput2/Defense_graphs/Org.pdf", width=6,height=4, paper='special')
p7
dev.off()

#pd <- position_dodge(0.1)
#p<-ggplot(ecs, aes(x=FireReturnInterval, y=EcosystemCStock, group=Season)) + ylim(180, 260) +
#labs(x="Fire Return Interval",y="Total ecosystem C Stock (Mg C ha-1)")+
#geom_errorbar(aes(ymin=EcosystemCStock-se, ymax=EcosystemCStock+se), colour="black", width=.1, position=pd)
##p1<-p+geom_line(aes(linetype=Season),position=pd, size=0.5) +
#p1<-p+geom_point(position=pd,size=2) +
#annotate("text", x=0.8, y=190, label= "1", size=4) +
#annotate("text", x = 0.8, y=200, label = "2", size=4)
#p2<-p1+ theme_bw()
#p2a<-p2+theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
#p3<-p2a+annotation_custom(my_grobA)



##DOMC stock
#pd <- position_dodge(0.1)
#p4<-ggplot(scs, aes(x=FireReturnInterval, y=SoilCStock, group=Season)) + ylim(140, 200) +
#labs(x="Fire Return Interval",y="DOM C Stock (Mg C ha-1)")+
#geom_errorbar(aes(ymin=SoilCStock-se, ymax=SoilCStock+se), colour="black", width=.1, position=pd)
#p4a<-p4+geom_line(aes(linetype=Season),position=pd, size=0.5) +
#geom_point(position=pd,size=2) +
#annotate("text", x=0.8, y=147, label= "1", size=4) +
#annotate("text", x = 0.8, y=156, label = "2", size=4)
#p4ab<-p4a+theme_bw()
#p5<-p4ab + theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
#p5<-p5+annotation_custom(my_grobB)



#pd <- position_dodge(0.1)
#p<-ggplot(bcs, aes(x=FireReturnInterval, y=BiomassLiveCStock,  group=Season)) +ylim(40, 60) +
#labs(x="Fire Return Interval",y="Live Biomass C Stock (Mg C ha-1)")+
#geom_errorbar(aes(ymin=BiomassLiveCStock-se, ymax=BiomassLiveCStock+se), colour="black", width=.1, position=pd)
#p1<-p+geom_line(aes(linetype=Season),position=pd, size=0.5) +
#geom_point(position=pd,size=2) +
#annotate("text", x=0.8, y=42, label= "1", size=4) +
#annotate("text", x = 0.8, y=46, label = "2", size=4)
#p6a<-p1 + theme_bw()
#p6ab<-p6a+theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
#p6<-p6ab+annotation_custom(my_grobC)

##organic layer stock
#pd <- position_dodge(0.1)
#p<-ggplot(ol, aes(x=FireReturnInterval, y=Organic, group=Season)) + ylim(30,50) +
#labs(x="Fire Return Interval",y="Organic Layer C Stock (Mg C ha-1)")+
#geom_errorbar(aes(ymin=Organic-se, ymax=Organic+se), colour="black", width=.1)
#p1<-p+geom_line(aes(linetype=Season),position=pd, size=0.5) +
#geom_point(size=2) +
#annotate("text", x=0.8, y=36, label= "1", size=4) +
#annotate("text", x = 0.8, y=39, label = "2", size=4)
#p2<-p1+theme_bw()
#p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
#p7<-p7a+annotation_custom(my_grobD)


##Woody debris stock
pd <- position_dodge(0.1)
p<-ggplot(wd, aes(x=FireReturnInterval, y=WoodyDebris))+ ylim(30,53)  +
labs(x="Fire Return Interval",y="Woody debris C Stock (MgC/ha)")+
geom_errorbar(aes(ymin=WoodyDebris-ci, ymax=WoodyDebris+ci), colour="black", width=.1, position=pd)
p1<-p+geom_point(position=pd,size=2) 
p2<-p1 + theme_bw()
p9a<-p2+theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p9<-p9a+annotation_custom(my_grobD)
pdf("C:/Users/psladmin/Documents/ModelOutput2/Defense_graphs/Wd.pdf", width=6,height=4, paper='special')
p9
dev.off()




##Mineral stock
pd <- position_dodge(0.1)
p<-ggplot(mscs, aes(x=FireReturnInterval, y=Mineral,  group=Season)) + ylim(84, 122) +
labs(x="Fire Return Interval",y="Mineral Soil C Stock (MgC/ha)")+
geom_errorbar(aes(ymin=Mineral-ci, ymax=Mineral+ci), colour="black", width=.1, position=pd)
p1<-p+geom_point(aes(shape=Season),position=pd,size=2) +
scale_shape_manual(values=c(15, 19))+
annotate("text", x=0.8, y=89, label= "1", size=4) +
annotate("text", x = 0.8, y=85, label = "2", size=4)
p2<-p1 + theme_bw()
p8a<-p2+theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p8<-p8a+annotation_custom(my_grobE)
pdf("C:/Users/psladmin/Documents/ModelOutput2/Defense_graphs/Minr.pdf", width=6,height=4, paper='special')
p8
dev.off()


###Woody debris stock
#pd <- position_dodge(0.1)
#p<-ggplot(wd, aes(x=FireReturnInterval, y=WoodyDebris, group=Season))+ ylim(32,45)  +
#labs(x="Fire Return Interval",y="Woody debris C Stock (Mg C ha-1)")+
#geom_errorbar(aes(ymin=WoodyDebris-se, ymax=WoodyDebris+se), colour="black", width=.1, position=pd)
#p1<-p+geom_line(aes(linetype=Season),position=pd, size=0.5) +
#geom_point(position=pd,size=2) +
#annotate("text", x=0.8, y=34.7, label= "1", size=4) +
#annotate("text", x = 0.8, y=35.95, label = "2", size=4)
#p2<-p1 + theme_bw()
#p9a<-p2+theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
#p9<-p9a+annotation_custom(my_grobF)


pd <- position_dodge(0.1)
p4<-ggplot(scs, aes(x=FireReturnInterval, y=SoilCStock, group=Season)) + ylim(156, 230) +
labs(x="Fire Return Interval",y="DOM C Stock (MgC/ha)")+
geom_errorbar(aes(ymin=SoilCStock-ci, ymax=SoilCStock+ci), colour="black", width=.1, position=pd)
p4a<-p4 +geom_point(aes(shape=Season),position=pd,size=2) +
scale_shape_manual(values=c(15, 19))+
annotate("text", x=0.8, y=164, label= "1", size=4) +
annotate("text", x = 0.8, y=158, label = "2", size=4)
p4ab<-p4a+theme_bw()
p5<-p4ab + theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p5<-p5+annotation_custom(my_grobF)
pdf("C:/Users/psladmin/Documents/ModelOutput2/Defense_graphs/DOM.pdf", width=6,height=4, paper='special')
p5
dev.off()



a<-multiplot(p3,p9,p6,p8,p7,p5, cols=3)

######post-hoc
library(lsmeans)
lsmeans(mod.ecs, pairwise~FireReturnInterval*Season, adjust="tukey")


#####get % differences
library(quantmod)
ecs<-tapply(All$EcosystemCStock, list(FireReturnInterval=All$FireReturnInterval,
Season=All$Season), mean, na.rm=TRUE)
v1<-as.vector(ecs[1:6,2])
ec<-tapply(All[,27],All$FireReturnInterval,mean)
60<-rbind(ecs
150<-rbind(



#######################################################################################

pd <- position_dodge(0.1)
p4<-ggplot(scs, aes(x=FireReturnInterval, y=SoilCStock, group=Season)) + ylim(148, 210) +
labs(x="Fire Return Interval",y="DOM C Stock (Mg C ha-1)")+
geom_errorbar(aes(ymin=SoilCStock-se, ymax=SoilCStock+se), colour="black", width=.1, position=pd)
p4a<-p4 +geom_point(position=pd,size=2) +
annotate("text", x=0.8, y=153, label= "1", size=4) +
annotate("text", x = 0.8, y=150, label = "2", size=4)
p4ab<-p4a+theme_bw()
p5<-p4ab + theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p5<-p5+annotation_custom(my_grobF)




####################graph all tracked c stocks
npp <- summarySE(All, measurevar="NPP", groupvars=c("FireReturnInterval"))
nep <- summarySE(All, measurevar="NEP", groupvars=c("FireReturnInterval"))
rh<- summarySE(All, measurevar="SoilRespiration", groupvars=c("FireReturnInterval"))
#ecs <- summarySE(All, measurevar="EcosystemCStock", groupvars=c("FireReturnInterval","Season#"))
ecs <- summarySE(All, measurevar="EcosystemCStock", groupvars=c("FireReturnInterval"))
scs<- summarySE(All, measurevar="SoilCStock", groupvars=c("FireReturnInterval"))
bcs<-summarySE(All, measurevar="BiomassLiveCStock", groupvars=c("FireReturnInterval"))
bcs<-summarySE(All, measurevar="Snags", groupvars=c("FireReturnInterval"))

sn<-summarySE(All, measurevar="Snags", groupvars=c("FireReturnInterval"))
sb<-summarySE(All, measurevar="SnagBranch", groupvars=c("FireReturnInterval"))
am<-summarySE(All, measurevar="AGMedium", groupvars=c("FireReturnInterval"))
af<-summarySE(All, measurevar="AGfast", groupvars=c("FireReturnInterval"))
avf<-summarySE(All, measurevar="AGveryfast", groupvars=c("FireReturnInterval"))
as<-summarySE(All, measurevar="AGslow", groupvars=c("FireReturnInterval"))
bgvf<-summarySE(All, measurevar="BGveryfast", groupvars=c("FireReturnInterval"))
bgf<-summarySE(All, measurevar="BGfast", groupvars=c("FireReturnInterval"))
bgs<-summarySE(All, measurevar="BGslow", groupvars=c("FireReturnInterval"))



my_grobA = grobTree(textGrob("A)", x=0.01,  y=0.95, hjust=0,
  gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobB = grobTree(textGrob("B)", x=0.01,  y=0.95, hjust=0,
  gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobC = grobTree(textGrob("C)", x=0.01,  y=0.95, hjust=0,
  gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobD = grobTree(textGrob("D)", x=0.01,  y=0.95, hjust=0,
  gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobE = grobTree(textGrob("E)", x=0.01,  y=0.95, hjust=0,
  gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobF = grobTree(textGrob("F)", x=0.01,  y=0.95, hjust=0,
  gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobG = grobTree(textGrob("G)", x=0.01,  y=0.95, hjust=0,
gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobH = grobTree(textGrob("H)", x=0.01,  y=0.95, hjust=0,
  gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobI = grobTree(textGrob("I)", x=0.01,  y=0.95, hjust=0,
gp=gpar(col="black", fontsize=12, fontface="bold")))

pd <- position_dodge(0.1)
p<-ggplot(sn, aes(x=FireReturnInterval, y=Snags)) + ylim(3.5,7.2) +
labs(x="Fire Return Interval",y="Snags (MgC/ha)")+
geom_errorbar(aes(ymin=Snags-ci, ymax=Snags+ci), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p7<-p7a+annotation_custom(my_grobA)


pd <- position_dodge(0.1)
p<-ggplot(sb, aes(x=FireReturnInterval, y=SnagBranch)) + ylim(1.5,2.2) +
labs(x="Fire Return Interval",y="SnagBranch (MgC/ha)")+
geom_errorbar(aes(ymin=SnagBranch-ci, ymax=SnagBranch+ci), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p8<-p7a+annotation_custom(my_grobB)


pd <- position_dodge(0.1)
p<-ggplot(am, aes(x=FireReturnInterval, y=AGMedium)) + ylim(16,33) +
labs(x="Fire Return Interval",y="AG Medium (MgC/ha)")+
geom_errorbar(aes(ymin=AGMedium-se, ymax=AGMedium+se), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p9<-p7a+annotation_custom(my_grobC)

pd <- position_dodge(0.1)
p<-ggplot(af, aes(x=FireReturnInterval, y=AGfast)) + ylim(8,12) +
labs(x="Fire Return Interval",y="AG fast (MgC/ha)")+
geom_errorbar(aes(ymin=AGfast-se, ymax=AGfast+se), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p10<-p7a+annotation_custom(my_grobD)


pd <- position_dodge(0.1)
p<-ggplot(avf, aes(x=FireReturnInterval, y=AGveryfast)) + ylim(7,11) +
labs(x="Fire Return Interval",y="AG very fast (MgC/ha)")+
geom_errorbar(aes(ymin=AGveryfast-se, ymax=AGveryfast+se), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p11<-p7a+annotation_custom(my_grobE)


pd <- position_dodge(0.1)
p<-ggplot(as, aes(x=FireReturnInterval, y=AGslow)) + ylim(30,43) +
labs(x="Fire Return Interval",y="AG slow (MgC/ha)")+
geom_errorbar(aes(ymin=AGslow-se, ymax=AGslow+se), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p12<-p7a+annotation_custom(my_grobF)


pd <- position_dodge(0.1)
p<-ggplot(bgvf, aes(x=FireReturnInterval, y=BGveryfast)) + ylim(1,2) +
labs(x="Fire Return Interval",y="BG very fast (MgC/ha)")+
geom_errorbar(aes(ymin=BGveryfast-se, ymax=BGveryfast+se), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p13<-p7a+annotation_custom(my_grobG)


pd <- position_dodge(0.1)
p<-ggplot(bgf, aes(x=FireReturnInterval, y=BGfast)) + ylim(2,5) +
labs(x="Fire Return Interval",y="BG fast (MgC/ha)")+
geom_errorbar(aes(ymin=BGfast-se, ymax=BGfast+se), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p14<-p7a+annotation_custom(my_grobH)



pd <- position_dodge(0.1)
p<-ggplot(bgs, aes(x=FireReturnInterval, y=BGslow)) + ylim(65,120) +
labs(x="Fire Return Interval",y="BG slow (MgC/ha)")+
geom_errorbar(aes(ymin=BGslow-se, ymax=BGslow+se), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p15<-p7a+annotation_custom(my_grobI)



nnn<-multiplot(p7,p10,p13,p8,p11,p14,p9,p12,p15,cols=3)



####Perform linear interpolarion