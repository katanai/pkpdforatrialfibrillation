install.packages("nlmeODE") 
install.packages("deSolve") 
install.packages("nlme") 
install.packages("lattice") 
library(PKPDmodels) 
library(nlmeODE)
 
library(lattice)
library(nlme)
library(deSolve)
??nlmeODE
##MIXED-EFFECTS MODELING USING DE. data(Theoph)

head(Theoph)
##make sure you change read.csv to your own FILE PATH
##WarData<- read.csv("___") 
##head(WarData)

WarODE <- WarData WarODE$Dose[WarODE$INR!=0] <- 0 WarODE$Cmt <- rep(1, dim(WarODE)[1]) 

#Pharmacokinetics
#One-compartment Model ####################################### OneComp <- list(
DiffEq =list(
dy1dt = ~ -ka*y1,
dy2dt = ~ ka*y1 - ke*y2), ObsEq = list(

c1=~0,
c2 = ~ y2/CL*ke),
Parms = c("ka", "ke", "CL"), States = c("y1", "y2"),
Init = list(0,0)
)
#Using Library NlmODE
War1Model <- nlmeODE(OneComp, WarODE) #################################
#PK : 2-compartment model #################################
TwoComp <- list(DiffEq = list(dy1dt = ~ -(k12 +k10)*y1+k21*y2 ,
dy2dt = ~ -k21*y2 + k12*y1), ObsEq = list(
c1=~y1,
c2=~0),
States = c("y1", "y2"),
Parms = c("k12", "k21", "k10", "start"), Init = list("start", 0))
War2Model <- nlmeODE(TwoComp, WarData)
############################

## PK MODEL VISUALIZATION ## ############################
#drug dosing is usually adapted to patient's weight.
#The blood concentration depends on the liberation, absorption, #distribution, metabolism and the excretion of the drug. install.packages("PK")
##no time/concentration though! ########################################################### ##ABSORPTION MODEL WITH ESTIMATION of TIME/RATE of INFUSION ###########################################################
OneCompAbs <- list(DiffEq = list(dA1dt = ~ -ka*A1, dA2dt = ~ ka*A1- CL/V1*A2),
ObsEq = list( SC = ~ 0,
C = ~ A2/V1),
States =c("A1", "A2"),
Parms =c("ka", "CL", "V1", "F1"), Init = list(0,0))
ID <- rep(seq(1:18), each = 11)
Time <- rep(seq(0,100, by = 10), 18)
Dose <- c(rep(c(100,0,0,100,0,0,0,0,0,0,0),6),rep(c(100,0,0,0,0,0,0,100,0,0,0),6),
rep(c(100,0,0,0,0,0,0,0,0,0,0),6))

Rate <- c(rep(rep(0,11),6),rep(c(5,rep(0,10)),6),rep(rep(0,11),6)) #Cmt=?
Cmt <-c(rep(1,6*11),rep(c(2,0,0,0,0,0,0,1,0,0,0),6),rep(2,6*11)) #Concentration
Conc <- rep(0,18*11)
Data <- as.data.frame(list(ID=ID,Time=Time,Dose=Dose,Rate=Rate,Cmt=Cmt,Conc=Conc))
#SimData <- groupedData(Conc ~ Time | ID, data = WarData, labels = list(x = "Time", y = "Concentration"))
#head(SimData)
OneCompAbsModel <- nlmeODE(OneCompAbs, WarData) #kaSim <- rep(log(rep(0.05, 18)) + 0.3*rnorm(18), each = 11) #CLSim <- rep(log(rep(0.5, 18)) + 0.2*rnorm(18), each = 11) #V1Sim <- rep(log(rep(10,18)) + 0.1*rnorm(18), each = 11) #F1Sim <- rep(log(0.8), 18*11)
#SimData$Sim <- OneCompAbsModel(kaSim, CLSim, V1Sim, F1Sim, SimData$Time, SimData$ID) #SimData$Conc <- SimData$Sim + 0.3*rnorm(dim(SimData)[1])
#Data <- groupedData( Conc ~ Time | ID, data = SimData, labels = list(x = "Time" , y = "Concentration"))
plot (WarData, aspect = 1/1) ################################ ##Estimation of Model Parameters ################################

#OneCompAbsModel <- nlmeODE(OneCompAbs, WarData) ####################################################### ## Simulation and Simultaneous Estimation of PK/PD Data #######################################################
PoolModel <- list( DiffEq=list(
dy1dt = ~ -ke*y1,
dy2dt = ~ krel * (1-Emax*(y1/Vd)**gamma/(EC50**gamma+(y1/Vd)**gamma)) * y3 - kout * y2, dy3dt = ~ Kin - krel * (1-Emax*(y1/Vd)**gamma/(EC50**gamma+(y1/Vd)**gamma))*y3),
ObsEq=list(
PK =~y1/Vd, PD =~y2, Pool=~0),
States=c("y1","y2","y3"), Parms=c("ke","Vd","Kin","kout","krel","Emax","EC50","gamma"), Init=list(0,"Kin/kout","Kin/krel"))
#ID <- rep(seq(1:12),each=2*12)
#Time <- rep(rep(c(0,0.25,0.5,0.75,1,2,4,6,8,10,12,24),each=2),12)
#Dose <- rep(c(100,rep(0,23)),12)
#Cmt <-rep(rep(c(1,2),12),12)
#Type <- rep(rep(c(1,2),12),12)
#Conc <- rep(0,2*12*12)
#Data <- as.data.frame(list(ID=ID,Time=Time,Dose=Dose,Cmt=Cmt,Type=Type,Conc=Conc))

#SimData <- groupedData( Conc ~ Time | ID/Type, # data = WarData,
# labels = list( x = "INR", y = "Dose")) PKPDpoolModel <- nlmeODE(PoolModel,WarData)
#keSim <- rep(log(rep(0.05,12))+0.1*rnorm(12),each=2*12) #VdSim <- rep(log(rep(10,12))+0.01*rnorm(12),each=2*12) #EC50Sim <- rep(log(rep(5,12))+0.1*rnorm(12),each=2*12) #KinSim <-rep(log(5),2*12*12)
#koutSim <-rep(log(0.5),2*12*12) #krelSim <-rep(log(2),2*12*12) #EmaxSim <- rep(log(1),2*12*12) #gammaSim <- rep(log(3),2*12*12)
#SimData$Sim <- PKPDpoolModel(keSim,VdSim,KinSim,koutSim,krelSim,EmaxSim,EC50Sim, # gammaSim,SimData$Time,SimData$ID,SimData$Type) #SimData$Conc[SimData$Type==1] <- SimData$Sim[SimData$Type==1]*(1 +
# 0.1*rnorm(length(SimData[SimData$Type==1,1]))) #SimData$Conc[SimData$Type==2] <- SimData$Sim[SimData$Type==2]*(1 +
# 0.05*rnorm(length(SimData[SimData$Type==2,1])))
#Data <- groupedData( Conc ~ Time | ID/Type,
# data = SimData,
# labels = list( x = "Time", y = "Concentration")) plot(WarData,display=1,aspect=1/1)

#Fixed parameters
Data$Emax <- rep(log(1),dim(Data)[1])
#Estimation of model parameters
PKPDpoolModel <- nlmeODE(PoolModel,Data) 

######################################## 
########################################
#MIXED-EFFECTS MODELLING
library(nlme)
summary(WarData)
str(WarData)
#Wt,Age, Target_INR, Dose
plot(WarData)
#Model1 = LM
model1 <- lm(Target_INR ~ Dose , data = WarData)
summary(model1)
plot(model1)
coef(model1)
#Model2 = Mixed-Effects
model2= lme(Target_INR~ Dose, data = WarData, random = ~1 | VKORC1.1173./CYP2C9) summary(model2)
coef(model2)
plot(ranef(model2))
plot(model2)
m2 <- aov(Target_INR~ Dose, data = WarData)

summary(m2)
