# import data here

#data pre-processing (technically correct data, consistent data) 

install.packages("dplyr")
library(dplyr)
str(data1)
summary(data1)
glimpse(data1)
dim(data1)
#ANS: 5700 15
names(data1)
#Baysian network
install.packages("bnlearn")
library(bnlearn)
#remove index -> integer type
bnvars <- c ("Race", "Gender", "Height", "W eight", "CYP2C9", "VKORC1", "VKORC1.1173.", "VKORC1..1639.", "DVT", "Smoker", "Age", "Target_INR", "DOSE_AND", "Amiodarone")
bndata <- data1[bnvars] bn_df <- data.frame(bndata) res <- hc(bn_df) plot(res)
#Training
fittedbn <- bn.fit (res, data = bn_df)
print(fittedbn$Age)
print(fittedbn)
#some other variable such as other genotypes
#Infering
head(fittedbn)
#cpquery(fittedbn, event = (Age == "<55.0"), evidence = (Target_INR == "<2.5")) #Evaluating
######
#Subset Data to NUM vars: AGE, TARGET_INR, DOSE_AND, INDEX
myvars <- c("Age", "Target_INR", "VKORC1..1639.", "CYP2C9" ) numdata <- data1[myvars] head(numdata)
#Age, CYP2C9, VKORC1 most important variables names(data1) vars <- c("VKORC1..1639.", "CYP2C9", "Age", "Target_INR")
subdata1 <- data1[vars] head(subdata1) #Target INR: 2.5
#Let's do bayesian network for these important variables
bndata1 <- data1[vars]
bn_df1 <- data.frame(bndata1) res1 <- hc(bn_df1) plot(res1)
#Training
fittedbn1 <- bn.fit(res1, data = bn_df1) print(fittedbn1)
################################## #Synthetic Data out of the observed
install.packages("synthpop") library(synthpop)
#reproduce the data1
my.seed <- 123456780
myavatars <- syn(data1, seed = my.seed) names(myavatars)
str(myavatars) print(myavatars) myavatars$syn

#lets compare original vs synthesised data
compare(myavatars, data1, vars = c("Age", "Smoker", "Race")) compare(myavatars,data1)
#export synth myavatars dataset write.csv(myavatars$syn, "myavatars1.csv") source("anticoagulation_therapy_simulator.R")
