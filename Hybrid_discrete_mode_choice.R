My_Hybrid <- read.csv("dataset.csv",head=TRUE)

colnames(My_Hybrid)
table (My_Hybrid$CHOICE)
nrow(My_Hybrid)

# Latent attitudes
############################################################
# q1:Pro-car
# (+): support Pro_car
# (-): against Pro_car
##indicators:
# q1a: It is hard to take PT when I travel with my children.(+)
# q1b: I do not like to change mode when I travel.(+)
# q1c: Driving a car is a cool way to travel.(+) 
# q1d: I do not feel more independent when I drive a car.(-)


# q2: Environmental concern
# (+): support Environmental concern
# (-): against Environmental concern
##indicators:
# q2a: I am concerned about global warming.(+)
# q2b: We should increase the price of gasoline to reduce congestion and air pollution.(+)
# q2c: We must act and take decisions to limit emissions of greenhouse gases.(+)
# q2d: The environmental impacts of using private car is not very important.(-) 

# Strongly disagree(1)
# Disagree(2)	
# Neither disagree nor agree(3)	
# Agree(4)	
# Strongly agree(5)

##############################################################################
# CHOICE == 1 <- "TRAIN"
# CHOICE == 2 <- "SM"
# CHOICE == 3 <- "CAR"
##############################################################################

LV.1 <- My_Hybrid[c("q1a","q1b","q1c","q1d","q2a","q2b","q2c","q2d")]
colnames(LV.1)

# Cronbach's alpha reliability test
require(psy)
cronbach(LV.1)

# Kaiser-Meyer-Olkin (KMO) Test
require (psych)
KMO(LV.1)

# correlation matrix with p-value
library(psych)
library(Hmisc)
pairs(LV.1,panel=panel.smooth)
mycor = rcorr(as.matrix(LV.1), type="pearson")
head(mycor)

# Bartlett's test for sphericity 
cortest.bartlett(cor(LV.1), n = nrow(LV.1))

## I do not need to do EFA for this study
## EFA 
#require (psych)
#fa.parallel(cor(LV.1), n.obs=nrow(LV.1), fa="both", n.iter=100)
# fm="pa" will do the principal factor solution, jasper used this method
#fa(cor(LV.1), nfactors=2, n.obs = nrow(LV.1), rotate="varimax",fm="ml")
#fit.2 <- factanal(LV.1,factors=2,rotation="varimax",fm="pa")
#print(fit.2) 

########################################################################
# Add ID to the LV.1
#ADD_ID <- data.frame(new_data["ID"],LV.1)
#My_Hybrid <- merge(Hybrid_dummy,ADD_ID,by="ID" )
#colnames(My_Hybrid)
#nrow(My_Hybrid)


### only measurment model
require (lavaan)
SEM_NML <- "
Pro_Car =~ q1a+q1b+q1c+q1d
Env_Con =~ q2a+q2b+q2c+q2d
Pro_Car~~0*Env_Con
"

ft_NML <-sem(SEM_NML, data=My_Hybrid)
summary(ft_NML, standardized=T,fit.measures=T)
fitMeasures(ft_NML, c("pvalue", "cfi", "rmsea", "srmr"))
inspect(ft_NML,"cov.lv")

# install.packages("semPlot")
require(semPlot) 
semPaths(ft_NML,"std")

parameterEstimates(ft_NML)
#########################################################################
#install.packages("mlogit")
library("mlogit")
Hyb <- mlogit.data(My_Hybrid, shape = "wide", choice = "CHOICE", varying = 10:15, 
                   sep = ".",alt.levels = c("CAR","SM","TRAIN"), id.var = "ID")
nrow(Hyb)
colnames(Hyb)
which(colnames(Hyb)=="AV.TRAIN")
which(colnames(Hyb)=="AV.SM")
which(colnames(Hyb)=="AV.CAR")
which(colnames(Hyb)=="alt")


# varying choice set 
Hyb <- Hyb[!(Hyb[,7] == 0 & Hyb[,33]=="TRAIN") ,]
Hyb <- Hyb[!(Hyb[,8] == 0 & Hyb[,33]=="SM") ,]
Hyb <- Hyb[!(Hyb[,9] == 0 & Hyb[,33]=="CAR") ,]
nrow(Hyb)
## 1: Commuter, 2: Shopping, 3: Business, 4: Leisure , 5: other
## 1: age<= 24, 2: 24<age<=39, 3: 39<age<=54, 4: 54<age<=65, 5: 65<age
ML_model <- mlogit(CHOICE ~ TT + CO|GENDER+Pur2_D1+Pur3_D2+Pur4_D3+Pur5_D4+Age2_D5+Age3_D6+Age4_D7+Age5_D8, reflevel="SM", Hyb)
summary(ML_model)
colnames(My_Hybrid)
Hyb_model <- mlogit(CHOICE ~ TT + CO|Pro_Car+Env_Con+GENDER+Pur2_D1+Pur3_D2+Pur4_D3+Pur5_D4+Age2_D5+Age3_D6+Age4_D7+Age5_D8, reflevel="CAR", Hyb)
summary(Hyb_model)

#install.packages("stargazer")
library(stargazer)
stargazer(ML_model, Hyb_model,type="text", title="Traditional MNL vs. ICLV MNL model",
          column.labels=c("MNL","ICLV MNL"), report=("vct*") , align=TRUE)
lrtest(ML_model,Hyb_model)
###############################################################################
# nested model
nest_ML_model <- mlogit(CHOICE ~ TT+CO|GENDER+Pur2_D1+Pur3_D2+Pur4_D3+Pur5_D4+Age2_D5+Age3_D6+Age4_D7+Age5_D8, reflevel="CAR", Hyb, nests = list(CAR = "CAR", Public = c("SM", "TRAIN")), un.nest.el = TRUE)
summary(nest_ML_model)

nest_Hyb_model <- mlogit(CHOICE ~ TT+CO|Pro_Car+Env_Con+GENDER+Pur2_D1+Pur3_D2+Pur4_D3+Pur5_D4+Age2_D5+Age3_D6+Age4_D7+Age5_D8,reflevel="CAR", Hyb, nests = list(CAR = "CAR", Public = c("SM", "TRAIN")), un.nest.el = TRUE)
summary(nest_Hyb_model)

lrtest(Hyb_model,nest_Hyb_model)
##################################################################################
## mixed logit model
mixed.1 <- mlogit(CHOICE ~ TT+CO|GENDER+Pur2_D1+Pur3_D2+Pur4_D3+Pur5_D4+Age2_D5+Age3_D6+Age4_D7+Age5_D8 , reflevel="SM", rpar= c("CO"="n", "TT"="n"),R= 500, halton=NA, Hyb)
summary(mixed.1)

mixed.2 <- mlogit(CHOICE ~ TT+CO|Pro_Car+Env_Con+GENDER+Pur2_D1+Pur3_D2+Pur4_D3+Pur5_D4+Age2_D5+Age3_D6+Age4_D7+Age5_D8 , reflevel="SM", rpar= c("CO"="n", "TT"="n","CAR:(intercept)"="n" , "TRAIN:(intercept)" ="n" ),R= 500, halton=NA, Hyb)
summary(mixed.2)

