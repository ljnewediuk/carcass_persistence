#LOAD DATA
library(readxl)
Frogs <- read_excel("School/Research/Manuscripts/4. Students/Carcass Persistance-Balsdon BSc/recarcasspersistence/Frogs.xlsx")
View(Frogs)

#LOAD PACKAGES

library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn)

#VARIABLES
persist=(Frogs$Days)
MeanDTV = (Frogs$MDTV)
svl=(Frogs$SVL)
species=as.factor(Frogs$Species)
cf=(Frogs$CF)
Totalrain=(Frogs$Precipitation)
rain = Totalrain/persist
temp=(Frogs$Temperature)


#Determine optimal random-effect structure
mixed_model_IntOnly <- lmer(persist ~ rain +traffic+svl+cf+temp + (1|species) , REML = TRUE, 
                            data = Frogs)
mixed_model_IntSlope <- lmer(persist ~ rain  +traffic+svl+cf+temp+ (1 + rain|species), REML = TRUE, 
                             data = Frogs)

AICc(mixed_model_IntOnly, mixed_model_IntSlope)
#  AIC mixed_model_IntOnly = 312.33
#  AIC mixed_model_IntSlope = 310.93
    #AIC mixed_model_IntOnly is higher, thus structure best with 
        #random intercept and slope for each species


#Determine optimal fixed effect structure
mixed_model_IntSlope_FULL <- lmer(persist ~ rain+traffic+svl+cf+temp+ (1 + rain|species), REML = FALSE, 
                             data = Frogs)
mixed_model_IntSlope_FULL <- lmer(persist ~ rain+traffic+svl+cf+temp+ (1 + rain|species), REML = FALSE, 
                                  data = Frogs)



###WITHOUT RANDOM EFFECTS

globalcarcass <- lm(persist ~ rain+MeanDTV+svl+cf+temp+species, data = Frogs)
AIC(globalcarcass)

options(na.action = "na.fail") # Required for dredge to run

Gmodel_dredge <- dredge(globalcarcass, beta = F, evaluate = T, rank = AICc)

options(na.action = "na.omit") # set back to default

head(Gmodel_dredge)
nrow(Gmodel_dredge)#double check number of models = 64


top_model <- get.models(Gmodel_dredge, subset = 1)[[1]]
top_model
summary(top_model)
importance(top_model)
#Model average
summary(model.avg(Gmodel_dredge, subset = delta <= 2))
      #Full model-averaged coefficients
              #Estimate Std. Error Adjusted SE z value Pr(>|z|)    
#(Intercept) 21.7281490  4.6351592   4.6974915   4.625 3.74e-06 ***
#cfH          1.6158286  1.0069931   1.0211944   1.582 0.113583    
#MeanDTV     -0.0014801  0.0008322   0.0008440   1.754 0.079466 .  
#rain        -0.3012819  0.0898638   0.0910966   3.307 0.000942 ***
#temp        -0.6157807  0.2077670   0.2106103   2.924 0.003458 ** 


    #TAKE AWAY: increased rain and temp decrease persistence
      #  traffic is margninally, but not sig.
    

traf<-lm(persist~MeanDTV, data=Frogs)
summary(traf)
plot(persist, MeanDTV)


##Frequncy distribution

library(ggplot2)
freqdist <- ggplot(Frogs, aes(persist, colour=species))+stat_ecdf(geom="step")+labs(y="Cumulative frequency", x="Carcass Persistence (days)")
freqdist

freqdistall <-ggplot(Frogs, aes(persist))+stat_ecdf(geom="step")+labs(y="Cumulative frequency", x="Carcass persistence (days)")+geom_vline(xintercept = 13, size = 1, colour = "#666666",
            linetype = "dashed")+geom_vline(xintercept = 7, size = 1, colour = "#666666", linetype = "dotted")+
  theme_classic()+geom_text(x=8.5, y=0.05, label="78.0%")+
  geom_text(x=14.5, y=0.05, label="95.6%")+geom_text(x=2.5, y=0.05, label = "14.3%")+
  geom_vline(xintercept = 1, size = 1, colour = "#666666", linetype = "twodash")
freqdistall

#Simple GLMs to know r2 value for relationship of persist and each var

#setup species to have NLFR as reference cat
species<- relevel(species, 'NLFR')

persistlm<- glm(persist~temp+rain+MeanDTV+species+cf+svl, family = poisson(link="log"), data=Frogs)
summary(persistlm)
sigma(persistlm)/mean(Frogs$Days)
confint(persistlm)
aov(persistlm)
contrasts(species)

templm <- glm(persist~temp)
summary(templm)$r.squared #0.0558
length(coefficients(templm))
RSS <- c(crossprod(templm$residuals))
MSE <- RSS / length(templm$residuals)

traflm <- glm(persist~MeanDTV)
summary(traflm)$r.squared  #0.0269
length(coefficients(traflm))
RSS <- c(crossprod(traflm$residuals))
MSE <- RSS / length(traflm$residuals)


rainlm <- glm(persist~rain)
summary(rainlm)$r.squared  #0.0568
length(coefficients(rainlm))
RSS <- c(crossprod(rainlm$residuals))
MSE <- RSS / length(rainlm$residuals)


###Plot varibales against persist
library(ggplot2)
Tr1<-ggplot(Frogs, aes(x=persist,y=MeanDTV ))+geom_point()+theme_classic()+
  labs(y="Traffic volume (vehicles/hour)", x = "Carcass Persistence (days)")+
  geom_smooth(method="glm")+geom_text(x=20, y=3500, label= "R2 = 0.03")
Tr1
Tr2<-ggplot(Frogs, aes(x=persist,y=MeanDTV, colour=species))+geom_point()+theme_classic()+
  labs(y="Mean number of vehicles per hour", x = "Carcass Persistence (days)")+
  geom_smooth(method="gam")
Tr2#species separated out

Te1<-ggplot(Frogs, aes(x=persist,y=temp))+geom_point()+theme_classic()+
  labs(y="Mean daily temperature (oC)", x = "Carcass Persistence (days)")+
  geom_smooth(method="glm")+geom_text(x = 20, y=23, label = "R2 = 0.06")
Te1

Pr1<-ggplot(Frogs, aes(x=persist,y=rain))+geom_point()+theme_classic()+
  labs(y="Mean daily precipitation (mm)", x = "Carcass Persistence (days)")+
  geom_smooth(method="glm")+geom_text(x=20, y=19, label="R2 = 0.06")
Pr1


Pr2<-ggplot(Frogs, aes(x=persist,y=rain))+geom_point()+theme_classic()+
  labs(y="Mean daily precipitation (mm)", x = "Carcass Persistence (days)")+
  stat_smooth()+geom_text(x=20, y=19, label="R2 = 0.06")
Pr2


###Historgram of days - is persistence bimodal?
persisthisto <- ggplot(Frogs, aes(x=persist))+geom_histogram(colour="black", binwidth = 1)+
  theme_gray()+labs(x = "Carcass Persistence (days)", y = "Count" )+
  geom_vline(data=Frogs, aes(xintercept=mean(Frogs$Days)), colour="black", linetype="dashed", size = 1)+
  geom_text(x=12, y=12, label = "Mean persistance = 5.5 days")
persisthisto

####Bootstrapping removal probabilities################

library(readxl)
Frogs <- read_excel("School/Research/Manuscripts/4. Students/Carcass Persistance-Balsdon BSc/recarcasspersistence/Frogs.xlsx")
View(Frogs)

persist=(Frogs$Days)
MeanDTV = (Frogs$MDTV)
svl=(Frogs$SVL)
species=as.factor(Frogs$Species)
cf=(Frogs$CF)
Totalrain=(Frogs$Precipitation)
rain = Totalrain/persist
temp=(Frogs$Temperature)


persistanova <- aov(Frogs$Moose~year, abundance)
summary(mooseanova) #F = 4.95


k=10000
mysamples = t(replicate(k,sample(days)))
dim(mysamples)
mymeans=apply(mysamples, 2, mean)
mysds=apply(mysamples, 2, sd)
hist(mymeans)
quantile(mymeans, c(0.025, 0.975)) #2.5%=5.34, 97.5%=5.52 DAYS - MEAN CARCASS PERSISTENCE WITH CIs
df<-c(mymeans, mysamples)
mean(mymeans)
mean(mysds)


df <- data.frame(
  mm=(mymeans))

meanpersistplot <- ggplot(df, aes(x=mm))+geom_histogram(colour="black", binwidth = 0.01)+
  theme_bw()+labs(x = "Mean Carcass Persistance (days)", y = "Count" )+
  geom_vline(data=df, aes(xintercept=mean(mm)), colour="black", linetype="dashed", size = 1)+
  geom_text(x=12, y=12, label = "Mean persistance = 5.5 days")


#######################trying stuff

trafrain<-glm(persist~MeanDTV, data=Frogs)
AIC(trafrain)