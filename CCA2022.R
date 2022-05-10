library(haven)
library(tidyverse)
library(dplyr)
dat <- read_sav(file = file.choose())

####################
#Data Transformation
####################
dat <- select(dat, -starts_with("Old"), -starts_with("OLD"))
dim(dat)
colnames(dat)

#############
#Cronbach's
##############
#Ran EFA on SPSS for 4 Dimensions, P-E, and Outcomes Separately

#Need to consider alpha for DVs
## Calculating Cronbach's alpha for DV

library(psych)

DV1 <- data.frame(dat$OUTQ81_3, dat$OUTQ81_4, dat$OUTQ81_8, dat$OUTQ81_9, dat$OUTQ81_14)
alpha(DV1)

DV2 <- data.frame(dat$OUTQ81_5, dat$OUTQ81_7, dat$OUTQ81_20, dat$OUTQ81_21, dat$OUTQ81_25, dat$OUTQ81_27)
alpha(DV2)

DV3 <- data.frame(dat$OUTQ81_15, dat$OUTQ81_16, dat$OUTQ81_26, dat$OUTQ81_34)
alpha(DV3)

DV4 <- data.frame(dat$OUTQ81_35, dat$OUTQ81_36, dat$OUTQ81_37, dat$OUTQ81_38)
alpha(DV4)

DV5 <- data.frame(dat$OUTQ81_10, dat$OUTQ81_11)
alpha(DV5)

DV6 <- data.frame(dat$OUTQ81_6, dat$OUTQ81_12, dat$OUTQ81_13)
alpha(DV6)


## Calculating alpha for Outcomes
Efficacy <- data.frame(dat$OUTQ81_3, dat$OUTQ81_6, dat$OUTQ81_7, dat$OUTQ81_8)
alpha(Efficacy)
#So low, maybe just go with EFA for DV and P-E then

#######################
#Create New Variables
#######################

#Create the average of the items from EFA

#dat$PEContent <- ((dat$InsParQ41 + dat$InsParQ42 + dat$InsParQ43 + dat$InsParQ44 + dat$InsParQ45 + dat$InsParQ46)/6)

dat$PETotal <- ((dat$PEQ32 + dat$PEQ33 + dat$PEQ34 + dat$PEQ35 + dat$PEQ36 + dat$PEQ37 + dat$PEQ38 + dat$PEQ39)/8)

#dat$PEKnowledge <- ((dat$InsParQ47 + dat$InsParQ48 + dat$InsParQ49 + dat$InsParQ50)/4)

#dat$PETotal <- ((dat$PEContent + dat$PEProcess + dat$PEKnowledge)/3)

#Descriptives
summary(dat$PETotal)

dat$DV1 <- ((dat$OUTQ81_3 + dat$OUTQ81_4 + dat$OUTQ81_8 + dat$OUTQ81_9 + dat$OUTQ81_14)/5)

dat$DV2 <- ((dat$OUTQ81_5 + dat$OUTQ81_7 + dat$OUTQ81_20 + dat$OUTQ81_21 + dat$OUTQ81_25 + dat$OUTQ81_27)/6)

dat$DV3 <- ((dat$OUTQ81_15 + dat$OUTQ81_16 + dat$OUTQ81_26 + dat$OUTQ81_34)/4)

dat$DV4 <- ((dat$OUTQ81_35 + dat$OUTQ81_36 + dat$OUTQ81_37 + dat$OUTQ81_38)/4)

dat$DV5 <- ((dat$OUTQ81_10 + dat$OUTQ81_11)/2)

dat$DV6 <- ((dat$OUTQ81_6 + dat$OUTQ81_12 + dat$OUTQ81_13)/3)




#################################
#Cross-Cultural Fit
#Polynomial Regression
#################################

#visualize the data
library(ggplot2)
ggplot(dat, aes(x=AgentQ74, y=StakeQ53)) +
  geom_point()
#There is no clear pattern

ggplot(dat, aes(x=AgentQ75, y=StakeQ54)) +
  geom_point()

##Linear Regression First
set.seed(1234)
Model7453 <- lm(PETotal ~ AgentQ74 + StakeQ53, data = dat)
summary(Model7453)
#There is a relationship between these two and therefore we can test the quadratic term

#First way 
#Quadratic 
Model7453_2 <- lm(PETotal ~ AgentQ74 + StakeQ53 + I(AgentQ74^2) + (AgentQ74*StakeQ53) + I(StakeQ53^2), data = dat)
summary(Model7453_2)
#It is significant, so we will test cubic terms

#Cubic
Model7453_3 <- lm(PETotal ~ AgentQ74 + StakeQ53 + I(AgentQ74^2) + AgentQ74:StakeQ53 + I(StakeQ53^2) + I(AgentQ74^3) + I((AgentQ74^2)*StakeQ53) + I(AgentQ74*(StakeQ53^2)) + I(StakeQ53^3), data = dat)
summary(Model7453_3)

#second way, easier and faster
Model7453_4 <- lm(PETotal ~ polym(AgentQ74, StakeQ53, degree=3,raw=TRUE), data = dat)
summary(Model7453_4)

#Compare Models
anova(Model7453, Model7453_2)
#No significant differences, we can stick with the linear model

#EFA categorized a few, remaining ones will be examined as single measure

#Agent Cultural Orientation Score
dat$ACO <- (dat$AgentQ74 + dat$AgentQ78 + dat$AgentQ79 + dat$AgentQ80)
#Stakeholder Cultural Orientation Score
dat$SCO <- (dat$StakeQ53 + dat$StakeQ57 + dat$StakeQ58 + dat$StakeQ59)

#Now compare fit between ACO and SCO

#Visualize first
ggplot(dat, aes(x = ACO, y = SCO)) +
  geom_point()
#clearer pattern for linear

## Linear Regression for Cultural Orientations
ModelACOSCO <- lm(PETotal ~ ACO + SCO, data = dat)
summary(ModelACOSCO)
#Significant, moving onto polynomial

ModelACOSCO_2 <- lm(PETotal ~ polym(ACO, SCO, degree = 2, raw = TRUE), data = dat)
summary(ModelACOSCO_2)


ModelACOSCO_3 <- lm(PETotal ~ polym(ACO, SCO, degree = 3, raw = TRUE), data = dat)
summary(ModelACOSCO_3)

#Compare Models
anova(ModelACOSCO, ModelACOSCO_2)
#Significant difference
anova(ModelACOSCO_2, ModelACOSCO_3)
#not a significant difference; we can do with model 2 with 2 degree

#Visualization 
library(rsm)
par(mfrow=c(1,3))
image(ModelACOSCO_2, ACO~SCO)
contour(ModelACOSCO_2, ACO~SCO)
persp(ModelACOSCO_2, ACO~SCO, zlab = "PETotal")
# when there is a fit in instructors and stakeholders cultural orientation,  will use elicitive process
# when there is a misfit, they will use prescriptive process





####Linear Regression for Power-Distance
Model7554 <- lm(PETotal ~ AgentQ75 + StakeQ54, data = dat)
summary(Model7554)
#Significant, moving onto polynomial

Model7554_2 <- lm(PETotal ~ polym(AgentQ75, StakeQ54, degree = 2, raw = TRUE), data = dat)
summary(Model7554_2)


#Compare Models
anova(Model7554, Model7554_2)
#significant  difference, go with quadric

#Visualization 
par(mfrow=c(1,3))
image(Model7554_2, AgentQ75 ~StakeQ54)
contour(Model7554_2, AgentQ75 ~StakeQ54)
persp(Model7554_2, AgentQ75 ~StakeQ54, zlab = "DV_PETotal")
# the lower power-distance for both ACO & SCO, the more elicitive
# the higher, the more prescriptive
#mostly still look linear


####Linear Regression for Tolerance for Ambiguity
Model7655 <- lm(PETotal ~ AgentQ76 + StakeQ55, data = dat)
summary(Model7655)
#Significant, moving onto polynomial

Model7655_2 <- lm(PETotal ~ polym(AgentQ76, StakeQ55, degree = 2, raw = TRUE), data = dat)
summary(Model7655_2)


#Compare Models
anova(Model7655, Model7655_2)
#Significant difference; go with quad

#Visualization 
par(mfrow=c(1,3))
image(Model7655_2, AgentQ76 ~StakeQ55)
contour(Model7655_2, AgentQ76 ~StakeQ55)
persp(Model7655_2, AgentQ76 ~StakeQ55, zlab = "DV_PETotal")
#The  higher  tolerance for ambiguity from both agent and stakeholder, the more likely instructors to use elicitive


#Linear Regression for Egalitarian Values
Model7756 <- lm(PETotal ~ AgentQ77 + StakeQ56, data = dat)
summary(Model7756)
#Significant, moving onto polynomial

Model7756_2 <- lm(PETotal ~ polym(AgentQ77, StakeQ56, degree = 2, raw = TRUE), data = dat)
summary(Model7756_2)

Model7756_3 <- lm(PETotal ~ polym(AgentQ77, StakeQ56, degree = 3, raw = TRUE), data = dat)
summary(Model7756_3)



#Compare Models
anova(Model7756, Model7756_2, Model7756_3)
# No Significant difference, stay with linear


#Visualization 
par(mfrow=c(1,3))
image(Model7756, AgentQ77 ~StakeQ56)
contour(Model7756, AgentQ77 ~StakeQ56)
persp(Model7756, AgentQ77 ~StakeQ56, zlab = "DV_PETotal")
#High Egalitarian values of both agents and stakeholders tend to result in more elicitive training
#low values of both agents and stakeholders tend to result in more prescriptive training


#####Perceived Fit vs. Objective Fit

## Linear Regression for Cultural Fit
ModelCulFit <- lm(AgentQ65 ~ ACO + SCO, data = dat)
summary(ModelCulFit)
#Significant, moving onto polynomial

ModelCulFit_2 <- lm(AgentQ65 ~ polym(ACO, SCO, degree = 2, raw = TRUE), data = dat)
summary(ModelCulFit_2)


ModelCulFit_3 <- lm(AgentQ65 ~ polym(ACO, SCO, degree = 3, raw = TRUE), data = dat)
summary(ModelCulFit_3)

#Compare Models
anova(ModelCulFit, ModelCulFit_2)
#Significant difference
anova(ModelCulFit_2, ModelCulFit_3)
#not a significant difference; we can do with model 2 with 2 degree

#Visualization 
library(rsm)
par(mfrow=c(1,3))
image(ModelCulFit_2, ACO~SCO)
contour(ModelCulFit_2, ACO~SCO)
persp(ModelCulFit_2, ACO~SCO, zlab = "Perceived Fit")
## Instructors with high ACO in high SCO context seem to have pretty good understanding of cross-cultural fit
## Instructors with low ACO in low SCO context does not perceive their fit as accurately
## Instructors with higher degrees of misfit perceive them quite accurately


############################
## Creating IVs from EFA
############################

#IV1 is Agent Cultural Embeddedness
dat$IV1 <- ((dat$AgentQ67 + dat$AgentQ66 + dat$AgentQ68 + dat$AgentQ69 + dat$AgentQ70)/5)

#IV2 is basically PETotal/ Process

#IV3 = Stakeholder Western to Eastern Value
dat$IV3 <- ((dat$StakeQ53 + dat$StakeQ54 + dat$StakeQ57 + dat$StakeQ58 + dat$StakeQ60)/5)

#IV4 = Agent Western to Eastern Value
dat$IV4 <- ((dat$AgentQ79 + dat$AgentQ80)/2)

#IV5 = Contextual Challenges & Demands
dat$IV5 <- ((dat$ContextQ23+dat$ContextQ25+dat$StakeQ62)/3)

#IV6 = Cultural Constraints & Tightness
dat$IV6 <- ((dat$ContextQ26+dat$ContextQ27+dat$ContextQ28)/3)

#IV7= Agent Tolerance for Ambiguity
dat$IV7 <- ((dat$AgentQ73+dat$AgentQ76)/2)


##################################
## Testing the Conditions in P-E
###################################

#Correlations
library(Hmisc)
datcorr <- select(dat, IV1, IV3, IV4, IV5, IV6, IV7, PETotal)

# Correlations with P values to see significance
correlations <- rcorr(as.matrix(select(datcorr, 1:6, 7)))

# Visualize the correlations; insignificant are left blank
library(corrplot)
corrplot(correlations$r, type = "upper", order = "hclust",
         p.mat = correlations$r, sig.level = 0.05, insig = "blank")
#seems like all has significant negative correlations

#Check for actual correlations
cor.test(dat$IV1, dat$PETotal) #p <.001; r = -.35
#The more embedded the instructor is, the more likely they will use an elicitive approach

cor.test(dat$IV3, dat$PETotal) # p<.01, r = -.28
#The more Western the stakeholder are perceived to be, the more likely they will use an elicitive approach

cor.test(dat$IV4, dat$PETotal) #n.s.

cor.test(dat$IV5, dat$PETotal) # p <.01, r = -.29
# The less challenging and demanding the context is (mono-cultural, have no functional systems, and local decision making are unjust)
# the more likely an elicitive approach will be used. 

cor.test(dat$IV6, dat$PETotal) # p<.001, r = -.40
#In looser culture with fewer constraints, instructors are more likely to use elicitive methods

cor.test(dat$IV7, dat$PETotal) # p<.001, r = .31
#The higher the agent tolerance for ambiguity, the more likely they will use an elicitive approach

####################
#Individual Items
#######################


#************Agent***************#


###Create Variables based on 
ASkill <- data.frame(dat$AgentQ67, dat$AgentQ66, dat$AgentQ70, dat$AgentQ71)
alpha(ASkill)
#alpha of .86; merge into one variable
dat$ASkill <- ((dat$AgentQ67 + dat$AgentQ66 + dat$AgentQ70 + dat$AgentQ71)/4)

#Visualization
ggplot(dat, aes(x = A$Skill, y = PETotal)) +
  geom_point()
#maybe polynomial
cor.test(dat$ASkill, dat$PETotal) 


ModelSkillPE <- lm(PETotal ~ ASkill, data = dat)
summary(ModelSkillPE)
#Significant, moving onto polynomial
ModelSkillPE2 <- lm(PETotal ~ polym(ASkill, degree = 2, raw = TRUE), data = dat)
summary(ModelSkillPE2)

#Compare Models
anova(ModelSkillPE, ModelSkillPE2)
#Significant difference, move with polynomial


ggplot(dat, aes(x = ASkill, y = PEContent)) +
  geom_point() #visualize
cor.test(dat$ASkill, dat$PETotal) #p <.001, r = -.43
#The more cross-cultural skills, the more they use prescriptive
#the less cross-cultural skills, the more they use elicitive
#This is in line with our assumptions


## Access to local partners

APartner <- data.frame(dat$AgentQ68, dat$AgentQ69)
alpha(APartner) #.78, not bad, group into one
dat$APartner <- ((dat$AgentQ68 + dat$AgentQ69)/2)
ggplot(dat, aes(x = APartner, y = PETotal)) +
  geom_point() #seems linear
##Correlations
library(Hmisc)
cor.test(dat$APartner, dat$PETotal) #significant correlations at p <.001, r = -.33
#The more access to local partnerships, the more likely to use prescriptive methods and vice versa
#In line with predictions


#*********************Stakeholders*******************#

## Urgent Objectives
ggplot(dat, aes(x = StakeQ52, y = PETotal)) +
  geom_point() #seems linear
cor.test(dat$StakeQ52, dat$PETotal) #Significant at p <.001; r = .32
#The more urgent the objectives, the more likely to use elicitive
#contrary to our predictions


##Local Capacities for CC-CR
ggplot(dat, aes(x = StakeQ60, y = PETotal)) +
  geom_point() #seems linear
cor.test(dat$StakeQ60, dat$PETotal) #not significant, not a predictor of P-E Process


## Community commitment, efficacy, and agency 
ggplot(dat, aes(x = StakeQ61, y = PETotal)) +
  geom_point() #the end tail is a bit upwards
cor.test(dat$StakeQ61, dat$PETotal) #significant at the p<.01 level, r = -.26
# The higher the community commitment, efficacy, and agency, the more instructors would use prescriptive methods
# Except for when community commitment are extremely high, instructors would use more hybrid and elicitive methods
# Contrary to our predictions

### Community inclusiveness
ggplot(dat, aes(x = StakeQ62, y = PETotal)) +
  geom_point() #kind of cone shaped as it gets narrower at the end
cor.test(dat$StakeQ62, dat$PETotal) #barely significant at p = .51,  r = -.18
#The more inclusive a community, the more likely to do prescriptive
## Linear Regression for ComIncluside
ModelComIn <- lm(PETotal ~ StakeQ62, data = dat)
summary(ModelComIn)
#Barely significant, moving onto polynomial
ModelComIn_2 <- lm(PETotal ~ polym(StakeQ62, degree = 2, raw = TRUE), data = dat)
summary(ModelComIn_2) #it is not that much of a change
ModelComIn_3 <- lm(PETotal ~ polym(StakeQ62, degree = 3, raw = TRUE), data = dat)
summary(ModelComIn_3) #not significant


#*************Context****************#


##Degree of Conflict Intensity & Complexity
ggplot(dat, aes(x = ContextQ24, y = PETotal)) +
  geom_point() #the end tail is a bit upwards
cor.test(dat$ContextQ24, dat$PETotal) #not at all significant


## Functional local systems and processes
ggplot(dat, aes(x = ContextQ25, y = PETotal)) +
  geom_point() #kinda linear
cor.test(dat$ContextQ25, dat$PETotal) #p = .032, r = -.20
# The more functional the local systems and processes, the more likely to use prescriptive


## Multi/Monocultural
ggplot(dat, aes(x = ContextQ23, y = PETotal)) +
  geom_point() #messy
cor.test(dat$ContextQ23, dat$PETotal) #not at all significant

##Cultural tightness-looseness
ggplot(dat, aes(x = ContextQ26, y = PETotal)) +
  geom_point() #kinda linear
cor.test(dat$ContextQ26, dat$PETotal) #p <.001, r = -.39
# The tighter the culture, the more likely to use prescriptive

##Honor cultures
ggplot(dat, aes(x = ContextQ27, y = PETotal)) +
  geom_point() #kinda linear
cor.test(dat$ContextQ27, dat$PETotal) #not at all significant


##Cultural salience
ggplot(dat, aes(x = ContextQ28, y = PETotal)) +
  geom_point() #looks linear
cor.test(dat$ContextQ28, dat$PETotal) #p <.001, r = .-37
#The higher the cultural salience, the more likely to use prescriptive
# The lower the cultural salience, the more likely to use elicitive



#*******************Process****************#


#Adaptability (Could be easily altered and changed to fit local cultural needs and conditions)
ggplot(dat, aes(x = ProcessQ30, y = PETotal)) +
  geom_point() #looks linear
cor.test(dat$ProcessQ30, dat$PETotal) #p <.001; r = -.32
# The more adaptable the more likely to use prescriptive methods??
# contradictory to our findings

#Focus of adaptation (fit culturally with local community)
ggplot(dat, aes(x = ProcessQ31, y = PETotal)) +
  geom_point() #looks linear
cor.test(dat$ProcessQ31, dat$PETotal) #p =.01; r = .23
# In line w predictions, the more fitting with local community, the more likely to be elicitive



###################
### One-way ANOVA
####################
library(car)
library(emmeans) #for pairwise comparison later

#Create P-E Group Levels
summary(dat$PETotal) #use these numbers to divide into groups
dat$PEGroup <- cut(dat$PETotal,
                   breaks = c(-Inf, 3.25, 4.625, Inf),
                   levels = c(1,2,3),
                   labels = c("P", "H", "E"))

summary(dat$PEGroup) #P has 38; H has 51; E has 28
#Create weights for the groups due to uneven sizes
library(plyr)
dat$PEGroupWts <- revalue(dat$PEGroup, c("P"="1.34", "H"="1", "E"="1.82"))
dat$PEWts <- as.numeric(as.vector(dat$PEGroupWts)) #turn into numeric

#Change to effect coding instead of dummy coding
options()$contrasts
options(contrasts = c("contr.sum", "contr.poly"))

####### testing for the DVs against PE ###########

### DV1 ###
#full model
lmDV1F <- lm(DV1 ~ PEGroup, data = dat, weights = PEWts) #using weighted analysis
summary(lmDV1F)

Anova(lmDV1F, type = 3)
# The PE group does not have significant effect on DV1 


######DV2#######
lmDV2F <- lm(DV2 ~ PEGroup, data = dat, weights = PEWts) #using weighted analysis
summary(lmDV2F)
#n.s.

######DV3#######
lmDV3F <- lm(DV3 ~ PEGroup, data = dat, weights = PEWts) #using weighted analysis
summary(lmDV3F)
#n.s

######DV4#######
lmDV4F <- lm(DV4 ~ PEGroup, data = dat, weights = PEWts) #using weighted analysis
summary(lmDV4F)
#n.s.


####DV5#######
lmDV5F <- lm(DV5 ~ PEGroup, data = dat, weights = PEWts) #using weighted analysis
summary(lmDV5F)
#n.s.

######DV6#######
lmDV6F <- lm(DV6 ~ PEGroup, data = dat, weights = PEWts) #using weighted analysis
summary(lmDV6F)
#F(2,111) = 4.69; p = .01
#significant
Anova(lmDV6F, type = 3)
#significant differences among PE groups

# Now follow up with pairwise comparisons.
emm6 <- emmeans(object = lmDV6F,
                specs = ~ PEGroup) #only need the full model here
#want groups to be broken up by P-E Approach

emm6 # individual means 

pairs(x = emm6, 
      adjust = "Bonferroni")  # Bonferroni-adjusted. Ok to interpret.
#Significant difference between P (M = 4.28) and E (mean = 3.48) (p <.01)
#n.s. for hybrid (m = 3.97)
#Elicitive methods are significantly less efficient than Prescriptive


####Coded Effectiveness#######
lmDV8F <- lm(DV8_QualiGenEff ~ PEGroup, data = dat, weights = PEWts) #using weighted analysis
summary(lmDV8F)
#n.s.

######Coded Sustainability & Learning#####
lmDV9F <- lm(DV9_QualiSustain ~ PEGroup, data = dat, weights = PEWts) #using weighted analysis
summary(lmDV9F)
#n.s.

###############################################
#ANCOVA
###############################################


#Check for a strong linear relationship with the outcome
dat2 <- select(dat, IV1, IV3, IV4, IV5, IV6, IV7, PETotal, 
                         DV1, DV2, DV3, DV4, DV5, DV6, DV8_QualiGenEff, DV9_QualiSustain)

# Correlations with P values to see significance
correlations2 <- rcorr(as.matrix(dat2))
cor(dat2, use = "complete.obs") #tell it to ignore NA variables

# Visualize the correlations; insignificant are left blank
corrplot(correlations2$r, type = "upper", order = "original", 
         p.mat = correlations$r, sig.level = 0.05, insig = "blank")
corrplot(cor(dat2, use = "complete.obs"))
#Seems like most IVs are somewhat correlated with the DVs so we can run hierarchical regression


###########Models################

##DV1 (Example of full process)#####

# Consider controlling for all IVs as covariates. 
# From the correlations plot, it seems that all IVs have a strong linear relationship w DV1

plot(DV1 ~ IV1,
     data = dat,
     xlab = "Agent Embeddedness",
     ylab = "Self-Rated Effectiveness",
     pch = c(19:0)[(as.character(dat$PEGroup) == "T") + 1],
     col = (as.character(dat$PEGroup) == "T") + 1,
     main = "Model Assuming No Interaction")
legend(x = "bottomright",
       lwd = 2, col = 1:2, pch = c(19:0), lty = 1:2,
       legend = c("P", "H", "E"), seg.len = 4)
#not really separating into different colors... 
#need to double check this code

# The test of the slope coefficient from a regression 
# will test if the linear relationship 
# is significant. It is not significant 
# (p-value = .0546).
lm_slope11 <- lm(DV1 ~ IV1+IV3+IV4+IV5+IV6+IV7, data = dat, weights = PEWts)

summary(lm_slope11)
### Can stop here, but will leave full process as a model


# The interaction is NOT significant, which does not violates the 
# assumption of no treatment by covariate interaction
# in ANCOVA. 

# ANCOVA full model 
lmF11 <- lm(DV1 ~ IV1+IV3+IV4+IV5+IV6+IV7 + PEGroup, data = dat, weights = PEWts)
Anova(lmF11, type = 3)
summary(lmF11) #model parameters
#it is significant for PE

# Fit emmeans here to get estimated marginal
# means for each group, controlling for the
# linear relationship between IV1 and DV1.
# By default, the emmeans() function will plug 
# in the means (averages) of any continuous
# covariates that are averaged over. Here, we
# ask it to average over IV1, so it uses the 
# mean of that variable by default.
emm11 <- emmeans(object = lmF11,
                specs = ~ PEGroup)
emm11

pairs(emm11, adjust = "Holm")
#when controlling for contextual factors
#significant differences between H (M = 4.31) and E (M = 4.84) for Self-Rated Effectiveness
# p =.03
#n.s. for Prescriptive (M = 4.47)


##DV 2#####

# Consider controlling for all IVs as covariates. 
# From the correlations plot, it seems that all but IV7 have a strong linear relationship w DV2


# The test of the slope coefficient from a regression 
# will test if the linear relationship 
# is significant. It is very significant 
# (p-value <.01).
lm_slope21 <- lm(DV2 ~ IV1+IV3+IV4+IV5+IV6, data = dat, weights = PEWts)
summary(lm_slope21)


# ANCOVA full model 
lmF21 <- lm(DV2 ~ IV1+IV3+IV4+IV5+IV6 + PEGroup, data = dat, weights = PEWts)
Anova(lmF21, type = 3)
summary(lmF21) #model parameters
#it is NOT significant for PE
#so PE does not explain additional variance with DV2


##DV 3#####

# Consider controlling for all IVs as covariates. 
# From the correlations plot, it seems that all IVs have a strong linear relationship w DV3


# The test of the slope coefficient from a regression 
# will test if the linear relationship 
# is significant. It is not significant 
lm_slope31 <- lm(DV3 ~ IV1+IV3+IV4+IV5+IV6+IV7, data = dat, weights = PEWts)
summary(lm_slope31)


# ANCOVA full model 
lmF31 <- lm(DV3 ~ IV1+IV3+IV4+IV5+IV6+IV7 + PEGroup, data = dat, weights = PEWts)
Anova(lmF31, type = 3)
summary(lmF31) #model parameters
#it is significant for PE
emm31 <- emmeans(object = lmF31,
                 specs = ~ PEGroup)
emm31

pairs(emm31, adjust = "Holm")
#Significant differences between H (4.38) and E (4.97)
# p =.02
#no significant differences for P (4.52)


####DV4#####
# Consider controlling for all IVs as covariates. 
# From the correlations plot, it seems that all IVs have a strong linear relationship w DV4

# ANCOVA full model 
lmF41 <- lm(DV4 ~ IV1+IV3+IV4+IV5+IV6+IV7 + PEGroup, data = dat, weights = PEWts)
Anova(lmF41, type = 3)
summary(lmF41) #model parameters
#it is NOT significant for PE
#PE did not explain additional variance in this when controlling for other factors


####DV5#####
# Consider controlling for all IVs as covariates. 
# From the correlations plot, it seems that all but IV5 have a  linear relationship w DV5

# ANCOVA full model 
lmF51 <- lm(DV5 ~ IV1+IV3+IV4+IV6+IV7 + PEGroup, data = dat, weights = PEWts)
Anova(lmF51, type = 3)
summary(lmF51) #model parameters
#it is NOT significant for PE
#PE did not explain additional variance in this when controlling for other factors

###Skip DV6 because already significant####

####DV8#####
#All but IV4
lmF81 <- lm(DV8_QualiGenEff ~ IV1+IV3+IV5+IV6+IV7 + PEGroup, data = dat, weights = PEWts)
Anova(lmF81, type = 3)
summary(lmF81)
#n.s. 
#did not explain additional variance


#####DV9#######
#All but IV5
lmF91 <- lm(DV9_QualiSustain ~ IV1+IV3+IV4+IV6+IV7 + PEGroup, data = dat, weights = PEWts)
Anova(lmF91, type = 3)
summary(lmF91)


####################################
#Multiple Hierarchical Regression
####################################

#Check for correlations
cor.test(dat$PETotal, dat$DV1) #ns
cor.test(dat$PETotal, dat$DV2) #ns
cor.test(dat$PETotal, dat$DV3) #ns
cor.test(dat$PETotal, dat$DV4) #ns
cor.test(dat$PETotal, dat$DV5) #ns
cor.test(dat$PETotal, dat$DV6) #significant at p =.03, r = -.20

###DV1#######
AControl1 <- lm(DV1~ IV1+IV3+IV4+IV5+IV6+IV7, data = dat) #Control for context differences
summary(AControl1)

ModelPE1 <- lm(DV1~IV1+IV3+IV4+IV5+IV6+IV7 + PETotal, data = dat)
summary(ModelPE1) #PE Significant


anova(AControl1, ModelPE1) #Model 2 has significant differences
#When controlling for external factors, P-E Process significantly predict DV1


###DV2#######
AControl2 <- lm(DV2~ IV1+IV3+IV4+IV5+IV6, data = dat) #Control for context differences
summary(AControl2)

ModelPE2 <- lm(DV2~IV1+IV3+IV4+IV5+IV6 + PETotal, data = dat)
summary(ModelPE2) #PE Significant


anova(AControl2, ModelPE2) #Model 2 has significant differences
#When controlling for external factors, P-E Process significantly predict DV2

###DV3#######
AControl3 <- lm(DV3~ IV1+IV3+IV4+IV5+IV6+IV7, data = dat) #Control for context differences
summary(AControl3)

ModelPE3 <- lm(DV3~IV1+IV3+IV4+IV5+IV6+IV7 + PETotal, data = dat)
summary(ModelPE3) #PE Significant


anova(AControl3, ModelPE3) #Model 2 has significant differences
#When controlling for external factors, P-E Process significantly predict DV3

###DV4#######
AControl4 <- lm(DV4~ IV1+IV3+IV4+IV5+IV6+IV7, data = dat) #Control for context differences
summary(AControl4)

ModelPE4 <- lm(DV4~IV1+IV3+IV4+IV5+IV6+IV7 + PETotal, data = dat)
summary(ModelPE4) #PE Significant


anova(AControl4, ModelPE4) #Model 2 has significant differences
#When controlling for external factors, P-E Process significantly predict DV1


###DV5#######
AControl5 <- lm(DV5~ IV1+IV3+IV4+IV5+IV6+IV7, data = dat) #Control for context differences
summary(AControl5)

ModelPE5 <- lm(DV5~IV1+IV3+IV4+IV5+IV6+IV7 + PETotal, data = dat)
summary(ModelPE5) 
#n.s.

#P-E Process does not predict DV5

## DV Skip because already significant####
#This is more promising than ANCOVA



#####################
#Two-Way ANOVA
#####################

#Create new categorical variables for IV1, IV3, IV4, IV5, and IV6, and IV7 to work with PE Group
#Cut at the mean

#Agent Total
summary(dat$IV1) #4.46
dat$IV1Di <- cut(dat$IV1,
                   breaks = c(-Inf, 4.46, Inf),
                   levels = c(1,2),
                   labels = c("L", "H"))

summary(dat$IV1Di) 

#IV3
summary(dat$IV3) #4.03
dat$IV3Di <- cut(dat$IV3,
                        breaks = c(-Inf, 4.03, Inf),
                        levels = c(1,2),
                        labels = c("W", "E"))

#IV4
summary(dat$IV4) #3.84
dat$IV4Di <- cut(dat$IV4,
                 breaks = c(-Inf, 3.84, Inf),
                 levels = c(1,2),
                 labels = c("W", "E"))

#IV5
summary(dat$IV5) #4.03
dat$IV5Di <- cut(dat$IV5,
                 breaks = c(-Inf, 4.03, Inf),
                 levels = c(1,2),
                 labels = c("H", "L")) #lower scores higher demands

#IV6
summary(dat$IV6) #4.37
dat$IV6Di <- cut(dat$IV6,
                 breaks = c(-Inf, 4.37, Inf),
                 levels = c(1,2),
                 labels = c("L", "T"))

#IV7
summary(dat$IV7) #4.51
dat$IV7Di <- cut(dat$IV7,
                 breaks = c(-Inf, 4.51, Inf),
                 levels = c(1,2),
                 labels = c("L", "H"))

#DV7_QualiPE
summary(dat$DV7_QualiPE) #as expected, it is pretty low
#also do not have enough people to run 2 way ANOVA 


############  DV1 #############

##### IV1xPE
lmIV11 <- lm(DV1~PEGroup*IV1Di, data = dat, weights = PEWts)
summary(lmIV11)
#n.s.
#No interaction here

##### IV3 x PE
lmIV31 <- lm(DV1~PEGroup*IV3Di, data = dat, weights = PEWts)
summary(lmIV31)
#n.s.

##### IV4 x PE
lmIV41 <- lm(DV1~PEGroup*IV4Di, data = dat, weights = PEWts)
summary(lmIV41)
#n.s.

##### IV5 x PE
lmIV51 <- lm(DV1~PEGroup*IV5Di, data = dat, weights = PEWts)
summary(lmIV51)
#significant, but seems like main effect, not interaction
Anova(lmIV51, type = 3)
#n.s. for interaction


##### IV6 x PE
lmIV61 <- lm(DV1~PEGroup*IV6Di, data = dat,weights = PEWts)
summary(lmIV61)
#n.s.

##### IV7 x PE
lmIV71 <- lm(DV1~PEGroup*IV7Di, data = dat, weights = PEWts)
summary(lmIV71)
#n.s.


############  DV2 #############

##### IV1xPE
lmIV12 <- lm(DV2~PEGroup*IV1Di, data = dat, weights = PEWts)
summary(lmIV12)
#n.s.
#No interaction here

##### IV3 x PE
lmIV32 <- lm(DV2~PEGroup*IV3Di, data = dat, weights = PEWts)
summary(lmIV32)
#Significant, main effect for IV3
Anova(lmIV32, type = 3)

##### IV4 x PE
lmIV42 <- lm(DV2~PEGroup*IV4Di, data = dat, weights = PEWts)
summary(lmIV42)
#ns

##### IV5 x PE
lmIV52 <- lm(DV2~PEGroup*IV5Di, data = dat, weights = PEWts)
summary(lmIV52)
#n.s.


##### IV6 x PE
lmIV62 <- lm(DV2~PEGroup*IV6Di, data = dat, weights = PEWts)
summary(lmIV62)
#n.s.

##### IV7 x PE
lmIV72 <- lm(DV2~PEGroup*IV7Di, data = dat, weights = PEWts)
summary(lmIV72)
#n.s.

############  DV3 #############

##### IV1 x PE
lmIV13 <- lm(DV3~PEGroup*IV1Di, data = dat,weights = PEWts)
summary(lmIV13)
#significant but main effect?
Anova(lmIV13, type = 3)
#yep, just main effect

##### IV3 x PE
lmIV33 <- lm(DV3~PEGroup*IV3Di, data = dat, weights = PEWts)
summary(lmIV33)
#n.s.

##### IV4 x PE
lmIV43 <- lm(DV3~PEGroup*IV4Di, data = dat, weights = PEWts)
summary(lmIV43)
#n.s.

##### IV5 x PE
lmIV53 <- lm(DV3~PEGroup*IV5Di, data = dat, weights = PEWts)
summary(lmIV53)
#n.s.


##### IV6 x PE
lmIV63 <- lm(DV3~PEGroup*IV6Di, data = dat, weights = PEWts)
summary(lmIV63)
#n.s.

##### IV7 x PE
lmIV73 <- lm(DV3~PEGroup*IV7Di, data = dat, weights = PEWts)
summary(lmIV73)
#n.s.


############  DV4 #############

##### IV1 x PE
lmIV14 <- lm(DV4~PEGroup*IV1Di, data = dat, weights = PEWts)
summary(lmIV14)
#n.s.

##### IV3 x PE
lmIV34 <- lm(DV4~PEGroup*IV3Di, data = dat, weights = PEWts)
summary(lmIV34)
#n.s.

##### IV4 x PE
lmIV44 <- lm(DV4~PEGroup*IV4Di, data = dat, weights = PEWts)
summary(lmIV44)
#n.s.

##### IV5 x PE
lmIV54 <- lm(DV4~PEGroup*IV5Di, data = dat, weights = PEWts)
summary(lmIV54)
#Significant, but not for interaction


##### IV6 x PE
lmIV64 <- lm(DV4~PEGroup*IV6Di, data = dat, weights = PEWts)
summary(lmIV64)
#n.s.

##### IV7 x PE
lmIV74 <- lm(DV4~PEGroup*IV7Di, data = dat, weights = PEWts)
summary(lmIV74)
Anova(lmIV74)
#n.s.

############  DV5 #############

##### IV1 x PE
lmIV15 <- lm(DV5~PEGroup*IV1Di, data = dat, weights = PEWts)
summary(lmIV15)
#n.s.

##### IV3 x PE
lmIV35 <- lm(DV5~PEGroup*IV3Di, data = dat, weights = PEWts)
summary(lmIV35)
#n.s.

##### IV4 x PE
lmIV45 <- lm(DV5~PEGroup*IV4Di, data = dat, weights = PEWts)
summary(lmIV45)
#n.s.


##### IV5 x PE
lmIV55 <- lm(DV5~PEGroup*IV5Di, data = dat, weights = PEWts)
summary(lmIV55)
#n.s.


##### IV6 x PE
lmIV65 <- lm(DV5~PEGroup*IV6Di, data = dat)
summary(lmIV65)
#n.s.

##### IV7 x PE
lmIV75 <- lm(DV5~PEGroup*IV7Di, data = dat)
summary(lmIV75)
#Interaction is significant
Anova(lmIV75, type = 3) 
#interaction no longer significant

############  DV6 #############

##### IV1 x PE
lmIV16 <- lm(DV6~PEGroup*IV1Di, data = dat)
summary(lmIV16)
#Significant but not interaction

##### IV3 x PE
lmIV36 <- lm(DV6~PEGroup*IV3Di, data = dat)
summary(lmIV36)
#Significant but not interaction
Anova(lmIV36, type = 3)

##### IV4 x PE
lmIV46 <- lm(DV6~PEGroup*IV4Di, data = dat)
summary(lmIV46)
#Significant w interaction
Anova(lmIV46, type = 3)
#not anymore

##### IV5 x PE Contextual Challenge & Demands
lmIV56 <- lm(DV6~PEGroup*IV5Di, data = dat)
summary(lmIV56)
#Significant and there might be and interaction
Anova(lmIV56)
#Yesss there is an interaction effect finally

#Create interaction plot
emm56 <- emmeans(object = lmIV56,
                 specs = ~PEGroup:IV5Di)
emmip(object = emm56,
     formula = PEGroup~IV5Di,
     CIs = TRUE)

#In low contextual challenge and demand, prescriptive is the most efficient 
#while Elicitive is the least
#In high contextual challenge and demand, the efficiency is not different among approaches

##### IV6 x PE 
lmIV66 <- lm(DV6~PEGroup*IV6Di, data = dat)
summary(lmIV66)
#Significant 
Anova(lmIV66, type = 3)
#interaction significant at the p <.01 level

#Create interaction plot
emm66 <- emmeans(object = lmIV66,
                 specs = ~PEGroup:IV6Di)
emmip(object = emm66,
      formula = PEGroup~IV6Di,
      CIs = TRUE)
#in low cultural constraints with looser norms
# prescriptive methods and hybrid are more efficient than elicitive
#in high cultural constraints with tighter norms, elicitive is the most efficient



##### IV7 x PE 
lmIV76 <- lm(DV6~PEGroup*IV7Di, data = dat)
summary(lmIV76)
#Significant 
Anova(lmIV76, type = 3)
#interaction significant at the p =.05 level

#Create interaction plot
emm76 <- emmeans(object = lmIV76,
                 specs = ~PEGroup:IV7Di)
emmip(object = emm76,
      formula = PEGroup~IV7Di,
      CIs = TRUE)
#So confused about this
#so when Agent has high tolerance for ambiguity, elicitive process would be much less efficient
#compared to hybrid and prescriptive
# Actually nvm it makes sense now; they are just too chill


############  DV8 #############

##### IV1 x PE
lmIV18 <- lm(DV8_QualiGenEff~PEGroup*IV1Di, data = dat)
summary(lmIV18)
#n.s.

##### IV3 x PE
lmIV38 <- lm(DV8_QualiGenEff~PEGroup*IV3Di, data = dat)
summary(lmIV38)
#n.s.

##### IV4 x PE
lmIV48 <- lm(DV8_QualiGenEff~PEGroup*IV4Di, data = dat)
summary(lmIV48)
#n.s.


##### IV5 x PE
lmIV58 <- lm(DV8_QualiGenEff~PEGroup*IV5Di, data = dat)
summary(lmIV58)
#n.s.


##### IV6 x PE
lmIV68 <- lm(DV8_QualiGenEff~PEGroup*IV6Di, data = dat)
summary(lmIV68)
#n.s.

##### IV7 x PE
lmIV78 <- lm(DV8_QualiGenEff~PEGroup*IV7Di, data = dat)
summary(lmIV78)
#Interaction might be significant
Anova(lmIV78, type = 3) 
#interaction is significant at p = .05

#Create interaction plot
emm78 <- emmeans(object = lmIV78,
                 specs = ~PEGroup:IV7Di)
emmip(object = emm78,
      formula = ~PEGroup:IV7Di,
      CIs = TRUE)
#When agent has low tolerance for ambiguity
#hybrid is significantly less effective than prescriptive or elicitive (coded)
#when agent has high tolerance for ambiguity
#hybrid or elicitive is coded to be much more effective


############  DV9 #############

##### IV1 x PE
lmIV19 <- lm(DV9_QualiSustain~PEGroup*IV1Di, data = dat)
summary(lmIV19)
#n.s.

##### IV3 x PE
lmIV39 <- lm(DV9_QualiSustain~PEGroup*IV3Di, data = dat)
summary(lmIV39)
#n.s.

##### IV4 x PE
lmIV49 <- lm(DV9_QualiSustain~PEGroup*IV4Di, data = dat)
summary(lmIV49)
#n.s.


##### IV5 x PE
lmIV59 <- lm(DV9_QualiSustain~PEGroup*IV5Di, data = dat)
summary(lmIV59)
#n.s.


##### IV6 x PE
lmIV69 <- lm(DV9_QualiSustain~PEGroup*IV6Di, data = dat)
summary(lmIV69)
#n.s.

##### IV7 x PE
lmIV79 <- lm(DV9_QualiSustain~PEGroup*IV7Di, data = dat)
summary(lmIV79)
#n.s.



#########################
#Moderation Analysis
##########################

#Running PE as a continuous variable 

####IV1
ModelPE11 <- lm(DV1~IV1Di + PETotal + IV1Di:PETotal, data = dat)
summary(ModelPE11) #Interaction not significant, not a moderator for DV1

ModelPE21 <- lm(DV2~IV1Di + PETotal + IV1Di:PETotal, data = dat)
summary(ModelPE21) #N.S

ModelPE31 <- lm(DV3~IV1Di + PETotal + IV1Di:PETotal, data = dat)
summary(ModelPE31) #Interaction not significant, not a moderator for DV3

ModelPE41 <- lm(DV4~IV1Di + PETotal + IV1Di:PETotal, data = dat)
summary(ModelPE41) #Interaction not significant, not a moderator for DV4

ModelPE51 <- lm(DV5~IV1Di + PETotal + IV1Di:PETotal, data = dat)
summary(ModelPE51) #Interaction not significant, not a moderator for DV5

ModelPE61 <- lm(DV6~IV1Di + PETotal + IV1Di:PETotal, data = dat)
summary(ModelPE61) #Interaction not significant, not a moderator for DV6

ModelPE81 <- lm(DV8_QualiGenEff~IV1Di + PETotal + IV1Di:PETotal, data = dat)
summary(ModelPE81)

ModelPE91 <- lm(DV9_QualiSustain~IV1Di + PETotal + IV1Di:PETotal, data = dat)
summary(ModelPE91)

#IV1 does not have an effect on P-E Approach to effectiveness

####IV3
ModelPE13 <- lm(DV1~IV3Di + PETotal + IV3Di:PETotal, data = dat)
summary(ModelPE13) #Interaction not significant, not a moderator for DV1

ModelPE23 <- lm(DV2~IV3Di + PETotal + IV3Di:PETotal, data = dat)
summary(ModelPE23) #N.S

ModelPE33 <- lm(DV3~IV3Di + PETotal + IV3Di:PETotal, data = dat)
summary(ModelPE33) #Interaction not significant, not a moderator for DV3

ModelPE43 <- lm(DV4~IV3Di + PETotal + IV3Di:PETotal, data = dat)
summary(ModelPE43) #Interaction not significant, not a moderator for DV4

ModelPE53 <- lm(DV5~IV3Di + PETotal + IV3Di:PETotal, data = dat)
summary(ModelPE53) #Interaction not significant, not a moderator for DV5

ModelPE63 <- lm(DV6~IV3Di + PETotal + IV3Di:PETotal, data = dat)
summary(ModelPE63) #Interaction  significant finally at the p <.01 lvl

#Create interaction plot
ggplot(dat, aes(x = PETotal,
                y = DV6,
                color = IV3Di)) +
  theme_bw() +
  labs(x = "P to E",
       y = "Efficiency",
       color = "Stakeholder Value Orientation") +
  geom_smooth(method = "lm") #turn it into lines
#if want to remove the SE shadows, add se = F after method = "lm"
#If stakeholder have more western values, prescriptive methods are more efficient
#if stakeholder have more eastern values, elicitive are more efficient


ModelPE83 <- lm(DV8_QualiGenEff~IV3Di + PETotal + IV3Di:PETotal, data = dat)
summary(ModelPE83) #ns

ModelPE93 <- lm(DV9_QualiSustain~IV3Di + PETotal + IV3Di:PETotal, data = dat)
summary(ModelPE91) #ns

####IV4
ModelPE14 <- lm(DV1~IV4Di + PETotal + IV4Di:PETotal, data = dat)
summary(ModelPE14) #Interaction not significant, not a moderator for DV1

ModelPE24 <- lm(DV2~IV4Di + PETotal + IV4Di:PETotal, data = dat)
summary(ModelPE24) #N.S

ModelPE34 <- lm(DV3~IV4Di + PETotal + IV4Di:PETotal, data = dat)
summary(ModelPE34) #Interaction not significant, not a moderator for DV3

ModelPE44 <- lm(DV4~IV4Di + PETotal + IV4Di:PETotal, data = dat)
summary(ModelPE44) #Interaction not significant, not a moderator for DV4

ModelPE54 <- lm(DV5~IV4Di + PETotal + IV4Di:PETotal, data = dat)
summary(ModelPE54) #Interaction not significant, not a moderator for DV5

ModelPE64 <- lm(DV6~IV4Di + PETotal + IV4Di:PETotal, data = dat)
summary(ModelPE64) #Interaction is significant at p<.01 level

#Create interaction plot
ggplot(dat, aes(x = PETotal,
                y = DV6,
                color = IV4Di)) +
  theme_bw() +
  labs(x = "P to E",
       y = "Efficiency",
       color = "Agent Value Orientation") +
  geom_smooth(method = "lm") #turn it into lines
#If agent have more western values, prescriptive methods are more efficient
#if agent have more eastern values,there is no significant difference in approach and outcomes
#although elicitive is slightly better

ModelPE84 <- lm(DV8_QualiGenEff~IV4Di + PETotal + IV4Di:PETotal, data = dat)
summary(ModelPE84) #ns

ModelPE94 <- lm(DV9_QualiSustain~IV4Di + PETotal + IV4Di:PETotal, data = dat)
summary(ModelPE94) #ns

####IV5
ModelPE15 <- lm(DV1~IV5Di + PETotal + IV5Di:PETotal, data = dat)
summary(ModelPE15) #Interaction is significant at p =.04

#Create interaction plot
ggplot(dat, aes(x = PETotal,
                y = DV1,
                color = IV5Di)) +
  theme_bw() +
  labs(x = "P to E",
       y = "Self-Rated Effectiveness",
       color = "Contextual Challenges & Demands") +
  geom_smooth(method = "lm",
              se = F) #turn it into lines
#In context with low demand and challenges, prescriptive methods are more effective
#in context with high demand and challenges, elicitive methods are more effective

ModelPE25 <- lm(DV2~IV5Di + PETotal + IV5Di:PETotal, data = dat)
summary(ModelPE25) #N.S

ModelPE35 <- lm(DV3~IV5Di + PETotal + IV5Di:PETotal, data = dat)
summary(ModelPE35) #Interaction not significant, not a moderator for DV3

ModelPE45 <- lm(DV4~IV5Di + PETotal + IV5Di:PETotal, data = dat)
summary(ModelPE45) #Interaction not significant, not a moderator for DV4

ModelPE55 <- lm(DV5~IV5Di + PETotal + IV5Di:PETotal, data = dat)
summary(ModelPE55) #Interaction not significant, not a moderator for DV5

ModelPE65 <- lm(DV6~IV5Di + PETotal + IV5Di:PETotal, data = dat)
summary(ModelPE65) 
#Interaction significant at p <.01, but already got from 2 way anova

ModelPE85 <- lm(DV8_QualiGenEff~IV5Di + PETotal + IV5Di:PETotal, data = dat)
summary(ModelPE85) #ns

ModelPE95 <- lm(DV9_QualiSustain~IV5Di + PETotal + IV5Di:PETotal, data = dat)
summary(ModelPE95) #ns


####IV6
ModelPE16 <- lm(DV1~IV6Di + PETotal + IV6Di:PETotal, data = dat)
summary(ModelPE16) #Interaction not significant, not a moderator for DV1

ModelPE26 <- lm(DV2~IV6Di + PETotal + IV6Di:PETotal, data = dat)
summary(ModelPE26) #N.S

ModelPE36 <- lm(DV3~IV6Di + PETotal + IV6Di:PETotal, data = dat)
summary(ModelPE36) #Interaction not significant, not a moderator for DV3

ModelPE46 <- lm(DV4~IV6Di + PETotal + IV6Di:PETotal, data = dat)
summary(ModelPE46) #Interaction not significant, not a moderator for DV4

ModelPE56 <- lm(DV5~IV6Di + PETotal + IV6Di:PETotal, data = dat)
summary(ModelPE56) #Interaction not significant, not a moderator for DV5

ModelPE66 <- lm(DV6~IV6Di + PETotal + IV6Di:PETotal, data = dat)
summary(ModelPE66) #Interaction significant, already knew from 2 way anova

ModelPE86 <- lm(DV8_QualiGenEff~IV6Di + PETotal + IV6Di:PETotal, data = dat)
summary(ModelPE86) #ns

ModelPE96 <- lm(DV9_QualiSustain~IV6Di + PETotal + IV6Di:PETotal, data = dat)
summary(ModelPE96) #ns

####IV7
ModelPE17 <- lm(DV1~IV7Di + PETotal + IV7Di:PETotal, data = dat)
summary(ModelPE17) #Interaction significant at the p <.001 lvl

#Create interaction plot
ggplot(dat, aes(x = PETotal,
                y = DV1,
                color = IV7Di)) +
  theme_bw() +
  labs(x = "P to E",
       y = "Self-Rated Effectiveness",
       color = "Agent Tolerance for Ambiguity") +
  geom_smooth(method = "lm",
              se = F) #turn it into lines
#When agent has low TA, prescriptive is the most effective
#when agent has high TA, elicitive is the most effective

ModelPE27 <- lm(DV2~IV7Di + PETotal + IV7Di:PETotal, data = dat)
summary(ModelPE27) #Significant at the p <.001 level

#Create interaction plot
ggplot(dat, aes(x = PETotal,
                y = DV2,
                color = IV7Di)) +
  theme_bw() +
  labs(x = "P to E",
       y = "Self-Rated Cultural Appropriateness",
       color = "Agent Tolerance for Ambiguity") +
  geom_smooth(method = "lm",
              se = F) #turn it into lines
#When agent has low TA, prescriptive is the most culturally appropriate
#when agent has high TA, elicitive is the most culturally appropriate

ModelPE37 <- lm(DV3~IV7Di + PETotal + IV7Di:PETotal, data = dat)
summary(ModelPE37) #Interaction is significant at p <.01 level

#Create interaction plot
ggplot(dat, aes(x = PETotal,
                y = DV3,
                color = IV7Di)) +
  theme_bw() +
  labs(x = "P to E",
       y = "Sustainability",
       color = "Agent Tolerance for Ambiguity") +
  geom_smooth(method = "lm",
              se = F) #turn it into lines
#When agent has low TA, prescriptive is more sustainable
#when agent has high TA, elicitive is more sustainable

ModelPE47 <- lm(DV4~IV7Di + PETotal + IV7Di:PETotal, data = dat)
summary(ModelPE47) #Significant at the p <.01 level

#Create interaction plot
ggplot(dat, aes(x = PETotal,
                y = DV4,
                color = IV7Di)) +
  theme_bw() +
  labs(x = "P to E",
       y = "Cultural Sensitivity",
       color = "Agent Tolerance for Ambiguity") +
  geom_smooth(method = "lm",
              se = F) #turn it into lines
#When agent has low TA, prescriptive is more culturally sensitive
#when agent has high TA, elicitive is more culturally sensitive

ModelPE57 <- lm(DV5~IV7Di + PETotal + IV7Di:PETotal, data = dat)
summary(ModelPE57) #Significant, not a moderator for DV5

#Create interaction plot
ggplot(dat, aes(x = PETotal,
                y = DV5,
                color = IV7Di)) +
  theme_bw() +
  labs(x = "P to E",
       y = "Cost Effectiveness",
       color = "Agent Tolerance for Ambiguity") +
  geom_smooth(method = "lm",
              se = F) #turn it into lines
#When agent has low TA, prescriptive is more cost effective
#when agent has high TA, elicitive is more cost effective, but not that much of difference

ModelPE67 <- lm(DV6~IV7Di + PETotal + IV7Di:PETotal, data = dat)
summary(ModelPE67) #Significant, already with 2 way ANOVA

ModelPE87 <- lm(DV8_QualiGenEff~IV7Di + PETotal + IV7Di:PETotal, data = dat)
summary(ModelPE87) #significant at the p <.001 level

#Create interaction plot
ggplot(dat, aes(x = PETotal,
                y = DV8_QualiGenEff,
                color = IV7Di)) +
  theme_bw() +
  labs(x = "P to E",
       y = "Coded Effectiveness",
       color = "Agent Tolerance for Ambiguity") +
  geom_smooth(method = "lm",
              se = F) #turn it into lines
#When agent has low TA, prescriptive is coded to be slightly more effective
#when agent has high TA, elicitive is coded to be much more effective

ModelPE97 <- lm(DV9_QualiSustain~IV7Di + PETotal + IV7Di:PETotal, data = dat)
summary(ModelPE97) #significant at the p = .03 lvl

#Create interaction plot
ggplot(dat, aes(x = PETotal,
                y = DV9_QualiSustain,
                color = IV7Di)) +
  theme_bw() +
  labs(x = "P to E",
       y = "Coded Sustainability",
       color = "Agent Tolerance for Ambiguity") +
  geom_smooth(method = "lm",
              se = F) #turn it into lines
#When agent has low TA, prescriptive is coded to be more sustainable
#when agent has high TA, elicitive is coded to be more sustainable



#################
#CFA
#################

#first we need to run correlations between items
library(Hmisc)
correlations <- rcorr(as.matrix(select(dat, 2:17)))
library(corrplot)
corrplot(correlations$r, type = "upper", order = "hclust",
         p.mat = correlations$r, sig.level = 0.01, insig = "blank")
corrplot(correlations$r, type = "lower", method = "number",
         col = "black", diag = FALSE, t1.pos = "n", cl.pos = "n")
#Not super promising

install.packages("lavaan")
library(lavaan)
cfamodel <- '
ASkill = ~AgentQ65 + AgentQ67 + AgentQ66 + AgentQ70 + AgentQ71
ATA = ~AgentQ73 + AgentQ76 
ACollect = ~AgentQ74 + AgentQ78 + AgentQ80
AValues = ~AgentQ75 + AgentQ77 
APartner = ~AgentQ68 + AgentQ69
'
fit <- cfa(cfamodel, data = dat)
summary(fit, fit.measures = TRUE, standardized = TRUE)
#model doesnt look so good


###Let's try CFA with the 4 dimensions
cfacca <- '
Agent = ~AgentQ65 + AgentQ67 + AgentQ66 + AgentQ68+ AgentQ69+ AgentQ70 + AgentQ71 + AgentQ73 + AgentQ76 + AgentQ74 + AgentQ78 + AgentQ80 + AgentQ75 + AgentQ77
Stakeholders = ~StakeQ52 + StakeQ53 + StakeQ54 + StakeQ55 + StakeQ56 + StakeQ57 + StakeQ58 + StakeQ59 +StakeQ60 + StakeQ61+ StakeQ62
Context = ~ContextQ23 + ContextQ24 + ContextQ25 + ContextQ26 + ContextQ27 + ContextQ28
Process = ~ProcessQ30 + ProcessQ31'
fitcca <- cfa(cfacca, data = dat)
summary(fitcca, fit.measures = TRUE, standardized = TRUE)
#does not fit too well
#For the Agent, Q73, 76, and 77 are negative, separating 73+76 into tolerance for ambiguity
#For Stake, Q52, 55, and 56 are the only positives
#Process are not related

##Let's try again
cfacca2 <- '
Agent = ~AgentQ65 + AgentQ67 + AgentQ66 + AgentQ68+ AgentQ69+ AgentQ70 + AgentQ71 + AgentQ74 + AgentQ78 + AgentQ80 + AgentQ75
TA = ~AgentQ73 + AgentQ76 + StakeQ55
EgaVal = ~AgentQ77 + StakeQ56
Stakeholders = ~StakeQ53 + StakeQ54 + StakeQ57 + StakeQ58 + StakeQ59 +StakeQ60 + StakeQ61+ StakeQ62
Context = ~ContextQ23 + ContextQ24 + ContextQ25 + ContextQ26 + ContextQ27 + ContextQ28
Process1 = ~ProcessQ30
Process2 = ~ProcessQ31'
fitcca2 <- cfa(cfacca2, data = dat)
summary(fitcca2, fit.measures = TRUE, standardized = TRUE)
# It's a total fail but at least now I know how to run CFA

