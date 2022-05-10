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
# The PE group does not have significant effect on DV5 


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


########################################################################
#Mutliple Hierarchical Regression Controlling for Individual Differences
########################################################################


##First check correlations of Agent w P-E Total
cor.test(dat$AgentTotal, dat$PETotal) #significant, p <.001 , r = -.41

#Models

AControlPE <- lm(PETotal~ AgentTotal, data = dat) #Control for agent differences
summary(AControlPE)

#****************Stakeholders**************#

##Urgent Objectives
SControl52 <- lm(PETotal ~ AgentTotal + StakeQ52, data = dat)
summary(SControl52)
anova(SControl52, AControlPE) #Still not significant


##
SControl60 <- lm(PETotal ~ AgentTotal + StakeQ60, data = dat)
summary(SControl60)
anova(SControl60, AControlPE) #Not significant


##
SControl61 <- lm(PETotal ~ AgentTotal + StakeQ61, data = dat)
summary(SControl61)
anova(SControl61, AControlPE) #Not significant

##
SControl62 <- lm(PETotal ~ AgentTotal + StakeQ62, data = dat)
summary(SControl62)
anova(SControl62, AControlPE) #Also n.s.
#No significance in Stakeholder conditions

##All the contradictory findings are no longer significant when controlling for Agent Differences



#************Context********#
#*n.s are 23, 24, 27

CControl23 <- lm(PETotal ~ AgentTotal + ContextQ23, data = dat)
summary(CControl23)
anova(CControl23, AControlPE) #still n.s.

CControl24 <- lm(PETotal ~ AgentTotal + ContextQ24, data = dat)
summary(CControl24)
anova(CControl24, AControlPE) #still ns

CControl27 <- lm(PETotal ~ AgentTotal + ContextQ27, data = dat)
summary(CControl27)
anova(CControl27, AControlPE) #still ns

#********Process***************#

PControl30 <- lm(PETotal ~ AgentTotal + ProcessQ30, data = dat)
summary(PControl30)
anova(PControl30, AControlPE) #significant


PControl31 <- lm(PETotal ~ AgentTotal + ProcessQ31, data = dat)
summary(PControl31)
anova(PControl31, AControlPE) #significant


#####################################################
####Conditions with Approach in Outcomes Predictions
######################################################


#We will only account for ones that are significant and in line with our predictions


#*******Agent******#

#ASkill and APartner moves in the same direction, so we can just add them together
dat$AgentTotal <- ((dat$ASkill+dat$APartner)/2)

##################
##ANCOVA
###################

#Now for Agent, PE, and Outcomes using linear
#DV1
ModelAPEOut1 <- lm(DV1 ~ AgentTotal + PETotal, data = dat)
summary(ModelAPEOut1)
#Significant, moving onto polynomial
ModelAPEOut12 <- lm(DV1 ~ polym(AgentTotal + PETotal, degree = 2, raw = TRUE), data = dat)
summary(ModelAPEOut12)
#Significant
ModelAPEOut13 <- lm(DV1 ~ polym(AgentTotal + PETotal, degree = 3, raw = TRUE), data = dat)
summary(ModelAPEOut13)

#Compare Models
anova(ModelAPEOut1, ModelAPEOut12) #not significant, stick with linear
anova(ModelAPEOut12, ModelAPEOut13)
#not significant, sticking with Model 2

#Visualization 
par(mfrow=c(1,3))
image(ModelAPEOut12, AgentTotal~PETotal)
contour(ModelAPEOut12, AgentTotal~PETotal)
persp(ModelAPEOut12, AgentTotal~PETotal, zlab = "Effectiveness")
# Agents with high cross cultural facilitation skills tend to produce more effective outcomes
#Agents using more elicitive methods tend to produce more effective outcomes


#DV2
ModelAPEOut2 <- lm(DV2 ~ AgentTotal + PETotal, data = dat)
summary(ModelAPEOut2)
#Significant, moving onto polynomial
ModelAPEOut22 <- lm(DV2 ~ polym(AgentTotal + PETotal, degree = 2, raw = TRUE), data = dat)
summary(ModelAPEOut22)
#Significant
ModelAPEOut23 <- lm(DV2 ~ polym(AgentTotal + PETotal, degree = 3, raw = TRUE), data = dat)
summary(ModelAPEOut23)

#Compare Models
anova(ModelAPEOut2, ModelAPEOut22) #not significant, stay w linear

#Visualization 
par(mfrow=c(1,3))
image(ModelAPEOut2, AgentTotal~PETotal)
contour(ModelAPEOut2, AgentTotal~PETotal)
persp(ModelAPEOut2, AgentTotal~PETotal, zlab = "Effectiveness")
# Agents with high cross cultural facilitation skills tend to produce more effective outcomes
#Agents using more elicitive methods tend to produce more effective outcomes



#*************Context*********#

#Functional Local System, Tightness-Looseness, and Cultural Salience moves in the same direction
#Can just add them together
dat$ContextTotal <- ((dat$ContextQ25 + dat$ContextQ27 + dat$ContextQ28)/3)

#DV1
ModelCPEOut1 <- lm(DV1 ~ ContextTotal + PETotal, data = dat)
summary(ModelCPEOut1)
#Significant, moving onto polynomial
ModelCPEOut12 <- lm(DV1 ~ polym(ContextTotal + PETotal, degree = 2, raw = TRUE), data = dat)
summary(ModelCPEOut12)
#Significant
ModelCPEOut13 <- lm(DV1 ~ polym(ContextTotal + PETotal, degree = 3, raw = TRUE), data = dat)
summary(ModelCPEOut13)

#Compare Models
anova(ModelCPEOut1, ModelCPEOut12) #not significant, stick with linear


#Visualization 
par(mfrow=c(1,3))
image(ModelCPEOut1, ContextTotal~PETotal)
contour(ModelCPEOut12, ContextTotal~PETotal)
persp(ModelCPEOut12, ContextTotal~PETotal, zlab = "Effectiveness")



###############################################
#Multiple Hierarchical Regression for Individual differences
###############################################


##First check correlations of Agent w all DVs
cor.test(dat$AgentTotal, dat$DV1) #slightly significant, p = .051, r = .18
cor.test(dat$AgentTotal, dat$DV2) #significant, p <.001, r = .33
cor.test(dat$AgentTotal, dat$DV3) #significant, p <.01, r = .26
cor.test(dat$AgentTotal, dat$DV4) #NOT significant
cor.test(dat$AgentTotal, dat$DV5) #significant, p = .02, r = .21
cor.test(dat$AgentTotal, dat$DV6) #significant, p <.001, r = .44
cor.test(dat$AgentTotal, dat$DV8_QualiGenEff) #significant, p <.01, r = .32
cor.test(dat$AgentTotal, dat$DV9_QualiSustain) #not significant
#Run hierarchical for all Data except DV4
#Run with other variables that are still significant after controlling for Agent

##check correlations of Context w all DVs
cor.test(dat$ContextTotal, dat$DV1) # significant, p = .03, r = .21
cor.test(dat$ContextTotal, dat$DV2) #significant, p < .01, r = .27
cor.test(dat$ContextTotal, dat$DV3) #significant, p = .04, r = .20
cor.test(dat$ContextTotal, dat$DV4) #significant, p =.03, r = .21
cor.test(dat$ContextTotal, dat$DV5) #Not significant
cor.test(dat$ContextTotal, dat$DV6) #Not significant
cor.test(dat$ContextTotal, dat$DV8_QualiGenEff) #kinda significant; p =.05, r = .22
cor.test(dat$ContextTotal, dat$DV9_QualiSustain) # not significant


##check correlations of Stakeholders Cultural Orientation w all DVs
cor.test(dat$SCO, dat$DV1) #Ns
cor.test(dat$SCO, dat$DV2) #NS
cor.test(dat$SCO, dat$DV3) #ns
cor.test(dat$SCO, dat$DV4) #ns
cor.test(dat$SCO, dat$DV5) #ns
cor.test(dat$SCO, dat$DV6) #significant, p <.001; r = .42
cor.test(dat$SCO, dat$DV8_QualiGenEff) #ns
cor.test(dat$SCO, dat$DV9_QualiSustain) #ns



#Also check correlations of P-E w all DVs
cor.test(dat$PETotal, dat$DV1) #not significant
cor.test(dat$PETotal, dat$DV2) #not significant
cor.test(dat$PETotal, dat$DV3) #ns
cor.test(dat$PETotal, dat$DV4) #significant at .02, r =.22
cor.test(dat$PETotal, dat$DV5) #ns
cor.test(dat$PETotal, dat$DV6) #significant at p <.001, r = -.41
cor.test(dat$PETotal, dat$DV8_QualiGenEff) #ns
cor.test(dat$PETotal, dat$DV9_QualiSustain) #ns

#Visualize the data

ggplot(data = dat, mapping = aes(PEGroup,DV1)) +
  geom_boxplot() #looks kind similar
ggplot(data = dat, mapping = aes(PEGroup,DV2)) +
  geom_boxplot() # more variances
ggplot(data = dat, mapping = aes(PEGroup,DV3)) +
  geom_boxplot() #similar ish
ggplot(data = dat, mapping = aes(PEGroup,DV4)) +
  geom_boxplot() #significant difference in P

###########Models################

####DV1 Effectiveness####

AControl1 <- lm(DV1~ AgentTotal, data = dat) #Control for agent differences
summary(AControl1)

ModelPE1 <- lm(DV1~AgentTotal + PETotal, data = dat)
summary(ModelPE1) #PE Significant

ModelContext1 <- lm(DV1 ~ AgentTotal + ContextTotal + PETotal, data = dat) #Control for Context
summary(ModelContext1) #PE significant

ModelProcess1 <- lm(DV1 ~ AgentTotal + ContextTotal + ProcessQ30 + ProcessQ31 + PETotal, data = dat)
summary(ModelProcess1) #PE Significant

anova(AControl1, ModelPE1, ModelContext1, ModelProcess1) #Model Context has the most meaningful changes
#When controlling for Individual differences, and context factors, P-E Model significantly predict DV1



######DV2 Sensitivity & Satisfaction##########


AControl2 <- lm(DV2~ AgentTotal, data = dat) #Control for agent differences
summary(AControl2)

ModelPE2 <- lm(DV2~AgentTotal + PETotal, data = dat)
summary(ModelPE2) #PE Significant

ModelContext2 <- lm(DV2 ~ AgentTotal + ContextTotal + PETotal, data = dat)
summary(ModelContext2) #PE significant

ModelProcess2 <- lm(DV2 ~ AgentTotal + ContextTotal + ProcessQ30 + ProcessQ31 + PETotal, data = dat)
summary(ModelProcess2) #PE Significant

anova(AControl2, ModelPE2, ModelContext2, ModelProcess2) #Model Context has the most meaningful changes
#When controlling for Individual differences, and context factors, P-E Model significantly predict DV2



########DV3 Sustainability########
AControl3 <- lm(DV3~ AgentTotal, data = dat) #Control for agent differences
summary(AControl3)

ModelPE3 <- lm(DV3~AgentTotal + PETotal, data = dat)
summary(ModelPE3) #PE Significant

ModelContext3 <- lm(DV3 ~ AgentTotal + ContextTotal + PETotal, data = dat)
summary(ModelContext3) #PE significant

ModelProcess3 <- lm(DV3 ~ AgentTotal + ContextTotal + ProcessQ30 + ProcessQ31 + PETotal, data = dat)
summary(ModelProcess3) #PE Significant

anova(AControl3, ModelPE3, ModelContext3, ModelProcess3) #Model PE3 has the most meaningful changes
#When controlling for Individual differences, P-E Model significantly predict DV3


#Skip DV4 cause already significant


#########DV5######
AControl5 <- lm(DV5~ AgentTotal, data = dat) #Control for agent differences
summary(AControl5)

ModelPE5 <- lm(DV5~AgentTotal + PETotal, data = dat)
summary(ModelPE5) #PE not Significant

#P-E Model does not predict DV5
#Maybe it's not a linear predictions

ggplot(dat, aes(x = PETotal, y = DV5)) +
  geom_point() #does not look linear; run polynomial or ANOVA

##Shall I try polynomial
ModelPEDV5 <- lm(DV5 ~ PETotal, data = dat)
summary(ModelPEDV5)
#Not significant, moving onto polynomial
ModelPEDV52 <- lm(DV5 ~ polym(PETotal, degree = 2, raw = TRUE), data = dat)
summary(ModelPEDV52)

#Significant
ModelPEDV53 <- lm(DV5 ~ polym(PETotal, degree = 3, raw = TRUE), data = dat)
summary(ModelPEDV53)
#significant

#Compare Models
anova(ModelPEDV52, ModelPEDV52, ModelPEDV53) #signicant, stick with cubic

#Plot
ggplot(dat, aes(x= PETotal, y = DV5)) +
  geom_point()+
  stat_smooth(method='lm', formula = y ~ poly(x, 3), size =1) +
  xlab('PE') +
  ylab('DV5')
# I guess PE has nothing to do with DV5

#########DV6##############
#Also significant but I forgot
AControl6 <- lm(DV6~ AgentTotal, data = dat) #Control for agent differences
summary(AControl6)

ModelPE6 <- lm(DV6~AgentTotal + PETotal, data = dat)
summary(ModelPE6) #PE Significant

ModelContext6 <- lm(DV6 ~ AgentTotal + ContextTotal + PETotal, data = dat)
summary(ModelContext6) #PE significant

ModelProcess6 <- lm(DV6 ~ AgentTotal + ContextTotal + ProcessQ30 + ProcessQ31 + PETotal, data = dat)
summary(ModelProcess6) #PE Significant

anova(AControl6, ModelPE6, ModelContext6, ModelProcess6) #Model Context has the most meaningful changes
#When controlling for Individual differences, and context factors, P-E Model significantly predict DV6


###########DV8 QualiGen Eff###############3

#Run with PE Quant

AControl8 <- lm(DV8_QualiGenEff~ AgentTotal, data = dat) #Control for agent differences
summary(AControl8)

ModelPE8 <- lm(DV8_QualiGenEff~ AgentTotal + PETotal, data = dat)
summary(ModelPE8) #PE not Significant

ModelContext8 <- lm(DV8_QualiGenEff ~ AgentTotal + ContextTotal + PETotal, data = dat)
summary(ModelContext8) #PE not significant

ModelProcess8 <- lm(DV8_QualiGenEff ~ AgentTotal + ContextTotal + ProcessQ30 + ProcessQ31 + PETotal, data = dat)
summary(ModelProcess8) #PE not Significant

#PE Does not predict this

#Run with PE Qual
AControl82 <- lm(DV8_QualiGenEff~ AgentTotal, data = dat) #Control for agent differences
summary(AControl82)

ModelPE82 <- lm(DV8_QualiGenEff~ AgentTotal + DV7_QualiPE, data = dat)
summary(ModelPE82) #PE not Significant

ModelContext82 <- lm(DV8_QualiGenEff ~ AgentTotal + ContextTotal + DV7_QualiPE, data = dat)
summary(ModelContext82) #PE not significant


###########DV9 QualiSustain###############3
AControl9 <- lm(DV9_QualiSustain~ AgentTotal, data = dat) #Control for agent differences
summary(AControl9)

ModelPE9 <- lm(DV9_QualiSustain~ AgentTotal + PETotal, data = dat)
summary(ModelPE9) #PE not Significant



#when run for qualitative
AControl92 <- lm(DV9_QualiSustain~ AgentTotal, data = dat) #Control for agent differences
summary(AControl92)

ModelPE92 <- lm(DV9_QualiSustain~ AgentTotal + DV7_QualiPE, data = dat)
summary(ModelPE92) #PE Significant

cor.test(dat$DV7_QualiPE, dat$DV9_QualiSustain)
#highly significant, p <.001; r = .65





#########################
#Moderation Analysis
##########################

ModelPE12 <- lm(DV1~AgentTotal + PETotal + AgentTotal:PETotal, data = dat)
summary(ModelPE12) #Interaction not significant, not a moderator for DV1

ModelPE22 <- lm(DV2~AgentTotal + PETotal + AgentTotal:PETotal, data = dat)
summary(ModelPE22) #N.S

ModelPE32 <- lm(DV3~AgentTotal + PETotal + AgentTotal:PETotal, data = dat)
summary(ModelPE32) #Interaction not significant, not a moderator for DV3

ModelPE42 <- lm(DV4~AgentTotal + PETotal + AgentTotal:PETotal, data = dat)
summary(ModelPE42) #Interaction not significant, not a moderator for DV4

ModelPE52 <- lm(DV5~AgentTotal + PETotal + AgentTotal:PETotal, data = dat)
summary(ModelPE52) #Interaction not significant, not a moderator for DV5

ModelPE62 <- lm(DV6~AgentTotal + PETotal + AgentTotal:PETotal, data = dat)
summary(ModelPE62) #Interaction not significant, not a moderator for DV6


#In Conclusion, definitely not a moderator; #it's another independent variable




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

