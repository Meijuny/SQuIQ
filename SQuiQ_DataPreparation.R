library(lavaan)
library(dplyr)

##read the data in:
SQuIQ_data<-read.csv("./Data_Surveystudy_62p_19122024.csv")

##############################################################################
############## Make factor scores for all 4 factors  #########################
##############################################################################

#CoRegulation
CoRegulation_MM3<-'
CoRegulation=~SQuIQ23+SQuIQ3+SQuIQ4+SQuIQ26+SQuIQ25+SQuIQ24+SQuIQ2+SQuIQ5+SQuIQ31

SQuIQ3~~SQuIQ4
SQuIQ26~~SQuIQ2
'

CoRegulation_fit3<-cfa(data = SQuIQ_data,
                       model = CoRegulation_MM3,
                       estimator="MLR")

summary(CoRegulation_fit3, fit.measures=T, standardized=T)

CoRegulation_factorScores<-lavPredict(CoRegulation_fit3, assemble = T)

CoRegulation_factorScores <- data.frame(
  ID = SQuIQ_data$ID,
  CoRegulation = CoRegulation_factorScores[, "CoRegulation"]
)

#Sensitive Responsivity
##reverse the SQuIQ19 & SQuIQ20 scale
SQuIQ_data$SQuIQ19<-5-SQuIQ_data$SQuIQ19
SQuIQ_data$SQuIQ20<-5-SQuIQ_data$SQuIQ20

SensitiveResponsivity_MM2<-'
SensitiveResponsivity=~SQuIQ14+SQuIQ28+SQuIQ22+SQuIQ19+SQuIQ20+SQuIQ12+SQuIQ13+SQuIQ11+SQuIQ21

SQuIQ19 ~~ SQuIQ20
'

SensitiveResponsivity_fit2<-cfa(data = SQuIQ_data,
                                model=SensitiveResponsivity_MM2,
                                estimator="MLR")

summary(SensitiveResponsivity_fit2, fit.measures=T, standardized=T)

SensitiveResponsivity_factorScores<-lavPredict(SensitiveResponsivity_fit2, assemble = T)

SensitiveResponsivity_factorScores <- data.frame(
  ID = SQuIQ_data$ID,
  SensitiveResponsivity = SensitiveResponsivity_factorScores[, "SensitiveResponsivity"]
)

##Affective Connectedness
AffectiveConnect_MM1<-'
AffectiveConnect=~SQuIQ33+SQuIQ35+SQuIQ37+SQuIQ32+SQuIQ27+SQuIQ10+SQuIQ34
'

AffectiveConnect_fit1<-cfa(data = SQuIQ_data,
                           model = AffectiveConnect_MM1,
                           estimator="MLR")

summary(AffectiveConnect_fit1, fit.measures=T, standardized=T)

AffectiveConnect_factorScores<-lavPredict(AffectiveConnect_fit1, assemble = T)

AffectiveConnect_factorScores <- data.frame(
  ID = SQuIQ_data$ID,
  AffectiveConnect = AffectiveConnect_factorScores[, "AffectiveConnect"]
)

##Contact Stability
ContactStability_MM1<-'
ContactStability=~SQuIQ7+SQuIQ8+SQuIQ6+SQuIQ9+SQuIQ1
'

ContactStability_fit1<-cfa(data = SQuIQ_data,
                           model = ContactStability_MM1,
                           estimator="MLR")

summary(ContactStability_fit1, fit.measures=T, standardized=T)

ContactStability_factorScores<-lavPredict(ContactStability_fit1, assemble = T)

ContactStability_factorScores <- data.frame(
  ID = SQuIQ_data$ID,
  ContactStability = ContactStability_factorScores[, "ContactStability"]
)

##merge all factor scores together
FactorScoresDF<-merge(CoRegulation_factorScores, SensitiveResponsivity_factorScores,
                      by.x = "ID", by.y = "ID")

FactorScoresDF<-merge(FactorScoresDF, AffectiveConnect_factorScores,
                      by.x = "ID", by.y = "ID")

FactorScoresDF<-merge(FactorScoresDF, ContactStability_factorScores,
                      by.x = "ID", by.y = "ID")

rm(list=c("AffectiveConnect_factorScores","AffectiveConnect_fit1","AffectiveConnect_MM1",
          "ContactStability_factorScores","ContactStability_fit1","ContactStability_MM1",
          "CoRegulation_factorScores","CoRegulation_fit3","CoRegulation_MM3",
          "SensitiveResponsivity_factorScores", "SensitiveResponsivity_fit2",
          "SensitiveResponsivity_MM2","SQuIQ_data"))

################################################################################
############## Regression Analysis with complete data  #########################
################################################################################

SQuIQ_FullData<-read.csv("./SurveyData_62p_WithCFA.csv")

##select the variables needed for the regression:
SQuIQ_FullData<-SQuIQ_FullData %>%
  dplyr::select(ID,
                SCIBI_FRIEND, ##Staff Friendly Behavior
                SCIBI_NEGBEH, ##staff hostile behavior
                D.QEL_DeepAct, ##Staff Emotional labour - Deep acting
                COMMIT_SelfEff_BuildingCloseness, #Staff Self-efficacy - Building closeness
                COMMIT_SelfEff_ReflectiveFunctioning, #Staff Self-efficacy - Reflective functioning
                Client_LevelComm_Cat, ##Client Level communication
                Client_Level.ID_Cat, ##Client level of intellectual disability
                WORKLOAD_Total, ##workload
  )

##merge the SQuIQ data with the factor score data
SQuIQ_FullData<-merge(SQuIQ_FullData, FactorScoresDF,
                      by.x = "ID", by.y = "ID")

##make the dummy for Client Level communication and Client level of intellectual disability
SQuIQ_FullData<-SQuIQ_FullData %>%
  mutate(Client_LevelComm_Level2=case_when(
    Client_LevelComm_Cat==1 ~ 0,
    Client_LevelComm_Cat==2 ~ 1,
    Client_LevelComm_Cat==3 ~ 0
  ),
  Client_LevelComm_Level3=case_when(
    Client_LevelComm_Cat==1 ~ 0,
    Client_LevelComm_Cat==2 ~ 0,
    Client_LevelComm_Cat==3 ~ 1
  ))

SQuIQ_FullData<-SQuIQ_FullData %>%
  mutate(Client_Level.ID_level2=case_when(
    Client_Level.ID_Cat == 1 ~ 0,
    Client_Level.ID_Cat == 2 ~ 1
  ))

#write out the data in csv
write.csv(SQuIQ_FullData,
          file = "./SQuIQ_forPython.csv",
          row.names = F)
