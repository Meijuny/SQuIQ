library(lavaan)

##read the data in:
SQuIQ_data<-read.csv("./Data_Surveystudy_62p_19122024.csv")

################################################################################
######################## CFA for factor 1: Co-Regulation #######################
################################################################################

##Start the first model for co-regulation based on the EFA results
CoRegulation_MM1<-'
CoRegulation=~SQuIQ23+SQuIQ3+SQuIQ4+SQuIQ26+SQuIQ25+SQuIQ24+SQuIQ2+SQuIQ5+SQuIQ31
'

CoRegulation_fit1<-cfa(data = SQuIQ_data,
                      model = CoRegulation_MM1,
                      estimator="MLR")

summary(CoRegulation_fit1, fit.measures=T, standardized=T)

##The fit is unacceptable and inspect the modification indices
CoRegulation_fit1_mi<-modindices(CoRegulation_fit1, standardized = T)

##make the second model to add the error correlations between SQuIQ3~~SQuIQ4
CoRegulation_MM2<-'
CoRegulation=~SQuIQ23+SQuIQ3+SQuIQ4+SQuIQ26+SQuIQ25+SQuIQ24+SQuIQ2+SQuIQ5+SQuIQ31

SQuIQ3~~SQuIQ4
'

CoRegulation_fit2<-cfa(data = SQuIQ_data,
                       model = CoRegulation_MM2,
                       estimator="MLR")

summary(CoRegulation_fit2, fit.measures=T, standardized=T)

##inspect the modification indices
CoRegulation_fit2_mi<-modindices(CoRegulation_fit2, standardized = T)

##make the third model to add the error correlations between SQuIQ26~~SQuIQ2
CoRegulation_MM3<-'
CoRegulation=~SQuIQ23+SQuIQ3+SQuIQ4+SQuIQ26+SQuIQ25+SQuIQ24+SQuIQ2+SQuIQ5+SQuIQ31

SQuIQ3~~SQuIQ4
SQuIQ26~~SQuIQ2
'

CoRegulation_fit3<-cfa(data = SQuIQ_data,
                       model = CoRegulation_MM3,
                       estimator="MLR")

summary(CoRegulation_fit3, fit.measures=T, standardized=T)

##The fit is acceptable, and this will be our final model


################################################################################
############## CFA for factor 2: Sensitive Responsivity  #######################
################################################################################

##start the first model for Sensitive Responsivity based on the EFA results
SensitiveResponsivity_MM1<-'
SensitiveResponsivity=~SQuIQ14+SQuIQ28+SQuIQ22+SQuIQ19+SQuIQ20+SQuIQ12+SQuIQ13+SQuIQ11+SQuIQ21
'

SensitiveResponsivity_fit1<-cfa(data = SQuIQ_data,
                                model=SensitiveResponsivity_MM1,
                                estimator="MLR")

summary(SensitiveResponsivity_fit1, fit.measures=T, standardized=T)


##the fit is unacceptable --> inspect modification indices
SensitiveResponsivity_fit1_mi<-modindices(SensitiveResponsivity_fit1, standardized = T)

##make the second model for Sensitive Responsivity by allowing SQuIQ19 ~~ SQuIQ20
SensitiveResponsivity_MM2<-'
SensitiveResponsivity=~SQuIQ14+SQuIQ28+SQuIQ22+SQuIQ19+SQuIQ20+SQuIQ12+SQuIQ13+SQuIQ11+SQuIQ21

SQuIQ19 ~~ SQuIQ20
'

SensitiveResponsivity_fit2<-cfa(data = SQuIQ_data,
                                model=SensitiveResponsivity_MM2,
                                estimator="MLR")

summary(SensitiveResponsivity_fit2, fit.measures=T, standardized=T)

##The fit is good, and select this as the final model


################################################################################
############## CFA for factor 3: Affective Connectedness  ######################
################################################################################

##Start the first model based on the EFA results:
AffectiveConnect_MM1<-'
AffectiveConnect=~SQuIQ33+SQuIQ35+SQuIQ37+SQuIQ32+SQuIQ27+SQuIQ10+SQuIQ34
'

AffectiveConnect_fit1<-cfa(data = SQuIQ_data,
                           model = AffectiveConnect_MM1,
                           estimator="MLR")

summary(AffectiveConnect_fit1, fit.measures=T, standardized=T)

##The fit is already excellent --> select this as the final model

################################################################################
############## CFA for factor 4: Contact stability  ############################
################################################################################

##start the first model based on the EFA results
ContactStability_MM1<-'
ContactStability=~SQuIQ7+SQuIQ8+SQuIQ6+SQuIQ9+SQuIQ1
'

ContactStability_fit1<-cfa(data = SQuIQ_data,
                           model = ContactStability_MM1,
                           estimator="MLR")

summary(ContactStability_fit1, fit.measures=T, standardized=T)

##The fit is good, select this as the final model