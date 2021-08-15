library(readr)
library(quantreg)

sessionInfo()


# file.choose()

setwd("C:\\Users\\98001\\Desktop\\dissertation\\R\\Quantile regression")
getwd()

#data1 <- read_csv("data - qr.csv", na = "#N/A")
#data1 <- na.omit(as.data.frame(data1))
data1 <- read_csv("data - qr.csv")

colnames(data1)

# Transform data type

#for(i in c(1:3)){
#  data1[, i] <- factor(data1[, i])
#}


# Build the function to test the model

QLR.function<-function(object,hs=TRUE,mod=T){
  mt<-terms(object)
  m<-model.frame(object)
  y<-model.response(m)
  x<-model.matrix(mt,m,contrasts=object$contrasts)
  n<-length(y)
  tau<-object$tau
  coef<-coefficients(object)
  method<-object$method
  p<-dim(x)[2]
  rdf<-p-1
  res<-object$residuals
  fitO<-rq(y~1,method=method,tau=tau)
  v1<-object$rho
  v0<-fitO$rho
  if(mod){
    sps<-as.numeric((summary(object,se="iid",cov=T,hs=hs))$scale)
    sp<-1/sps
  }
  else{
    sps<-as.numeric((summary(object,se="ker",cov=T))$scale)
    sp<-1/sps
  }
  f<-2*(v0-v1)/(sp*tau*(1-tau))
  Pvalue<-1-pchisq(f,rdf)
  structure(list(tau=tau,QLR.Statistic=f,p.value=Pvalue))
}





# Build the function to calculate the fitness of the model

Fitness.function<-function(object){
  mt<-terms(object)
  m<-model.frame(object)
  y<-model.response(m)
  tau<-object$tau
  coef<-coefficients(object)
  method<-object$method
  fit0<-rq(y~1,method=method,tau=tau)
  v1<-object$rho
  v0<-fit0$rho
  R1<- 1-v1/v0
  structure(list(tau=tau,R1=R1))
}


# Run quantile regression

fit25 <- rq(FirstPumpArriving_AttendanceTime ~ ., 
            tau = c(0.25), 
            data = data1)
fit50 <- rq(FirstPumpArriving_AttendanceTime ~ ., 
            tau = c(0.5), 
            data = data1)
fit75 <- rq(FirstPumpArriving_AttendanceTime ~ ., 
            tau = c(0.75), 
            data = data1)


# Test the model
QLR.function(fit25)

#$tau
#[1] 0.25

#$QLR.Statistic
#[1] 1138.531

#$p.value
#[1] 0


QLR.function(fit50)

#$tau
#[1] 0.5

#$QLR.Statistic
#[1] 1245.152

#$p.value
#[1] 0

QLR.function(fit75)
#$tau
#[1] 0.75

#$QLR.Statistic
#[1] 891.3979

#$p.value
#[1] 0


# Calculate the fitness of the model
Fitness.function(fit25)

# $tau
#[1] 0.25

#$R1
#[1] 0.5963086


Fitness.function(fit50)

#$tau
#[1] 0.5

#$R1
#[1] 0.6172921



Fitness.function(fit75)

#$tau
#[1] 0.75

#$R1
#[1] 0.6344976


# Summary the results and do the hypothesis testing
summary(fit25, se = "nid")

#tau: [1] 0.25

#Coefficients:
#                                                  Value     Std. Error t value   Pr(>|t|) 
#(Intercept)                                       176.44857  12.30895   14.33498   0.00000
#Index_of_Multiple_Deprivation_Score                -0.06104   0.10876   -0.56127   0.57481
#Age_65                                              0.00386   0.01529    0.25265   0.80062
##Children_15                                        0.00893   0.01009    0.88504   0.37647
#Road_density                                       -0.32001   0.29645   -1.07945   0.28080
#Street_network_density                              0.05138   0.02165    2.37347   0.01792
#Household_spaces_with_at_least_one_usual_resident  -0.01004   0.00816   -1.22961   0.21931
#Traffic_light_density                               0.04602   0.05253    0.87613   0.38130
#Distance                                            0.08029   0.00185   43.30868   0.00000
#Winter                                             -0.36187   0.19372   -1.86794   0.06224
#Commute                                            -0.27757   0.19114   -1.45218   0.14695
#Weekend                                             0.08363   0.18308    0.45680   0.64797
#Fire_Station_number                                 0.59945   0.30625    1.95739   0.05075

summary(fit50, se = "nid")

#tau: [1] 0.5

#Coefficients:
#                                                  Value     Std. Error t value   Pr(>|t|) 
#(Intercept)                                       174.79905  14.81774   11.79661   0.00000
#Index_of_Multiple_Deprivation_Score                 0.08422   0.13385    0.62920   0.52945
#Age_65                                              0.01641   0.01782    0.92090   0.35746
#Children_15                                        -0.00509   0.01052   -0.48377   0.62872
#Road_density                                       -0.21796   0.38597   -0.56471   0.57248
#Street_network_density                              0.03715   0.02133    1.74185   0.08203
#Household_spaces_with_at_least_one_usual_resident  -0.00569   0.00775   -0.73430   0.46304
#Traffic_light_density                              -0.00346   0.05400   -0.06408   0.94892
#Distance                                            0.08304   0.00228   36.38417   0.00000
#Winter                                             -0.17568   0.24131   -0.72805   0.46686
#Commute                                             0.12162   0.24930    0.48784   0.62584
#Weekend                                            -0.08403   0.22647   -0.37106   0.71072
#Fire_Station_number                                 0.72624   0.35518    2.04469   0.04130

summary(fit75, se = "nid")

#tau: [1] 0.75

#Coefficients:
#                                                  Value     Std. Error t value   Pr(>|t|) 
#(Intercept)                                       203.66720  17.05576   11.94126   0.00000
#Index_of_Multiple_Deprivation_Score                -0.20937   0.14961   -1.39942   0.16218
#Age_65                                             -0.01071   0.02096   -0.51084   0.60965
#Children_15                                         0.01073   0.01456    0.73728   0.46123
#Road_density                                        0.00807   0.42659    0.01893   0.98490
#Street_network_density                              0.00403   0.02978    0.13541   0.89233
#Household_spaces_with_at_least_one_usual_resident  -0.00673   0.01069   -0.62959   0.52919
#Traffic_light_density                              -0.02510   0.03275   -0.76634   0.44376
#Distance                                            0.08517   0.00232   36.73664   0.00000
#Winter                                             -0.09104   0.24397   -0.37317   0.70915
#Commute                                            -0.27209   0.26951   -1.00957   0.31309
#Weekend                                            -0.07842   0.23688   -0.33104   0.74073
#Fire_Station_number                                 0.49072   0.42481    1.15517   0.24846



# Analysis of variance: Compare whether independent variables have the same influence 
#                        mechanism on dependent variables at different quantiles
anova(fit25,fit50,fit75)

#Joint Test of Equality of Slopes: tau in {  0.25 0.5 0.75  }
#
#  Df Resid Df F value   Pr(>F)   
#1 24     1887  1.4127 0.08828 .
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# Plot the comparison of estimated values of coefficients at different quantiles
fitplot1 = summary( rq(FirstPumpArriving_AttendanceTime ~ Winter, tau = c(0.05,0.25,0.5,0.75,0.95), data = data1) )
plot(fitplot1, mfrow = c(1,1))
fitplot2 = summary( rq(FirstPumpArriving_AttendanceTime ~ Weekend, tau = c(0.05,0.25,0.5,0.75,0.95), data = data1) )
plot(fitplot2, mfrow = c(1,1))
fitplot3 = summary( rq(FirstPumpArriving_AttendanceTime ~ Commute, tau = c(0.05,0.25,0.5,0.75,0.95), data = data1) )
plot(fitplot3, mfrow = c(1,1))
fitplot4 = summary( rq(FirstPumpArriving_AttendanceTime ~ Distance, tau = c(0.05,0.25,0.5,0.75,0.95), data = data1) )
plot(fitplot4, mfrow = c(1,1))
fitplot5 = summary( rq(FirstPumpArriving_AttendanceTime ~ Road_density, tau = c(0.05,0.25,0.5,0.75,0.95), data = data1) )
plot(fitplot5, mfrow = c(1,1))
fitplot6 = summary( rq(FirstPumpArriving_AttendanceTime ~ Street_network_density, tau = c(0.05,0.25,0.5,0.75,0.95), data = data1) )
plot(fitplot6, mfrow = c(1,1))
fitplot7 = summary( rq(FirstPumpArriving_AttendanceTime ~ Traffic_light_density, tau = c(0.05,0.25,0.5,0.75,0.95), data = data1) )
plot(fitplot7, mfrow = c(1,1))
fitplot8 = summary( rq(FirstPumpArriving_AttendanceTime ~ Age_65, tau = c(0.05,0.25,0.5,0.75,0.95), data = data1) )
plot(fitplot8, mfrow = c(1,1))
fitplot9 = summary( rq(FirstPumpArriving_AttendanceTime ~ Children_15, tau = c(0.05,0.25,0.5,0.75,0.95), data = data1) )
plot(fitplot9, mfrow = c(1,1))
fitplot10 = summary( rq(FirstPumpArriving_AttendanceTime ~ Household_spaces_with_at_least_one_usual_resident, tau = c(0.05,0.25,0.5,0.75,0.95), data = data1) )
plot(fitplot10, mfrow = c(1,1))
fitplot11 = summary( rq(FirstPumpArriving_AttendanceTime ~ Index_of_Multiple_Deprivation_Score, tau = c(0.05,0.25,0.5,0.75,0.95), data = data1) )
plot(fitplot11, mfrow = c(1,1))
fitplot12 = summary( rq(FirstPumpArriving_AttendanceTime ~ Fire_Station_number, tau = c(0.05,0.25,0.5,0.75,0.95), data = data1) )
plot(fitplot11, mfrow = c(1,1))

