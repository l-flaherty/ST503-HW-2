##########Written By Liam Flaherty For ST503 HW1##########
#####1. Load Required Packages#####
install.packages("faraway")
install.packages("estimability")
library(faraway)
library(estimability)
summary(teengamb)                     #Get a glimpse of the data#
str(teengamb)                         
summary(uswages)
str(uswages)

#From ?faraway->teengamb
#sex is 0 male, 1 female,
#status is socioeconomic status score based on parents' occupation#
#income is in pounds per week#
#verbal is score out of 12 words#
#gamble is expenditure on gambling in pounds per year#

#From ?faraway->uswages
#Wage is real wage in dollars deflated by PCE to 1992 base
#educ and exper is in years
#race is 1 if black, 0 if white (no other races)
#smsa is 1 if living in metro area, else 0
#ne, mw, we, so are location (1 if live in ne, 0 else)
#pt is 1 if part time, 0 if not




#####2. Estimability######
mymatrix=cbind(c(1,1,1,1,1,1), c(1,1,1,0,0,0),
               c(0,0,0,1,1,1), c(1,0,0,1,0,0),
               c(0,1,0,0,1,0), c(0,0,1,0,0,1))

qr(mymatrix)$rank                                  #4, as derived#

cvec1=c(0,1,-1,0,0,0)                              #Question 1c#
nb1=nonest.basis(mymatrix)                         #use 'estimability' package#
is.estble(cvec1, nb1)

cvec2=c(0,0,0,1,-2,1)                              #Question 1d#
nb2=nonest.basis(mymatrix)                         #use 'estimability' package#
is.estble(cvec2, nb2)
         



         
#####3. Gambling Regression#####
gambling_regression=lm(gamble ~ ., data=teengamb)        #response (y) ~ predictors (all other columns)#         
summary(gambling_regression)
summary(gambling_regression)$r.squared                   #variation explained by predictors#



###3a. Coefficients (automatic vs by hand just for practice)###
gambling_coef=gambling_regression$coefficients                  #calculated automatically from lm()$

x=model.matrix(gamble ~ ., data=teengamb)                       #try to calculate by hand#
y=teengamb$gamble
manual_coef=solve(t(x) %*%x)%*%t(x)%*% y                        #bhat is (X^TX)^{-1}X^T Y if X invertible# 

compare_coef=round(data.frame(manual_coef, gambling_coef),2)    #theoretical matches results#



###3b. predictions (automatic vs by hand just for practice)###
gambling_yhat=predict(gambling_regression, teengamb)             #calculated automatically from lm()#
predict_df=data.frame(
  name=paste0("Subject_", 1:nrow(teengamb)),
  gambling_yhat
)
predict_df=predict_df[order(-predict_df$gambling_yhat),]         #sort large to small#

manual_yhat=vector()                                             #initialize#
for(i in 1:nrow(teengamb)) {                                     #get sumproduct of coeff and observed#
  manual_yhat[i]=
    sum(teengamb[i,-which(names(teengamb)=="gamble")]*
      as.vector(gambling_coef)[-1])+
      as.vector(gambling_coef)[1]
}

compare_yhat=round(data.frame(gambling_yhat, manual_yhat),2)    #theoretical matches results#



###3c. Residuals###
gambling_yhat=predict(gambling_regression, teengamb)             #calculated automatically from lm()#
gambling_yobs=teengamb$gamble

residuals=data.frame(
  names=paste0("Subject_", 1:nrow(teengamb)),
  yhat=gambling_yhat,
  yobs=gambling_yobs,
  error=gambling_yobs-gambling_yhat
)
residuals=residuals[order(-residuals$error),]
residuals

mean(residuals$error)
median(residuals$error)

cor(residuals$yhat, residuals$error)
cor(teengamb[order(teengamb$gamble),"income"], 
    residuals[order(residuals$yobs),"error"])

coef(gambling_regression)["sex"]





#result is $Cov(\hat{beta})=\sigma^2(X^TX)^{-1}$#
#Then SE of any element is $SE(\beta_j)=\sigma\sqrt(\diag(X^TX^{-1}))$#

beta_se=sigma_hat*sqrt(diag(solve(t(x)%*%x)))

summary(gambling_regression)$coefficients[, "Std. Error"]  #calculated by R#
beta_se                                                    #by hand#


#c. Residual SE. estimate error variance with \sigma^2=(e^Te) / (n-rank(x))#

res=gambling_regression$residual
sse=sum(res^2)
n=nrow(x)
r=qr(x)$rank

sigmasq=sse/(n-r)
sigmahat=sqrt(sigmasq)

sigmahat
sigma_hat






max(bhat) #33#









#####4. Wage Regression#####
wage_regression=lm(wage~ educ+exper, data=uswages)
wage_regression
summary(wage_regression)

















____________________________________________________________________________________

#####2. Simple Data Visualization#####
boxplot(teengamb$income, teengamb$gamble/12,                       #Show income and expenses together#
        names=c("Income (Per Week)",                               #Can't use xlab#
                "Gambling Expenditures (Per Month)"),                
        ylab="British Pounds",
        col=c("green", "red"),                                     
        main="Teenage Income And Spending Habits In Britian")

par(mfrow=c(1,2))                                                  #Show 4 plots together#

plot(density(teengamb$status),                                     #Just for variety, hist likely better#
     main="Parental Socioeconomic Status",
     xlab="SES Score", ylab="Density")
polygon(density(teengamb$status), col="gold")                      #fill in for effect#

hist(teengamb$verbal, main="Verbal Definition Score (Out Of 12)",         
     xlab="Score", ylab="Frequency", col="blue")
par(mfrow=c(1,1))                                                  #Put plots back to normal#





#####3. Regression#####
out=lm(gamble ~ ., data=teengamb) #response (y) ~ predictors (x, all other variables)#
summary(out)




#####1. Estimability######
mymatrix=cbind(c(1,1,1,1,1,1), c(1,1,1,0,0,0),
               c(0,0,0,1,1,1), c(1,0,0,1,0,0),
               c(0,1,0,0,1,0), c(0,0,1,0,0,1))
cvec=c(0,1,-1,0,0,0)

nb=nonest.basis(mymatrix)
is.estble(cvec, nb)

qr(mymatrix)$rank


#####4. Sex  Breakdown#####
number.males=sum(teengamb$sex==0)                                  #count total males#
number.females=sum(teengamb$sex==1)                                #count total females#

male=data.frame(c("mean", "standard deviation"),                   #dummy column#
                rbind(                                             #stack rows on top of each other#
                sapply(teengamb[which(teengamb$sex==0),], mean),   #mean of each column, filtered to males#
                sapply(teengamb[which(teengamb$sex==0),], sd)      #sd of each column, filtered to males#
                ))
names(male)=c(paste0("MALE", "(n=", number.males, ")"),            #rename first colummn#
              names(male)[2:ncol(male)])                   

female=data.frame(c("mean", "standard deviation"),                 #dummy column#
                  rbind(                                           #stack rows on top of each other#
                  sapply(teengamb[which(teengamb$sex==1),], mean), #mean of each column, filtered to females#
                  sapply(teengamb[which(teengamb$sex==1),], sd)    #sd of each column, filtered to females#
                  ))
names(female)=c(paste0("FEMALE", "(n=", number.females, ")"),      #rename first colummn#
                names(female)[2:ncol(female)])          

male
female




#####5. Sex Differences Income/Spending Regression#####
maledf=teengamb[which(teengamb$sex==0),]                           #filter to only males#
femaledf=teengamb[which(teengamb$sex==1),]                         #filter to only females#

maleout=lm(maledf$gamble ~ maledf$income)                          #response(y) ~ predictor(x)#
maleslope=maleout[[1]][[2]]                                        #just get numeric output#
maleintercept=maleout[[1]][[1]]                                    #just get numeric output#

femaleout=lm(femaledf$gamble ~ femaledf$income)
femaleslope=femaleout[[1]][[2]]
femaleintercept=femaleout[[1]][[1]]

plot(teengamb$income, teengamb$gamble,                             #predictor(x), response(y)#
     pch=ifelse(teengamb$sex==0, 17, 16),                          #pch is shape of datapoints#
     col=ifelse(teengamb$sex==0, "blue", "hotpink"),               #col differentiates between male female#
     main="Relationship Between Income And Gambling",              
     xlab="Income (Weekly)",
     ylab="Gambling Expenditures (Annually)")
abline(maleintercept, maleslope, col="blue", lwd=2, lty=2)         #add male regression line. lwd is width#
abline(femaleintercept, femaleslope, col="hotpink", lwd=2, lty=2)  #add female reg line. lty is dashed#
legend("topleft", legend=c("Male", "Female"),
       fill=c("blue", "hotpink"))

