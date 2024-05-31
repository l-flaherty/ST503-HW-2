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

#From ?faraway->uswages#
#Wage is real (weekly) wage in dollars deflated by PCE to 1992 base#
#educ and exper is in years#
#race is 1 if black, 0 if white (no other races)#
#smsa is 1 if living in metro area, else 0#
#ne, mw, we, so are location (1 if live in ne, 0 else)#
#pt is 1 if part time, 0 if not#




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
gambling_regression=lm(gamble ~ ., data=teengamb)      #response ~ predictors (all other columns)#         
summary(gambling_regression)
summary(gambling_regression)$r.squared                 #variation explained by predictors#



###3a. Coefficients (automatic vs by hand just for practice)###
gambling_coef=gambling_regression$coefficients          #calculated from lm()$

x=model.matrix(gamble ~ ., data=teengamb)               #try to calculate by hand#
y=teengamb$gamble
manual_coef=solve(t(x) %*%x)%*%t(x)%*% y                #bhat is (X^TX)^{-1}X^T Y if X invertible# 

compare_coef=round(data.frame(                          #theoretical matches results#
  manual_coef, gambling_coef),2)    
compare_coef



###3b. Predictions (automatic vs by hand just for practice)###
gambling_yhat=predict(gambling_regression, teengamb)          #calculated from lm()#
predict_df=data.frame(
  name=paste0("Subject_", 1:nrow(teengamb)),
  gambling_yhat
)
predict_df=predict_df[order(-predict_df$gambling_yhat),]      #sort large to small#

manual_yhat=vector()                                          #initialize#
for(i in 1:nrow(teengamb)) {                                  #get sumproduct of coeff and obs#
  manual_yhat[i]=
    sum(teengamb[i,-which(names(teengamb)=="gamble")]*
      as.vector(gambling_coef)[-1])+
      as.vector(gambling_coef)[1]
}

compare_yhat=round(data.frame(gambling_yhat, manual_yhat),2)  #theoretical matches results#
compare_yhat



#3c. Residual SE (auto vs by hand just for practice)#
res=gambling_regression$residual
sse=sum(res^2)                       #or e^Te#
n=nrow(x)
r=qr(x)$rank
sigmasq=sse/(n-r)                    #estimate error variance with \sigma^2=(e^Te) / (n-rank(x))#
manual_sigma=sqrt(sigmasq)

manual_sigma
summary(gambling_regression)$sigma



###3d. Standard Errors (auto vs by hand just for practice; cov(beta)=sigma^2(X^TX)^{-1})###
gambling_se=summary(gambling_regression)$coefficients[, "Std. Error"]  

sigma_hat=summary(gambling_regression)$sigma
manual_se=sigma_hat*sqrt(diag(solve(t(x)%*%x)))       #"by hand" calc#
manual_se                                             #SE(\beta_j)=\sigma\sqrt(\diag(X^TX^{-1}))#

compare_se=data.frame(gambling_se, manual_se)
compare_se



###3e. Residuals###
gambling_yhat=predict(gambling_regression, teengamb)          #calculated automatically from lm()#
gambling_yobs=teengamb$gamble

residuals=data.frame(
  names=paste0("Subject_", 1:nrow(teengamb)),
  yhat=gambling_yhat,
  yobs=gambling_yobs,
  error=gambling_yobs-gambling_yhat
)
residuals=residuals[order(-residuals$error),]
residuals

mean(residuals$error)                                           #virtually 0#
median(residuals$error)                                         #about -1.45#



###3f. Correlations and Coefficients###
cor(residuals$yhat, residuals$error)                            #virtually 0#
cor(teengamb[order(teengamb$gamble),"income"],                  #about 0.03#
    residuals[order(residuals$yobs),"error"])

coef(gambling_regression)["sex"]





#####4. Wage Regression#####
wage_regression=lm(wage~ educ+exper, data=uswages)
wage_regression$coefficients
summary(wage_regression)

log_wage_regression=lm(log(wage)~ educ+exper, data=uswages)
summary(log_wage_regression)
