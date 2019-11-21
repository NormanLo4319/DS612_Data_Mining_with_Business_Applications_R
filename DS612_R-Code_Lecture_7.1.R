require(ISLR)
attach(Wage)


fit.Poly.Basic = lm(wage~age+I(age^2)+I(age^3)+I(age^4),data = Wage)
summary(fit.Poly.Basic)


fit.Poly =lm(wage~poly(age,4),data = Wage)


fit.Poly =lm(wage~poly(age,4),data = Wage)
summary(fit.Poly)


agelims = range(age)
age.grid=seq(agelims[1],to=agelims[2])
preds = predict(fit.Poly,newdata = list(age=age.grid),se = TRUE)
names(preds)
se.bands = cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
plot(age,wage,col = "darkgray")
lines(age.grid,preds$fit,lwd=2,col="blue") #lwd is linewidth
matlines(age.grid,se.bands,col="blue",lty=2)  #lty shows type of line lty = 2 is used for broken lines


################Using Anova in Nested sequence of Models########################
fita = lm(wage~education,data = Wage)
fitb = lm(wage~education + age,data = Wage)
fitc = lm(wage~education + poly(age,2),data = Wage)
fitd = lm(wage~education + poly(age,3),data = Wage)
fite = lm(wage~education + poly(age,4),data = Wage)

anova(fita,fitb,fitc,fitd,fite)




#### Logistic Regression ########
fit.Logistics = glm(I(wage > 250) ~ poly(age,3), data =Wage , family = binomial)
summary(fit.Logistics)
preds.Logistics = predict(fit.Logistics,newdata = list(age=age.grid),se = TRUE)
names(preds.Logistics)
se.bands = preds.Logistics$fit +cbind(fit = 0, lower = -2*preds.Logistics$se.fit,upper = 2*preds.Logistics$se.fit)
se.bands[1:5,]

#Convert logistics to probabilities
prob.bands = exp(se.bands)/(1+exp(se.bands))
prob.bands[1:5,]

matplot(age.grid,prob.bands,col="blue",lwd=c(2,1,1),lty=c(1,2,2),type="l")


##### Fitting step functions  ###
#In order to fit a step function, we need to use the cut() function
?cut
table(cut(age,4))
fit.Step = lm(age~cut(age,4),data=Wage)
summary(fit.Step)