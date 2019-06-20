# library("brms")
# library("tidyverse")
# library("ordPens")
# data("ICFCoreSetCWP")
# data(Howell1)
# cwp <- ICFCoreSetCWP
# prior1 <- prior(normal(0, 10), class = "b") +
#   prior(dirichlet(1, 1, 1), class = "simo", coef = "mod4501") +
#   prior(dirichlet(1, 1, 1, 1), class = "simo", coef = "mod4551")
# fit1 <- brm(phcs ~ mo(d450) + mo(d455), data = cwp, prior = prior1)

library (tidyverse)
library (brms)

a<-c("1","2","3","4")

ratt1<-sample(a,50,TRUE,c(3,3,2,2))
ratt2<-sample(a,50,TRUE,c(2,2,3,3))

rating<-factor(c(ratt1,ratt2), ordered=TRUE)
subject<-factor(rep(seq(1,50),2))

t <- factor(c(rep("t1",50),rep("t2",50)), ordered = FALSE)
dat <- data.frame(rating, t,subject)

ggplot(dat, aes(x=t, fill=rating))+geom_bar()
# 
# ggplot(data = dat, aes(x = rating, colorfill = ..x..)) +
#   geom_bar()


prior_ma <- prior(normal(0, 5), class = "b") +
  prior(normal(0, 5), class = "Intercept")

fit1 <- brm(rating ~ t +(1|subject), data = dat, prior= prior_ma, family=cumulative(link= "logit", threshold="flexible"))
fit2 <- brm(rating ~ cs(t) +(1|subject), data = dat, prior= prior_ma, family=acat(link= "logit", threshold="flexible"))

summary(fit1)
summary(fit2)
plot(fit1)
plot(fit2)
marginal_effects(fit2, categorical = TRUE)









                                      