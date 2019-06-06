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

a<-c("1","2","3")

rat1<-sample(a,10,TRUE,c(2/3,1/6,1/6))
rat2<-sample(a,10,TRUE,c(1/5,2/5,2/5))
rating<-factor(c(rat1,rat2), ordered=TRUE)
subject<-factor(rep(seq(1,10),2))

ab <- factor(c(rep(1,10),rep(2,10)), ordered = FALSE)

prior_ma <- prior(normal(0, 5), class = "b") +
  prior(normal(0, 5), class = "Intercept")

dat <- data.frame(rating, ab,subject)


fit1 <- brm(rating ~ ab+(1|subject), data = dat, prior= prior_ma, family=acat(link= "logit", threshold="flexible"))
summary(fit1)
plot(fit1)
plot(marginal_effects(fit1, categorical = TRUE))
marginal_smooths(fit1)
