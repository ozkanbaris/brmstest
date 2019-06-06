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

ratt1<-sample(a,100,TRUE,c(3,3,2,2))
ratt2<-sample(a,100,TRUE,c(1,3,3,3))

rating<-factor(c(ratt1,ratt2), ordered=TRUE)
subject<-factor(rep(seq(1,50),2))

t <- factor(c(rep("t1",50),rep("t2",50)), ordered = FALSE)
dat <- data.frame(rating, t,subject)

ggplot(dat, aes(x=t, fill=rating))+geom_bar()

ggplot(data = dat, aes(x = rating, fill = ..x..)) +
  geom_bar()


prior_ma <- prior(normal(0, 5), class = "b") +
  prior(normal(0, 5), class = "Intercept")

fit1 <- brm(rating ~ t+(1|subject), data = dat, prior= prior_ma, family=acat(link= "logit", threshold="flexible"))
summary(fit1)
plot(fit1)
plot(marginal_effects(fit1, categorical = TRUE))
 # marginal_smooths(fit1)
 brms::pp_check(fit1)








                                      