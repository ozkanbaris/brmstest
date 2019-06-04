library (tidyverse)
library (brms)

a<-c("1","2")
b<-c("1","2","3")
rat1<-factor(sample(a,50,TRUE))
rat2<-factor(sample(b,50,TRUE))

subject<-rep(seq(1,50),2)
rating<-c(rat1,rat2)
rep(seq(1,50),2)

ab_options<-c("1","2")
ab <- factor(c(seq(1,50),seq(2,50)), ordered = FALSE)

dat <- data.frame(rating, ab,subject)

mean_ls <- c(30, 60, 70, 75)
ls <- mean_ls[income] + rnorm(100, sd = 7)


fit1 <- brm(ls ~ mo(income)+ab+(1|subject), data = dat)
summary(fit1)
plot(fit1, pars = "simo")
plot(marginal_effects(fit1))
