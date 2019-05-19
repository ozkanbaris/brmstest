library(tidyverse)
library(rsq)
library(corrplot)
data("hcrabs")
crabsc<-hcrabs %>% mutate(scw=scale(width),cool=if_else(num.satellites>0,1,0))
corrplot(cor(crabsc[, c(5:6)]))
m1priors <- c(
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  prior(student_t(3, 0, 2.5), class = "b")
)

fit_ir1 <- brm(cool ~ scw, data = crabsc, family = bernoulli(),prior = m1priors, seed= 242)
plot(fit_ir1)
