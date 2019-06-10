library (tidyverse)
library (brms)
source("LoadPemm.R")

ggplot(assdata, aes(x=time, fill=pl))+geom_bar()

ggplot(data = assdata, aes(x = pl, fill = ..x..)) +  geom_bar()

prior_ma <- prior(normal(0, 5), class = "b") +
  prior(normal(0, 5), class = "Intercept")

fit1 <- brm(pl ~ time+(1|Persoon)+(1|variable), data = assdata, prior= prior_ma, family=cumulative(link= "logit", threshold="flexible"))
summary(fit1)
plot(fit1)
plot(marginal_effects(fit1, categorical = TRUE))
# marginal_smooths(fit1)
brms::pp_check(fit1)


library(tidybayes)
get






