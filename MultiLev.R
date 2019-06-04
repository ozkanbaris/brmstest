library(rethinking)
data(reedfrogs)
d <- reedfrogs

rm(reedfrogs)
detach(package:rethinking, unload = T)
library(brms)
library(tidyverse)

d %>%
  glimpse()

d <- 
  d %>%
  mutate(tank = 1:nrow(d)) %>% select (tank,surv,density)
b12.2 <- 
  brm(data = d, family = binomial,
      surv | trials(density) ~ 1 + (1 | tank),
      prior = c(prior(normal(0, 1), class = Intercept),
                prior(cauchy(0, 1), class = sd)),
      iter = 4000, warmup = 1000, chains = 4, cores = 4,
      seed = 12)
b12.2 <- add_criterion(b12.2, "waic")
plot(b12.2)

w <- loo_compare(b12.2, criterion = "waic")


dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS=-O3 -march=native -mtune=native",
    if( grepl("^darwin", R.version$os)) "CXX14FLAGS += -arch x86_64 -ftemplate-depth-256" else 
      if (.Platform$OS.type == "windows") "CXX11FLAGS=-O3 -march=native -mtune=native" else
        "CXX14FLAGS += -fPIC",
    file = M, sep = "\n", append = TRUE)