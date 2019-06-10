library(tidyverse)
library("ordinal")
source("LoadPemm.R")
assdata <- assdata %>% mutate (Persoon=factor(Persoon), time= factor(time))

fm1 <- clmm( pl ~ time +(1|Persoon)+(1|variable), data=assdata)
pr1 <- profile(fm1, alpha=1e-4)



