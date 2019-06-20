library(tidyverse)
library(reshape2)

postpem <- read_csv('../Pemm/PEMMPST.csv')
prepem  <- read_csv('../Pemm/PEMMPR.csv') 

melted_post <-
  melt(postpem, id.vars = "Persoon", value.name = "pl")  %>%
  mutate(
    time="t2",
    enabler = str_sub(variable, 1, 1),
    plev = str_sub(variable, 4, 5),
    comp = str_sub(variable, 2, 3)
  )   %>% replace(., is.na(.), 0)
melted_pre <-
  melt(prepem, id.vars = "Persoon", value.name = "pl")   %>%
  mutate(
    time="t1",
    enabler = str_sub(variable, 1, 1),
    plev = str_sub(variable, 4, 5),
    comp = str_sub(variable, 2, 3)
  )  %>% replace(., is.na(.), 0)


assdata<- bind_rows(melted_pre,melted_post) %>%  filter(pl !=0 )%>% 
  mutate(pl= factor(pl, ordered=TRUE, levels= c("1","2","3")))
                    # levels= c("0","1","2","3")))
  assdata<-rename(assdata, compItem = variable)
# 
# 
# postpem<-postpem %>% replace(., is.na(.), 0)
# 
# prepem<-prepem %>% replace(., is.na(.), 0)
# 
# cols <- c(2:53)
# 
# prepem <- lapply(prepem[cols], factor,levels = c("0","1", "2", "3"),ordered = TRUE)
# p1<- likert(as.data.frame(prepem))
# plot(p1, ordered=FALSE)
# 
# 
# postpem <- lapply(postpem[cols], factor,levels = c("0","1", "2", "3"),ordered = TRUE)
# p2<- likert(as.data.frame(postpem))
# plot(p2, ordered=FALSE)



