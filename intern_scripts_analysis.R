doc <- analyis_table
doc
library(tidyverse)
dat <- doc %>% 
  arrange(`Sl#`)
dat

dat$Name <- dat$Name %>% 
  str_remove("Dr.")

dat$Name <-  str_remove(dat$Name," ")
## data2 
dim(dat)
drc <- analyis_table
drc <- drc %>% 
  select(Name, Age)
drc
dim(drc)

merged <- dat %>% 
  inner_join(drc, by = "Name")
merged

dim(merged)

dat$Name 
drc$Name
agg <-  edit(drc)
view(drc)
bn <- intersect(vc, ac)
dc <- str_remove(dat$Name," ")
vc <- str_remove(dc," ")
ac <- str_remove(agg$Name," ")
ac <- str_remove(ac," ")
ac
bn <- intersect(vc, agg$Name)
length(bn)
setdiff(dat$Name, bn)
vc

dat$Name <- vc
dat$Name

agg$Name <- ac

##### merged 

data_m <- dat %>% 
  inner_join(agg, by = "Name")
dim(data_m)
data_m



################################

colnames(data_m)



data_m <- data_m %>% 
  mutate(agecat = ifelse(data_m$Age<=35,0,1))

View(data_m %>% 
  select(Age, agecat))
### age category

## pre analysis 
data_m %>% 
  group_by(agecat) %>% 
  summarise(men = mean(preTotal), std = sd(preTotal))
### post analysis
data_m %>% 
  group_by(agecat) %>% 
  summarise(men = mean(postTotal), std = sd(postTotal))

################   gender 

## pre analysis 
data_m %>% 
  group_by(Sex) %>% 
  summarise(men = mean(preTotal), std = sd(preTotal))
### post analysis
data_m %>% 
  group_by(Sex) %>% 
  summarise(men = mean(postTotal), std = sd(postTotal))

### designation pre analysis  

data_m %>% 
  group_by(Designation) %>% 
  summarise(men = mean(preTotal), std = sd(preTotal))
### post analysis
data_m %>% 
  group_by(Designation) %>% 
  summarise(men = mean(postTotal), std = sd(postTotal))




write.csv(data_m, file = "analysis.cvs")

########################

## extra analysis 


data_m$`Working facility`
data_m$`Working facility` %>%  str_extract("UHC")

data_m <- data_m %>% 
  mutate(work = case_when(
    str_detect(data_m$`Working facility` ,"UHC")==TRUE ~ "UHC",
    str_detect(data_m$`Working facility`,"USC")==TRUE ~ "USC",
    .default = "DH"
  ))

##############   ANALyis for working facilities 


## pres analysis 


data_m %>% 
  group_by(work) %>% 
  summarise(men = mean(preTotal), std = sd(preTotal))
### post analysis
data_m %>% 
  group_by(work) %>% 
  summarise(men = mean(postTotal), std = sd(postTotal))


