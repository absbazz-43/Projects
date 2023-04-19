library(pacman)
library(tm)

## facebook comment dataset 
comment <- facebook_group_comments63f6341b3df60_2078341245598841
dim(comment)



library(tidyverse)

table(comment$ExportComments.com)


for(i in 1:ncol(comment)){
  cat(colnames(comment)[i], "---->", sum(is.na(comment[,i])), "\n")
}


comment <- comment %>% 
  mutate(
    `Profile ID`  =  str_remove(`Profile ID`, "ID:"),
    `Profile ID`  =  str_remove(`Profile ID`, "\\s+")
  )
comment


edit(comment)


sum(comment$Likes)


g <- comment %>% 
  filter(Likes>0) %>% 
  select(Comment) %>% 
  tibble()# %>% 
  pull() 

g

word <- unnest_tokens(g, word, input = "Comment")

word %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head() %>% 
  ggplot( aes(word, n, fill = word ))+geom_col(stat = "identity")+coord_flip()

