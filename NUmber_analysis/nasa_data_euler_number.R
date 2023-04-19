library(rvest)
library(tidyverse)
url <- "https://apod.nasa.gov/htmltest/gifcity/e.1mil"

ht <- read_html(url) %>% 
  html_nodes("li") %>% 
  html_text()
vb <- ht[3]
vb
################


st <- str_remove_all(vb,"\n")
stp <- str_remove_all(st,"[a-z,A-Z,[:punct:]]")
stp
stpn <- str_remove_all(stp,"=")
stpn

stps <- str_remove_all(stp,"     ")
stps

### Clean data 

url <- "https://apod.nasa.gov/htmltest/gifcity/e.1mil"

ht <- read_html(url) %>% 
  html_nodes("li") %>% 
  html_text()


#######  golden ratio

grl <- "http://www2.cs.arizona.edu/icon/oddsends/phi.htm"
gr <- ht <- read_html(grl) %>% 
  html_nodes("code") %>% 
  html_text()
gr

rm_pun <- str_remove_all(gr, "\n")
rm_p <- str_remove_all(rm_pun, "[:punct:]")
rm_p
