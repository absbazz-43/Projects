library(rvest)
library(tidyverse)
## Euler number

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



### Euler constant
erl <- "http://www.plouffe.fr/simon/constants/gamma.txt"
el_constant <-  read_html(erl) %>% 
  html_nodes("body") %>% 
  html_text()
el_constant
et <- str_remove_all(el_constant,"\n")
etp <- str_remove_all(et,"[a-z,A-Z,[:punct:]]")
etpn <- str_remove_all(etp,"=")
etps <- str_remove_all(etpn,"     ")
etps

es <- str_remove_all(etps,"[\t || ^ > ]")




#########  Function of extracting number from Url
library(rvest)
library(tidyverse)
Extract_number <- function(url, node){
  Det <- read_html(url) %>% 
    html_nodes(node) %>% 
    html_text()
  re_letter <- str_remove_all(Det, "\n")
  re_letters <- str_remove_all(re_letter, "[a-b, A-Z]")
  re_punct <- str_remove_all(re_letters, "[:punct:]")
  re_equ <- str_remove_all(re_punct, "=")
  return(re_equ)
}
Extract_number("https://apod.nasa.gov/htmltest/gifcity/e.1mil", "li")

#############  Extracting

Extract_numbers <- function(url, node){
  Det <- read_html(url) %>% 
    html_nodes(node) %>% 
    html_text()
  
  remover <- function(dat){
    re_letter <- str_remove_all(dat, "\n")
    re_letters <- str_remove_all(re_letter, "[a-z A-Z]")
    re_punct <- str_remove_all(re_letters, "[:punct:]")
    re_equt <- str_remove_all(re_punct, "=")
    return(re_equt)
  }
  
  numbers <- remover(Det)
  return(numbers)
}
v <- Extract_numbers("https://apod.nasa.gov/htmltest/gifcity/e.1mil", "li")
length(v)
length(v[3])

