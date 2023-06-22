##########################################################
#       Problem Set 1. Predicting Income
#           Big data and Machine Learning
#             Author: Irina Vélez
#           Universidad de los Andes
##########################################################

#### Initial Configuration ####
require(pacman)
p_load(tidyverse, rvest)

## Web scraping
data_url = "https://ignaciomsarmiento.github.io/GEIH2018_sample/"

data_html = read_html(data_url) ## leer el html de la página
class(data_html) ## ver la clase del objeto

db = data_html %>% html_table()
length(my_table)


db_list <- lapply(1:10, function(i) {
  url <- paste0(data_url, "page", i, ".html")
  page <- read_html(url)
  
  tables <- page %>% html_table()
  tables
})

db <- do.call(rbind, db_list)


db_list [[1]]
db <- do.call(rbind, db_list)
View(db_list)

lapply(db_list, html_structure)



db_list <- lapply(1:10, data_url, read_html)
db_list [[1]]
db_list [[2]]
view(db_list)


