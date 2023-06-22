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
browseURL(data_url)

data_html = read_html(data_url) ## leer el html de la página
class(data_html) ## ver la clase del objeto
View(data_html)

db_list <- lapply(1:10, read_html())
