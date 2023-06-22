##########################################################
#         Problem Set 1. Predicting Income
#           Big data and Machine Learning
#             Universidad de los Andes
##########################################################
'''
Authors:

- Daniel Casas Bautista
- Lucia Fillippo
- Miguel Angel Victoria Simbaqueva 
- Irina Andrea Vélez López
'''

### Initial Configuration
if(!require(pacman)) install.packages("pacman")
require(pacman)
p_load(tidyverse, rvest)


### Loading data  
data_url = "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_"

# Loop
db_list <- lapply(1:10, function(i) {
  url <- paste0(data_url, "page_", i, ".html")
  page <- read_html(url) 
  tabla <- page %>% html_table
  db_geih <- data.frame(tabla)
})

db_geih <- do.call(rbind,db_list) # Concatenate the tables in a dataframe
view(db_geih)


### Transforming data

##Filtering the data
# Details of the variable age 
summary(db_geih$age)
sum(is.na(db_geih$age)) # Checking for missing values
class(db_geih$age)

db_geih_filtered <- db_geih[db_geih$age > 18, ]
summary(db_geih_filtered$age)

save(db_geih_filtered, file = "data.Rdata")

''' Revisar
# Save the resulting database in the local repository
# Specify the file path and name for the output file
output_db <- "Home/Documents/Repositorios/PS1_Predicting_incom/stores/db_geih_filtered"

# Save the database as a CSV file
write.csv(db_geih_filtered, file = output_db, row.names = FALSE)
'''

## Dealing with missing values
# Count missing values for each column and sorting
missing_counts <- sapply(db_geih_filtered, function(x) sum(is.na(x)))
sorted_missing <- sort(missing_counts, decreasing = TRUE)
top_missing <- head(sorted_missing, 20)

length(sorted_missing) # Correspond to the number of variables in the GEIH: 178

view(sorted_missing)  # Display the vector in decreasing order
view(top_missing)     # Display the vector with the variables that have the most missing values

## Dealing with 0 wages
names(db_geih_filtered)   # Checking variables names

# Verification of the min and max value of the main resultant variables, and their missing values
summary(db_geih_filtered$p6500)           # Monthly labor income. DANE Variable
summary(db_geih_filtered$y_ingLab_m)      # labor income salaried nominal monthly
summary(db_geih_filtered$y_ingLab_m_ha)   # labor income salaried nominal hourly
summary(db_geih_filtered$y_salary_m)      # salary - nominal monthly
summary(db_geih_filtered$y_salary_m_hu)   # salary - real hourly (usual)

missing_income <- sum(is.na(db_geih_filtered$y_ingLab_m_ha))
obs <- nrow(db_geih_filtered)

# The 59.32% of the observations for the main resultant variables have missing values
missing_perc = (missing_income/obs)*100
missing_perc

view(db_geih_filtered$y_ingLab_m_ha)