# - Daniel Casas Bautista
# - Lucia Fillippo
# - Miguel Angel Victoria Simbaqueva 
# - Irina Andrea Vélez López 

# Initial Configuration and packages insdtallment
if(!require(pacman)) install.packages("pacman")
require(pacman)
p_load(tidyverse, rvest, stargazer, broom, flextable, officer,boot)

# Configurando el espacio de trabajo 
getwd()
setwd("C:/Users/migue/OneDrive - Universidad de los andes/Intersemestral_2/Big_Data_Machine_Learning/Taller_1/PS1_Predicting_income/scripts")

# Data base load
load("../stores/data.Rdata")
geih_filtered <- geih_filtered[complete.cases(geih_filtered$maxEducLevel), ]

# Ajuste de variables ----
#Crear variable logaritmica para la y
geih_filtered$log_salarioreal <-log(geih_filtered$y_salary_m_hu)
geih_filtered$age_2 <- geih_filtered$age^2


#Modificar los valores de la variable dummy para que 1=mujer
geih_filtered$female <- ifelse(geih_filtered$sex == 1, 0, 1)

# Variable educacion
geih_filtered$educ <- factor(geih_filtered$maxEducLevel)

# Modelos ----
# Regresion inicial
reg_gendergap_1 <- lm(log_salarioreal~female, geih_filtered)
reg_gendergap_2 <- lm(log_salarioreal~female+age+age_2,geih_filtered)
reg_gendergap_3 <- lm(log_salarioreal~female+age+age_2+totalHoursWorked+educ,geih_filtered)

stargazer(reg_gendergap_1,reg_gendergap_2,reg_gendergap_3,type="text")

## Modelo FWL ----
# Regress female on Xcontrols
reg_gendergap_step_1 <- lm(female~age+age_2+totalHoursWorked+educ,geih_filtered)
geih_filtered<-geih_filtered %>% mutate(female_resid=reg_gendergap_step_1$residuals) #Residuals 

# Regress ln(w) on Xcontrols
reg_gendergap_step_2 <- lm(log_salarioreal~age+age_2+totalHoursWorked+educ,geih_filtered)
geih_filtered<-geih_filtered %>% mutate(lnw_resid=reg_gendergap_step_2$residuals) #Residuals 

# Regress the residuals from step 2 on the residuals from step 1
reg_gendergap_step_3<-lm(lnw_resid~female_resid,data=geih_filtered)
stargazer(reg_gendergap_step_3,reg_gendergap_3,type="text")

# MES
# Calcular el MSE para reg_gendergap_step_3
mse_step_3 <- mean(reg_gendergap_step_3$residuals^2)

# Calcular el MSE para reg_gendergap_3
mse_step_3 <- mean(reg_gendergap_3$residuals^2)

# Imprimir los resultados
cat("El MSE para la regresión inicial es:", mse_step_3, "\n")
cat("El MSE para la refresión con el teorema FWL:", mse_step_3, "\n")


## Modelo FWL con Boostrap ----





















