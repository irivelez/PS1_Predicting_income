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

# Bootstrap para reg_gendergap_step_3
eta_fn3 <- function(data, index) {
  regress_step_1 <- lm(female ~ age + age_2 + totalHoursWorked + educ, data = data, subset = index)
  residuals_step_1 <- regress_step_1$residuals
  
  regress_step_2 <- lm(log_salarioreal ~ age + age_2 + totalHoursWorked + educ, data = data, subset = index)
  residuals_step_2 <- regress_step_2$residuals
  
  data_step_3 <- data[index, ]  # Subconjunto de datos para el paso 3
  
  # Agregar residuos al conjunto de datos para el paso 3
  data_step_3$female_resid <- residuals_step_1
  data_step_3$lnw_resid <- residuals_step_2
  
  regress_step_3 <- lm(lnw_resid ~ female_resid, data = data_step_3)
  
  coef(regress_step_3)
}

boot_results3 <- boot(geih_filtered, eta_fn3, R = 1000)
boot_results3

# Intervalos de confianza
boot_ci3 <- boot.ci(boot_results3, type = "basic")  
boot_ci3

# MES ----
# Calcular el MSE para reg_gendergap_3 (Modelo long)
mse_reg_3 <- mean(reg_gendergap_3$residuals^2)

# Calcular el MSE para reg_gendergap_step_3 (Modelo FWL)
mse_step_3 <- mean(reg_gendergap_step_3$residuals^2)

# Calcular el MSE promedio del bootstrap para reg_gendergap_step_3 (Modelo FWL con Boostrap)
mse_boot_step_3 <- mean(boot_results3$t^2)

## Tabla
mse_table_p3 <- data.frame(Modelo = c( "Modelo long","Modelo FW", "Modelo FWL con Boostrap"),
                        MSE = c(mse_reg_3,mse_step_3, mse_boot_step_3))

# Asignar nombres a las columnas
names(mse_table_p3) <- c("Modelo", "MSE")

# Imprimir la tabla
print(mse_table)

# Plot ----
# Verificar si hay filas con valores faltantes
length(y_estimados)
length(db_geih_filtered$age)
sum(!is.na(db_geih_filtered$age))

missing_data <- complete.cases(db_geih_filtered$age, db_geih_filtered$log_salarioreal)

# Eliminar filas con valores faltantes
db_geih_filtered <- db_geih_filtered[missing_data, ]

# Ajustar el modelo de regresión
modelo_p3 <- lm(log_salarioreal ~ age + age_cuadradofinal, data = db_geih_filtered)

# Obtener los valores ajustados 
y_estimados_gendergap <- predict(reg_gendergap_3)

# Crear un gráfico de dispersión
plot(geih_filtered$age, geih_filtered$log_salarioreal, 
     xlab = "Edad", ylab = "Log Salario Real",
     main = "Regresión de Salario Real en función de la Edad",
     pch = 16, col = "blue")

# Agregar los valores ajustados
points(geih_filtered$age, y_estimados_gendergap, pch = 16, col = "green")
length(y_estimados_gendergap)


## Discusión Peak Age, comparar con el punto anterior
## Poner lindo los coef de todo (bootstrap)



















