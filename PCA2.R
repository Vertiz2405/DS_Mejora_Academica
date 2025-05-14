
# PCA - R (N dimensiones)
# Autor: Diego Vértiz Padilla

# Se carga un dataset multivariable, se centra, se calcula la matriz de covarianza
# y se aplica PCA para determinar el número de componentes necesarios para explicar al menos el 95% de la varianza.

library(readr)
library(dplyr)
library(ggplot2)

# Leer datos
datos <- read_csv("bci_clean_pca.csv")
datos_num <- select(datos, -Empleado)

# Limpiar y centrar
datos_num <- mutate_all(datos_num, ~as.numeric(.))
datos_num <- datos_num[, colSums(is.na(datos_num)) == 0]
datos_num <- mutate_all(datos_num, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))
datos_centered <- scale(datos_num, center = TRUE, scale = FALSE)

# Covarianza y correlación
print("Matriz de covarianza:")
print(cov(datos_centered))
print("Matriz de correlación:")
print(cor(datos_centered))

# PCA
pca_result <- prcomp(datos_centered, center = FALSE, scale. = FALSE)
var_exp <- summary(pca_result)$importance[2,]
var_acum <- cumsum(var_exp)

print("Varianza explicada:")
print(var_exp)
print("Varianza acumulada:")
print(var_acum)

# Componentes necesarios para  más de 95% de la varianza
n_comp <- which(var_acum >= 0.95)[1]
cat(paste0("Componentes necesarios para más de 95% varianza: ", n_comp, "\n"))

# Matriz proyectada Z (los primeros n componentes principales)
Z <- as.data.frame(pca_result$x[, 1:n_comp])
print(paste("Matriz proyectada Z con", n_comp, "componentes principales:"))
print(head(Z)) 
