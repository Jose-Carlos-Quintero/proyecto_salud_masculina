library(tidyverse)

#Se ejecuta la limpieza de la base de datos
source("cod/limpieza.R")


#Se emplean pruebas t-Student pareadas para ver si hay diferencia significativa entre los valores de las variables seleccionadas antes y despues del tratamiento
pruebat.volumen <- t.test(datos$volumen.previo, datos$volumen.posterior, paired = TRUE)

pruebat.ipss <- t.test(datos$ipss.previo, datos$ipss.posterior, paired = TRUE)

pruebat.flujo <- t.test(datos$flujo.previo, datos$flujo.posterior, paired = TRUE)

#Se prueba la normalidad de variables como edad y deltas
shapiro.test(datos$edad)
ggplot(datos, aes(sample = edad)) +
  geom_qq(color = "blue") +
  geom_qq_line(color = "red") +
  cowplot::theme_cowplot()

shapiro.test(datos$delta.volumen)
ggplot(datos, aes(sample = delta.volumen)) +
  geom_qq(color = "blue") +
  geom_qq_line(color = "red") +
  cowplot::theme_cowplot()

shapiro.test(datos$delta.ipss)
ggplot(datos, aes(sample = delta.ipss)) +
  geom_qq(color = "blue") +
  geom_qq_line(color = "red") +
  cowplot::theme_cowplot()

shapiro.test(datos$delta.flujo)
ggplot(datos, aes(sample = delta.flujo)) +
  geom_qq(color = "blue") +
  geom_qq_line(color = "red") +
  cowplot::theme_cowplot()


#Se emplean pruebas de correlacion por edad

#Se emplea el metodo de pearson para el volumen y el ipss, dado que demostraron cierta normalidad
prueba.corr.volumen <- cor.test(datos$edad, datos$delta.volumen, method = "pearson")

prueba.corr.ipss <- cor.test(datos$edad, datos$delta.ipss, method = "pearson")

#Se emplea el metodo de spearman para el flujo, dado que no demostro normalidad

prueba.corr.flujo <- cor.test(datos$edad, datos$delta.flujo, method = "spearman", exact = FALSE)



#Se hace un analisis diferenciando por pacientes con y sin RAO

pruebat.rao.volumen <- t.test(delta.volumen ~ rao, data = datos)  

pruebat.rao.ipss <- t.test(delta.ipss ~ rao, data = datos)

pruebaw.rao.flujo <- wilcox.test(delta.flujo ~ rao, data = datos)



