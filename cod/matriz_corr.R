#Se ejecuta la limpieza de la base de datos
library(here)
source(here("cod", "limpieza.R"))

# Grafico de dispersion entre variables de estudio
mat.corr.post <- corrplot(
  cor(
    datos %>% select(
      edad,
      volumen = volumen.posterior,
      ipss = ipss.posterior,
      flujo = flujo.posterior
    ),
    use = "complete.obs",
    method = "pearson"
  ),
  method = "color",
  type = "lower",
  tl.cex = 2,
  number.cex = 3,
  addCoef.col = "black"
)
