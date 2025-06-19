library(readxl)
library(tidyverse)

datos <- read_excel("data/DataREZUManonimos.xlsx", sheet = "Datos")
#Se seleccionan las columnas mas importantes
datos <- datos %>% select(
  codigo,
  edad,
  volumen.previo = volpre,
  volumen.posterior = volpost,
  rao,
  ipss.previo = ipsspre,
  ipss.posterior = ipsspost,
  flujo.previo = flujopre2,
  flujo.posterior = flujopost
)

#Se transforman las observaciones sin edad para que se tenga la edad promedio y el rao se codifica como un booleano
datos <- datos %>%
  mutate(edad = if_else(is.na(edad), round(mean(edad, na.rm = TRUE)), edad)) %>%
  mutate(rao = if_else(is.na(rao), 0, rao, ))

#Se eliminan observaciones que no contribuyen información de volumen, ipss ni flujo
datos <- datos %>%
  filter(!if_all(
    c(
      volumen.previo,
      volumen.posterior,
      ipss.previo,
      ipss.posterior,
      flujo.previo,
      flujo.posterior
    ),
    is.na
  ))


