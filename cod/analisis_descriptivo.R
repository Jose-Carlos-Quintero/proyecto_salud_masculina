library(tidyverse)
library(corrplot)

#Se ejecuta la limpieza de la base de datos
library(here)
source(here("cod", "limpieza.R"))

# Se crea una funcion para guardar graficos como imagen
guardar_grafico <- function(grafico, nombre_archivo, carpeta = "res/figuras", ancho = 8, alto = 6, dpi = 300) {
  if (!dir.exists(carpeta)) {
    dir.create(carpeta, recursive = TRUE)
  }
  
  ruta_completa <- file.path(carpeta, paste0(nombre_archivo, ".png"))
  
  ggsave(filename = ruta_completa,
         plot = grafico,
         width = ancho,
         height = alto,
         dpi = dpi)
  
  message("Gráfico guardado en: ", ruta_completa)
}


# Histograma con la distribucion de las edades
graf.edad <- datos %>%
  ggplot(aes(x = edad)) +
  geom_histogram(
    binwidth = 10,
    boundary = 0,
    color = 'white',
    fill = 'gray'
  ) +
  geom_vline(aes(xintercept = mean(edad, na.rm = TRUE)),
             color = 'darkblue',
             linewidth = 1) +
  annotate(
    "text",
    x = mean(datos$edad, na.rm = TRUE),
    y = 25,
    label =  paste0('AVG: ', round(
      mean(datos$edad, na.rm = TRUE), 0
    )),
    hjust = -0.1,
    color = 'darkblue',
    size = 7
  ) +
  labs(x = "Edad", y = "") +
  scale_x_continuous(
    breaks = seq(40, 100, by = 10),       
    limits = c(40, 100)                   
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 20, color = 'black'),
    axis.text.y = element_text(size = 20, color = 'black'),
    axis.title.x = element_text(size = 20, face = 'bold')
  )

# Histograma con la distribucion de las edades
graf.num.apps <- datos %>%
  ggplot(aes(x = num.aplicaciones)) +
  geom_histogram(
    binwidth = 2,
    boundary = 0,
    color = 'white',
    fill = 'gray'
  ) +
  geom_vline(aes(xintercept = mean(num.aplicaciones, na.rm = TRUE)),
             color = 'darkblue',
             linewidth = 1) +
  annotate(
    "text",
    x = mean(datos$num.aplicaciones, na.rm = TRUE),
    y = 60,
    label =  paste0('AVG: ', round(
      mean(datos$num.aplicaciones, na.rm = TRUE), 2
    )),
    hjust = -0.1,
    color = 'darkblue',
    size = 7
  ) +
  labs(x = "Número de aplicaciones", y = "") +
  scale_x_continuous(breaks = seq(0, 14, by = 2), limits = c(0, 14)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 20, color = 'black'),
    axis.text.y = element_text(size = 20, color = 'black'),
    axis.title.x = element_text(size = 20, face = 'bold')
  )

# Columnas de frecuencia de personas con o sin RAO
graf.rao <- datos %>%
  mutate(rao = recode(rao, '0' = 'No', '1' = 'Sí'),
         rao = fct_relevel(rao, "Sí", "No")) %>%
  count(rao) %>%
  ggplot(aes(x = rao, y = n)) +
  geom_col(fill = 'gray') +
  annotate(
    "text",
    x = 1,
    y = datos %>%
      count(rao) %>%
      filter(rao == 1) %>%
      pull(n) + 10,
    label = datos %>%
      count(rao) %>%
      filter(rao == 1) %>%
      pull(n),
    size = 7,
    color = "black",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 2,
    y = datos %>%
      count(rao) %>%
      filter(rao == 0) %>%
      pull(n) + 10,
    label = datos %>%
      count(rao) %>%
      filter(rao == 0) %>%
      pull(n),
    size = 7,
    color = "black",
    fontface = "bold"
  ) +
  labs(x = "Presentó Retención Aguda de Orina (RAO)", y = "") +
  scale_y_continuous(breaks = seq(0, 300, by = 50), limits = c(0, 300)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 20, color = 'black'),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 20, face = 'bold')
  )
# Barra de frecuencia relativa de personas con o sin RAO
valor.pct <- datos %>%
  count(rao) %>%
  mutate(pct = n / sum(n)) %>%
  filter(rao == 0) %>%
  pull(pct)

graf.rao.porc <- datos %>%
  mutate(rao = recode(rao, '0' = 'No', '1' = 'Sí'),
         rao = fct_relevel(rao, "Sí", "No")) %>%
  count(rao) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = "", y = pct, fill = rao)) +
  geom_col(width = 0.8) +
  annotate(
    "text",
    x = 1,
    y = valor.pct/2,
    label = scales::percent(valor.pct, accuracy = 0.1),
    size = 7,
    color = "black",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 1,
    y = valor.pct + (1 - valor.pct)/2,
    label = scales::percent(1 - valor.pct, accuracy = 0.1),
    size = 7,
    color = "white",
    fontface = "bold"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("darkblue", "gray")) +
  labs(x = NULL, y = NULL, fill = "Presentó Retención Aguda de Orina (RAO)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = 'top',
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20)
  )

rm(valor.pct)
