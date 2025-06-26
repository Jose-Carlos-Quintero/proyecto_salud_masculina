#Se ejecuta la limpieza de la base de datos
library(here)
source(here("cod", "limpieza.R"))

# #Graficos de correlacion con edad
# 
# graf.corr.edad.vol <- ggplot(datos, aes(x = edad, y = delta.volumen)) +
#   geom_point(alpha = 0.6,
#              size = 2,
#              color = "steelblue") +
#   geom_smooth(
#     method = "lm",
#     se = FALSE,
#     color = "darkred",
#     linetype = "dashed"
#   ) +
#   labs(
#     title = "Relación entre edad y cambio en el volumen prostático",
#     subtitle = "Cambio en volumen después de tratamiento con Rezūm®",
#     x = "Edad (años)",
#     y = "Δ Volumen prostático (mL)"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     plot.title = element_text(face = "bold"),
#     plot.subtitle = element_text(size = 10),
#     panel.grid.minor = element_blank()
#   )
# 
# graf.corr.edad.ipss <- ggplot(datos, aes(x = edad, y = delta.ipss)) +
#   geom_point(alpha = 0.6,
#              size = 2,
#              color = "darkorange") +
#   geom_smooth(
#     method = "lm",
#     se = FALSE,
#     color = "darkred",
#     linetype = "dashed"
#   ) +
#   labs(
#     title = "Relación entre edad e IPSS",
#     subtitle = "Cambio en IPSS después de tratamiento con Rezūm®",
#     x = "Edad (años)",
#     y = "Δ IPSS"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     plot.title = element_text(face = "bold"),
#     plot.subtitle = element_text(size = 10),
#     panel.grid.minor = element_blank()
#   )
# 
# graf.corr.edad.flujo <- ggplot(datos, aes(x = edad, y = delta.flujo)) +
#   geom_point(alpha = 0.6,
#              size = 2,
#              color = "darkgreen") +
#   geom_smooth(
#     method = "lm",
#     se = FALSE,
#     color = "darkred",
#     linetype = "dashed"
#   ) +
#   labs(
#     title = "Relación entre edad y flujo miccional",
#     subtitle = "Cambio en el flujo miccional después de tratamiento con Rezūm®",
#     x = "Edad (años)",
#     y = "Δ Flujo urinario (mL/s)"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     plot.title = element_text(face = "bold"),
#     plot.subtitle = element_text(size = 10),
#     panel.grid.minor = element_blank()
#   )

#Graficos de comparacion previa-posterior
graf.delta.volumen <- ggplot(datos, aes(x = volumen.previo, y = volumen.posterior)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred", linewidth = 1) +
  labs(
    title = "Comparación entre volumen prostático antes y\ndespués del tratamiento",
    subtitle = "Cada punto representa un paciente",
    x = "Volumen prostático previo (mL)",
    y = "Volumen prostático posterior (mL)",
    caption = "Línea punteada = sin cambio. Valores debajo indican mejoría"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 15),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 20, color = 'black'),
    axis.text.y = element_text(size = 20, color = 'black'),
    axis.title.x = element_text(size = 20, face = 'bold'),
    axis.title.y = element_text(size = 20, face = 'bold'),
    plot.caption = element_text(size = 15)
  )


graf.delta.ipss <- ggplot(datos, aes(x = ipss.previo, y = ipss.posterior)) +
  geom_point(alpha = 0.6, color = "darkorange", size = 5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Comparación del puntaje IPSS antes y después\ndel tratamiento",
    subtitle = "Cada punto representa un paciente",
    x = "IPSS previo",
    y = "IPSS posterior",
    caption = "Línea punteada = sin cambio. Valores debajo indican mejoría"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 15),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 20, color = 'black'),
    axis.text.y = element_text(size = 20, color = 'black'),
    axis.title.x = element_text(size = 20, face = 'bold'),
    axis.title.y = element_text(size = 20, face = 'bold'),
    plot.caption = element_text(size = 15)
  )

graf.delta.flujo <- ggplot(datos, aes(x = flujo.previo, y = flujo.posterior)) +
  geom_point(alpha = 0.6, color = "darkgreen", size = 5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Comparación del flujo miccional antes y después\ndel tratamiento",
    subtitle = "Cada punto representa un paciente",
    x = "Flujo previo (mL/s)",
    y = "Flujo posterior (mL/s)",
    caption = "Línea punteada = sin cambio. Puntos arriba indican mejoría"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 15),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 20, color = 'black'),
    axis.text.y = element_text(size = 20, color = 'black'),
    axis.title.x = element_text(size = 20, face = 'bold'),
    axis.title.y = element_text(size = 20, face = 'bold'),
    plot.caption = element_text(size = 15)
  )

#Graficos de comparacion de distribuciones previa-posterior
datos.largo <- datos %>%
  pivot_longer(
    cols = c(
      volumen.previo,
      volumen.posterior,
      ipss.previo,
      ipss.posterior,
      flujo.previo,
      flujo.posterior
    ),
    names_to = c("variable", "momento"),
    names_sep = "\\.",
    values_to = "valor"
  ) %>%
  mutate(momento = factor(momento, levels = c("previo", "posterior")))

graf.dist.vars <- ggplot(datos.largo, aes(x = momento, y = valor, fill = momento)) +
  geom_boxplot(alpha = 0.7, width = 0.6, outlier.shape = 16, outlier.size = 1.5) +
  facet_wrap(~ variable, scales = "free_y", labeller = labeller(
    variable = c(volumen = "Volumen prostático (mL)",
                 ipss = "IPSS",
                 flujo = "Flujo miccional (mL/s)")
  )) +
  scale_fill_manual(values = c("previo" = "#515151", "posterior" = "lightgray")) +
  labs(
    title = "Distribución antes y después del tratamiento con Rezūm®",
    x = NULL,
    y = NULL,
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 15),
    axis.text.x = element_text(size = 20, color = 'black'),
    axis.text.y = element_text(size = 20, color = 'black'),
  )
rm(datos.largo)