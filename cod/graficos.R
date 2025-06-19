source("cod/analisis.R")

#Graficos de correlacion con edad

ggplot(datos, aes(x = edad, y = delta.volumen)) +
  geom_point(alpha = 0.6,
             size = 2,
             color = "steelblue") +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "darkred",
    linetype = "dashed"
  ) +
  labs(
    title = "Relación entre edad y cambio en el volumen prostático",
    subtitle = "Cambio en volumen después de tratamiento con Rezūm®",
    x = "Edad (años)",
    y = "Δ Volumen prostático (mL)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggplot(datos, aes(x = edad, y = delta.ipss)) +
  geom_point(alpha = 0.6,
             size = 2,
             color = "darkorange") +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "darkred",
    linetype = "dashed"
  ) +
  labs(
    title = "Relación entre edad e IPSS",
    subtitle = "Cambio en IPSS después de tratamiento con Rezūm®",
    x = "Edad (años)",
    y = "Δ IPSS"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggplot(datos, aes(x = edad, y = delta.flujo)) +
  geom_point(alpha = 0.6,
             size = 2,
             color = "darkgreen") +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "darkred",
    linetype = "dashed"
  ) +
  labs(
    title = "Relación entre edad y flujo miccional",
    subtitle = "Cambio en el flujo miccional después de tratamiento con Rezūm®",
    x = "Edad (años)",
    y = "Δ Flujo urinario (mL/s)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

#Graficos de comparacion previa-posterior
ggplot(datos, aes(x = volumen.previo, y = volumen.posterior)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Comparación entre volumen prostático antes y después del tratamiento",
    subtitle = "Cada punto representa un paciente",
    x = "Volumen prostático previo (mL)",
    y = "Volumen prostático posterior (mL)",
    caption = "Línea punteada = sin cambio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggplot(datos, aes(x = ipss.previo, y = ipss.posterior)) +
  geom_point(alpha = 0.6, color = "darkorange", size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Comparación del puntaje IPSS antes y después del tratamiento",
    subtitle = "Cada punto representa un paciente",
    x = "IPSS previo",
    y = "IPSS posterior",
    caption = "Línea punteada = sin cambio. Valores debajo indican mejoría"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggplot(datos, aes(x = flujo.previo, y = flujo.posterior)) +
  geom_point(alpha = 0.6, color = "darkgreen", size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Comparación del flujo miccional antes y después del tratamiento",
    subtitle = "Cada punto representa un paciente",
    x = "Flujo previo (mL/s)",
    y = "Flujo posterior (mL/s)",
    caption = "Línea punteada = sin cambio. Puntos arriba indican mejoría"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

#Graficos de comparacion de distribuciones previa-posterior
datos_largo <- datos %>%
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

ggplot(datos_largo, aes(x = momento, y = valor, fill = momento)) +
  geom_boxplot(alpha = 0.7, width = 0.6, outlier.shape = 16, outlier.size = 1.5) +
  facet_wrap(~ variable, scales = "free_y", labeller = labeller(
    variable = c(volumen = "Volumen prostático (mL)",
                 ipss = "IPSS",
                 flujo = "Flujo miccional (mL/s)")
  )) +
  scale_fill_manual(values = c("previo" = "#236e96", "posterior" = "#afd7df")) +
  labs(
    title = "Distribución antes y después del tratamiento con Rezūm®",
    x = NULL,
    y = NULL,
    caption = "Cada panel muestra la distribución de valores antes y después del procedimiento"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 11)
  )
