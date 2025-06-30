library(here)
source(here("cod", "limpieza.R"))
source(here("cod", "analisis.R"))
library(xtable)
guardar_prueba <- function(objeto, nombre_archivo, caption) {
  estadisticos <- c()
  valores <- c()
  
  if (!is.null(objeto$statistic)) {
    estadisticos <- c(estadisticos, names(objeto$statistic))
    valores <- c(valores, format(round(objeto$statistic, 3), scientific = TRUE))
  }
  
  if (!is.null(objeto$parameter)) {
    estadisticos <- c(estadisticos, names(objeto$parameter))
    valores <- c(valores, format(round(objeto$parameter, 3), scientific = TRUE))
  }
  
  estadisticos <- c(estadisticos, "Valor-p")
  valores <- c(valores, format(signif(objeto$p.value, 4), scientific = TRUE))
  
  if (!is.null(objeto$conf.int)) {
    estadisticos <- c(estadisticos, "IC inferior", "IC superior")
    valores <- c(valores, 
                 format(round(objeto$conf.int[1], 3), scientific = TRUE),
                 format(round(objeto$conf.int[2], 3), scientific = TRUE))
  }
  
  if (!is.null(objeto$estimate)) {
    nombre_estimado <- if (!is.null(names(objeto$estimate))) names(objeto$estimate) else "Media de diferencias"
    estadisticos <- c(estadisticos, nombre_estimado)
    valores <- c(valores, format(round(objeto$estimate, 3), scientific = TRUE))
  }
  
  tabla <- data.frame(Estadístico = estadisticos, Valor = valores)
  
  sink(file = file.path("tablas", nombre_archivo))
  print(xtable::xtable(tabla, caption = caption, label = paste0("tab:", tools::file_path_sans_ext(nombre_archivo))),
        include.rownames = FALSE, caption.placement = "top")
  sink()
}

# Función para guardar resumen de modelo lineal
guardar_modelo <- function(modelo, nombre_archivo, caption) {
  sumario <- summary(modelo)
  tabla <- data.frame(
    Variable = rownames(sumario$coefficients),
    Estimate = format(round(sumario$coefficients[, 1], 4), scientific = TRUE),
    Std.Error = format(round(sumario$coefficients[, 2], 4), scientific = TRUE),
    t.value = format(round(sumario$coefficients[, 3], 4), scientific = TRUE),
    p.value = format(signif(sumario$coefficients[, 4], 4), scientific = TRUE)
  )
  sink(file = file.path("tablas", nombre_archivo))
  print(xtable(tabla, caption = caption, label = paste0("tab:", tools::file_path_sans_ext(nombre_archivo))),
        include.rownames = FALSE, caption.placement = "top")
  sink()
}

# === Pruebas t y wilcox ===
guardar_prueba(pruebat.volumen, "prueba_t_volumen.tex", "Prueba t pareada: volumen antes y después")
guardar_prueba(pruebat.ipss, "prueba_t_ipss.tex", "Prueba t pareada: IPSS antes y después")
guardar_prueba(pruebat.flujo, "prueba_t_flujo.tex", "Prueba t pareada: flujo antes y después")

guardar_prueba(pruebat.rao.volumen, "prueba_t_rao_volumen.tex", "Prueba t: cambio de volumen según RAO")
guardar_prueba(pruebat.rao.ipss, "prueba_t_rao_ipss.tex", "Prueba t: cambio de IPSS según RAO")
guardar_prueba(pruebaw.rao.flujo, "prueba_w_rao_flujo.tex", "Prueba de Wilcoxon: cambio de flujo según RAO")

# === Correlaciones ===
guardar_prueba(prueba.corr.volumen, "correlacion_volumen.tex", "Correlación entre edad y cambio en volumen (Pearson)")
guardar_prueba(prueba.corr.ipss, "correlacion_ipss.tex", "Correlación entre edad y cambio en IPSS (Pearson)")
guardar_prueba(prueba.corr.flujo, "correlacion_flujo.tex", "Correlación entre edad y cambio en flujo (Spearman)")

# === Modelos de regresión lineal ===
guardar_modelo(modelo.volumen, "modelo_volumen.tex", "Modelo de regresión: predicción de volumen posterior")
guardar_modelo(modelo.ipss, "modelo_ipss.tex", "Modelo de regresión: predicción de IPSS posterior")
guardar_modelo(modelo.flujo, "modelo_flujo.tex", "Modelo de regresión: predicción de flujo posterior")

