library(plotly)
library(dplyr)

DIM_COLORS <- c(
  "Planificación"      = "#4C72B0",
  "Contenidos"         = "#DD8452",
  "Clases"             = "#55A868",
  "Evaluación"         = "#C44E52",
  "Ambiente y vínculo" = "#8172B3",
  "Balance"            = "#937860"
)

plot_evolucion <- function(scores_df, materia_sel = "Todas") {
  df <- scores_df
  if (materia_sel != "Todas") {
    df <- df %>% dplyr::filter(materia_nombre == materia_sel)
  }
  if (nrow(df) == 0) return(plotly::plot_ly() %>% plotly::layout(title = "Sin datos"))

  df <- df %>%
    dplyr::group_by(cuatrimestre_ord, dimension) %>%
    dplyr::summarise(promedio = mean(promedio, na.rm = TRUE),
                     n_respuestas = sum(n_respuestas), .groups = "drop")

  plotly::plot_ly(df, x = ~cuatrimestre_ord, y = ~promedio, color = ~dimension,
                  colors = DIM_COLORS, type = "scatter", mode = "lines+markers",
                  text = ~paste0(dimension, "<br>Cuatrimestre: ", cuatrimestre_ord,
                                 "<br>Promedio: ", round(promedio, 2),
                                 "<br>N: ", n_respuestas),
                  hoverinfo = "text") %>%
    plotly::layout(
      title = list(text = paste("Evolución por dimensión —", materia_sel), font = list(size = 14)),
      xaxis = list(title = "Cuatrimestre"),
      yaxis = list(title = "Promedio (1-5)", range = c(1, 5)),
      legend = list(orientation = "h", y = -0.25),
      hovermode = "closest"
    )
}

plot_por_materia <- function(scores_df, dimension_sel = NULL, cuatrimestre_sel = NULL) {
  df <- scores_df
  if (!is.null(dimension_sel) && dimension_sel != "Todas")
    df <- df %>% dplyr::filter(dimension == dimension_sel)
  if (!is.null(cuatrimestre_sel) && length(cuatrimestre_sel) > 0)
    df <- df %>% dplyr::filter(cuatrimestre_ord %in% cuatrimestre_sel)
  if (nrow(df) == 0) return(plotly::plot_ly() %>% plotly::layout(title = "Sin datos"))

  df <- df %>%
    dplyr::group_by(materia_nombre, dimension) %>%
    dplyr::summarise(promedio = mean(promedio, na.rm = TRUE),
                     n_respuestas = sum(n_respuestas), .groups = "drop")

  plotly::plot_ly(df, x = ~materia_nombre, y = ~promedio, color = ~dimension,
                  colors = DIM_COLORS, type = "bar",
                  text = ~paste0(dimension, "<br>Materia: ", materia_nombre,
                                 "<br>Promedio: ", round(promedio, 2),
                                 "<br>N: ", n_respuestas),
                  hoverinfo = "text") %>%
    plotly::layout(
      title = list(text = "Comparación por materia", font = list(size = 14)),
      barmode = "group",
      xaxis = list(title = ""),
      yaxis = list(title = "Promedio (1-5)", range = c(1, 5)),
      legend = list(orientation = "h", y = -0.3)
    )
}

plot_radar <- function(df_global) {
  dims <- names(DIMENSION_COLS)
  promedios <- sapply(dims, function(d) {
    cols <- intersect(DIMENSION_COLS[[d]], colnames(df_global))
    if (length(cols) == 0) return(NA_real_)
    mean(unlist(df_global[, cols]), na.rm = TRUE)
  })
  promedios <- round(promedios, 2)
  valid <- !is.na(promedios)
  if (sum(valid) < 3) return(plotly::plot_ly() %>% plotly::layout(title = "Datos insuficientes"))

  plotly::plot_ly(
    type = "scatterpolar",
    r = c(promedios[valid], promedios[valid][[1]]),
    theta = c(dims[valid], dims[valid][[1]]),
    fill = "toself",
    fillcolor = "rgba(76,114,176,0.3)",
    line = list(color = "#4C72B0")
  ) %>%
    plotly::layout(
      polar = list(radialaxis = list(visible = TRUE, range = c(0, 5))),
      showlegend = FALSE
    )
}

plot_distribucion <- function(dist_df, titulo = "") {
  if (is.null(dist_df) || nrow(dist_df) == 0)
    return(plotly::plot_ly() %>% plotly::layout(title = "Sin datos"))

  df <- dist_df %>%
    dplyr::group_by(pregunta, valor) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(pregunta) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::ungroup()

  plotly::plot_ly(df, x = ~valor, y = ~prop, color = ~pregunta,
                  type = "bar",
                  text = ~paste0(pregunta, "<br>Valor: ", valor,
                                 "<br>", round(prop * 100, 1), "%"),
                  hoverinfo = "text") %>%
    plotly::layout(
      title = list(text = titulo, font = list(size = 13)),
      barmode = "group",
      xaxis = list(title = "Respuesta (1-5)"),
      yaxis = list(title = "Proporción", tickformat = ".0%"),
      legend = list(orientation = "h", y = -0.35)
    )
}

plot_sentiment <- function(sent_df) {
  if (is.null(sent_df) || nrow(sent_df) == 0)
    return(plotly::plot_ly() %>% plotly::layout(title = "Sin datos de sentiment"))

  sent_df <- sent_df %>% dplyr::filter(!emocion %in% c("positive", "negative"))

  plotly::plot_ly(sent_df, x = ~cuatrimestre_ord, y = ~score, color = ~emocion_es,
                  type = "bar",
                  text = ~paste0(emocion_es, ": ", round(score, 2)),
                  hoverinfo = "text") %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = "Cuatrimestre"),
      yaxis = list(title = "Score promedio NRC"),
      legend = list(orientation = "h", y = -0.3)
    )
}
