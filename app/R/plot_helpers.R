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

MATERIA_ORDER <- c(
  "Proc datos y estadisticas cs",
  "Metodos Multivariados",
  "Machine Learning",
  "Laboratorio de Datos"
)

# Paleta para el radar overlay (line + fill rgba)
RADAR_PALETTE <- list(
  list(line = "#1f77b4", fill = "rgba(31,119,180,0.15)"),
  list(line = "#ff7f0e", fill = "rgba(255,127,14,0.15)"),
  list(line = "#2ca02c", fill = "rgba(44,160,44,0.15)"),
  list(line = "#d62728", fill = "rgba(214,39,40,0.15)"),
  list(line = "#9467bd", fill = "rgba(148,103,189,0.15)"),
  list(line = "#8c564b", fill = "rgba(140,86,75,0.15)")
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
                     n_respuestas = sum(n_respuestas), .groups = "drop") %>%
    dplyr::mutate(
      materia_nombre = factor(materia_nombre,
                              levels = intersect(MATERIA_ORDER, unique(materia_nombre)))
    )

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

.radar_scores <- function(df) {
  dims <- names(DIMENSION_COLS)
  round(sapply(dims, function(d) {
    cols <- intersect(DIMENSION_COLS[[d]], colnames(df))
    if (length(cols) == 0) return(NA_real_)
    mean(unlist(df[, cols]), na.rm = TRUE)
  }), 2)
}

plot_radar <- function(df, materias_overlay = NULL) {
  dims <- names(DIMENSION_COLS)

  if (is.null(materias_overlay) || length(materias_overlay) == 0) {
    # Traza global única
    scores <- .radar_scores(df)
    valid  <- !is.na(scores)
    if (sum(valid) < 3) return(plotly::plot_ly() %>% plotly::layout(title = "Datos insuficientes"))

    return(
      plotly::plot_ly(
        type = "scatterpolar",
        r     = c(scores[valid], scores[valid][[1]]),
        theta = c(dims[valid],   dims[valid][[1]]),
        fill = "toself", fillcolor = "rgba(76,114,176,0.25)",
        line = list(color = "#4C72B0"), showlegend = FALSE
      ) %>%
        plotly::layout(
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 5))),
          showlegend = FALSE
        )
    )
  }

  # Overlay: una traza por materia
  p <- plotly::plot_ly(type = "scatterpolar")
  added <- 0L
  for (mat in materias_overlay) {
    df_mat <- df %>% dplyr::filter(materia_nombre == mat)
    if (nrow(df_mat) == 0) next
    scores <- .radar_scores(df_mat)
    pal    <- RADAR_PALETTE[[(added %% length(RADAR_PALETTE)) + 1L]]
    added  <- added + 1L
    p <- p %>% plotly::add_trace(
      r         = c(as.numeric(scores), as.numeric(scores)[[1]]),
      theta     = c(dims, dims[[1]]),
      name      = mat,
      fill      = "toself",
      fillcolor = pal$fill,
      line      = list(color = pal$line),
      mode      = "lines+markers"
    )
  }
  if (added == 0L) return(plotly::plot_ly() %>% plotly::layout(title = "Sin datos"))

  p %>%
    plotly::layout(
      polar      = list(radialaxis = list(visible = TRUE, range = c(0, 5))),
      showlegend = TRUE,
      legend     = list(orientation = "h", y = -0.15)
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
