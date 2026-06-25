server <- function(input, output, session) {

  # ── Carga de datos ────────────────────────────────────────────────────────────
  raw_data <- eventReactive(
    ignoreNULL = FALSE,
    eventExpr  = input$btn_reload,
    valueExpr  = {
      withProgress(message = "Cargando encuestas...", value = 0, {
        tryCatch(
          load_all_excel(DATA_PATH),
          error = function(e) {
            showNotification(paste("Error al cargar datos:", e$message), type = "error")
            NULL
          }
        )
      })
    }
  )

  # ── Actualizar selectors cuando cambian los datos ────────────────────────────
  observeEvent(raw_data(), {
    df <- raw_data()
    if (is.null(df)) return()

    materias      <- sort(unique(df$materia_nombre))
    cuatrimestres <- levels(df$cuatrimestre_ord)
    mat_ord       <- intersect(MATERIA_ORDER, materias)

    updateSelectInput(session, "sel_materia",
                      choices  = c("Todas", materias),
                      selected = "Todas")
    updateSelectInput(session, "sel_desde",
                      choices  = cuatrimestres,
                      selected = cuatrimestres[1])
    updateSelectInput(session, "sel_hasta",
                      choices  = cuatrimestres,
                      selected = cuatrimestres[length(cuatrimestres)])
    updateSelectizeInput(session, "sel_radar_materias",
                         choices  = mat_ord,
                         selected = character(0))
  })

  # ── Datos filtrados por tiempo (para radar overlay, sin filtro de materia) ───
  datos_temporal <- reactive({
    df <- raw_data()
    if (is.null(df)) return(NULL)
    cuats <- levels(df$cuatrimestre_ord)
    desde <- input$sel_desde
    hasta <- input$sel_hasta
    if (!is.null(desde) && !is.null(hasta) && desde %in% cuats && hasta %in% cuats) {
      idx_desde <- which(cuats == desde)
      idx_hasta <- which(cuats == hasta)
      cuats_sel <- cuats[seq(min(idx_desde, idx_hasta), max(idx_desde, idx_hasta))]
      df <- df %>% dplyr::filter(cuatrimestre_ord %in% cuats_sel)
    }
    df
  })

  # ── Datos filtrados (tiempo + materia) ───────────────────────────────────────
  datos_filtrados <- reactive({
    df <- datos_temporal()
    if (is.null(df)) return(NULL)
    if (!is.null(input$sel_materia) && input$sel_materia != "Todas")
      df <- df %>% dplyr::filter(materia_nombre == input$sel_materia)
    df
  })

  # ── Scores de dimensiones ─────────────────────────────────────────────────────
  dim_scores <- reactive({
    df <- datos_filtrados()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    compute_dim_scores(df, group_vars = c("materia_nombre", "cuatrimestre_ord"))
  })

  # ── Value boxes ───────────────────────────────────────────────────────────────
  output$vb_n <- renderUI({
    df <- datos_filtrados()
    if (is.null(df)) return("—")
    format(nrow(df), big.mark = ".")
  })

  output$vb_global <- renderUI({
    df <- datos_filtrados()
    if (is.null(df)) return("—")
    g <- compute_global_score(df)
    if (is.na(g)) return("—")
    sprintf("%.2f / 5", g)
  })

  output$vb_materias <- renderUI({
    df <- datos_filtrados()
    if (is.null(df)) return("—")
    as.character(length(unique(df$materia_nombre)))
  })

  output$vb_cuatrimestres <- renderUI({
    df <- datos_filtrados()
    if (is.null(df)) return("—")
    as.character(length(unique(as.character(df$cuatrimestre_ord))))
  })

  output$ui_n_total <- renderUI({
    df <- raw_data()
    if (is.null(df)) return(NULL)
    tags$small(class = "text-muted",
               paste0(format(nrow(df), big.mark = "."), " respuestas en total"))
  })

  # ── Resumen: radar ─────────────────────────────────────────────────────────────
  # Sincronizar choices del overlay: excluir la materia ya seleccionada en sidebar
  observe({
    sel_mat <- input$sel_materia
    df <- raw_data()
    if (is.null(df)) return()
    mat_ord <- intersect(MATERIA_ORDER, unique(df$materia_nombre))
    if (!is.null(sel_mat) && sel_mat != "Todas")
      mat_ord <- setdiff(mat_ord, sel_mat)
    updateSelectizeInput(session, "sel_radar_materias",
                         choices  = mat_ord,
                         selected = intersect(input$sel_radar_materias, mat_ord))
  })

  output$plot_radar <- renderPlotly({
    df <- datos_temporal()
    if (is.null(df) || nrow(df) == 0)
      return(plotly::plot_ly() %>% plotly::layout(title = "Sin datos"))
    sel_mat <- input$sel_materia
    overlay <- input$sel_radar_materias
    mats <- if (!is.null(sel_mat) && sel_mat != "Todas")
      unique(c(sel_mat, overlay))   # materia del sidebar primero, luego las extra
    else
      if (length(overlay) > 0) overlay else NULL
    plot_radar(df, materias_overlay = mats)
  })

  output$plot_dim_bar <- renderPlotly({
    scores <- dim_scores()
    if (is.null(scores) || nrow(scores) == 0)
      return(plotly::plot_ly() %>% plotly::layout(title = "Sin datos"))

    df_bar <- scores %>%
      dplyr::group_by(dimension) %>%
      dplyr::summarise(promedio = mean(promedio, na.rm = TRUE),
                       n = sum(n_respuestas), .groups = "drop")

    plotly::plot_ly(df_bar, x = ~reorder(dimension, promedio), y = ~promedio,
                    color = ~dimension, colors = DIM_COLORS,
                    type = "bar", showlegend = FALSE,
                    text = ~paste0(dimension, "<br>Promedio: ", round(promedio, 2),
                                   "<br>N: ", n),
                    hoverinfo = "text") %>%
      plotly::layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Promedio (1-5)", range = c(1, 5)),
        margin = list(b = 100)
      )
  })

  # ── Evolución temporal ────────────────────────────────────────────────────────
  output$plot_evolucion <- renderPlotly({
    scores <- dim_scores()
    if (is.null(scores) || nrow(scores) == 0)
      return(plotly::plot_ly() %>% plotly::layout(title = "Sin datos"))
    plot_evolucion(scores, materia_sel = input$sel_materia %||% "Todas")
  })

  # ── Por materia ───────────────────────────────────────────────────────────────
  output$plot_por_materia <- renderPlotly({
    scores <- dim_scores()
    if (is.null(scores) || nrow(scores) == 0)
      return(plotly::plot_ly() %>% plotly::layout(title = "Sin datos"))
    plot_por_materia(scores, dimension_sel = input$sel_dim_materia)
  })

  # ── Distribución ──────────────────────────────────────────────────────────────
  output$plot_distribucion <- renderPlotly({
    df <- datos_filtrados()
    req(df, input$sel_dim_dist)
    dist_df <- compute_distribution(df, input$sel_dim_dist)
    plot_distribucion(dist_df, titulo = paste("Distribución —", input$sel_dim_dist))
  })

  # ── Texto libre ───────────────────────────────────────────────────────────────
  word_freq <- reactive({
    df  <- datos_filtrados()
    col <- input$sel_texto_col
    if (is.null(df) || is.null(col)) return(NULL)
    use_bi <- isTRUE(input$sel_ngram == "bi")
    prepare_text_cloud(df, col, use_bigrams = use_bi)
  })

  output$plot_wordcloud <- renderWordcloud2({
    wf <- word_freq()
    if (is.null(wf) || nrow(wf) == 0) return(NULL)
    wordcloud2::wordcloud2(wf, size = 0.6, color = "random-dark")
  })

  output$plot_top_words <- renderPlotly({
    wf <- word_freq()
    if (is.null(wf) || nrow(wf) == 0)
      return(plotly::plot_ly() %>% plotly::layout(title = "Sin datos"))
    top20 <- head(wf, 20)
    plotly::plot_ly(top20, x = ~freq, y = ~reorder(word, freq),
                    type = "bar", orientation = "h",
                    marker = list(color = "#4C72B0")) %>%
      plotly::layout(
        xaxis = list(title = "Frecuencia"),
        yaxis = list(title = ""),
        margin = list(l = 120)
      )
  })

  output$plot_sentiment <- renderPlotly({
    df  <- datos_filtrados()
    col <- input$sel_texto_col
    if (is.null(df) || is.null(col)) return(NULL)
    sent <- compute_sentiment(df, col, group_vars = "cuatrimestre_ord")
    plot_sentiment(sent)
  })

  output$tabla_texto <- DT::renderDataTable({
    df  <- datos_filtrados()
    col <- input$sel_texto_col
    if (is.null(df) || is.null(col) || !col %in% colnames(df)) return(NULL)
    df %>%
      dplyr::select(cuatrimestre_ord, materia_nombre, dplyr::all_of(col)) %>%
      dplyr::filter(!is.na(.data[[col]]), nchar(trimws(.data[[col]])) > 0) %>%
      dplyr::rename(respuesta = dplyr::all_of(col)) %>%
      DT::datatable(options = list(pageLength = 15, scrollX = TRUE),
                    rownames = FALSE)
  })

  # ── Datos crudos ──────────────────────────────────────────────────────────────
  output$tabla_datos <- DT::renderDataTable({
    df <- datos_filtrados()
    if (is.null(df)) return(NULL)
    texto_cols <- c("texto_docente", "texto_aprendiste", "texto_positivos",
                    "texto_negativos", "texto_recomendaciones")
    meta_cols  <- c("cuatrimestre_ord", "materia_nombre", "situacion_cat", "archivo_origen")
    likert_cols <- unlist(DIMENSION_COLS)
    show_cols <- intersect(c(meta_cols, likert_cols[likert_cols %in% colnames(df)],
                              texto_cols[texto_cols %in% colnames(df)]),
                           colnames(df))
    DT::datatable(
      df[, show_cols, drop = FALSE],
      extensions = "Buttons",
      options = list(
        pageLength = 20,
        scrollX    = TRUE,
        dom        = "Bfrtip",
        buttons    = c("csv", "excel")
      ),
      rownames = FALSE
    )
  })
}

# Operador null-coalesce
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && a != "") a else b
