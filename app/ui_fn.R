ui <- bslib::page_navbar(
  title = "Encuestas de Enseñanza",
  theme = bslib::bs_theme(bootswatch = "flatly", base_font = bslib::font_google("Inter")),
  fillable = FALSE,

  # ── Sidebar global ──────────────────────────────────────────────────────────
  sidebar = bslib::sidebar(
    width = 240,
    selectInput("sel_materia", "Materia:",
                choices = c("Todas"), selected = "Todas"),
    selectInput("sel_desde", "Desde:", choices = NULL),
    selectInput("sel_hasta", "Hasta:", choices = NULL),
    hr(),
    actionButton("btn_reload", "Recargar datos",
                 icon = icon("rotate"), class = "btn-sm btn-outline-secondary w-100"),
    hr(),
    uiOutput("ui_n_total"),
    includeCSS("www/custom.css")
  ),

  # ── Tab 1: Resumen ───────────────────────────────────────────────────────────
  bslib::nav_panel(
    title = "Resumen",
    icon  = icon("chart-pie"),
    fluidRow(
      column(3, bslib::value_box("Respuestas", uiOutput("vb_n"), showcase = icon("users"),
                                 theme = "primary")),
      column(3, bslib::value_box("Promedio global", uiOutput("vb_global"), showcase = icon("star"),
                                 theme = "success")),
      column(3, bslib::value_box("Materias", uiOutput("vb_materias"), showcase = icon("book"),
                                 theme = "info")),
      column(3, bslib::value_box("Cuatrimestres", uiOutput("vb_cuatrimestres"), showcase = icon("calendar"),
                                 theme = "warning"))
    ),
    br(),
    fluidRow(
      column(6,
        bslib::card(
          bslib::card_header("Radar por dimensión"),
          selectizeInput("sel_radar_materias", NULL,
                         choices = NULL, multiple = TRUE,
                         options = list(placeholder = "Global — seleccioná materias para superponer")),
          plotlyOutput("plot_radar", height = "320px")
        )
      ),
      column(6,
        bslib::card(
          bslib::card_header("Promedio por dimensión"),
          plotlyOutput("plot_dim_bar", height = "350px")
        )
      )
    )
  ),

  # ── Tab 2: Evolución temporal ────────────────────────────────────────────────
  bslib::nav_panel(
    title = "Evolución temporal",
    icon  = icon("chart-line"),
    bslib::card(
      bslib::card_header("Evolución de dimensiones a lo largo del tiempo"),
      plotlyOutput("plot_evolucion", height = "450px")
    )
  ),

  # ── Tab 3: Por materia ───────────────────────────────────────────────────────
  bslib::nav_panel(
    title = "Por materia",
    icon  = icon("book-open"),
    fluidRow(
      column(4,
        selectInput("sel_dim_materia", "Dimensión:",
                    choices = c("Todas", names(DIMENSION_COLS)))
      )
    ),
    bslib::card(
      bslib::card_header("Comparación entre materias"),
      plotlyOutput("plot_por_materia", height = "420px")
    )
  ),

  # ── Tab 4: Distribución de respuestas ───────────────────────────────────────
  bslib::nav_panel(
    title = "Distribución",
    icon  = icon("bar-chart"),
    fluidRow(
      column(4,
        selectInput("sel_dim_dist", "Dimensión:",
                    choices = names(DIMENSION_COLS), selected = "Balance")
      )
    ),
    bslib::card(
      bslib::card_header("Distribución de respuestas por pregunta"),
      plotlyOutput("plot_distribucion", height = "420px")
    )
  ),

  # ── Tab 5: Texto libre ───────────────────────────────────────────────────────
  bslib::nav_panel(
    title = "Texto libre",
    icon  = icon("comment"),
    fluidRow(
      column(4,
        selectInput("sel_texto_col", "Columna de texto:",
                    choices = setNames(names(TEXTO_COLS_LABELS), TEXTO_COLS_LABELS))
      ),
      column(3,
        radioButtons("sel_ngram", "Tipo de término:",
                     choices = c("Unigramas" = "uni", "Bigramas" = "bi"),
                     inline = TRUE)
      )
    ),
    bslib::navset_tab(
      bslib::nav_panel("Nube de palabras",
        bslib::card(
          bslib::card_header("Palabras más frecuentes"),
          if (exists("HAS_WORDCLOUD2") && HAS_WORDCLOUD2)
            wordcloud2::wordcloud2Output("plot_wordcloud", height = "380px")
          else
            tags$p(class = "text-muted p-3",
                   "Instalar el paquete 'wordcloud2' para ver la nube de palabras.")
        )
      ),
      bslib::nav_panel("Top 20 palabras",
        bslib::card(
          bslib::card_header("Frecuencia de términos"),
          plotlyOutput("plot_top_words", height = "420px")
        )
      ),
      bslib::nav_panel("Análisis de sentimiento",
        bslib::card(
          bslib::card_header("Emociones por cuatrimestre (NRC en español)"),
          plotlyOutput("plot_sentiment", height = "400px")
        )
      ),
      bslib::nav_panel("Respuestas",
        DT::dataTableOutput("tabla_texto")
      )
    )
  ),

  # ── Tab 6: Datos crudos ──────────────────────────────────────────────────────
  bslib::nav_panel(
    title = "Datos",
    icon  = icon("table"),
    bslib::card(
      bslib::card_header("Datos procesados"),
      DT::dataTableOutput("tabla_datos")
    )
  )
)
