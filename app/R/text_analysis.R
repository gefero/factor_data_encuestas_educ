library(dplyr)
library(stringr)

TEXTO_COLS_LABELS <- c(
  texto_docente         = "Comentarios sobre el/la docente",
  texto_positivos       = "Aspectos positivos",
  texto_negativos       = "Aspectos a mejorar",
  texto_aprendiste      = "Lo más importante aprendido",
  texto_recomendaciones = "Recomendaciones al docente"
)

STOPWORDS_ES <- c(
  "de", "la", "que", "el", "en", "y", "a", "los", "del", "se", "las",
  "un", "por", "con", "no", "una", "su", "para", "es", "al", "lo",
  "como", "mas", "pero", "sus", "le", "ya", "o", "fue", "si", "porque",
  "esta", "entre", "cuando", "muy", "sin", "sobre", "también", "me",
  "hasta", "hay", "donde", "han", "quien", "están", "estado", "desde",
  "todo", "nos", "durante", "todos", "uno", "les", "ni", "contra",
  "otros", "ese", "eso", "ante", "ellos", "e", "esto", "mi", "antes",
  "algunos", "qué", "unos", "yo", "otro", "otras", "son", "cual",
  "sean", "ha", "cada", "mejor", "ser", "mucho", "bien", "tiene",
  "creo", "puede", "hacer", "asi", "buena", "bueno", "materia",
  "docente", "german", "rosati", "catedra", "clase", "clases",
  "curso", "cursada", "siempre", "nunca",
  # pronombres/adverbios funcionales sin valor semántico en wordcloud
  "tan", "nan", "sea", "mismo", "misma", "igual",
  "ninguno", "ninguna", "nada",
  "este", "esta", "estos", "estas",   # demostrativos
  "tener", "tuve", "tipo"             # verbos/sustantivos genéricos
)

ACCENT_MAP_TEXT <- c(
  "á"="a","à"="a","é"="e","è"="e","í"="i","ì"="i",
  "ó"="o","ò"="o","ú"="u","ù"="u","ñ"="n",
  "ä"="a","ë"="e","ï"="i","ö"="o","ü"="u"
)

clean_tokens <- function(textos, all_stop) {
  textos %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(ACCENT_MAP_TEXT) %>%
    stringr::str_replace_all("[^a-z\\s]", " ") %>%
    stringr::str_squish() %>%
    stringr::str_split("\\s+") %>%
    lapply(function(ws) ws[nchar(ws) >= 3 & !ws %in% all_stop])
}

prepare_text_cloud <- function(df, col_name, extra_stopwords = character(0),
                               use_bigrams = FALSE) {
  if (!col_name %in% colnames(df)) return(data.frame(word = character(), freq = integer()))

  textos <- df[[col_name]]
  textos <- textos[!is.na(textos) & nchar(trimws(textos)) > 2]
  if (length(textos) == 0) return(data.frame(word = character(), freq = integer()))

  all_stop  <- c(STOPWORDS_ES, extra_stopwords)
  tok_list  <- clean_tokens(textos, all_stop)

  terminos <- if (!use_bigrams) {
    unlist(tok_list)
  } else {
    unlist(lapply(tok_list, function(ws) {
      if (length(ws) < 2) return(character(0))
      paste(ws[-length(ws)], ws[-1])
    }))
  }

  if (length(terminos) == 0) return(data.frame(word = character(), freq = integer()))

  freq_tbl <- sort(table(terminos), decreasing = TRUE)
  data.frame(
    word = names(freq_tbl),
    freq = as.integer(freq_tbl),
    stringsAsFactors = FALSE
  )
}

compute_sentiment <- function(df, col_name, group_vars = "cuatrimestre_ord") {
  if (!requireNamespace("syuzhet", quietly = TRUE)) return(NULL)
  if (!col_name %in% colnames(df)) return(NULL)

  df_text <- df %>%
    dplyr::filter(!is.na(.data[[col_name]]), nchar(trimws(.data[[col_name]])) > 2) %>%
    dplyr::select(dplyr::all_of(c(group_vars, col_name)))

  if (nrow(df_text) == 0) return(NULL)

  sentimientos <- tryCatch(
    syuzhet::get_nrc_sentiment(df_text[[col_name]], language = "spanish"),
    error = function(e) NULL
  )
  if (is.null(sentimientos)) return(NULL)

  emociones_principales <- c("positive", "negative", "anger", "fear", "joy",
                              "sadness", "surprise", "trust", "anticipation", "disgust")
  cols_sent <- intersect(emociones_principales, colnames(sentimientos))

  dplyr::bind_cols(df_text[group_vars], sentimientos[, cols_sent, drop = FALSE]) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(cols_sent), ~ mean(.x, na.rm = TRUE)),
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(cols_sent),
      names_to = "emocion",
      values_to = "score"
    ) %>%
    dplyr::mutate(
      emocion_es = dplyr::case_when(
        emocion == "positive"     ~ "Positivo",
        emocion == "negative"     ~ "Negativo",
        emocion == "anger"        ~ "Enojo",
        emocion == "fear"         ~ "Miedo",
        emocion == "joy"          ~ "Alegría",
        emocion == "sadness"      ~ "Tristeza",
        emocion == "surprise"     ~ "Sorpresa",
        emocion == "trust"        ~ "Confianza",
        emocion == "anticipation" ~ "Anticipación",
        emocion == "disgust"      ~ "Disgusto",
        TRUE ~ emocion
      )
    )
}
