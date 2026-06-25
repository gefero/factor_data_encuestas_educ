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
  "curso", "cursada", "siempre", "nunca"
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

# TF-IDF scores summed across documents.
# tok_list: list of character vectors (one per document), already filtered.
# Returns named numeric vector sorted by score descending.
compute_tfidf_scores <- function(tok_list) {
  N <- length(tok_list)
  if (N == 0) return(setNames(numeric(0), character(0)))

  rows <- do.call(rbind, lapply(seq_along(tok_list), function(i) {
    ws <- tok_list[[i]]
    if (length(ws) == 0) return(NULL)
    tbl <- table(ws)
    data.frame(doc_id = i, term = names(tbl),
               tf = as.numeric(tbl) / length(ws),
               stringsAsFactors = FALSE)
  }))
  if (is.null(rows) || nrow(rows) == 0) return(setNames(numeric(0), character(0)))

  df_counts  <- tapply(rows$doc_id, rows$term, function(x) length(unique(x)))
  idf        <- log((N + 1) / (df_counts + 1)) + 1    # sklearn-style smoothed IDF
  rows$tfidf <- rows$tf * idf[rows$term]

  sort(tapply(rows$tfidf, rows$term, sum), decreasing = TRUE)
}

prepare_text_cloud <- function(df, col_name, extra_stopwords = character(0),
                               use_bigrams = FALSE) {
  if (!col_name %in% colnames(df)) return(data.frame(word = character(), freq = numeric()))

  textos <- df[[col_name]]
  textos <- textos[!is.na(textos) & nchar(trimws(textos)) > 2]
  if (length(textos) == 0) return(data.frame(word = character(), freq = numeric()))

  all_stop <- c(STOPWORDS_ES, extra_stopwords)
  tok_list <- clean_tokens(textos, all_stop)

  term_list <- if (!use_bigrams) {
    tok_list
  } else {
    lapply(tok_list, function(ws) {
      if (length(ws) < 2) return(character(0))
      paste(ws[-length(ws)], ws[-1])
    })
  }

  scores <- compute_tfidf_scores(term_list)
  if (length(scores) == 0) return(data.frame(word = character(), freq = numeric()))

  data.frame(word = names(scores), freq = as.numeric(scores), stringsAsFactors = FALSE)
}

compute_sentiment <- function(df, col_name, group_vars = "cuatrimestre_ord") {
  if (!requireNamespace("syuzhet", quietly = TRUE)) return(NULL)
  if (!col_name %in% colnames(df)) return(NULL)

  df_text <- df %>%
    dplyr::filter(!is.na(.data[[col_name]]), nchar(trimws(.data[[col_name]])) > 2)
  if (nrow(df_text) == 0) return(NULL)

  nrc <- tryCatch(
    syuzhet::get_sentiment_dictionary("nrc", language = "spanish"),
    error = function(e) NULL
  )
  if (is.null(nrc)) return(NULL)

  emociones <- c("positive", "negative", "anger", "fear", "joy",
                 "sadness", "surprise", "trust", "anticipation", "disgust")
  nrc_cols <- intersect(emociones, colnames(nrc))
  word_col <- colnames(nrc)[vapply(nrc, is.character, logical(1))][1]

  textos   <- df_text[[col_name]]
  tok_list <- clean_tokens(textos, STOPWORDS_ES)
  N        <- length(tok_list)

  # Build per-document TF-IDF table
  rows <- do.call(rbind, lapply(seq_along(tok_list), function(i) {
    ws <- tok_list[[i]]
    if (length(ws) == 0) return(NULL)
    tbl <- table(ws)
    data.frame(doc_id = i, term = names(tbl),
               tf = as.numeric(tbl) / length(ws),
               stringsAsFactors = FALSE)
  }))
  if (is.null(rows) || nrow(rows) == 0) return(NULL)

  df_counts  <- tapply(rows$doc_id, rows$term, function(x) length(unique(x)))
  idf        <- log((N + 1) / (df_counts + 1)) + 1
  rows$tfidf <- rows$tf * idf[rows$term]

  # Join TF-IDF table with NRC lexicon (only rows with at least one emotion)
  nrc_sub <- nrc[rowSums(nrc[, nrc_cols, drop = FALSE]) > 0,
                 c(word_col, nrc_cols), drop = FALSE]
  matched <- merge(rows, nrc_sub, by.x = "term", by.y = word_col, all.x = FALSE)
  if (nrow(matched) == 0) return(NULL)

  # Accumulate TF-IDF-weighted emotion scores into a doc × emotion matrix
  doc_emo <- matrix(0, nrow = N, ncol = length(nrc_cols),
                    dimnames = list(seq_len(N), nrc_cols))
  for (em in nrc_cols) {
    agg <- tapply(matched$tfidf * matched[[em]], matched$doc_id, sum)
    doc_emo[as.integer(names(agg)), em] <- agg
  }

  dplyr::bind_cols(df_text[group_vars], as.data.frame(doc_emo)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(nrc_cols), ~ mean(.x, na.rm = TRUE)),
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(nrc_cols), names_to = "emocion", values_to = "score"
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
