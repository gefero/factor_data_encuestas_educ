library(readxl)
library(dplyr)
library(stringr)
library(purrr)

ACCENT_MAP <- c(
  "á"="a","à"="a","ä"="a","â"="a",
  "é"="e","è"="e","ë"="e","ê"="e",
  "í"="i","ì"="i","ï"="i","î"="i",
  "ó"="o","ò"="o","ö"="o","ô"="o",
  "ú"="u","ù"="u","ü"="u","û"="u",
  "ñ"="n",
  "Á"="a","É"="e","Í"="i","Ó"="o","Ú"="u"
)

remove_accents <- function(x) stringr::str_replace_all(x, ACCENT_MAP)

clean_names_df <- function(df) {
  nms <- remove_accents(tolower(colnames(df)))
  nms <- gsub("[^a-z0-9]+", "_", nms)
  nms <- gsub("_+", "_", nms)
  nms <- gsub("^_|_$", "", nms)
  colnames(df) <- nms
  df
}

detect_sheet <- function(path) {
  sheets <- readxl::excel_sheets(path)
  if ("Base Encuesta" %in% sheets) return(list(sheet = "Base Encuesta", skip = 19))
  if ("Encuesta" %in% sheets)      return(list(sheet = "Encuesta",      skip = 14))
  list(sheet = sheets[[1]], skip = 14)
}

extract_year <- function(filepath) {
  m <- regmatches(basename(filepath), regexpr("\\d{4}", basename(filepath)))
  if (length(m) == 1) m else NA_character_
}

parse_likert <- function(x) {
  n <- suppressWarnings(as.integer(substr(as.character(x), 1, 1)))
  ifelse(is.na(n) | n > 5, NA_integer_, n)
}

load_one_excel <- function(path) {
  spec <- detect_sheet(path)
  raw  <- readxl::read_xlsx(path, sheet = spec$sheet, skip = spec$skip,
                             col_types = "text")
  df   <- clean_names_df(raw)
  col_map <- resolve_columns(df)

  present <- Filter(function(x) !is.na(x), col_map)
  df_sel  <- df[, unlist(present), drop = FALSE]
  colnames(df_sel) <- names(present)

  df_sel$archivo_origen <- basename(path)
  df_sel$year_archivo   <- extract_year(path)
  df_sel
}

load_all_excel <- function(path = "./data/raw/") {
  files <- list.files(path, pattern = "\\.xlsx$", full.names = TRUE)
  files <- files[!grepl("^~\\$", basename(files))]  # excluir temporales de LibreOffice
  if (length(files) == 0) stop("No se encontraron archivos .xlsx en ", path)

  dfs <- purrr::map(files, function(f) {
    tryCatch(
      load_one_excel(f),
      error = function(e) {
        warning("Error al leer ", basename(f), ": ", e$message)
        NULL
      }
    )
  })

  combined <- dplyr::bind_rows(Filter(Negate(is.null), dfs))
  normalize_data(combined)
}

MATERIA_CANONICAL <- list(
  "Laboratorio de Datos"  = "labo.*dato|laboratorio.*dato",
  "Machine Learning"      = "machine.learning",
  "Metodos Multivariados" = "multivaria|met.*analisis|metodo.*analisis"
)

normalize_materia <- function(x) {
  xn <- stringr::str_to_lower(stringr::str_replace_all(x, ACCENT_MAP))
  result <- x
  for (canonical in names(MATERIA_CANONICAL)) {
    pat <- MATERIA_CANONICAL[[canonical]]
    result <- ifelse(grepl(pat, xn), canonical, result)
  }
  result
}

# Valores literales que los estudiantes escriben para indicar "no respondí"
SENTINELS_REGEX <- paste0("^(", paste(c(
  "sin respuesta", "nan",
  "s/r", "sr",
  "ninguno\\.?", "ninguna\\.?", "ninguno/a\\.?",
  "nada\\.?", "nada en particular\\.?",
  "no hubo\\.?", "no hubieron\\.?",
  "no se me ocurre\\.?", "no tiene\\.?",
  "creo que nada\\.?"
), collapse = "|"), ")$")

replace_sentinels <- function(x) {
  is_sentinel <- grepl(SENTINELS_REGEX, trimws(tolower(x)))
  ifelse(is_sentinel, NA_character_, x)
}

normalize_data <- function(df) {
  texto_cols <- intersect(
    c("texto_docente", "texto_positivos", "texto_negativos",
      "texto_aprendiste", "texto_recomendaciones"),
    colnames(df)
  )

  likert_cols <- c(
    "plan_programa", "plan_coherencia", "plan_tiempo", "plan_campus",
    "cont_relacion", "cont_ejemplos", "cont_bibliografia",
    "clase_claridad", "clase_participacion", "clase_tecnologia", "clase_actividades",
    "eval_criterios", "eval_correspondencia", "eval_instrumentos",
    "eval_devolucion", "eval_tiempo_dev",
    "amb_ambiente", "amb_trato", "amb_preguntas", "amb_interes",
    "bal_aprendizaje", "bal_eleccion"
  )
  existing_likert <- intersect(likert_cols, colnames(df))

  if (length(texto_cols) > 0)
    df <- df %>% dplyr::mutate(dplyr::across(dplyr::all_of(texto_cols), replace_sentinels))

  df <- df %>%
    dplyr::mutate(
      materia_nombre = normalize_materia(materia_nombre),
      dplyr::across(dplyr::all_of(existing_likert), parse_likert),
      situacion_cat = dplyr::case_when(
        grepl("^1-", situacion_estudiante) ~ "Finalizo cursada",
        grepl("^2-", situacion_estudiante) ~ "Abandono >50%",
        grepl("^3-", situacion_estudiante) ~ "Abandono <50%",
        TRUE ~ "Otro"
      ),
      periodo_norm = dplyr::case_when(
        grepl("^1er|^1[^0-9]|primer", periodo_lectivo, ignore.case = TRUE) ~ "C1",
        grepl("^2do|^2[^0-9]|segundo", periodo_lectivo, ignore.case = TRUE) ~ "C2",
        TRUE ~ "C?"
      ),
      cuatrimestre_ord = paste0(year_archivo, "-", periodo_norm)
    )

  df$cuatrimestre_ord <- factor(
    df$cuatrimestre_ord,
    levels = sort(unique(df$cuatrimestre_ord))
  )

  df <- df %>%
    dplyr::filter(situacion_cat %in% c("Finalizo cursada", "Abandono >50%"))

  df
}
