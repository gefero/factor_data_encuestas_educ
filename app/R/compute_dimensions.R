library(dplyr)
library(tidyr)
library(purrr)

DIMENSION_COLS <- list(
  "Planificación"      = c("plan_programa", "plan_coherencia", "plan_tiempo", "plan_campus"),
  "Contenidos"         = c("cont_relacion", "cont_ejemplos", "cont_bibliografia"),
  "Clases"             = c("clase_claridad", "clase_participacion", "clase_tecnologia", "clase_actividades"),
  "Evaluación"         = c("eval_criterios", "eval_correspondencia", "eval_instrumentos",
                           "eval_devolucion", "eval_tiempo_dev"),
  "Ambiente y vínculo" = c("amb_ambiente", "amb_trato", "amb_preguntas", "amb_interes"),
  "Balance"            = c("bal_aprendizaje", "bal_eleccion")
)

LIKERT_LABELS <- c("1-Nada", "2-Poco", "3-Medio", "4-Bastante", "5-Mucho")

# Promedio por dimensión agrupado según group_vars
compute_dim_scores <- function(df, group_vars = c("materia_nombre", "cuatrimestre_ord")) {
  purrr::imap_dfr(DIMENSION_COLS, function(cols, dim_name) {
    cols_present <- intersect(cols, colnames(df))
    if (length(cols_present) == 0) return(NULL)

    df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(.score = mean(dplyr::c_across(dplyr::all_of(cols_present)), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.nan(.score)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(
        promedio     = mean(.score, na.rm = TRUE),
        mediana      = median(.score, na.rm = TRUE),
        n_respuestas = sum(!is.na(.score)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(dimension = dim_name)
  })
}

# Distribución de respuestas (1-5) para una dimensión específica
compute_distribution <- function(df, dimension_name) {
  cols <- intersect(DIMENSION_COLS[[dimension_name]], colnames(df))
  if (length(cols) == 0) return(NULL)

  df %>%
    dplyr::select(materia_nombre, cuatrimestre_ord, dplyr::all_of(cols)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(cols), names_to = "pregunta", values_to = "valor") %>%
    dplyr::filter(!is.na(valor)) %>%
    dplyr::mutate(valor = factor(valor, levels = 1:5,
                                 labels = c("1","2","3","4","5")))
}

# Score global (promedio de todas las dimensiones)
compute_global_score <- function(df) {
  all_likert <- unlist(DIMENSION_COLS)
  cols_present <- intersect(all_likert, colnames(df))
  if (length(cols_present) == 0) return(NA_real_)
  mean(unlist(df[, cols_present]), na.rm = TRUE)
}
