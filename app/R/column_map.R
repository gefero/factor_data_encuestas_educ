# Patrones regex para mapear columnas reales (snake_case) a nombres canónicos.
# Cada patrón cubre tanto el formato 2022-2024 como el 2025.

COL_PATTERNS <- list(
  # Metadatos
  periodo_lectivo      = "^periodo_lectivo$",
  ano_academico        = "^ano_academico$",
  materia_nombre       = "^materia_nombre$",
  situacion_estudiante = "^tu_situacion_en_relacion_a_esta_materia$",

  # Dim 1 - Planificación
  plan_programa   = "1_1_al_inicio_de_la_cursada",                                           # solo 2025
  plan_coherencia = "hubo_coherencia_entre_la_propuesta|1_2_hubo_coherencia",
  plan_tiempo     = "todos_los_contenidos_tuvieron_el_tiempo_necesario|1_3_todos_los_contenidos_tuvieron_el_tiempo_suficiente",
  plan_campus     = "el_uso_del_campus_virtual_de_la_unsam_me_resulto_amigable|1_4_el_la_docente_utilizo_el_campus_virtual",

  # Dim 2 - Contenidos (solo 2025)
  cont_relacion    = "2_1_el_la_docente_relaciono_conceptos",
  cont_ejemplos    = "2_2_el_la_docente_uso_ejemplos",
  cont_bibliografia = "2_3_los_recursos_y_o_bibliografia",

  # Dim 3 - Clases
  clase_claridad      = "el_desarrollo_de_los_temas_fue_organizado_y_claro|3_1_el_la_docente_desarrollo_los_temas_con_claridad",
  clase_participacion = "el_la_docente_alento_tu_participacion_durante_la_cursada|3_2_el_la_docente_fomento_la_participacion",
  clase_tecnologia    = "se_incluyeron_guias_didacticas_y_materiales_multimediales|3_3_las_tecnologias_digitales",
  clase_actividades   = "las_actividades_propuestas_fueron_pertinentes_para_la_comprension|3_4_las_actividades_propuestas_en_la_clase_contribuyeron",

  # Dim 4 - Evaluación
  eval_criterios       = "4_1_el_la_docente_comunico_claramente_los_criterios",               # solo 2025
  eval_correspondencia = "las_instancias_de_evaluacion_se_correspondieron_con_lo_ensenado|4_2_la_evaluacion_se_correspondio",
  eval_instrumentos    = "los_instrumentos_para_evaluar_fueron_adecuados",
  eval_devolucion      = "las_devoluciones_de_las_instancias_de_evaluacion_aportaron_a_tu_aprendizaje|4_4_despues_de_las_evaluaciones",
  eval_tiempo_dev      = "las_devoluciones_se_hicieron_en_un_tiempo_adecuado|4_5_el_la_docente_realizo_las_devoluciones_en_un_tiempo",

  # Dim 5 - Ambiente y vínculo
  amb_ambiente  = "5_1_el_la_docente_establecio_un_ambiente",                                 # solo 2025
  amb_trato     = "5_2_el_la_docente_promovio_un_trato_respetuoso",                          # solo 2025
  amb_preguntas = "5_3_el_la_docente_acogio_positivamente_las_preguntas",                    # solo 2025
  amb_interes   = "el_la_docente_se_mostro_muy_interesado_a_sobre_tu_progreso|5_4_el_la_docente_genero_interes",

  # Dim 6 - Balance
  bal_aprendizaje = "esta_materia_aporto_a_tu_proceso_de_formacion|6_1_he_aprendido_y_comprendido",
  bal_eleccion    = "esta_materia_te_genero_entusiasmo_y_ganas_de_aprender|6_2_la_materia_contribuyo_a_confirmar",

  # Texto libre
  texto_docente         = "^tus_comentarios_sobre_el_la_docente$",
  texto_aprendiste      = "que_es_para_vos_lo_mas_importante_que_aprendiste",
  texto_positivos       = "aspectos_positivos_destacarias",
  texto_negativos       = "aspectos_negativos_destacarias|aspectos_consideras_que_se_podrian_mejorar|6_c_pensando_en_futuras",
  texto_recomendaciones = "que_recomendaciones_le_harias_a_tu_docente"
)

# Dado un df con colnames en snake_case, retorna un named vector canónico → real
resolve_columns <- function(df) {
  real_cols <- colnames(df)
  mapping <- list()
  for (canonical in names(COL_PATTERNS)) {
    pat <- COL_PATTERNS[[canonical]]
    matches <- real_cols[grepl(pat, real_cols, ignore.case = TRUE)]
    mapping[[canonical]] <- if (length(matches) >= 1) matches[[1]] else NA_character_
  }
  mapping
}
