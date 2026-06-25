library(tidyverse)

 
list.files(path, pattern=".xlsx", full.names = TRUE)
get_colnames_list <- function(path='./data/raw/'){
        files <- list.files(path, pattern=".xlsx", full.names = TRUE)
        cols <- list()
        
        for (f in files){
#                print(f)
                df <- readxl::read_xlsx(f, sheet = "Encuesta",skip = 14) %>% 
                        janitor::clean_names()
                
                cols <- df %>% colnames()
                
                df <- df %>%
                
                
                cols[[f]] <- col
        }        
        
        return(cols)
}

cols <- get_colnames_list()

cols[[3]][is.element(cols[[3]], cols[[1]])]





cols_to_use <- c(
        "periodo_lectivo",
        "materia_nombre",
        "tu_situacion_en_relacion_a_esta_materia",
        "tus_comentarios_sobre_el_la_docente",
        "todos_los_contenidos_tuvieron_el_tiempo_necesario_para_su_desarrollo",
        "el_desarrollo_de_los_temas_fue_organizado_y_claro",
        "esta_materia_aporto_a_tu_proceso_de_formacion",                                                                                                                                                                                           
        "esta_materia_te_genero_entusiasmo_y_ganas_de_aprender",
        "que_es_para_vos_lo_mas_importante_que_aprendiste_en_esta_materia",
        "en_general_que_aspectos_positivos_destacarias_de_esta_materia",                                                                                                                                                                           
        "en_general_que_aspectos_negativos_destacarias_de_esta_materia")


get_data <- function(path='./data/raw/', cols_to_use=cols_to_use){
        files <- list.files(path, pattern=".xlsx", full.names = TRUE)
        dff <- cols_to_use %>% purrr::map_dfc(setNames, object = list(character()))
        for (f in files){
                df <- readxl::read_xlsx(f, sheet = "Encuesta",skip = 14) %>% 
                        janitor::clean_names() %>% 
                        select(all_of(cols_to_use))

                dff <- bind_rows(dff, df)
        }        
        
        return(dff)
}

df<-get_data(path='./data/raw/', cols_to_use=cols_to_use)

df <- df %>%
        rename(tiempo=todos_los_contenidos_tuvieron_el_tiempo_necesario_para_su_desarrollo,
               claridad=el_desarrollo_de_los_temas_fue_organizado_y_claro,
               aporte=esta_materia_aporto_a_tu_proceso_de_formacion,                                                                                                                                                                                           
               entusiasmo=esta_materia_te_genero_entusiasmo_y_ganas_de_aprender
        ) %>%
        mutate(periodo_lectivo = case_when(
                periodo_lectivo == "2° cuatrimestre" ~ "2do cuatrimestre",
                TRUE ~ periodo_lectivo)
        ) %>%
        filter(tu_situacion_en_relacion_a_esta_materia %in% 
                       c("1-La cursé y finalicé (hayas o no aprobado)", 
                         "2-Abandoné con más del 50% cursado")
        )

x<-df %>%
        select(materia_nombre, tiempo:entusiasmo) %>%
        pivot_longer(cols=tiempo:entusiasmo) %>%
        group_by(materia_nombre, name, value) %>%
        summarise(n=n()) %>%
        mutate(prop=n/sum(n))

x %>% ggplot() + 
        geom_col(aes(x=value, fill=value, y=prop), show.legend = FALSE) + 
        facet_wrap(~materia_nombre+name) +
        scale_fill_viridis_d() +
        labs(x="Escala de acuerdo",
             y="%",
             fill="Valor") +
        theme_minimal() +
                theme(axis.text.x=element_text(angle = 45, hjust = 1))
