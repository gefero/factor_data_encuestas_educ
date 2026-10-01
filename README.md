# Dashboard de Encuestas de Enseñanza

Dashboard Shiny para analizar encuestas de evaluación docente de la Licenciatura en Ciencia de Datos (UNSAM). Permite explorar la evolución temporal de indicadores por dimensión pedagógica, comparar materias, y analizar respuestas abiertas mediante frecuencia de términos y análisis de sentimiento.

---

## Estructura del repositorio

```
.
├── app/
│   ├── app.R              # Entry point de la app
│   ├── global.R           # Carga de paquetes y constantes globales
│   ├── ui_fn.R            # Definición de la UI (bslib page_navbar)
│   ├── server_fn.R        # Lógica del servidor (reactivos, plots, tablas)
│   ├── www/
│   │   └── custom.css     # Estilos adicionales
│   └── R/
│       ├── column_map.R      # Mapeo de columnas (COL_PATTERNS + resolve_columns)
│       ├── load_data.R       # Carga y normalización de Excel
│       ├── compute_dimensions.R  # Scores por dimensión y distribución Likert
│       ├── text_analysis.R   # Frecuencia de términos y análisis de sentimiento
│       └── plot_helpers.R    # Funciones de visualización (plotly)
├── data/
│   └── raw/               # Archivos .xlsx de encuestas (ver sección "Datos")
└── src/
    └── 0_proc.R           # Scripts auxiliares de preprocesamiento
```

---

## Cómo activar la app

Desde la raíz del repositorio, en una sesión de R:

```r
shiny::runApp("app/")
```

O desde la terminal:

```bash
Rscript -e "shiny::runApp('app/')"
```

La app carga los datos automáticamente al iniciarse. Si se agrega un nuevo archivo `.xlsx` a `data/raw/`, hacer click en el botón **"Recargar datos"** en el sidebar para incorporarlo.

---

## Dependencias

### Requeridas (se instalan automáticamente si faltan)

| Paquete  | Uso                                      |
|----------|------------------------------------------|
| shiny    | Framework web                            |
| bslib    | Tema Bootstrap 5 (flatly + Google Fonts) |
| dplyr    | Manipulación de datos                    |
| tidyr    | Pivoting y reshape                       |
| purrr    | Iteración funcional                      |
| stringr  | Procesamiento de texto                   |
| readxl   | Lectura de archivos Excel                |
| plotly   | Gráficos interactivos                    |
| DT       | Tablas interactivas con export CSV/Excel |

### Opcionales

| Paquete   | Uso                                      |
|-----------|------------------------------------------|
| wordcloud2 | Nube de palabras (tab "Texto libre")    |
| syuzhet    | Análisis de sentimiento NRC en español  |

Si alguno de estos no está instalado, la funcionalidad correspondiente se deshabilita gracefully (mensaje informativo en lugar del widget).

---

## Datos

Los archivos `.xlsx` deben colocarse en `data/raw/`. La app soporta **dos generaciones de formato**:

| Formato | Período    | Hoja          | Filas de encabezado a saltear |
|---------|------------|---------------|-------------------------------|
| Antiguo | 2022–2024  | `Encuesta`    | 14                            |
| Nuevo   | 2025+      | `Base Encuesta` | 19                          |

La detección del formato es automática. Las columnas se mapean a nombres canónicos mediante patrones regex definidos en `R/column_map.R`, lo que permite que ambos formatos convivan sin conflicto.

### Nombre de los archivos

Los nombres de archivo **no deben contener tildes ni caracteres especiales**. Usar nombres como:

```
German_Rosati_2022_C1_Labo_de_Datos.xlsx
```

El año se extrae automáticamente del nombre del archivo (primer número de 4 dígitos encontrado).

### Materias reconocidas

La app normaliza automáticamente variantes de nombres a las siguientes formas canónicas:

| Nombre canónico              | Variantes reconocidas                                                        |
|------------------------------|------------------------------------------------------------------------------|
| Laboratorio de Datos         | Labo de datos, Laboratorio de datos, Laboratorio webscraping, etc.           |
| Machine Learning             | Machine Learning aplicado a las ciencias sociales                            |
| Metodos Multivariados        | Met. Análisis Multivariados, Métodos de análisis cuantitativo multivariados  |
| Proc datos y estadisticas cs | (nombre base de archivos con este contenido)                                 |

---

## Dimensiones pedagógicas

El instrumento mide 6 dimensiones mediante ítems Likert de 1 a 5:

| Dimensión          | Ítems                                                                         | Disponibilidad     |
|--------------------|-------------------------------------------------------------------------------|--------------------|
| Planificación      | Programa, coherencia, tiempo, campus virtual                                  | Todos los formatos |
| Contenidos         | Relación de conceptos, ejemplos, bibliografía                                 | Solo formato 2025  |
| Clases             | Claridad, participación, tecnología, actividades                              | Todos los formatos |
| Evaluación         | Criterios, correspondencia, instrumentos, devolución, tiempo de devolución   | Todos los formatos |
| Ambiente y vínculo | Ambiente, trato, preguntas, interés docente                                   | Solo formato 2025  |
| Balance            | Aprendizaje, elección de la materia                                           | Todos los formatos |

> Las dimensiones **Contenidos** y **Ambiente y vínculo** no aparecen en Métodos Multivariados ni en Proc. datos y estadísticas cs porque esas preguntas no existían en el formulario 2022–2024. No es un error.

---

## Tabs del dashboard

### Resumen
- **Value boxes**: total de respuestas, promedio global (1–5), cantidad de materias y cuatrimestres en el filtro activo.
- **Radar por dimensión**: muestra el perfil de la materia seleccionada. Permite superponer materias adicionales mediante el selector múltiple (la materia del sidebar siempre aparece primero).
- **Promedio por dimensión**: gráfico de barras con el promedio de cada dimensión.

### Evolución temporal
- Líneas temporales de las 6 dimensiones a lo largo de los cuatrimestres disponibles, filtradas por materia si se seleccionó una en el sidebar.

### Por materia
- Comparación entre materias para la dimensión elegida (selector interno). Las materias se ordenan canónicamente: Proc. datos → Metodos Multivariados → Machine Learning → Laboratorio de Datos.

### Distribución
- Distribución de respuestas (1–5) por pregunta dentro de la dimensión seleccionada.

### Texto libre
Cuatro columnas de respuestas abiertas analizables:

| Selector           | Columna                       |
|--------------------|-------------------------------|
| Comentarios sobre el/la docente | `texto_docente`  |
| Aspectos positivos | `texto_positivos`             |
| Aspectos a mejorar | `texto_negativos`             |
| Lo más importante aprendido | `texto_aprendiste`  |
| Recomendaciones al docente | `texto_recomendaciones` |

Sub-tabs disponibles:
- **Nube de palabras** (requiere `wordcloud2`)
- **Top 20 palabras** — frecuencia de unigramas o bigramas (selector "Tipo de término")
- **Análisis de sentimiento** — emociones NRC por cuatrimestre (requiere `syuzhet`)
- **Respuestas** — tabla completa con filtros y paginación

### Datos
Tabla con todos los datos procesados. Incluye botones de exportación a CSV y Excel.

---

## Filtros globales (sidebar)

| Filtro   | Efecto                                                                                      |
|----------|---------------------------------------------------------------------------------------------|
| Materia  | Filtra todos los gráficos (excepto el radar overlay, que opera sobre todos los datos temporales) |
| Desde / Hasta | Restringe el rango de cuatrimestres (formato `AAAA-C1` / `AAAA-C2`)               |

---

## Pipeline de procesamiento de datos

1. **Detección de formato** → `detect_sheet()` identifica la hoja y las filas a saltear.
2. **Limpieza de nombres de columna** → minúsculas, sin tildes, snake_case.
3. **Mapeo de columnas** → `resolve_columns()` usa regex para asignar nombres canónicos.
4. **Parsing Likert** → `parse_likert()` extrae el primer dígito de strings del tipo "1-Muy de acuerdo"; valores >5 o no numéricos → `NA`.
5. **Normalización de texto abierto** → `replace_sentinels()` convierte `"sin respuesta"` y `"nan"` literales a `NA`.
6. **Normalización de materias** → `normalize_materia()` unifica variantes de nombre.
7. **Creación de variables derivadas**:
   - `periodo_norm`: "C1" / "C2" a partir del campo de período lectivo.
   - `cuatrimestre_ord`: factor ordenado `"AAAA-C1"` / `"AAAA-C2"`.
   - `situacion_cat`: categorización de la situación del estudiante.
8. **Filtro de calidad** → solo se conservan respuestas de estudiantes que finalizaron la cursada o abandonaron con >50% de asistencia.
