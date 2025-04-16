# ---- LIBRERÍAS Y FUNCIONES ----
# Función para verificar, instalar y cargar paquetes
instalar_y_cargar <- function(paquetes) {
  paquetes_faltantes <- paquetes[!paquetes %in% installed.packages()[,"Package"]]
  
  if (length(paquetes_faltantes) > 0) {
    message("Instalando paquetes faltantes: ", paste(paquetes_faltantes, collapse = ", "))
    install.packages(paquetes_faltantes, dependencies = TRUE)
  }
  
  # Cargar todos los paquetes
  lapply(paquetes, library, character.only = TRUE)
}
# Función para cargar los datos de distintas carpetas
cargar_datos <- function(anio_inicio, anio_fin, archivo, filtrar = TRUE){
  anios <- anio_inicio:anio_fin
  
  lista_datos <- lapply(anios, function(ANIO) {
    carpeta <- glue("{archivo}_por_establecimiento")
    ruta_csv <- here("data", ANIO, carpeta, glue("{archivo}.csv"))
    ruta_xlsx <- here("data", ANIO, carpeta, glue("{archivo}.xlsx"))
    
    if (file.exists(ruta_csv) && file.info(ruta_csv)$size > 0) {
      df <- read.csv(ruta_csv, sep = ";", encoding = "UTF-8", stringsAsFactors = FALSE, fill = TRUE, blank.lines.skip = FALSE)
    } else if (file.exists(ruta_xlsx) && file.info(ruta_xlsx)$size > 0) {
      df <- read_excel(ruta_xlsx)
    } else {
      message(glue("rchivo no encontrado para el año {ANIO}: {ruta_csv} o {ruta_xlsx}"))
      return(NULL)
    }
    
    # Verificar si el archivo tiene datos
    if (nrow(df) == 0 || ncol(df) == 0) {
      message(glue("Archivo de {ANIO} está vacío o tiene formato incorrecto."))
      return(NULL)
    }
    
    # Ver estructura del archivo antes de procesarlo
    message(glue("Año {ANIO}: {nrow(df)} filas, {ncol(df)} columnas."))
    
    # Convertir nombres de columnas a mayúsculas para estandarizar
    colnames(df) <- toupper(colnames(df))
    
    # Convertir COD_DEPE2 a numérico si existe
    if ("COD_DEPE2" %in% colnames(df)) {
      df <- df %>% mutate(COD_DEPE2 = as.numeric(trimws(COD_DEPE2)))
    }
    
    # Filtrar solo si el argumento filtrar es TRUE y la columna existe
    if (filtrar && "COD_DEPE2" %in% colnames(df)) {
      df <- df %>% filter(COD_DEPE2 == 5)
    }
    
    # Agregar la columna ANIO si hay datos
    if (nrow(df) > 0) {
      df$ANIO <- ANIO
    }
    
    return(df)
  })
  
  # Unir los dataframes, omitiendo los NULL
  datos_completos <- bind_rows(Filter(Negate(is.null), lista_datos))
  
  return(datos_completos)
}
# Función para cargar datos de SIMCE y IDPS en un rango de años con nombres personalizados
cargar_simce_idps_rango <- function(anio_inicio, anio_fin, niveles) {
  anios <- anio_inicio:anio_fin
  
  lista_datos <- list()
  
  for (anio in anios) {
    for (nivel in niveles) {
      carpeta <- here("data", anio, glue("simce_{nivel}_{anio}"))
      
      archivos <- list(
        simce_csv = here(carpeta, glue("simce_{nivel}_{anio}.csv")),
        simce_xlsx = here(carpeta, glue("simce_{nivel}_{anio}.xlsx")),
        idps_csv = here(carpeta, glue("idps_{nivel}_{anio}.csv")),
        idps_xlsx = here(carpeta, glue("idps_{nivel}_{anio}.xlsx"))
      )
      
      leer_archivo <- function(archivo_csv, archivo_xlsx) {
        if (file.exists(archivo_csv)) return(read_delim(archivo_csv, delim = ";", col_types = cols()))
        if (file.exists(archivo_xlsx)) return(read_excel(archivo_xlsx))
        warning(glue("Archivo no encontrado: {archivo_csv} o {archivo_xlsx}"))
        return(NULL)
      }
      
      simce <- leer_archivo(archivos$simce_csv, archivos$simce_xlsx)
      idps <- leer_archivo(archivos$idps_csv, archivos$idps_xlsx)
      
      if (!is.null(simce)) names(simce) <- toupper(names(simce))
      if (!is.null(idps)) names(idps) <- toupper(names(idps))
      
      nombre_lista <- glue("{nivel}_{anio}")
      lista_datos[[nombre_lista]] <- list(simce = simce, idps = idps)
    }
  }
  
  return(lista_datos)
}
# Función para cargar los df de subvenciones (esto fue complicado)
cargar_subvencion <- function(anio_inicio, anio_fin, filtrar = TRUE) {
  anios <- anio_inicio:anio_fin
  
  lista_datos <- lapply(anios, function(ANIO) {
    carpeta <- "subvencion_por_establecimiento"
    ruta_csv <- here("data", ANIO, carpeta, "subvencion.csv")
    ruta_xlsx <- here("data", ANIO, carpeta, "subvencion.xlsx")
    
    # 1. Años 2017 y 2018 => XLSX
    if (ANIO %in% c(2017, 2018)) {
      if (file.exists(ruta_xlsx) && file.info(ruta_xlsx)$size > 0) {
        df <- read_excel(ruta_xlsx)
      } else {
        message(glue("Archivo XLSX no encontrado o vacío para el año {ANIO}: {ruta_xlsx}"))
        return(NULL)
      }
      
      # 2. Años 2019–2021 => CSV con Latin-1
    } else if (ANIO %in% c(2019, 2020, 2021)) {
      if (file.exists(ruta_csv) && file.info(ruta_csv)$size > 0) {
        df <- read.csv(
          ruta_csv,
          sep = ";",
          fileEncoding = "ISO-8859-1",  # clave para 2019–2021
          stringsAsFactors = FALSE,
          fill = TRUE,
          blank.lines.skip = FALSE,
          check.names = TRUE
        )
      } else {
        message(glue("Archivo CSV no encontrado o vacío para el año {ANIO}: {ruta_csv}"))
        return(NULL)
      }
      
      # 3. Años 2022 en adelante => CSV con UTF-8
    } else {
      if (file.exists(ruta_csv) && file.info(ruta_csv)$size > 0) {
        df <- read.csv(
          ruta_csv,
          sep = ";",
          fileEncoding = "UTF-8",    # clave para 2022 en adelante
          stringsAsFactors = FALSE,
          fill = TRUE,
          blank.lines.skip = FALSE,
          check.names = TRUE
        )
      } else {
        message(glue("Archivo CSV no encontrado o vacío para el año {ANIO}: {ruta_csv}"))
        return(NULL)
      }
    }
    
    # Verificamos que tenga datos
    if (nrow(df) == 0 || ncol(df) == 0) {
      message(glue("Archivo de {ANIO} está vacío o tiene formato incorrecto."))
      return(NULL)
    }
    
    # Mensaje informativo
    message(glue("Año {ANIO}: {nrow(df)} filas, {ncol(df)} columnas."))
    
    # Convertimos los nombres de columna a mayúsculas
    colnames(df) <- toupper(colnames(df))
    
    df <- df %>%
      mutate(across(everything(), ~ as.numeric(as.character(.))))
    
    # Convertir COD_DEPE2 a numérico si existe
    if ("COD_DEPE2" %in% colnames(df)) {
      df <- df %>%
        mutate(COD_DEPE2 = as.numeric(trimws(COD_DEPE2)))
    }
    
    # Filtrar por COD_DEPE2 == 5 (si filtrar = TRUE)
    if (filtrar && "COD_DEPE2" %in% colnames(df)) {
      df <- df %>% filter(COD_DEPE2 == 5)
    }
    
    # Agregar la columna ANIO
    if (nrow(df) > 0) {
      df$ANIO <- ANIO
    }
    
    return(df)
  })
  
  # Unimos dataframes, omitiendo los NULL
  datos_completos <- bind_rows(Filter(Negate(is.null), lista_datos))
  return(datos_completos)
}
# Función para convertir todas las columnas a character
convertir_a_character <- function(df) {
  df %>%
    mutate(across(everything(), as.character))
}
# Para reproducir este código, establecer la carpeta "Tesis_NT" como working directory
# Lista de paquetes necesarios
paquetes_necesarios <- c(
  "here", "readxl", "dplyr", "ggplot2", "tidyr", "readr", "glue", "stringr", "scales",
  "kableExtra", "factoextra", "cluster", "pacman", "DescTools", "ggdendro", "showtext"
)
instalar_y_cargar(paquetes_necesarios)
set_here(getwd())
# Raíz de here(), muy útil para las cargas automatizadas de BBDD 
here()
# ------------------------------------------------------------------------------ #

# ---- CARGA DE DATOS ----
# Datos triestamentales de los establecimientos SLEP
matricula_og <- cargar_datos(2017, 2024, "matricula", filtrar = FALSE)
docentes_og <- cargar_datos(2017, 2024, "docentes", filtrar = FALSE)
asistentes_og <- cargar_datos(2017, 2024, "asistentes", filtrar = FALSE)

# No se usará
#subvenciones_og <- cargar_subvencion(2017,2023, filtrar = TRUE)

# Para cargar los alumnos prioritarios, al ser bases tan grandes, se filtró x RBD
# Se generó el DF prio_filtrado que solo contiene los establecimientos de la muestra
# prio_og <- cargar_datos(2017, 2024, "prio", filtrar = FALSE)
# prio_filtrado <- prio_og %>% 
#   filter(RBD %in% rbd_filtro)
# Se guardó en la siguiente ruta para poder ser usado
ruta_prio <- here('data/2024/prio_por_establecimiento/prio_filtrado_total.csv')

 # Datos simce-idps 2012 a 2023
simce <- cargar_simce_idps_rango(2012, 2023, c('4b','2m'))

# Nombre de los SLEP seleccionados para el análisis posterior
# Definidos en el marco metodológico
# Código de los SLEP:
# 1 - Huasco
# 2 - Puerto Cordillera
# 3 - Barrancas
# 4 - Costa Araucanía
# 5 - Chinchorro
# 6 - Gabriela Mistral
# 7 - Andalién Sur

sleps_seleccionados <- c(1:7)

# DataFrame que contiene la muestra que se usará, 
# con RBD (identificador) de cada establecimiento 
# y SLEP al que pertenece

muestra <- matricula_og %>%
  filter(SLEP %in% sleps_seleccionados) %>% 
  filter(ESTADO_ESTAB == 1) %>%
  filter(MAT_TOTAL > 0) %>% 
  select(1:14, (ncol(matricula_og)-1):ncol(matricula_og)) 

# RBD unicos
rbd_filtro <- unique(muestra$RBD)

# Para establecimientos municipales que no fueron seleccionados
no_muestra <- matricula_og %>%
  filter(COD_DEPE2 == 1) %>%
  filter(ESTADO_ESTAB == 1) %>%
  filter(MAT_TOTAL > 0) %>%
  filter(!(RBD %in% rbd_filtro)) %>%  # Eliminar RBD ya considerados en la muestra
  select(1:14, (ncol(matricula_og)-1):ncol(matricula_og))

no_rbd_filtro <- unique(no_muestra$RBD)


# Datos triestamentales de los establecimientos seleccionados:
matricula_muestra <- inner_join(muestra, matricula_og, by= 'RBD', suffix=c('_MUESTRA','_TOTAL'))
docentes_muestra <- inner_join(muestra, docentes_og, by='RBD', suffix=c('_MUESTRA','_TOTAL'))
asistentes_muestra <- inner_join(muestra, asistentes_og, by='RBD', suffix=c('_MUESTRA','_TOTAL'))
#subvenciones_muestra <- inner_join(muestra, subvenciones_og, by='RBD', suffix=c('_MUESTRA', '_TOTAL'))

prio_og <- read.csv(ruta_prio, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE, fill = TRUE, blank.lines.skip = FALSE)

prio_muestra <- inner_join(muestra, prio_og, by='RBD', suffix=c('_MUESTRA','_TOTAL'))

#------------------------------------------------------------------------------#

# ---- FILTRADO Y LIMPIEZA ----
# **Datos triestamentales----

# ***Dimensiones a analizar en matricula***
d_mat <- c(
  # Identificacion de los establecimientos
  'AGNO_TOTAL','RBD','RURAL_RBD_MUESTRA', 'NOM_COM_RBD_MUESTRA','NOMBRE_SLEP_MUESTRA', 'ESTADO_ESTAB_MUESTRA', 'SLEP_MUESTRA',
  # Información seleccionada para matricula
  'MAT_HOM_TOT','MAT_MUJ_TOT', 'MAT_TOTAL')
d_mat_categoricas <- c(
  'AGNO_TOTAL','RURAL_RBD_MUESTRA', 'NOM_COM_RBD_MUESTRA','NOMBRE_SLEP_MUESTRA', 'ESTADO_ESTAB_MUESTRA', 'SLEP_MUESTRA'
)

# DF que se usará
matricula_final <- matricula_muestra %>% 
  select(any_of(d_mat)) %>% 
  mutate(across(any_of(intersect(d_mat_categoricas, colnames(.))), as.factor))

sum(is.na(matricula_final))

# ***Dimensiones a analizar en docentes***
d_doc <- c(  
  # Identificacion de los establecimientos
  'AGNO_TOTAL','RBD','RURAL_RBD_MUESTRA', 'NOM_COM_RBD_MUESTRA','NOMBRE_SLEP', 'ESTADO_ESTAB_MUESTRA',  'SLEP_MUESTRA',

  'DOC_GENERO', 'TIP_INSTI_ID_1', 'DURACION_CARRERA_1', 'MODALIDAD_ESTUDIO_1',
  'HORAS_CONTRATO', 'HORAS_DIRECT','HORAS_TEC_PED', 'HORAS_AULA', 'HORAS_DENTRO_ESTAB','HORAS_FUERA_ESTAB', 'ANO_SERVICIO_SISTEMA')

# categoricas
d_doc_categoricas <- c('AGNO_TOTAL', 'RURAL_RBD_MUESTRA', 'NOM_COM_RBD_MUESTRA', 
                       'NOMBRE_SLEP_MUESTRA', 'ESTADO_ESTAB_MUESTRA', 'SLEP_MUESTRA', 
                       'DOC_GENERO', 'TIP_INSTI_ID_1', 'DURACION_CARRERA_1', 'MODALIDAD_ESTUDIO_1')

# Filtrar el dataset solo con estas columnas y transformar las categóricas en factor
docentes_final <- docentes_muestra %>%
  select(any_of(d_doc)) %>%
  mutate(across(any_of(intersect(d_doc_categoricas, colnames(.))), as.factor))

sum(is.na(docentes_final))

docentes_final <- drop_na(docentes_final)

sum(is.na(docentes_final))

# DF que se usará
docentes_final <- docentes_final %>%
  mutate(NOMBRE_SLEP = str_to_title(NOMBRE_SLEP))

# ***Dimensiones a analizar en asistentes***
d_asi <- c(
  # Identificacion de los establecimientos
  'AGNO_TOTAL','RBD','RURAL_RBD_MUESTRA', 'NOM_COM_RBD_MUESTRA','NOMBRE_SLEP', 'ESTADO_ESTAB_MUESTRA', 'SLEP_MUESTRA',
  "N_ASIS", "N_HOMBRES", "N_MUJERES", "N_PROF", "N_PARA", "N_AUX",
  "JORN_PROF", "JORN_PARA", "JORN_AUX",  "TOT_JORN")

# categoricas
d_asi_categoricas <- c('AGNO_TOTAL', 'RURAL_RBD_MUESTRA', 'NOM_COM_RBD_MUESTRA', 
                       'NOMBRE_SLEP', 'ESTADO_ESTAB_MUESTRA', 'SLEP_MUESTRA')

# Filtrar el dataset solo con estas columnas y transformar las categóricas en factor
asistentes_final <- asistentes_muestra %>%
  select(any_of(d_asi)) %>%
  mutate(across(any_of(intersect(d_asi_categoricas, colnames(.))), as.factor)) %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

sum(is.na(asistentes_final))

# DF que se usará

asistentes_final <- asistentes_final %>%
  mutate(NOMBRE_SLEP = str_to_title(NOMBRE_SLEP))

# ***Dimensiones a analizar en prioritarios***

d_prio <- c(
  'AGNO_TOTAL','RBD','RURAL_RBD_MUESTRA', 'NOM_COM_RBD_MUESTRA','NOMBRE_SLEP', 'ESTADO_ESTAB_MUESTRA', 'SLEP_MUESTRA',
  'PRIORITARIO_ALU', 'PREFERENTE_ALU', 'BEN_SEP',
  'CRITERIO_SEP', 'CONVENIO_SEP', 'CLASIFICACION_SEP'
  
)

d_prio_categoricas <- c(
  'CRITERIO_SEP', 'CONVENIO_SEP', 'CLASIFICACION_SEP', 'AGNO_TOTAL', 'RURAL_RBD_MUESTRA', 'NOM_COM_RBD_MUESTRA',
  'NOMBRE_SLEP', 'ESTADO_ESTAB_MUESTRA', 'SLEP_MUESTRA'
)

prio_final <- prio_muestra %>%
  select(any_of(d_prio)) %>%
  mutate(across(any_of(intersect(d_prio_categoricas, colnames(.))), as.factor)) %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

sum(is.na(prio_final))

glimpse(prio_final)
# ** Datos SIMCE **----
# Estos dataframes contienen todas las ediciones de SIMCE del 2012 al 2023, separados por nivel, SIMCE e IDPS
# Se crea otra lista de dataframes que solo contiene los establecimientos que definimos por RBD anteriormente
simce_filtrado <- lapply(simce, function(sublista) {
  list(
    simce = if (!is.null(sublista$simce) && "RBD" %in% colnames(sublista$simce)) {
      sublista$simce %>% filter(RBD %in% rbd_filtro)
    } else {
      sublista$simce
    },
    idps = if (!is.null(sublista$idps) && "RBD" %in% colnames(sublista$idps)) {
      sublista$idps %>% filter(RBD %in% rbd_filtro)
    } else {
      sublista$idps
    }
  )
})

# ---- PARA ESTABLECIMIENTOS  SELECCIONADOS----
# En las siguientes líneas de código se crean dos listas (simce_total_filtrado y simce_filtrado_excluidos)
# Estos contienen la información de todos los años de SIMCE en los establecimientos seleccionados (el primero)
# Y de los no seleccionados, en el segundo
# El filtro se realizó por RBD, usando rbd_filtro, lista tipo int que contiene los 383 establecimientos de los SLEP seleccionados

# Unir todos los dataframes en un solo dataframe general para SIMCE 
# para establecimientos seleccionados (e.g están dentro de la lista de SLEP)

simce_total_filtrado <- bind_rows(lapply(names(simce_filtrado), function(nivel_anio) {
  df <- simce_filtrado[[nivel_anio]]$simce
  if (!is.null(df)) {
    df <- convertir_a_character(df)  # Convertir todas las columnas a character
    df <- df %>% mutate(nivel_anio = nivel_anio)  # Agregar la etiqueta de nivel y año
  }
  return(df)
}), .id = "source")

# Convertir RBD a integer en ambos dataframes
muestra$RBD <- as.integer(muestra$RBD)
simce_total_filtrado$RBD <- as.integer(simce_total_filtrado$RBD)

simce_slep <- inner_join(muestra, simce_total_filtrado, by = "RBD", suffix=c('_MUESTRA','_TOTAL'))

# ---- PARA ESTABLECIMIENTOS NO SELECCIONADOS----
# Crear versión filtrada de simce con los no_slep
simce_filtrado_no_slep <- lapply(simce, function(sublista) {
  list(
    simce = if (!is.null(sublista$simce) && "RBD" %in% colnames(sublista$simce)) {
      sublista$simce %>% filter(RBD %in% no_rbd_filtro)
    } else {
      sublista$simce
    },
    idps = if (!is.null(sublista$idps) && "RBD" %in% colnames(sublista$idps)) {
      sublista$idps %>% filter(RBD %in% no_rbd_filtro)
    } else {
      sublista$idps
    }
  )
})

# Unir todos los dataframes en un solo dataframe general para SIMCE no SLEP
simce_total_excluidos <- bind_rows(lapply(names(simce_filtrado_no_slep), function(nivel_anio) {
  df <- simce_filtrado_no_slep[[nivel_anio]]$simce
  if (!is.null(df)) {
    df <- convertir_a_character(df)  # Convertir todas las columnas a character
    df <- df %>% mutate(nivel_anio = nivel_anio)  # Agregar la etiqueta de nivel y año
  }
  return(df)
}), .id = "source")

# Asegurar tipo de dato para join si fuera necesario más adelante
simce_total_excluidos$RBD <- as.integer(simce_total_excluidos$RBD)

