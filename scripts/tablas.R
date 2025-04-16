# Tabla 1: Definición de muestra a trabajar ----
matricula_por_slep <- matricula_final %>%
  filter(AGNO_TOTAL==2024) %>% 
  group_by(NOMBRE_SLEP_MUESTRA) %>%
  summarise(Matrícula = sum(MAT_TOTAL, na.rm = TRUE)) %>%
  mutate(NOMBRE_SLEP_MUESTRA = str_to_title(tolower(NOMBRE_SLEP_MUESTRA)))


tabla_muestra_slep <- muestra %>%
  mutate(NOMBRE_SLEP = str_to_title(NOMBRE_SLEP)) %>% 
  count(NOMBRE_SLEP, sort = TRUE) %>%
  rename('Nombre SLEP' = NOMBRE_SLEP) %>%
  mutate(
    Porcentaje = round((n / sum(n)) * 100, 2)
  ) %>%
  left_join(matricula_por_slep, by = c("Nombre SLEP" = "NOMBRE_SLEP_MUESTRA")) %>%
  add_row(
    `Nombre SLEP` = "Total",
    n = sum(.$n),
    Porcentaje = 100,
    Matrícula = sum(.$Matrícula, na.rm = TRUE)
  ) %>%
  rename(`Nº Establecimientos` = n) %>%
  mutate(
    Región = case_when(
      `Nombre SLEP` == "Huasco" ~ "Atacama",
      `Nombre SLEP` == "Puerto Cordillera" ~ "Coquimbo",
      `Nombre SLEP` == "Barrancas" ~ "Metropolitana",
      `Nombre SLEP` == "Costa Araucanía" ~ "Araucanía",
      `Nombre SLEP` == "Chinchorro" ~ "Arica y Parinacota",
      `Nombre SLEP` == "Gabriela Mistral" ~ "Metropolitana",
      `Nombre SLEP` == "Andalién Sur" ~ "Bío Bío",
      TRUE ~ " "
    ),
    Comunas = case_when(
      `Nombre SLEP` == "Huasco" ~ "Alto del Carmen, Freirina, Huasco, Vallenar",
      `Nombre SLEP` == "Puerto Cordillera" ~ "Andacollo, Coquimbo",
      `Nombre SLEP` == "Barrancas" ~ "Cerro Navia, Lo Prado, Pudahuel",
      `Nombre SLEP` == "Costa Araucanía" ~ "Carahue, Nueva Imperial, Puerto Saavedra, Teodoro Schmidt, Toltén",
      `Nombre SLEP` == "Chinchorro" ~ "Arica, Camarones, General Lagos, Putre",
      `Nombre SLEP` == "Gabriela Mistral" ~ "La Granja, Macul, San Joaquín",
      `Nombre SLEP` == "Andalién Sur" ~ "Chiguayante, Concepción, Florida, Hualqui",
      TRUE ~ " "
    ),
    `Año Inicio de Funciones` = case_when(
      `Nombre SLEP` == "Huasco" ~ '2018',
      `Nombre SLEP` == "Puerto Cordillera" ~ '2018',
      `Nombre SLEP` == "Barrancas" ~ '2018',
      `Nombre SLEP` == "Costa Araucanía" ~ '2018',
      `Nombre SLEP` == "Chinchorro" ~ '2019',
      `Nombre SLEP` == "Gabriela Mistral" ~ '2019',
      `Nombre SLEP` == "Andalién Sur" ~ '2019',
      TRUE ~ " "
    )
  ) %>%
  relocate(`Año Inicio de Funciones`, .after = `Nombre SLEP`) %>%  # Mueve la columna
  arrange(as.numeric(`Año Inicio de Funciones`)) %>% # Ordena cronológicamente
  mutate(Matrícula = format(Matrícula, big.mark = ".", decimal.mark = ","))



# Tabla 2 : detalle slep y comuna ----
# Crear dataframe estructurado

tabla_establecimientos <- data.frame(
  SLEP = c("Huasco", "Puerto Cordillera", "Barrancas", "Costa Araucanía", 
                        "Chinchorro", "Gabriela Mistral", "Andalién Sur"),
  Comunas = c("Alto del Carmen, Freirina, Huasco, Vallenar",
              "Andacollo, Coquimbo",
              "Cerro Navia, Lo Prado, Pudahuel",
              "Carahue, Nueva Imperial, Puerto Saavedra, Theodoro Schmidt, Toltén",
              "Arica, Camarones, General Lagos, Putre",
              "La Granja, Macul, San Joaquín",
              "Chiguayante, Concepción, Florida, Hualqui")
)

# Tabla 3: KDD ----
tabla_KDD <- data.frame(
  Paso = c("Selección", 
           "Pre-procesamiento", 
           "Transformación", 
           "Minería de datos", 
           "Interpretación y evaluación", 
           "Conocimiento"),
  
  Descripción = c("Se eligen datos relevantes de diversas fuentes para su posterior análisis.",
                  "Limpieza y organización de la información, eliminando valores erróneos e inconsistentes.",
                  "Se convierten, estandarizan o reducen los datos para hacer más fácil su trabajo.",
                  "Aplicación de algoritmos para identificar patrones y relaciones entre los datos.",
                  "Se analizan los patrones del paso anterior para obtener conocimiento útil y no trivial.",
                  "Los hallazgos se documentan y se utilizan para la toma de decisiones, mejoras en sistemas, o sistematización de la información."),
  
  stringsAsFactors = FALSE # Evitar que convierta los textos en factores
)

# Tabla 4: Total de SLEP por Cluster----
# Corregir y poner nombres y total para luego pegarla en el doc
tabla_cluster <- resumen_cluster %>%
  group_by(cluster) %>%
  summarise(total = sum(n))

# Tabla 5: Puntaje de silueta 2:7 clusters con PCA ----

tabla_silueta <- resultados_silueta

# Cambiar nombres de las columnas
colnames(tabla_silueta) <- c("Número de clústers", "Puntaje")
tabla_silueta$Puntaje <- round(tabla_silueta$Puntaje, 2)

# Verificar los cambios
print(tabla_silueta)

# Tabla 6: N de establecimientos rurales y urbanos por SLEP-----

ruralidad_slep <- matricula_final %>% 
  distinct(RBD, .keep_all = TRUE) %>%  
  group_by(NOMBRE_SLEP_MUESTRA, RURAL_RBD_MUESTRA) %>%  
  summarise(count = n(), .groups = "drop") %>%  
  pivot_wider(names_from = RURAL_RBD_MUESTRA, values_from = count, values_fill = 0) %>%  
  rename(SLEP = NOMBRE_SLEP_MUESTRA, Urbano = `0`, Rural = `1`) %>% 
  mutate(SLEP = str_to_title(SLEP)) %>%  
  mutate(
    `Total Establecimientos` = Urbano + Rural,
    `% Urbano` = round((Urbano / `Total Establecimientos`) * 100, 1),
    `% Rural` = round((Rural / `Total Establecimientos`) * 100, 1)
  ) %>%
  select(SLEP, Urbano, `% Urbano`, Rural, `% Rural`, `Total Establecimientos`) %>%
  bind_rows(
    summarise(., 
              SLEP = "Total",
              Urbano = sum(Urbano, na.rm = TRUE),
              Rural = sum(Rural, na.rm = TRUE),
              `% Urbano` = NA,
              `% Rural` = NA,
              `Total Establecimientos` = sum(`Total Establecimientos`, na.rm = TRUE))
  ) %>%
  mutate(
    `% Urbano` = ifelse(is.na(`% Urbano`), "No aplica", as.character(`% Urbano`)),
    `% Rural` = ifelse(is.na(`% Rural`), "No aplica", as.character(`% Rural`))
  )


# Tabla 7: Resumen caracterización SLEP----

caracterizacion_clusters_resumen <- data.frame(
  Grupo = c(1, 2, 3),
  Descripción = c(
    "Escuelas con condiciones intermedias, carga docente moderada y equilibrio general. Presentan niveles medios de estudiantes prioritarios, preferentes y beneficiarios SEP. Contienen la mayor cantidad de SLEP.",
    "Escuelas con alta vulnerabilidad: concentran el mayor porcentaje de estudiantes prioritarios, preferentes y beneficiarios SEP. Tienen menor dotación de personal y los rendimientos académicos más bajos. Predominan los establecimientos rurales.",
    "Escuelas con alto rendimiento SIMCE y menores niveles de estudiantes prioritarios, preferentes y SEP. Presentan mayores cargas docentes y dotación profesional, en contextos más urbanos y estables."
  )
)


# Tabla Anexo I: Detalle de Columnas utilizadas para la investigación:

etiquetas_variables_2 <- c(
  "ANO_SERVICIO_SISTEMA" = "Años de Servicio de Profesores",
  "HORAS_AULA" = "Horas de Aula de Profesores",
  "HORAS_CONTRATO" = "Horas Contrato Profesores",
  "HORAS_DENTRO_ESTAB" = "Horas Dentro del establecimiento Profesores",
  "HORAS_DIRECT" = "Horas Directas Profesores",
  "HORAS_FUERA_ESTAB" = "Horas fuera del establecimiento Profesores",
  "HORAS_TEC_PED" = "Horas Técnicos Pedagógicos",
  "JORN_AUX" = "Jornadas de los auxiliares",
  "JORN_PARA" = "Jornadas Paradocentes",
  "JORN_PROF" = "Jornadas Asistentes Profesionales",
  "MAT_HOM_TOT" = "Matrícula Hombres Establecimiento",
  "MAT_MUJ_TOT" = "Matrícula Mujeres Establecimiento",
  "MAT_TOTAL" = "Matrícula Total Establecimiento",
  "NALU_2M_RBD" = "N° Alumnos 2M",
  "NALU_4B_RBD" = "N° Alumnos 4B",
  "N_ASIS" = "N° Asistentes por Establecimiento",
  "N_AUX" = "N° Auxiliares por Establecimiento",
  "N_HOMBRES" = "N° Asistentes Hombres",
  "N_MUJERES" = "N° Asistentes Mujeres",
  "N_PARA" = "N° Paradocentes",
  "N_PROF" = "N° Profesores",
  "PROM_LECT2M_RBD" = "Prom. Lectura 2M",
  "PROM_LECT4B_RBD" = "Prom. Lectura 4B",
  "PROM_MATE2M_RBD" = "Prom. Matemática 2M",
  "PROM_MATE4B_RBD" = "Prom. Matemática 4B",
  "SLEP" = "Servicio Local de Educación Pública",
  "BEN_SEP" = "N° Beneficiarios SEP",
  "PREFERENTE_ALU" = "N° Alumnos Preferentes",
  "PORC_PRIORITARIO_ALU" = "% Alumnos Prioritarios"
)

# Convertir el vector en data frame
tabla_etiquetas <- data.frame(
  Variable = names(etiquetas_variables_2),
  Descripción = unname(etiquetas_variables_2)
)