# ---- CONFIGURACIONES DE LOS GRÁFICOS, TEMAS Y COLORES ----
# ---- COLORES ----
# Definir colores Nord Aurora
nord_aurora_colors <- list(
  background = "#FFFFFF",  # Blanco PDF
  grid = "#4C566A",        # Color de la cuadrícula
  text = "#2e3440",        # Texto oscuro
  accent1 = '#507297',     # Cada accent representa un SLEP en los gráficos comparativos
  accent2 = '#d08770',     # Y son utilizados en los gráficos no comparativos para no romper la estética general del doc
  accent3 = "#A3BE8C",     
  accent4 = "#B48EAD",
  accent5 = "#00C094",
  accent6 = '#bf616a',
  accent7 = '#ebcb8b'
  )
# Colores de SLEP y dicionario con numero y nombre
# colores_slep con colores accent de nord_aurora_colors
colores_slep <- c(
  "Andalién Sur" = nord_aurora_colors$accent1,  
  "Barrancas" = nord_aurora_colors$accent2,     
  "Chinchorro" = nord_aurora_colors$accent3,    
  "Costa Araucanía" = nord_aurora_colors$accent4,  
  "Gabriela Mistral" = nord_aurora_colors$accent5, 
  "Huasco" = nord_aurora_colors$accent6,        
  "Puerto Cordillera" = nord_aurora_colors$accent7 
)

# También se dejó en numero, quizá no se utilice pero es bueno tenerlo
colores_nslep <- c(
  '1' = nord_aurora_colors$accent1,  # Azul oscuro
  "2" = nord_aurora_colors$accent2,  # Naranjo intenso
  "3" = nord_aurora_colors$accent3,  # Verde Aurora
  "4" = nord_aurora_colors$accent4,  # Morado Aurora
  "5" = nord_aurora_colors$accent5,  # Azul oscuro
  "6" = nord_aurora_colors$accent6,  # Naranjo intenso
  "7" = nord_aurora_colors$accent7   # Verde Aurora
)

# Código de BBDD de cada SLEP
codigo_slep <- c('1' = "Andalién Sur", 
                 "2" = "Barrancas", 
                 "3" = "Chinchorro",
                 "4" = "Costa Araucanía",
                 "5" = "Gabriela Mistral",
                 "6" = "Huasco",
                 "7" = "Puerto Cordillera")

# paleta de colores en formato vector, para acceder a ellos de manera más simple
nord_palette <- c(nord_aurora_colors$accent1, 
                  nord_aurora_colors$accent2, 
                  nord_aurora_colors$accent3, 
                  nord_aurora_colors$accent4,
                  nord_aurora_colors$accent5,
                  nord_aurora_colors$accent6,
                  nord_aurora_colors$accent7) 

# ---- TEMA ----
# Tema para nuestros gráficos, con fuente, configuraciones de grid, leyendas, colores que definimos anteriormente, etc.
theme_nord_aurora <- function() {
  theme_minimal(base_family = "sans", base_size = 12) +  # Reduce tamaño base
    theme(
      plot.background = element_rect(fill = nord_aurora_colors$background, color = NA),
      panel.background = element_rect(fill = nord_aurora_colors$background, color = NA),
      panel.grid.major = element_line(color = adjustcolor("gray60", alpha.f = 0.3), size = 0.3),  # Gris claro y más transparente
      panel.grid.minor = element_blank(),  # Ocultar la cuadrícula menor
      
      # Ajustes de tamaño para evitar texto gigante
      axis.text = element_text(color = nord_aurora_colors$text, size = 10),  # Tamaño más compacto
      axis.title = element_text(color = nord_aurora_colors$text, face = "bold", size = 10),  # Más pequeño pero resaltado
      plot.title = element_text(color = nord_aurora_colors$text, face = "bold", size = 12, hjust = 0.5),  # Centrado
      
      # Ajustes en la leyenda
      legend.background = element_rect(fill = nord_aurora_colors$background, color = NA),
      legend.text = element_text(color = nord_aurora_colors$text, size = 10),  # Reduce tamaño de la leyenda
      legend.position = "bottom",
      
      # Ocultar líneas de ejes y ticks
      axis.line = element_blank(),
      axis.ticks = element_blank()
    )
}

# ---- CONFIGURACIONES VARIAS ----
showtext_auto()  # Habilita el uso de fuentes en gráficos

# Función para normalizar datos, se usa en el de heatmap
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Puntos en vez de comas
bm_point<- number_format(big.mark = ".", decimal.mark = ',')
# ==============================================================================

# ==== ESTADISTICA DESCRIPTIVA: ANALISIS UNIVARIADO-BIVARIADO ====
# ---- Cálculos para gráficos 1,2----
# Preámbulo: DF que contengan en formato long las evaluaciones seleccionadas de los SLEPS seleccionados y excluidos
# Para posterior comparación

# Evaluaciones SIMCE que se usarán

evaluaciones_seleccionadas <- c("PROM_LECT4B_RBD", "PROM_LECT2M_RBD", "PROM_MATE4B_RBD", "PROM_MATE2M_RBD")

# DF simce en formato long para graficado a lo largo del tiempo - SELECCIONADOS
# Transformar la base de establecimientos seleccionados a formato long

simce_long <- simce_slep %>%
  pivot_longer(
    cols = starts_with("NALU_") | starts_with("PROM_"),  # Seleccionar columnas relevantes
    names_to = "tipo_evaluacion",
    values_to = "valor"
  ) %>% drop_na(valor) 
  
# Asegurar que AGNO y valor sean numéricos

simce_long$AGNO_TOTAL <- as.numeric(simce_long$AGNO_TOTAL)
simce_long$valor <- as.numeric(simce_long$valor)

# DF simce en formato long para graficado a lo largo del tiempo - NO SELECCIONADOS
# Transformar la base de establecimientos NO seleccionados a formato long

simce_long_excluidos <- simce_total_excluidos %>%
  pivot_longer(
    cols = starts_with("NALU_") | starts_with("PROM_"),  # Seleccionar columnas relevantes
    names_to = "tipo_evaluacion",
    values_to = "valor"
  ) %>% drop_na(valor)

# Asegurar que AGNO y valor sean numéricos

simce_long_excluidos$AGNO <- as.numeric(simce_long_excluidos$AGNO)
simce_long_excluidos$valor <- as.numeric(simce_long_excluidos$valor)

# La comparación será para el promedio general vs promedio SLEP

# Calcular promedio por año de los establecimientos NO seleccionados (lectura)
simce_excluidos_lect <- simce_long_excluidos %>%
  filter(tipo_evaluacion %in% evaluaciones_seleccionadas[1:2]) %>%  # Filtrar mismas evaluaciones
  group_by(AGNO, tipo_evaluacion) %>%
  summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") %>% # eliminamos NA ya que significa que no rinde tal prueba
  mutate(grupo = case_when(
    str_detect(tipo_evaluacion, "_LECT2M") ~ "Lectura 2M No Seleccionado",
    str_detect(tipo_evaluacion, "_LECT4B") ~ "Lectura 4B No Seleccionado",))

# Calcular promedio por año de los establecimientos NO seleccionados (mate)
simce_excluidos_mate <- simce_long_excluidos %>%
  filter(tipo_evaluacion %in% evaluaciones_seleccionadas[3:4]) %>%  # Filtrar mismas evaluaciones
  group_by(AGNO, tipo_evaluacion) %>%
  summarise(valor = mean(valor, na.rm = TRUE)) %>%
  mutate(grupo = case_when(
    str_detect(tipo_evaluacion, "_MATE2M") ~ "Matemática 2M No Seleccionado",
    str_detect(tipo_evaluacion, "_MATE4B") ~ "Matemática 4B No Seleccionado",)) # Etiqueta para diferenciar en el gráfico

# Transformaciones a factor para no tener problemas con la visualización
simce_long$tipo_evaluacion <- as.factor(simce_long$tipo_evaluacion)
simce_excluidos_mate$grupo <- as.factor(simce_excluidos_mate$grupo)
simce_excluidos_lect$grupo <- as.factor(simce_excluidos_lect$grupo)

# DF final que contiene el promedio por año de las evaluacionies de los establecimientos no-SLEP
simce_excluidos <- bind_rows(simce_excluidos_mate, simce_excluidos_lect)
# ---- GRAFICOS SIMCE ----
# GRAFICO 1: Promedio de lenguaje  4b y 2m juntos----
# Gráfico base que contiene los puntajes de los establecimientos SLEP (naranjo-azul)
g_simce_l <- ggplot(simce_long %>% filter(tipo_evaluacion %in% evaluaciones_seleccionadas[1:2],
                                   AGNO_TOTAL >= 2017 & AGNO_TOTAL <= 2024), 
             aes(x = AGNO_TOTAL, y = valor, color = tipo_evaluacion, group = tipo_evaluacion)) +
  
  stat_summary(fun = mean, geom = "line", size = 1, na.rm = TRUE) +  # Línea principal
  stat_summary(fun = mean, geom = "point", size = 1.8) +  # Puntos individuales
  geom_vline(xintercept = 2018, linetype = "dashed", color = nord_aurora_colors$grid, size = 0.7) +  # Línea vertical
  annotate("text", x = 2018.36, y = 300, label = "Inicio traspaso", 
           color = nord_aurora_colors$text, angle = 0, vjust = 1, size = 3) +  # Texto
  labs(
    x = "Año",
    y = "Puntaje SIMCE promedio",
    color = "Tipo de Evaluación",
    caption = "Fuente: Elaboración propia en base a datos MINEDUC y ACE"
  )+
  theme(legend.position = "right") +
  scale_x_continuous(breaks = seq(min(simce_long$AGNO_TOTAL, na.rm = TRUE), 
                                  max(simce_long$AGNO_TOTAL, na.rm = TRUE), by = 1)) +
  scale_y_continuous(limits = c(150, 300), breaks = seq(150, 300, by = 10)) +
  theme_nord_aurora()  # Aplicar el tema personalizado

# Agregar la línea punteada para "Simce Lectura General"
g_simce_l <- g_simce_l + 
  geom_line(data = simce_excluidos_lect %>% filter(AGNO >= 2017 & AGNO <= 2024),
            aes(x = AGNO, y = valor, color = grupo, group = tipo_evaluacion), 
            linetype = "dashed", size = 0.8, alpha= 0.8) +
  scale_color_manual(
    values = c(
      "PROM_LECT2M_RBD" = nord_palette[1],  
      "PROM_LECT4B_RBD" = nord_palette[2],  
      "Lectura 2M No Seleccionado" = nord_palette[3],  
      "Lectura 4B No Seleccionado" = nord_palette[4]  
    ),
    labels = c(
      "PROM_LECT2M_RBD" = "Lectura 2° Medio Seleccionados",
      "PROM_LECT4B_RBD" = "Lectura 4° Básico Seleccionados",
      "Lectura 2M No Seleccionado" = "Lectura 2° Medio General",
      "Lectura 4B No Seleccionado" = "Lectura 4° Básico General"
    )
  ) +  
  guides(
    color = guide_legend(title = "Evaluación", 
                         override.aes = list(linetype = c("solid", "solid", "dashed", "dashed")), 
                         nrow = 2)  # Ajusta la leyenda en 2 filas
  )
# Grafico final, con cuatro líneas
g_simce_l

# GRAFICO 2: Promedio de mate  4b y 2m juntos----
# En matemáticas es exactamente el mismo proceso, solamente se cambian las evaluaciones_seleccionadas
g_simce_m <- ggplot(simce_long %>% filter(tipo_evaluacion %in% evaluaciones_seleccionadas[3:4],
                                   AGNO_TOTAL >= 2017 & AGNO_TOTAL <= 2024), 
             aes(x = AGNO_TOTAL, y = valor, color = tipo_evaluacion, group = tipo_evaluacion)) +
  stat_summary(fun = mean, geom = "line", size = 1, na.rm = TRUE) +  # Línea principal
  stat_summary(fun = mean, geom = "point", size = 1.8) +  # Puntos individuales
  geom_vline(xintercept = 2018, linetype = "dashed", color = nord_aurora_colors$grid, size = 0.7) +  # Línea vertical
  annotate("text", x = 2018.36, y = 300, label = "Inicio traspaso", 
           color = nord_aurora_colors$text, angle = 0, vjust = 1, size = 3) +  # Texto
  labs(
    x = "Año",
    y = "Puntaje SIMCE promedio",
    color = "Tipo de Evaluación",
    caption = "Fuente: Elaboración propia en base a datos MINEDUC y ACE"
  )+
  theme(legend.position = "right") +
  scale_x_continuous(breaks = seq(min(simce_long$AGNO_TOTAL, na.rm = TRUE), 
                                  max(simce_long$AGNO_TOTAL, na.rm = TRUE), by = 1)) +
  scale_y_continuous(limits = c(150, 300), breaks = seq(150, 300, by = 10)) +
  theme_nord_aurora()  # Aplicar el tema personalizado

# Agregar la línea punteada para "Simce Matemática General"
g_simce_m <- g_simce_m + 
  geom_line(data = simce_excluidos_mate %>% filter(AGNO >= 2017 & AGNO <= 2024),
            aes(x = AGNO, y = valor, color = grupo, group = tipo_evaluacion), 
            linetype = "dashed", size = 0.8, alpha= 0.8) +
  scale_color_manual(
    values = c(
      "PROM_MATE2M_RBD" = nord_palette[1],  
      "PROM_MATE4B_RBD" = nord_palette[2],  
      "Matemática 2M No Seleccionado" = nord_palette[3],  
      "Matemática 4B No Seleccionado" = nord_palette[4]  
    ),
    labels = c(
      "PROM_MATE2M_RBD" = "Matemática 2° Medio Seleccionados",
      "PROM_MATE4B_RBD" = "Matemática 4° Básico Seleccionados",
      "Matemática 2M No Seleccionado" = "Matemática 2° Medio General",
      "Matemática 4B No Seleccionado" = "Matemática 4° Básico General"
    )
  ) +  
  guides(
    color = guide_legend(title = "Evaluación", 
                         override.aes = list(linetype = c("solid", "solid", "dashed", "dashed")), 
                         nrow = 2)  # Ajusta la leyenda en 2 filas
  )

# ---- GRAFICOS MATRICULA ----
# GRAFICO 3: MATRICULA ----

# Asegurar que AGNO_TOTAL es factor con orden correcto
matricula_time_slep <- matricula_final %>%
  group_by(AGNO_TOTAL, NOMBRE_SLEP_MUESTRA) %>%
  summarise(Matricula_Total = sum(MAT_TOTAL, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    NOMBRE_SLEP_MUESTRA = str_to_title(NOMBRE_SLEP_MUESTRA),
    AGNO_TOTAL = factor(AGNO_TOTAL, levels = sort(unique(AGNO_TOTAL)))  # ordenar niveles
  )

# Crear gráfico
g_matricula <- ggplot(matricula_time_slep, aes(
  x = AGNO_TOTAL, y = Matricula_Total,
  color = NOMBRE_SLEP_MUESTRA,
  group = NOMBRE_SLEP_MUESTRA)) +
  
  geom_line(size = 1) +
  geom_point(size = 2) +
  
  labs(
    x = "Año",
    y = "Número de Estudiantes Matriculados",
    color = "SLEP",
    caption = "Fuente: Elaboración propia en base a datos MINEDUC y ACE"
  ) +
  
  scale_y_continuous(
    labels = bm_point,  
    breaks = seq(8000, 24000, by = 4000),
    limits = c(8000, 24000)
  ) +
  
  scale_x_discrete(drop = FALSE) +  # usamos escala discreta para factores
  scale_color_manual(values = colores_slep) +  # tu paleta personalizada
  theme_nord_aurora()  # tu tema

g_matricula


# GRAFICO 4: RURALIDAD----

# Contar los establecimientos rurales y urbanos
ruralidad_counts <- matricula_final %>%
  distinct(RBD, .keep_all = TRUE) %>%  # Filtrar solo los establecimientos seleccionados
  group_by(RURAL_RBD_MUESTRA) %>%
  summarise(count = n(), .groups = "drop")  # Contar cuántos son rurales y urbanos

# Gráfico de torta (PRUEBA)
ggplot(ruralidad_counts, aes(x = "", y = count, fill = factor(RURAL_RBD_MUESTRA))) +
  geom_bar(width = 1, stat = "identity", alpha = 0.7) +
  coord_polar("y") +
  labs(
    title = "Proporción de Establecimientos Urbanos y Rurales",
    fill = "Tipo de Establecimiento",
    caption = "Fuente: Elaboración propia en base a datos MINEDUC y ACE"
  ) +
  scale_fill_manual(
    values = c("0" = nord_palette[1], "1" = nord_palette[2]),
    labels = c("0" = "Urbano", "1" = "Rural")
  )

# Gráfico de barras urbano/rural SLEP 
g_ruralidad <- ggplot(ruralidad_counts, aes(x = factor(RURAL_RBD_MUESTRA), y = count, fill = factor(RURAL_RBD_MUESTRA))) +
  geom_col(alpha = 0.9) +  
  geom_text(aes(label = count), vjust = -0.5, size = 5, color = nord_aurora_colors$text) +  # Etiquetas encima de cada barra
  labs(
    x = "Tipo de Establecimiento",
    y = "Cantidad de Establecimientos",
    fill = "Ruralidad",
    caption = "Fuente: Elaboración propia en base a datos MINEDUC y ACE"
  ) +
  scale_x_discrete(labels = c("0" = "Urbano", "1" = "Rural")) +  # Etiquetas personalizadas en el eje X
  scale_fill_manual(
    values = c("0" = nord_palette[4], "1" = nord_palette[3]),  # Colores personalizados
    labels = c("0" = "Urbano", "1" = "Rural")  # Etiquetas en la leyenda
  ) 
# ---- GRAFICOS DOCENTES -----
# GRAFICO 5: Distribución de género por docentes ----

# Contar cantidad de docentes por género en cada SLEP
genero_slep <- docentes_final %>%
  count(NOMBRE_SLEP, DOC_GENERO) %>%
  group_by(NOMBRE_SLEP) %>%
  mutate(prop = n / sum(n))

# 1 hombre
# 2 mujer

# Gráfico de barras apiladas
g_genero_docente <- ggplot(genero_slep, aes(x = NOMBRE_SLEP, y = n, fill = as.factor(DOC_GENERO))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(nord_palette[1], nord_palette[2]), labels = c("Masculino", "Femenino")) +
  labs(x = "SLEP", y = "Cantidad de Docentes", fill = "Género",
       caption = "Fuente: Elaboración propia en base a datos MINEDUC y ACE") +
  theme_nord_aurora() +
  scale_y_continuous(labels = bm_point) +
  coord_flip()
# GRAFICO 6: Horas promedio contrato por género docentes----

# Agrupar por género y calcular el promedio de horas de contrato
horas_por_genero <- docentes_final %>%
  group_by(DOC_GENERO) %>%
  summarise(promedio_horas = mean(HORAS_CONTRATO))

# Convertir género a factor con etiquetas
horas_por_genero$DOC_GENERO <- factor(horas_por_genero$DOC_GENERO, 
                                      levels = c(1, 2), 
                                      labels = c("Masculino", "Femenino"))

# Gráfico de barras con colores personalizados
g_genero_horas_docente <- ggplot(horas_por_genero, aes(x = DOC_GENERO, y = promedio_horas, fill = DOC_GENERO)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("Masculino" = nord_palette[1], "Femenino" = nord_palette[2])) + # Azul y Naranjo de Nord Aurora
  labs(x = "Género",
       y = "Horas de Contrato Promedio",
       caption = "Fuente: Elaboración propia en base a datos MINEDUC y ACE",
       fill = "Género") +
  theme_nord_aurora() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = bm_point)

# GRAFICO 7: Horas de contrato promedio por SLEP, ambos sexos ----
# Agrupar por SLEP y calcular el promedio de horas de contrato
horas_por_slep <- docentes_final %>%
  group_by(NOMBRE_SLEP) %>%
  summarise(promedio_horas = mean(HORAS_CONTRATO)) %>%
  arrange(desc(promedio_horas))

# Gráfico de barras con colores personalizados
g_prom_horas_docente <- ggplot(horas_por_slep, aes(x = reorder(NOMBRE_SLEP, -promedio_horas), y = promedio_horas, fill = NOMBRE_SLEP)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colores_slep) +
  labs(title = "Carga Horaria Promedio por SLEP", x = "SLEP", y = "Horas de Contrato Promedio") +
  theme_nord_aurora() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = bm_point) +
  coord_flip()


# ---- GRAFICOS ASISTENTES ----
# ---- GRAFICO 8: Total de horas trabajadas por tipo de asistente----
# Agrupar por SLEP y sumar las jornadas de cada categoría
jornadas_categorias <- asistentes_final %>%
  group_by(NOMBRE_SLEP) %>%
  summarise(`Asistentes Profesionales` = sum(JORN_PROF), Paradocentes = sum(JORN_PARA), Auxiliares = sum(JORN_AUX)) %>%
  pivot_longer(cols = c(`Asistentes Profesionales`, Paradocentes, Auxiliares), names_to = "Cargo", values_to = "Total_Jornadas")

# Gráfico de barras apiladas
g_horas_asis <- ggplot(jornadas_categorias, aes(x = NOMBRE_SLEP, y = Total_Jornadas, fill = Cargo)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Asistentes Profesionales" = nord_palette[1], "Paradocentes" = nord_palette[2], "Auxiliares" = nord_palette[3])) +
  labs(x = "SLEP", y = "Total de Horas Trabajadas", fill = "Cargo",
       caption = "Fuente: Elaboración propia en base a datos MINEDUC y ACE") +
  theme_nord_aurora() +
  scale_y_continuous(labels = bm_point) +
  coord_flip()


# ==== ANALISIS MULTIVARIADO: K-MEANS y PCA ====
# ---- Pre-procesamiento de base de datos a utilizar ----

# Definir las columnas a mantener de SIMCE
columnas_seleccionadas <- c(
  "RBD",
  "NALU_4B_RBD",  # Número de alumnos que pueden rendir (4° Básico)
  "NALU_LECT4B_RBD", "NALU_MATE4B_RBD",  # Alumnos que rindieron
  "PROM_LECT4B_RBD", "PROM_MATE4B_RBD",  # Puntajes promedio
  "NALU_2M_RBD",  # Número de alumnos que pueden rendir (2° Medio)
  "NALU_LECT2M_RBD", "NALU_MATE2M_RBD",  # Alumnos que rindieron
  "PROM_LECT2M_RBD", "PROM_MATE2M_RBD"  # Puntajes promedio
)

# Crear el nuevo dataframe con solo las columnas seleccionadas
simce_final <- simce_slep %>% select(all_of(columnas_seleccionadas))

# Establecimientos (RBD) que solo rinden 4° Básico
simce_4b <- simce_final %>%
  filter(!is.na(NALU_4B_RBD) & is.na(NALU_2M_RBD)) %>%
  select(RBD, starts_with("NALU_4B"), starts_with("PROM_LECT4B"), starts_with("PROM_MATE4B")) %>% 
  mutate(across(everything(), as.numeric))

# Establecimientos (RBD) que solo rinden 2° Medio
simce_2m <- simce_final %>%
  filter(is.na(NALU_4B_RBD) & !is.na(NALU_2M_RBD)) %>%
  select(RBD, starts_with("NALU_2M"), starts_with("PROM_LECT2M"), starts_with("PROM_MATE2M")) %>% 
  mutate(across(everything(), as.numeric))

# Resumir docentes y asistentes por RBD (sumando valores)
# Agrupar docentes 
docentes_agg <- docentes_final %>%
  group_by(RBD) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE))  # Promedio en numérica

# Agrupar asistentes 
asistentes_agg <- asistentes_final %>%
  group_by(RBD) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE))

# Agrupar alumnos prioritarios 
prio_agg <- prio_final %>%
  group_by(RBD) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE))

# Agrupar matricula por total en los años que estudiaremos
matricula_agg <- matricula_final %>% 
  group_by(RBD) %>% 
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE))

# inner join - unir matricula (df que originalmente no tiene nulos y contiene todas las observaciones que queremos)
df_kmeans <- muestra %>%
  inner_join(docentes_agg, by = "RBD") %>%
  inner_join(asistentes_agg, by = "RBD") %>% 
  inner_join(prio_agg, by = "RBD") %>% 
  inner_join(matricula_agg, by="RBD")

# lef join - para unir los datos SIMCE y que no se nos borren aquellos que no están en uno u otro
df_kmeans <- df_kmeans %>%
  left_join(simce_4b, by = "RBD", suffix = c("", "_simce4b")) %>%
  left_join(simce_2m, by = "RBD", suffix = c("", "_simce2m"))

# primer filtro de columnas que o están repetidas, o son identificadores
cols_excl_1 <- c('AGNO_TOTAL_simce2M', 'AGNO_TOTAL_docentes', 
               'RURAL_RBD_MUESTRA_docentes', 'ESTADO_ESTAB_MUESTRA_docentes',
               'ESTADO_ESTAB_MUESTRA_simce2M', 'ESTADO_ESTAB_MUESTRA.x', 'AGNO_TOTAL.x ',
               'AGNO_TOTAL.y','RURAL_RBD_MUESTRA.y', 'ESTADO_ESTAB_MUESTRA.y','AGNO_TOTAL',
               'RURAL_RBD_MUESTRA','ESTADO_ESTAB_MUESTRA', 'AGNO', "DGV_RBD","COD_DEPE","COD_DEPE2", "RURAL_RBD", "COD_REG_RBD",
               "COD_PRO_RBD", "COD_COM_RBD", "COD_DEPROV_RBD", "ESTADO_ESTAB")

# Una observación por RBD
df_kmeans <- df_kmeans %>%
  distinct() %>% 
  group_by(RBD) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(RBD = as.integer(RBD)) %>%  # Convertir a integer si es necesario
  filter(RBD %in% rbd_filtro) %>%  # Aplicar filtro
  distinct(RBD, .keep_all = TRUE) %>% # Eliminar duplicados
  select(-any_of(cols_excl_1))

# Crear variable que tiene el porcentaje del total de alumnos que son prioritarios
df_kmeans <- df_kmeans %>%
  mutate(
    PORC_PRIORITARIO_ALU = ifelse(
      !is.na(PRIORITARIO_ALU) & !is.na(MAT_TOTAL) & MAT_TOTAL > 0,
      round((PRIORITARIO_ALU / MAT_TOTAL) * 100, 2),
      0
    )) %>% 
  select(-PRIORITARIO_ALU)

# Rellenar con 0 en establecimientos que no rinden simce 4b o 2m
df_kmeans[is.na(df_kmeans)] <- 0
glimpse(df_kmeans)

# Exlcuimos identificadores (factors) y dejamos solamente variables numericas
cols_excl_2 <- c('RBD', 'SLEP')

# Definición y muestra del DF para trabajar
df_kmeans_num <- df_kmeans %>% select(-any_of(cols_excl_2)) 
  sum(duplicated(df_kmeans_num))
glimpse(df_kmeans_num)


# Escalar datos para que no nos de problemas con el modelo
df_kmeans_scaled <- df_kmeans_num %>% mutate(across(everything(), scale))
glimpse(df_kmeans_scaled)

# Este será, entonces, el dataframe que se usará

# -> df_kmeans_scaled <-

# Después de todo el preprocesamiento, aplicaremos PCA a df_kmeans_scaled para poder
# generar clusters con las dimensiones reducidas, para obtener mejores resultados

# ---- PCA ----
# Aplicar PCA para reducir dimensiones, a los datos escalados
# por eso df_kmeans_scaled

pca <- prcomp(df_kmeans_scaled, center = TRUE)

# Elegir los primeros componentes que explican al menos el 80-90% de la varianza

var_exp <- summary(pca)$importance[2,]  # Proporción de varianza explicada
var_exp_acumulada <- cumsum(var_exp) # Varianza explicada acumulada
num_comp <- min(which(var_exp_acumulada >= 0.90))   # Numero de componentes que explican el 80% de la varianza
num_comp # 9 en nuestro caso

# GRAFICO 9: Visualización de la varianza explicada por los componentes principales----

# Calcular la varianza explicada acumulada
var_exp_acumulada_porc <- cumsum(var_exp) * 100  # Convertir a porcentaje acumulado
num_pcs <- seq_along(var_exp_acumulada_porc)  # Número de PCs

# Crear un dataframe con los valores
df_varianza <- data.frame(
  PC = num_pcs,
  Varianza_Acumulada = var_exp_acumulada_porc)

# Grafico
g_var_expl <- ggplot(df_varianza, aes(x = PC, y = Varianza_Acumulada)) +
  geom_line(color = nord_palette[1], size = 1) +  # Línea principal
  geom_point(color =nord_palette[1], size = 1.6) +  # Puntos en cada PC
  labs(
    title = NULL,  # Sin título
    x = "Número de Componentes Principales",
    y = "Varianza Explicada Acumulada (%)"
  ) +
  scale_x_continuous(breaks = seq(1, length(var_exp_acumulada), by = 1)) +  # Mostrar todos los PCs
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Eje Y con 10% de intervalo
  geom_hline(yintercept = 90, linetype = "dashed", color = nord_aurora_colors$grid, size = 1) +  # Línea de referencia 80%
  annotate("text", x = max(num_pcs) - 10, y = 92, label = "90% Varianza Explicada Acumulada", 
           color = nord_aurora_colors$text, size = 4, fontface = "bold") +  # Texto para la referencia
  theme_nord_aurora()  # Aplicar tu tema personalizado

# GRAFICO 10: Importancia de cada variable en los componentes principales ----

# Ordenar las contribuciones de PC1
pc1_contributions <- sort(abs(pca$rotation[,1]), decreasing = TRUE)
print(pc1_contributions)

# Ordenar las contribuciones de PC2
pc2_contributions <- sort(abs(pca$rotation[,2]), decreasing = TRUE)
print(pc2_contributions)

# Convertir en data frame para graficar
df_loadings <- data.frame(
  Variable = rep(rownames(pca$rotation), 2),
  PC = c(rep("PC1", nrow(pca$rotation)), rep("PC2", nrow(pca$rotation))),
  Carga = c(abs(pca$rotation[,1]), abs(pca$rotation[,2])))

# Graficar
g_contribs <- ggplot(df_loadings, aes(x = reorder(Variable, -Carga), y = Carga, fill = PC)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(x = "Variable",
       y = "Carga Absoluta",
       caption = "Fuente: Elaboración propia en base a datos MINEDUC y ACE") 

# GRAFICO 11: METODO DEL CODO----

g_codo <- fviz_nbclust(pca$x[, 1:num_comp], kmeans, method = "wss") +
  labs(
    title = NULL,  # Eliminar título
    x = "Número de Clusters (K)",
    y = "WSS – Medida del error dentro de los clústers"
  ) +
  scale_color_manual(values = nord_palette) +  # Usar colores de Nord Aurora
  scale_fill_manual(values = nord_palette) +  # Colores de fondo de clusters
  theme_nord_aurora()  # Aplicar tu tema personalizado

# Mostrar gráfico
print(g_codo)

# 4 a 6 (6 y 7 son básicamente iguales)

# ---- K-MEANS ----
# El método del codo nos dice que deberíamos usar 4 a 7 K
# En un proceso iterativo, probaremos de 2 a 7 para ver que tal es su desempeño en silueta

# Lista de cantidad de clusters a probar
n_clusters <- c(2:10)

# DataFrame para almacenar resultados de silueta
resultados_silueta <- data.frame(n_clusters = integer(), puntuacion_silueta = numeric())

# Loop para aplicar K-Means con diferentes n_clusters
for (k in n_clusters) {
  set.seed(42)  # Fijar semilla para reproducibilidad
  
  # Aplicar K-Means a los primeros num_comp componentes del PCA
  kmeans_pca_test <- kmeans(pca$x[, 1:num_comp], centers = k, nstart = 25)
  
  # Calcular índice de silueta
  sil_score <- silhouette( kmeans_pca_test$cluster, dist(pca$x[, 1:num_comp]))
  
  # Almacenar el promedio del índice de silueta
  avg_sil <- mean(sil_score[, 3])
  
  # Agregar a dataframe de resultados
  resultados_silueta <- rbind(resultados_silueta, data.frame(n_clusters = k, puntuacion_silueta = avg_sil))
}

# Elegimos 5K, para obtener menor cantidad de grupos y mejorar su caracterización (se pierde 1% de silueta score)

num_comp_g <- 10 # para visualizar datos en 2d
set.seed(42)
centers <- 3
# Aplicamos K-means a nuestra instancia de PCA creada anteriormente
kmeans_pca <- kmeans(pca$x[, 1:num_comp_g], centers = centers , nstart = 25) 

# pca$x[, 1:num_com] = primeros k componentes
# centers = k-centros

# Agregar la clasificación de clusters al DataFrame
df_kmeans$cluster <- as.factor(kmeans_pca$cluster) 

# GRAFICO 12: Visualizar clustering con PCA y KMEANS-----
# Generar gráfico de clusters con el tema Nord Aurora
g_cluster <- fviz_cluster(kmeans_pca, 
                   data = pca$x[, 1:num_comp_g], 
                   geom = "point", 
                   stand = FALSE) +
  labs(
    title = NULL,  # Sin título
    x = "Componente Principal 1",
    y = "Componente Principal 2"
  ) +
  scale_color_manual(values = nord_palette) +  # Usar colores de Nord Aurora
  scale_fill_manual(values = nord_palette) +  # Colores de fondo de clusters
  theme_nord_aurora()  # Aplicar tu tema personalizado

# Mostrar gráfico
print(g_cluster)

# Graficar el puntaje de silueta para esta instancia de k-means y pca (la que se usará finalmente)
silhouette_score <- silhouette(kmeans_pca$cluster, dist(pca$x[, 1:num_comp_g]))

g_silueta_score <- fviz_silhouette(silhouette_score)


# ==== RESULTADOS DE AGRUPAMIENTO ====
# GRAFICO 13: Número de SLEPS en cada cluster ----

resumen_cluster <- df_kmeans %>%
  count(SLEP, cluster) %>%
  mutate(SLEP = codigo_slep[as.character(SLEP)]) %>%
  arrange(cluster, desc(n))

# Barras apiladas sin total
g_n_cluster <- ggplot(resumen_cluster, aes(x = factor(cluster), y = n, fill = factor(SLEP))) +
  geom_bar(stat = "identity", position = "stack") +  
  labs(
    x = "Cluster",
    y = "Cantidad de Observaciones",
    fill = "SLEP"
  ) +
  scale_fill_manual(values = colores_slep)

# Con total (se ve mal)
ggplot(resumen_cluster, aes(x = factor(cluster), y = n, fill = factor(SLEP))) +
  geom_bar(stat = "identity", position = "stack") +  
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 5, color = "white") +  # Agregar totales
  labs(x = "Cluster",
       y = "Cantidad de Observaciones",
       fill = "SLEP"
  ) +
  scale_fill_manual(values = colores_slep)

# Para tablas
tabla_cluster <- resumen_cluster %>%
  group_by(cluster) %>%
  summarise(total = sum(n))

# GRAFICO 14: Heatmap de variables más comunes por cluster----
# Crear etiquetas más legibles para las variables
etiquetas_variables <- c(
  "ANO_SERVICIO_SISTEMA" = "Años de Servicio Prof.",
  "HORAS_AULA" = "Horas Aula Prof.",
  "HORAS_CONTRATO" = "Horas Contrato Prof",
  "HORAS_DENTRO_ESTAB" = "Horas Dentro Estab. Prof.",
  "HORAS_DIRECT" = "Horas Directas Prof.",
  "HORAS_FUERA_ESTAB" = "Horas Fuera Estab. Prof.",
  "HORAS_TEC_PED" = "Horas Tec. Ped. Prof.",
  "JORN_AUX" = "Jornadas Aux.",
  "JORN_PARA" = "Jornadas Paradocentes",
  "JORN_PROF" = "Jornadas Asistentes Prof.",
  "MAT_HOM_TOT" = "Matrícula Hombres",
  "MAT_MUJ_TOT" = "Matrícula Mujeres",
  "MAT_TOTAL" = "Matrícula Total",
  "NALU_2M_RBD" = "N° Alumnos 2M",
  "NALU_4B_RBD" = "N° Alumnos 4B",
  "N_ASIS" = "N° Asistentes",
  "N_AUX" = "N° Auxiliares",
  "N_HOMBRES" = "N° Asistentes Hombres",
  "N_MUJERES" = "N° Asistentes Mujeres",
  "N_PARA" = "N° Paradocentes",
  "N_PROF" = "N° Profesores",
  "PROM_LECT2M_RBD" = "Prom. Lectura 2M",
  "PROM_LECT4B_RBD" = "Prom. Lectura 4B",
  "PROM_MATE2M_RBD" = "Prom. Matemática 2M",
  "PROM_MATE4B_RBD" = "Prom. Matemática 4B",
  "SLEP" = "SLEP",
  "BEN_SEP" = "N° Beneficiarios SEP",
  "PREFERENTE_ALU" = "N° Alumnos Preferentes",
  "PORC_PRIORITARIO_ALU" = "% Alumnos Prioritarios"
)

# Características de las variables por SLEP
caract_cluster <- df_kmeans %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric) & !all_of(c("RBD","TOT_JORN")), mean, na.rm = TRUE))

# Convertir a formato largo para mejor visualización
caract_cluster_l <- caract_cluster %>%
  pivot_longer(-cluster, names_to = "Variable", values_to = "Valor") %>%
  group_by(cluster, Variable) %>%
  summarise(promedio = mean(Valor, na.rm = TRUE), .groups = "drop")

# Aplicar normalización a los valores de cada variable por cluster
caract_cluster_norm <- caract_cluster_l %>%
  group_by(Variable) %>%
  mutate(promedio_norm = normalize(promedio)) %>%
  ungroup()

# Filtrar para eliminar la variable "SLEP_MUESTRA"
caract_cluster_norm_filtrado <- caract_cluster_norm %>%
  filter(Variable != "SLEP") # ! =

g_heatmap <- ggplot(caract_cluster_norm_filtrado, aes(x = Variable, y = as.factor(cluster), fill = promedio_norm)) +
  geom_tile() +
  scale_fill_gradientn(colors = c(nord_aurora_colors$accent1, "white", nord_aurora_colors$accent2),
                       name = "Valor Normalizado") +
  scale_x_discrete(labels = etiquetas_variables[etiquetas_variables != "SLEP"]) +  # Excluir del etiquetado
  labs(x = "Variables", y = "Cluster") +
  theme_nord_aurora() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.position = "right")

# GRAFICO 15 (OPCIONAL): Grafico de Barras con variables por cluster----
# Grafico de barras de caracteristicas de cluster
g_pesos_cluster <- ggplot(caract_cluster_l, aes(x = Variable, y = promedio, fill = factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Variable",
    y = "Mediana",
    fill = "Clúster",
    caption = "Fuente: Elaboración propia en base a datos MINEDUC y ACE" 
  ) +
  coord_flip()



