#=====================================================
# OPS- Segunda Capacitación en análisis de datos, automatización y 
# visualización de la información epidemiológica de la 
# Red Argentina de Vigilancia Centinela de IRAG 
#=====================================================

# ----------------------------------------------------
# 📎 ESTADÍSTICA DESCRIPTIVA EN RSTUDIO
# ----------------------------------------------------

# PROCESAMIENTO DE LOS DATOS

#Filtro evento de interés y año en estudio

base <- base %>% filter(CLASIFICACION_MANUAL != "Caso invalidado por epidemiología" & 
                          ANIO_MIN_INTERNACION == 2025)


# ----------------------------------------------------
# 🧮 FRECUENCIA ABSOLUTA Y RELATIVA
# ----------------------------------------------------

# Frecuencia absoluta

casos_IRAGE <- sum(base$CLASIFICACION_MANUAL == "IRAG extendida", na.rm = TRUE)

casos_IRAG <-  sum(base$CLASIFICACION_MANUAL == "Infección respiratoria aguda grave (IRAG)", na.rm = TRUE)

casos_totales <- sum(casos_IRAGE + casos_IRAG)

# Frecuencia relativa

proporcion_irage <- round((casos_IRAGE/casos_totales)*100,1)

proporcion_irag <- round((casos_IRAG/casos_totales)*100,1)

# Calcular frecuencia absoluta y relativa utilizando sintaxis tidyverse

frecuencia_clasificacion_manual <- base %>%
  count(`CLASIFICACION_MANUAL`) %>%
  mutate(proporcion = round(100 * n / sum(n), 1))

frecuencia_IRAG <- frecuencia_clasificacion_manual %>% filter(`CLASIFICACION_MANUAL` == "Infección respiratoria aguda grave (IRAG)") %>% pull(n)
frecuencia_IRAGE <- frecuencia_clasificacion_manual %>% filter(`CLASIFICACION_MANUAL` == "IRAG extendida") %>% pull(n)

prop_IRAG <- frecuencia_clasificacion_manual %>% filter(`CLASIFICACION_MANUAL` == "Infección respiratoria aguda grave (IRAG)") %>% pull(proporcion)
prop_IRAGE <- frecuencia_clasificacion_manual %>% filter(`CLASIFICACION_MANUAL` == "IRAG extendida") %>% pull(proporcion)


# -------------------------------------------------------
# 🔢 MEDIDAS DE TENDENCIA CENTRAL, DISPERSIÓN Y POSICIÓN 
# -------------------------------------------------------

# Medidas de tendencia central

media <- mean(base$EDAD_DIAGNOSTICO,na.rm = TRUE)

media_redondeada <- round(mean(base$EDAD_DIAGNOSTICO,na.rm = TRUE),1)

mediana <- median(base$EDAD_DIAGNOSTICO,na.rm = TRUE)

# Medidas de dispersión

desvio <-sd(base$EDAD_DIAGNOSTICO,na.rm = TRUE)

varianza <-var(base$EDAD_DIAGNOSTICO,na.rm = TRUE)


#Medidas de posición

Q1 <- quantile(base$EDAD_DIAGNOSTICO, 0.25, na.rm = TRUE)

Q2 <- quantile(base$EDAD_DIAGNOSTICO, 0.50, na.rm = TRUE)

Q3 <- quantile(base$EDAD_DIAGNOSTICO, 0.75, na.rm = TRUE)

rango_intercuartilico <- IQR(base$EDAD_DIAGNOSTICO, na.rm = TRUE)

minimo <- min(base$EDAD_DIAGNOSTICO, na.rm = TRUE)

maximo <- max(base$EDAD_DIAGNOSTICO, na.rm = TRUE)
    

# =====================================================
# GRAFICO GGPLOT2: CURVA EPIDEMIOLÓGICA 
# =====================================================

# Preparación del data frame

#Agrupo casos notificados por semana epidemiológica

curva_epidemiologica_casos <- base %>% group_by(CLASIFICACION_MANUAL,SEPI_MIN_INTERNACION) %>%
  summarise(n = n()) %>%
  ungroup()

#Completo tabla con las SE donde no hubo casos notificados. El número de casos se completa con 0

curva_epidemiologica_casos <- curva_epidemiologica_casos %>% 
  complete(SEPI_MIN_INTERNACION = 1:53,
           fill = list (n= 0))

# Creo la variable SE para utilizar como etiqueta del eje x.Se 
# normaliza la escritura para que todas las SE estén compuestas por 2 dígitos

curva_epidemiologica_casos <- curva_epidemiologica_casos %>% mutate(SE = str_pad(SEPI_MIN_INTERNACION, #variable a normalizar
                                                                     width = 2, #cantidad de dígitos
                                                                     side = "left", #posición del número que se utilizará para "completar"
                                                                     pad = "0")) #número que se utilizará para "completar"

# Gráfico- Casos de IRAG e IRAGE notificados por SE en ggplot2

colores <- c("#7fc97f","#beaed4") 

grafico_curva_casos <- ggplot(curva_epidemiologica_casos, aes(y= n, x = SE, fill= CLASIFICACION_MANUAL)) +
  geom_col () +
  labs (title = "",
        x = "Semana epidemiológica",
        y = "Casos",
        caption = "Fuente: elaboración propia a partir de datos del SNVS 2.0.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5)) +
scale_fill_manual(values = colores,
                  name = "",
                  labels = c("IRAG","IRAGe"),
                  na.translate = FALSE) +
  theme(legend.position = "bottom")


grafico_curva_casos


# =====================================================
# GRAFICO HIGHCHARTER: CURVA EPIDEMIOLÓGICA 
# =====================================================

curva_wider <- curva_epidemiologica_casos %>% pivot_wider(names_from = CLASIFICACION_MANUAL,
                                                          values_from = n)

curva_interactiva <-highchart() %>%
  hc_chart(type= "column") %>%
  hc_plotOptions(column = list(stacking = "normal",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = curva_wider$SE, #categorías en eje X
    title = list(text = "Semana epidemiológica")) %>%  #título del eje X) 
   hc_yAxis(title= list(text="Casos notificados")) %>%
   hc_credits(text = "Fuente: Elaboración propia en base a datos del SNVS 2.0", 
           enabled = TRUE) %>% 
  hc_add_series(
    data = curva_wider$`Infección respiratoria aguda grave (IRAG)`,
    name = "IRAG",
    color = "#7fc97f") %>%
  hc_add_series(
    data = curva_wider$`IRAG extendida`,
    name = "IRAGe",
    color = "#beaed4") 

curva_interactiva
