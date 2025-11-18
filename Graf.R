#==============================
#  Muestreo Aleatorio simple
#==============================

library(dplyr)
library(ggplot2)
library(scales)

# Suponiendo que ya tienes tu muestra aleatoria
# muestra_aleatoria <- persona %>% slice_sample(n = 100)

#-------------------------------
# 1. Estado laboral
#-------------------------------
muestra_estado <- muestra_alt %>%
  mutate(estado = case_when(
    empleo == 1 & (is.na(desempleo) | desempleo != 1) ~ "Empleado",
    desempleo == 1 & (is.na(empleo) | empleo != 1) ~ "Desempleado",
    TRUE ~ "Otro/NA"
  ))

ggplot(muestra_estado %>% filter(estado != "Otro/NA"), aes(x = estado, fill = estado)) +
  geom_bar() +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Estado laboral - Muestra aleatoria", y = "Cantidad de personas", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

#-------------------------------
# 2. Rangos de ingresos
#-------------------------------
muestra_ingresos <- muestra_alt %>%
  filter(!is.na(ingrl)) %>%
  mutate(rango_ingresos = case_when(
    ingrl < 300 ~ "Bajo (<300)",
    ingrl >= 300 & ingrl < 700 ~ "Medio (300–699)",
    ingrl >= 700 ~ "Alto (≥700)"
  ))

ggplot(muestra_ingresos, aes(x = rango_ingresos, fill = rango_ingresos)) +
  geom_bar() +
  labs(title = "Frecuencia de ingresos por rango - Muestra aleatoria",
       x = "Rango de ingresos", y = "Cantidad de personas") +
  theme_minimal() +
  theme(legend.position = "none")


#==============================
#  Muestreo Sistemático
#==============================
library(dplyr)
library(ggplot2)
library(scales)

#--------------------------------------
# 1. FILTRAR SOLO PERSONAS CON EMPLEO O DESEMPLEO
#--------------------------------------
muestra_laboral <- muestra_sis %>%
  filter(empleo == 1 | desempleo == 1)

#--------------------------------------
# 2. DEFINIR CUÁNTAS CIUDADES GRANDES TOMAR
#--------------------------------------
top_n_ciudades <- 5   # <-- cámbialo si quieres 6, 8, 10, etc.

#--------------------------------------
# 3. IDENTIFICAR LAS CIUDADES MÁS GRANDES (CON DATOS LABORALES)
#--------------------------------------
ciudades_grandes <- muestra_laboral %>% 
  count(ciudad) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = top_n_ciudades) %>% 
  pull(ciudad)

#--------------------------------------
# 4. FILTRAR SOLO ESAS CIUDADES GRANDES
#--------------------------------------
datos_filtrados <- muestra_laboral %>% 
  filter(ciudad %in% ciudades_grandes)

#--------------------------------------
# 5. DETERMINAR CUÁNTAS PERSONAS TOMAR POR CIUDAD
#--------------------------------------
n_por_ciudad <- datos_filtrados %>%
  count(ciudad) %>%
  summarise(minimo = min(n)) %>%
  pull(minimo)

#--------------------------------------
# 6. CREAR MUESTRA BALANCEADA (MISMO N POR CIUDAD)
#--------------------------------------
set.seed(123)
muestra_balanceada <- datos_filtrados %>%
  group_by(ciudad) %>%
  sample_n(n_por_ciudad) %>%
  ungroup()

#--------------------------------------
# 7. CLASIFICAR EMPLEO / DESEMPLEO
#--------------------------------------
datos_laborales <- muestra_balanceada %>%
  mutate(
    condicion_laboral = case_when(
      empleo == 1 ~ "Empleado",
      desempleo == 1 ~ "Desempleado"
    )
  ) %>%
  count(ciudad, condicion_laboral) %>%
  group_by(ciudad) %>%
  mutate(p = n / sum(n))

#--------------------------------------
# 8. GRAFICAR
#--------------------------------------
ggplot(datos_laborales,
       aes(x = factor(ciudad), y = p, fill = condicion_laboral)) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Condición laboral en las ciudades con datos (muestra balanceada)",
    x = "Ciudad",
    y = "Porcentaje",
    fill = "Condición"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#================================
# MUESTREO ESTRAT 
#=================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# 1. Quitar duplicados (dejando cada persona solo una vez)
persona <- persona %>% 
  distinct(estrato, empleo, desempleo, ingrl, ingpc, .keep_all = TRUE)

# 2. Arreglar NA que generan ruido
persona <- persona %>%
  mutate(
    empleo = ifelse(is.na(empleo), 0, empleo),
    desempleo = ifelse(is.na(desempleo), 0, desempleo)
  )

# 3. Crear variable laboral limpia
persona <- persona %>%
  mutate(estado_laboral = case_when(
    empleo == 1 & desempleo == 0 ~ "Empleado",
    desempleo == 1 & empleo == 0 ~ "Desempleado",
    TRUE ~ "Otro"
  ))




N_estratos <- 10   # Número de estratos visibles (tu límite)

# Seleccionar los estratos más grandes ANTES del análisis
estratos_top <- persona %>%
  count(estrato) %>%
  arrange(desc(n)) %>%
  slice_head(n = N_estratos) %>%
  pull(estrato)

persona_filtrada <- persona %>% 
  filter(estrato %in% estratos_top)

# Tabla de estratos
tabla_estratos <- persona_filtrada %>%
  count(estrato, name = "N_h")

# Tamaño total de la muestra
n_total <- 200

# Cálculo proporcional por estrato
tabla_estratos <- tabla_estratos %>%
  mutate(n_h = round(N_h / sum(N_h) * n_total))

# Ajuste final para que sum(n_h) = n_total EXACTAMENTE
diferencia <- n_total - sum(tabla_estratos$n_h)

if(diferencia != 0){
  idx <- which.max(tabla_estratos$N_h)   # corregir en el estrato más grande
  tabla_estratos$n_h[idx] <- tabla_estratos$n_h[idx] + diferencia
}

# Muestreo estratificado correcto
set.seed(2)
muestra_estratificada <- persona_filtrada %>%
  inner_join(tabla_estratos, by = "estrato") %>%
  group_by(estrato) %>%
  group_modify(~ slice_sample(.x, n = .x$n_h[1])) %>%
  ungroup()


#==============================
#   Gráfica 1: Población vs Muestra
#==============================

tabla_pop <- persona_filtrada %>%
  count(estrato) %>%
  mutate(tipo = "Población")

tabla_samp <- muestra_estratificada %>%
  count(estrato) %>%
  mutate(tipo = "Muestra")

combo <- bind_rows(tabla_pop, tabla_samp) %>%
  group_by(tipo) %>%
  mutate(p = n / sum(n))

ggplot(combo, aes(x = factor(estrato), y = p, fill = tipo)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Proporción por estrato: población vs muestra",
       x = "Estrato", y = "%") +
  theme_minimal()


#==============================
#   Gráfica 2: Estado laboral por estrato
#==============================

muestra_estratificada %>%
  count(estrato, estado_laboral) %>%
  group_by(estrato) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(x = factor(estrato), y = p, fill = estado_laboral)) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Estado laboral por estrato (muestra)", y = "%") +
  theme_minimal()



#==============================
#  Muestreo por Conglomerados
#==============================
library(dplyr)
library(ggplot2)

set.seed(4)
n_conglomerados <- 10

# Obtener lista de conglomerados únicos
conglomerados_unicos <- persona %>% 
  distinct(conglomerado) %>% #elimina duplicados
  pull(conglomerado) #convierte en vector para poder usar sample

# Validar que no se pidan más conglomerados de los disponibles
if (n_conglomerados > length(conglomerados_unicos)) {
  stop("Número de conglomerados a seleccionar mayor que los disponibles")
}

# Seleccionar aleatoriamente los conglomerados
conglomerados_muestra <- sample(conglomerados_unicos, n_conglomerados)

# Filtrar personas que pertenecen a esos conglomerados seleccionados
muestra_conglomerado <- persona %>% 
  filter(conglomerado %in% conglomerados_muestra)



#=============================
#   GRÁFICO 1: INGRESO PROMEDIO POR CONGLOMERADO
#=============================

resumen_ingresos <- muestra_conglomerado %>%
  filter(!is.na(ingrl)) %>%
  group_by(conglomerado) %>% #los agrupa
  summarise(ingreso_promedio = mean(ingrl, na.rm = TRUE), .groups = "drop") #crea una nueva tabla  y luego desagrupa

ggplot(resumen_ingresos, aes(x = reorder(factor(conglomerado), ingreso_promedio), y = ingreso_promedio)) +
  geom_bar(stat = "identity", fill = "darkorange", alpha = 0.8) +
  labs(
    title = "Ingreso promedio por conglomerado",
    x = "Conglomerado (ordenado por ingreso promedio)",
    y = "Ingreso promedio (ingrl)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

#=============================
#   GRÁFICO 2: EMPLEADOS Y DESEMPLEADOS POR CONGLOMERADO
#=============================

resumen_empleo <- muestra_conglomerado %>%
  mutate(condicion_laboral = case_when(
    empleo == 1 ~ "Empleado",
    desempleo == 1 ~ "Desempleado",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(condicion_laboral)) %>%
  group_by(conglomerado, condicion_laboral) %>%
  summarise(cantidad = n(), .groups = "drop")

ggplot(resumen_empleo, aes(x = factor(conglomerado), y = cantidad, fill = condicion_laboral)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Empleados y desempleados por conglomerado",
    x = "Conglomerado",
    y = "Cantidad de personas",
    fill = "Condición laboral"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

#=============================
#   RESUMEN GLOBAL DE EMPLEO VS DESEMPLEO
#=============================

resumen_global <- resumen_empleo %>%
  group_by(condicion_laboral) %>%
  summarise(total = sum(cantidad), .groups = "drop")

cat("\n===== RESUMEN GLOBAL =====\n")
print(resumen_global)

total_empleados <- resumen_global$total[resumen_global$condicion_laboral == "Empleado"]
total_desempleados <- resumen_global$total[resumen_global$condicion_laboral == "Desempleado"]
total_personas <- sum(resumen_global$total)

cat("\nTotal empleados: ", total_empleados,
    "\nTotal desempleados: ", total_desempleados,
    "\nTotal general (empleo+desempleo): ", total_personas, "\n", sep = "")

#=============================
#   GRÁFICO 3: COMPARATIVA GLOBAL (opcional)
#=============================

ggplot(resumen_global, aes(x = condicion_laboral, y = total, fill = condicion_laboral)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  labs(
    title = "Comparativa global: empleados vs desempleados",
    x = "Condición laboral",
    y = "Cantidad total"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
