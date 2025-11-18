#==============================
#  muestreo aleatorio simple
#==============================
#cargan librerias necesarias
library(dplyr)
library(sampling) 
#define una semilla(esto permite a otras personas replicar estos procesos y obtener los mismos resultados)
set.seed(2)
#genera la muestra de 100 datos de manera aleatoria en base a la semilla
muestra_alt <- persona %>% slice_sample(n = 100)
muestra_alt <- muestra_alt %>%
select(estrato,empleo,desempleo,ingrl,ingpc,ciudad,conglomerado)
View(muestra_alt)
#=============================
#   muestreo sistematico
#=============================
library(dplyr)
#-----------------------------------------
# 1. Limpiar solo los casos donde empleo y desempleo están ambos vacíos
#-----------------------------------------
persona_limpia <- persona %>%
  filter(!(is.na(empleo) & is.na(desempleo)))
#-----------------------------------------
# 2. Muestreo sistemático (versión correcta)
#-----------------------------------------
N <- nrow(persona_limpia)   # población limpia
n <- 100                    # tamaño de muestra neto
r <- N / n                  # intervalo sistemático
set.seed(9)
# punto de arranque aleatorio dentro del intervalo
b <- sample(1:floor(r), 1)
# secuencia sistemática sin error (usar `seq` sin redondear)
m <- floor(seq(from = b, to = N, by = r))
# eliminar duplicados si r < 1 (pasa cuando N < n)
m <- unique(m)
# tomar la muestra
muestra_sis <- persona_limpia[m, ]

# seleccionar columnas si quieres
muestra_sis <- muestra_sis %>%
  select(ciudad, conglomerado, estrato, empleo, desempleo, ingrl, ingpc, p10a)

View(muestra_sis)
#================================
#   Muestreo estratificado
#================================
library(dplyr)
# Calcular tamaño de cada estrato
tabla_estratos <- persona %>%
  group_by(estrato) %>%
  summarise(N_h = n(), .groups = "drop")
# Definir tamaño total de la muestra
n_total <- 200  # ajustado segun usuario
# Calcular tamaño proporcional por estrato
tabla_estratos <- tabla_estratos %>%
  mutate(n_h = round(N_h / sum(N_h) * n_total))
# Seleccionar muestra estratificada
set.seed(2)
muestra_estrat <- persona %>%
  inner_join(tabla_estratos, by = "estrato") %>%
  group_by(estrato) %>%
  group_modify(~ slice_sample(.x, n = unique(.x$n_h))) %>%
  ungroup()
View(muestra_estrat)
# Totales por estrato dentro de la muestra
resumen_muestra <- muestra_estrat %>%
  summarise(
    total_empleados = sum(!is.na(empleo) & empleo == 1),
    total_desempleados = sum(!is.na(desempleo) & desempleo == 1),
    .by = estrato
  )
View(resumen_muestra)
# Empleados de la muestra por rangos de ingresos
empleados_rango <- muestra_estrat %>%
  filter(!is.na(empleo) & empleo == 1 & !is.na(ingrl)) %>%
  mutate(rango_ingresos = case_when(
    ingrl < 300 ~ "Bajo (<300)",
    ingrl >= 300 & ingrl < 700 ~ "Medio (300–699)",
    ingrl >= 700 ~ "Alto (≥700)"
  )) %>%
  group_by(estrato, rango_ingresos) %>%
  summarise(personas = n(), .groups = "drop")
View(empleados_rango)
#================================
#   Muestreo conglomerado
#================================
library(dplyr)
set.seed(3)
# 1. Definir cuántos conglomerados quieres seleccionar
n_conglomerados <- 10
# 2. Identificar los conglomerados únicos
conglomerados_unicos <- persona %>%
  distinct(conglomerado) %>%
  pull(conglomerado)
# 3. Seleccionar aleatoriamente los conglomerados
conglomerados_muestra <- sample(conglomerados_unicos, n_conglomerados)
# 4. Tomar todas las personas dentro de esos conglomerados seleccionados
muestra_conglomerado <- persona %>%
  filter(conglomerado %in% conglomerados_muestra) %>%
  select(ciudad,conglomerado,ingrl,estrato)
# 5. Ver resultado general
View(muestra_conglomerado)
# 6. (Opcional) Conteo de personas por conglomerado
conteo_conglomerados <- muestra_conglomerado %>%
  group_by(conglomerado) %>%
  summarise(total_personas = n(), .groups = "drop")

print(conteo_conglomerados)
