#============================
#INTEGRANTES:
#JAIRO GUILLEN
#MOISES BELTRAN
#MARIA BENAVIDES
#REINALDO RUILOVA
#JOE LEMA
#============================

#================================
#   DISTRIBUCIONES: NORMAL, BERNOULLI, BINOMIAL
#================================

#================================
# Parte I – Distribución Normal
#================================

# Generar una muestra con distribución normal
set.seed(456)
muestra_normal <- rnorm(1000, mean = 50, sd = 10)

# Cálculo de medidas descriptivas
media <- mean(muestra_normal)
desviacion <- sd(muestra_normal)
minimo <- min(muestra_normal)
maximo <- max(muestra_normal)
rango <- maximo - minimo

# Mostrar resultados
cat("Media:", media, "\nDesviación estándar:", desviacion,
    "\nMínimo:", minimo, "\nMáximo:", maximo, "\nRango:", rango, "\n")

# Gráfico
hist(muestra_normal, breaks = 30, col = "lightblue",
     main = "Distribución Normal",
     xlab = "Valores", ylab = "Frecuencia", freq = FALSE)
curve(dnorm(x, mean = 50, sd = 10), add = TRUE, col = "red", lwd = 2)


#================================
# Parte II – Distribución Bernoulli
#================================

# Simulación de variable Bernoulli (1 = éxito, 0 = fracaso)
set.seed(777)
p <- 0.4      # Probabilidad teórica de éxito
n <- 100      # Número de ensayos
bernoulli <- rbinom(n, size = 1, prob = p)

# Tabla de frecuencias
print(table(bernoulli))

# Gráfico
barplot(table(bernoulli), col = c("yellow", "purple"),
        names.arg = c("Fracaso (0)", "Éxito (1)"),
        main = "Distribución Bernoulli", ylab = "Frecuencia")

# Proporciones observadas
prop_exito <- mean(bernoulli)
prop_fracaso <- 1 - prop_exito
cat("Proporción de éxitos:", prop_exito,
    "\nProporción de fracasos:", prop_fracaso, "\n")


#================================
# Parte III – Distribución Binomial
#================================

# Simulación de una distribución binomial
set.seed(777)
n_intentos <- 10      # Número de ensayos por experimento
p_exito <- 0.5        # Probabilidad de éxito
experimentos <- 1000  # Número de repeticiones

resultados_binom <- rbinom(experimentos, size = n_intentos, prob = p_exito)

# Gráfico
hist(resultados_binom, breaks = 10, col = "blue",
     main = "Distribución Binomial", xlab = "Número de éxitos", ylab = "Frecuencia")

# Media observada
media_binom <- mean(resultados_binom)
cat("Media de éxitos:", media_binom, "\n")
