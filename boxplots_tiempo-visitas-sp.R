setwd("/home/cruz/Documentos/UNAM/Ecologia I/Reporte_Campo")

# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Cargar los datos y renombrar la columna si no se ha hecho ya
data <- read.csv("Visitadores_florales.csv")
colnames(data)[which(colnames(data) == "TIEMPO.EN.CADA.FLOR")] <- "Tiempo_en_flor"

# 1. Calcular el número de visitas por individuo en cada morfoespecie
visitas_por_individuo <- data %>%
  group_by(MORFOESPECIE, Individuo) %>%
  summarise(Visitas = n())

# Crear un boxplot para la distribución del número de visitas por individuo en cada morfoespecie
ggplot(visitas_por_individuo, aes(x = MORFOESPECIE, y = Visitas)) +
  geom_boxplot(outlier.color = "red", fill = "skyblue", color = "black") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
  labs(
    title = "Distribución del Número de Visitas por Individuo en Cada Morfoespecie",
    x = "Morfoespecie",
    y = "Número de Visitas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  )

# Guardar el primer gráfico en alta calidad
ggsave("numero_visitas_por_individuo_boxplot.png", dpi = 600, width = 8, height = 6)

# 2. Crear un boxplot para la distribución del tiempo de visita en cada flor en cada morfoespecie
ggplot(data, aes(x = MORFOESPECIE, y = Tiempo_en_flor)) +
  geom_boxplot(outlier.color = "red", fill = "lightgreen", color = "black") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
  labs(
    title = "Distribución del Tiempo de Visita en Cada Flor por Morfoespecie",
    x = "Morfoespecie",
    y = "Tiempo en cada flor (segundos)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  )

# Guardar el segundo gráfico en alta calidad
ggsave("tiempo_visita_por_flor_boxplot.png", dpi = 600, width = 8, height = 6)

