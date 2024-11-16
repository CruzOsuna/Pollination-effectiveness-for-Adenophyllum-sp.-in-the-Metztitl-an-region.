setwd("/home/cruz/Documentos/UNAM/Ecologia I/Reporte_Campo")

# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(viridis)  # Librería para la paleta de colores viridis

# Cargar los datos
data <- read.csv("Visitadores_florales.csv")
colnames(data)[which(colnames(data) == "TIEMPO.EN.CADA.FLOR")] <- "Tiempo_en_flor"

# Calcular el número de flores visitadas y el tiempo de visita por individuo
datos_por_individuo <- data %>%
  group_by(MORFOESPECIE, Individuo) %>%
  summarise(
    Flores_visitadas = n(),
    Tiempo_total_visita = sum(Tiempo_en_flor)
  )

# Excluir morfoespecies con menos de 3 observaciones
datos_filtrados <- datos_por_individuo %>%
  group_by(MORFOESPECIE) %>%
  filter(n() >= 3)

# 1. Crear un boxplot para el número de flores visitadas por individuo
ggplot(datos_filtrados, aes(x = MORFOESPECIE, y = Flores_visitadas, fill = MORFOESPECIE)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = 0.6) +
  labs(
    title = "Distribución del Número de Flores Visitadas por Individuo",
    x = "Morfoespecie",
    y = "Número de Flores Visitadas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_fill_viridis_d()  # Usar la paleta viridis

# Guardar el boxplot
ggsave("boxplot_flores_visitadas_viridis.png", dpi = 600, width = 10, height = 6)

# 2. Crear un gráfico de barras de las medianas del número de flores visitadas por morfoespecie
medianas_flores <- datos_filtrados %>%
  group_by(MORFOESPECIE) %>%
  summarise(Mediana_flores = median(Flores_visitadas))

ggplot(medianas_flores, aes(x = MORFOESPECIE, y = Mediana_flores, fill = MORFOESPECIE)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(
    title = "Medianas del Número de Flores Visitadas por Morfoespecie",
    x = "Morfoespecie",
    y = "Mediana de Flores Visitadas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_fill_viridis_d()  # Usar la paleta viridis

# Guardar el gráfico de barras
ggsave("barras_medianas_flores_visitadas_viridis.png", dpi = 600, width = 10, height = 6)

# 3. Crear un boxplot para el tiempo total de visita a cada flor por individuo
ggplot(datos_filtrados, aes(x = MORFOESPECIE, y = Tiempo_total_visita, fill = MORFOESPECIE)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = 0.6) +
  labs(
    title = "Distribución del Tiempo Total de Visita por Individuo",
    x = "Morfoespecie",
    y = "Tiempo Total de Visita (segundos)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_fill_viridis_d()  # Usar la paleta viridis

# Guardar el boxplot
ggsave("boxplot_tiempo_visita_viridis.png", dpi = 600, width = 10, height = 6)

# 4. Crear un gráfico de barras de las medianas del tiempo total de visita por morfoespecie
medianas_tiempo <- datos_filtrados %>%
  group_by(MORFOESPECIE) %>%
  summarise(Mediana_tiempo = median(Tiempo_total_visita))

ggplot(medianas_tiempo, aes(x = MORFOESPECIE, y = Mediana_tiempo, fill = MORFOESPECIE)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(
    title = "Medianas del Tiempo Total de Visita por Morfoespecie",
    x = "Morfoespecie",
    y = "Mediana del Tiempo de Visita (segundos)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_fill_viridis_d()  # Usar la paleta viridis

# Guardar el gráfico de barras
ggsave("barras_medianas_tiempo_visita_viridis.png", dpi = 600, width = 10, height = 6)
