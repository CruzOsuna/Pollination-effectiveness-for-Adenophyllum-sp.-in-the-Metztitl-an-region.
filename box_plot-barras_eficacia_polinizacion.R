setwd("/home/cruz/Documentos/UNAM/Ecologia I/Reporte_Campo")

# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(viridis)  # Librería para la paleta de colores viridis

# Cargar los datos
data <- read.csv("Visitadores_florales.csv")

# Verificar los nombres de las columnas
print(colnames(data))

# Asegúrate de que el nombre de la columna sea correcto
if (!"MORFOESPECIE" %in% colnames(data)) {
  stop("La columna 'MORFOESPECIE' no se encontró. Verifica el nombre de la columna en tu archivo.")
}

# Calcular el número total de visitas por morfoespecie
tasa_visitas_data <- data %>%
  group_by(MORFOESPECIE) %>%
  summarise(
    Total_visitas = n(),
    Tasa_visitas_por_hora = Total_visitas / (200 / 60)  # Calcular visitas por hora considerando 200 minutos
  )

# Excluir las morfoespecies con menos de 3 visitas
tasa_visitas_data_filtrada <- tasa_visitas_data %>%
  filter(Total_visitas >= 10)

# Crear un gráfico de barras de las tasas de visitas por hora con la paleta viridis
ggplot(tasa_visitas_data_filtrada, aes(x = MORFOESPECIE, y = Tasa_visitas_por_hora, fill = MORFOESPECIE)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(
    title = "Tasa de Visitas por Hora por Morfoespecie",
    x = "Morfoespecie",
    y = "Tasa de Visitas (visitas/hora)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_fill_viridis_d()  # Usar la paleta viridis

# Guardar el gráfico de barras
ggsave("barras_tasa_visitas_por_hora_filtrada.png", dpi = 600, width = 10, height = 6)
