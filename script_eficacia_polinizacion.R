
install.packages("FactoMineR")
install.packages("factoextra")


#instala matriz
install.packages("https://cran.r-project.org/src/contrib/Archive/Matrix/Matrix_1.3-4.tar.gz", repos = NULL, type = "source")


# Instalar las bibliotecas necesarias solo si aún no están instaladas
if (!requireNamespace("readr")) install.packages("readr")
if (!requireNamespace("ggplot2")) install.packages("ggplot2")
if (!requireNamespace("dplyr")) install.packages("dplyr")
if (!requireNamespace("tidyr")) install.packages("tidyr")
if (!requireNamespace("FactoMineR")) install.packages("FactoMineR")
if (!requireNamespace("factoextra")) install.packages("factoextra")
if (!requireNamespace("lme4")) install.packages("lme4")
if (!requireNamespace("multcomp")) install.packages("multcomp")
if (!requireNamespace("factoextra")) install.packages("factoextra")


# Cargar las bibliotecas necesarias
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lme4)
library(FactoMineR)
library(factoextra)
library(multcomp)

# Cargar los datos
data <- read.csv("Visitadores_florales.csv")

# 1. Número total de visitas por especie
total_visits <- data %>%
  group_by(MORFOESPECIE) %>%
  summarise(Total_Visitas = n())

# 2. Tiempo promedio por visita por especie
average_time <- data %>%
  group_by(MORFOESPECIE) %>%
  summarise(Tiempo_Promedio = mean(TIEMPO.EN.CADA.FLOR..seg., na.rm = TRUE))

# 3. Tasa de visitas por individuo por especie
visits_per_individual <- data %>%
  group_by(MORFOESPECIE, Individuo) %>%
  summarise(Visitas = n()) %>%
  group_by(MORFOESPECIE) %>%
  summarise(Visitas_Promedio = mean(Visitas, na.rm = TRUE))

# Tema personalizado para gráficos científicos
theme_scientific <- theme_classic() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 4. Gráfico: Número total de visitas por especie
ggplot(total_visits, aes(x = MORFOESPECIE, y = Total_Visitas)) +
  geom_bar(stat = "identity", fill = "#1b9e77", color = "black") +
  labs(title = "Número total de visitas por especie", x = "Especie", y = "Total de visitas") +
  theme_scientific

# 5. Gráfico: Tiempo promedio por visita por especie
ggplot(average_time, aes(x = MORFOESPECIE, y = Tiempo_Promedio)) +
  geom_bar(stat = "identity", fill = "#d95f02", color = "black") +
  labs(title = "Tiempo promedio por visita por especie", x = "Especie", y = "Tiempo promedio (seg)") +
  theme_scientific

# 6. Gráfico: Tasa de visitas por individuo por especie
ggplot(visits_per_individual, aes(x = MORFOESPECIE, y = Visitas_Promedio)) +
  geom_bar(stat = "identity", fill = "#7570b3", color = "black") +
  labs(title = "Tasa de visitas por individuo por especie", x = "Especie", y = "Visitas promedio por individuo") +
  theme_scientific

# 7. Gráfico: Boxplot del tiempo en cada flor por especie
ggplot(data, aes(x = MORFOESPECIE, y = TIEMPO.EN.CADA.FLOR..seg.)) +
  geom_boxplot(fill = "#e7298a", color = "black", outlier.color = "red", outlier.shape = 16) +
  labs(title = "Distribución del tiempo en cada flor por especie", x = "Especie", y = "Tiempo en cada flor (seg)") +
  theme_scientific

# 8. ANOVA: Comparación del tiempo de visita entre especies
anova_result <- aov(TIEMPO.EN.CADA.FLOR..seg. ~ MORFOESPECIE, data = data)
summary(anova_result)

# Prueba post-hoc Tukey HSD
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# 9. Modelo mixto: Tiempo de visita con individuo como efecto aleatorio
modelo_mixto <- lmer(TIEMPO.EN.CADA.FLOR..seg. ~ MORFOESPECIE + (1 | Individuo), data = data)
summary(modelo_mixto)

# 10. Histograma del tiempo de visita
ggplot(data, aes(x = TIEMPO.EN.CADA.FLOR..seg.)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  facet_wrap(~ MORFOESPECIE) +
  labs(title = "Distribución del tiempo de visita por especie", x = "Tiempo (seg)", y = "Frecuencia") +
  theme_minimal()

# 11. Análisis de Componentes Principales (PCA)
pca_data <- data[, c("NUM..DE.FLOR", "TIEMPO.EN.CADA.FLOR..seg.")]
pca_result <- PCA(pca_data, scale.unit = TRUE)

# Visualización de PCA
fviz_pca_biplot(pca_result, repel = TRUE, col.var = "blue", col.ind = "red") +
  labs(title = "Análisis de Componentes Principales (PCA)") +
  theme_minimal()









