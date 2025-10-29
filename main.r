## Identificar grupos de clientes 


print("hello world")

# install.packages("tidyverse")
# install.packages("cluster")
# install.packages("factoextra")


library(tidyverse)
library(cluster)
library(factoextra)


# Criar um conjunto de dados fictícios

set.seed(123) # para reprodutibilidade

clientes <- data.frame(
    Idade = sample(18:70, 100, replace = TRUE),
    Renda_Anual = sample(20:150, 100, replace = TRUE),
    Pontuacao_Credido = sample(300:850, 100, replace = TRUE),
    Saldo_Medio = sample(0:100, 100, replace = TRUE)
)

# Visualizar as primeiras linhas do conjuto de dados

print(head(clientes))

# Pré-processamento de dados

# Padronizar os dados (z-score)

clientes_scaled <- scale(clientes)

#vizualizar as primeiras linhas dos dados padronizados

print(head(clientes))

# Determinar o número ótimo de clientes

# Calcular o total de within-cluster sum of squeres wss para diferentes valores de k

wss <- function(k) {


    kmeans(clientes_scaled, k, nstart = 10)$tot.withinss

}

# Definir uma faixa de valores de k

k_values <- 1:10

# Calcular o wss para cada k

wss_values <- map_dbl(k_values, wss)

# Plotar o gráfico do Elbow method

plot(k_values, wss_values,
type = "b" , pch = 19, frame = FALSE,
xlab = "Número de Clusters k",
ylab = "Total de Wss")


# Aplicar K-means


# Aplicar o K-means com 3 clusters


set.seed(123) # Manter a reprodutibilidade, se quisesse aleatório não precisava

kmeans_result <- kmeans(clientes_scaled, centers= 3, nstart = 25)

# Adicionar os rótulos dos clusters ao conjunto de dados original

clientes$Cluster <- as.factor(kmeans_result$cluster)

#Visualizar as primeiras linhas com os clusters atribuídos

head(clientes)



# Visualização dos dados dos clusters 

fviz_cluster(kmeans_result, data = clientes_scaled,
geom = "point", ellipse.type = "convex", ggtheme = theme_minimal()) + 
labs(title = "Segmentação de Clientes usando K-means")