#' ----
#' título: ecologia quantitativa
#' autor: mauricio vancine
#' data: 2023-05-09
#' ----

# preparar r -------------------------------------------------------------

# pacotes
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(readxl)) install.packages("readxl")
if(!require(harrypotter)) install.packages("harrypotter")# https://github.com/aljrico/harrypotter
if(!require(wesanderson)) install.packages("wesanderson")# https://github.com/karthik/wesanderson
if(!require(ggsci)) install.packages("ggsci")# https://github.com/nanxstats/ggsci
if(!require(vegan)) install.packages("vegan")
if(!require(ggdendro)) install.packages("ggdendro")
if(!require(GGally)) install.packages("GGally")
if(!require(factoextra)) install.packages("factoextra")
if(!require(dendextend)) install.packages("dendextend")

# 1 linguagem r -----------------------------------------------------------

# console
# onde os codigos sao executados

# script
# onde os codigos sao escritos e salvos

# comentarios
# o r nao le o codigo depois do # (hash)
42 # essas palavras nao sao executadas, apenas o 42

# operadores
# sem especificar - segue a ordem das operações
1 * 2 + 2 / 2 ^ 2

# especificando - segue a ordem dos parênteses
((1 * 2) + (2 / 2)) ^ 2

# objetos
# atribuicao - simbolo (<-)
eco <- 10 
eco

# definir dois objetos
eco1 <- 10
eco2 <- 2

# operacoes com objetos
eco1 + eco2 # adicao
eco1 - eco2 # subtracao
eco1 * eco2 # multiplicacao
eco1 / eco2 # divisao

# operacoes com objetos e atribuicao
adi <- eco1 + eco2 # adicao
adi

sub <- eco1 - eco2 # subtracao
sub

mul <- eco1 * eco2 # multiplicacao
mul

div <- eco1 / eco2 # divisao
div

# funcoes
# soma
sum(10, 2)

# soma de objetos
sum(eco1, eco2)

# soma de objetos atribuidos a objetos
eco_sum <- sum(eco1, eco2)
eco_sum

# argumentos
sum(1, 2, 3, NA)
sum(1, 2, 3, NA, na.rm = TRUE)

# funcoes - argumentos como valores
sum(10, 2)
prod(10, 2)

# funcoes - argumentos como objetos
sum(eco1, eco2)
prod(eco1, eco2)

# funcoes - argumentos como parametos
# repeticao - vezes
rep(x = 1:5, times = 5)

# repeticao - cada
rep(x = 1:5, each = 5)

# atribuicao dos resultados
rep_times <- rep(x = 1:5, times = 5)
rep_times

rep_each <- rep(x = 1:5, each = 5)
rep_each

# criar dois objetos
foo <- 2
bar <- 3

# somar e atribuir
su <- sum(foo, bar)
su

# raiz e atribuir
sq <- sqrt(su)
sq

# ajuda
# descreve as informacoes de uma funcao
help("mean") # arquivo .html
?mean

# pacotes
# numero de pacotes no cran
nrow(available.packages())

# instalar pacotes
# install.packages("vegan")

# verificar pacotes instalados
library()

# carregar pacotes
library(vegan)

# verificar pacotes carregados
search()


# 2 analise exploratoria de dados ----------------------------------------

# importar os dados
da <- readxl::read_excel("Eco_quant_dados_2023.xlsx")
da

# espiar os dados
tibble::glimpse(da)

View(da)

# mudar os modos dos dados
da <- da %>% 
    dplyr::mutate(peso = as.numeric(peso))
da

da <- da %>% 
    dplyr::mutate(across(peso:alt_bico, as.numeric))
da

# medidas resumo
# moda
table(da$peso)
table(da$especie)
table(da$comp_corpo)

# mediana
median(da$peso)

# media aritmetica
mean(da$peso)

# variancia
var(da$peso)

# desvio padrao
sd(da$peso)

# erro padrao
sd(da$peso)/sqrt(length(da$peso))

# tabelas
da$peso

sort(da$peso)

classes_peso <- cut(da$peso, breaks = seq(40, 70, 5))
classes_peso

classes_peso_tabela <- tibble::as_tibble(table(classes_peso))
classes_peso_tabela    

classes_peso_tabela <- classes_peso_tabela %>% 
    dplyr::rename(fi = n) %>% 
    dplyr::mutate(fri = fi/sum(fi) * 100) %>% 
    dplyr::mutate(Fi = cumsum(fi)) %>% 
    dplyr::mutate(Fri = cumsum(fri))
classes_peso_tabela

# graficos

## histograma
ggplot(data = da, aes(x = peso)) +
    geom_histogram() +
    labs(x = "Peso (g)", y = "Frequência") +
    theme_bw(base_size = 20)

ggplot(data = da, aes(x = peso)) +
    geom_histogram(color = "white", fill = "steelblue") +
    labs(x = "Peso (g)", y = "Frequência") +
    theme_bw(base_size = 20)

ggplot(data = da, aes(x = peso)) +
    geom_histogram(color = "white", fill = "steelblue", bins = 60) +
    labs(x = "Peso (g)", y = "Frequência") +
    theme_bw(base_size = 20)

ggplot(data = da, aes(x = peso)) +
    geom_density(fill = "steelblue") +
    labs(x = "Peso (g)", y = "Densidade") +
    theme_bw(base_size = 20)

ggplot(data = da, aes(x = peso, fill = especie)) +
    geom_density() +
    scale_fill_manual(values = wesanderson::wes_palette(name = "Zissou1", type = "continuous", n = 6)) +
    labs(x = "Peso (g)", y = "Frequência") +
    theme_bw(base_size = 20)

ggplot(data = da, aes(x = peso, fill = especie)) +
    geom_density(color = "NA", alpha = .7) +
    scale_fill_manual(values = wesanderson::wes_palette(name = "Zissou1", type = "continuous", n = 6)) +
    labs(x = "Peso (g)", y = "Densidade", fill = "Espécie") +
    theme_bw(base_size = 20) +
    theme(legend.position = c(.9, .8),
          legend.text = element_text(face = "italic"))

## boxplot
ggplot(data = da, aes(x = especie, y = peso, fill = especie)) +
    geom_boxplot() +
    labs(x = "Espécies", y = "Peso (g)") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none")

ggplot(data = da, aes(x = especie, y = peso, fill = especie)) +
    geom_boxplot() +
    scale_fill_manual(values = c("red", "blue", "darkgreen", "darkorange", "purple", "steelblue")) +
    labs(x = "Espécies", y = "Peso (g)") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none")

ggplot(data = da, aes(x = especie, y = peso, fill = especie)) +
    geom_boxplot() +
    scale_fill_hp_d(option = "Gryffindor") + # "Slytherin" "Ravenclaw" "Hufflepuff"
    labs(x = "Espécies", y = "Peso (g)") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none")

ggplot(data = da, aes(x = especie, y = peso, fill = especie)) +
    geom_boxplot() +
    scale_fill_manual(values = wesanderson::wes_palette(name = "Zissou1", type = "continuous", n = 6)) +
    labs(x = "Espécies", y = "Peso (g)") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none")

ggplot(data = da, aes(x = especie, y = peso, fill = especie)) +
    geom_boxplot() +
    scale_fill_simpsons() +
    labs(x = "Espécies", y = "Peso (g)") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none")

## finalizacao
ggplot(data = da, aes(x = especie, y = peso, fill = especie)) +
    geom_boxplot() +
    scale_fill_manual(values = wesanderson::wes_palette(name = "Zissou1", type = "continuous", n = 6)) +
    labs(x = "Espécies", y = "Peso (g)") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none",
          axis.text.x = element_text(face = "italic"))

## todas as colunas
GGally::ggpairs(data = da[, -1], 
                mapping = aes(color = especie), 
                columns = c("peso", "comp_corpo", "comp_asa", "com_cauda",
                "comp_tarso", "comp_bico", "larg_bico", "alt_bico"),
                upper = "blank") +
    scale_color_manual(values = pal_nejm("default")(6)) +
    theme_bw()

# 3 analise de dados multivariados ------------------------------------------

# dados em matrizes
da_matriz <- read.csv("dados_livro.csv")
da_matriz

# dividir
da_matriz_locais <- da_matriz %>% 
    dplyr::select(id)
da_matriz_locais

da_matriz_especies <- da_matriz %>% 
    dplyr::select(a:e)    
da_matriz_especies

rownames(da_matriz_especies) <- da_matriz_locais$id
da_matriz_especies

da_matriz_ambientais <- da_matriz %>% 
    dplyr::select(depth_x:temperature_z)    
da_matriz_ambientais

rownames(da_matriz_ambientais) <- da_matriz_locais$id
da_matriz_ambientais

# padronizar
mean(da_matriz_ambientais$depth_x)
sd(da_matriz_ambientais$depth_x)

(da_matriz_ambientais$depth_x[1] - mean(da_matriz_ambientais$depth_x))/sd(da_matriz_ambientais$depth_x)

da_matriz_ambientais_st <- vegan::decostand(da_matriz_ambientais, method = "standardize")
head(da_matriz_ambientais_st, 12)

# matriz de distancia
dist_ambientais <- vegan::vegdist(da_matriz_ambientais_st, method = "euclidean")
dist_ambientais

# matriz de dissimilaridade de bray-curtis
dist_especies <- vegan::vegdist(da_matriz_especies, method = "bray")
dist_especies

dist_especies_100 <- round(dist_especies * 100, 1)
dist_especies_100

# matriz de dissimilaridade de jaccard
da_matriz_incidencia <- read.csv("dados_livro_incidencia.csv")
da_matriz_incidencia

da_matriz_incidencia_locais <- da_matriz_incidencia[, 1]
da_matriz_incidencia_locais

da_matriz_incidencia_especies <- da_matriz_incidencia[, -1]
da_matriz_incidencia_especies

rownames(da_matriz_incidencia_especies) <- da_matriz_incidencia_locais
da_matriz_incidencia_especies

dist_especies_incidencia_total <- vegan::vegdist(da_matriz_incidencia_especies, method = "jaccard", diag = TRUE, upper = TRUE) %>% 
    round(3)
dist_especies_incidencia_total

dist_especies_incidencia <- vegan::vegdist(da_matriz_incidencia_especies, method = "jaccard")
dist_especies_incidencia

# agrupamentos
cluster_especies_incidencia_complete <- stats::hclust(dist_especies_incidencia, method = "complete")
cluster_especies_incidencia_complete

# avaliacao do agrupamento
cluster_especies_incidencia_complete_coph <- stats::cophenetic(cluster_especies_incidencia_complete)
cluster_especies_incidencia_complete_coph

cor(cluster_especies_incidencia_complete_coph, dist_especies_incidencia) # correlacao deve ser maior que 0.7

# plot
plot(cluster_especies_incidencia_complete)
plot(cluster_especies_incidencia_complete, hang = -1)

cluster_especies_incidencia_complete_data <- ggdendro::dendro_data(as.dendrogram(cluster_especies_incidencia_complete), type = "rectangle")
cluster_especies_incidencia_complete_data

ggplot(cluster_especies_incidencia_complete_data$segments) + 
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), color = "#157353", linewidth = 1.5)+
    geom_text(data = cluster_especies_incidencia_complete_data$labels, color = "#157353",
              aes(x, y, label = label), vjust = 1.2, size = 5) +
    ylim(0, 1) +
    labs(title = "Complete", x = "", y = "") +
    theme_classic() +
    theme(title = element_text(size = 20),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())

# grupos
fviz_dend(cluster_especies_incidencia_complete)

fviz_dend(cluster_especies_incidencia_complete, 
          k = 2,                 # Cut in four groups
          cex = 2,                 # label size
          k_colors = c("#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,  # color labels by groups
          ggtheme = theme_void()     # Change theme
)

fviz_dend(cluster_especies_incidencia_complete, 
          k = 3,                 # Cut in four groups
          cex = 2,                 # label size
          k_colors = c("#2E9FDF", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,  # color labels by groups
          ggtheme = theme_void()     # Change theme
)

fviz_dend(cluster_especies_incidencia_complete, 
          k = 4,                 # Cut in four groups
          cex = 2,                 # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,  # color labels by groups
          ggtheme = theme_void()     # Change theme
)

# analises dos dados ----------------------------------------------------------

# importar dados
da <- readxl::read_excel("Eco_quant_dados_2023.xlsx") %>% 
    dplyr::mutate(across(peso:alt_bico, as.numeric)) %>% 
    dplyr::mutate(id = 1:30) %>% 
    dplyr::mutate(especie = c(rep("Ct", 5), rep("Ev", 5), rep("Ps", 5), 
                              rep("Ts", 5), rep("Tl", 5), rep("Tm", 5))) %>% 
    dplyr::mutate(id = paste0(id, especie)) %>% 
    as.data.frame()
da

# preparar dados
da_medidas <- da %>% 
    dplyr::select(peso:alt_bico)
da_medidas

da_especimes <- da %>% 
    dplyr::select(id) %>% 
    dplyr::pull()
da_especimes

rownames(da_medidas) <- da_especimes
da_medidas

# matriz de distancia
da_dist <- vegan::vegdist(da_medidas, method = "euc")
da_dist

# agrupamento
da_cluster_comp <- hclust(d = da_dist, method = "complete")
da_cluster_comp

da_cluster_sing <- hclust(d = da_dist, method = "single")
da_cluster_sing

da_cluster_aveg <- hclust(d = da_dist, method = "average")
da_cluster_aveg

# correlacao cofenetica
cor(da_dist, stats::cophenetic(da_cluster_comp))
cor(da_dist, stats::cophenetic(da_cluster_sing))
cor(da_dist, stats::cophenetic(da_cluster_aveg))

# graficos
fviz_dend(da_cluster_comp) 
fviz_dend(da_cluster_sing) 
fviz_dend(da_cluster_aveg) 

# graficos com grupos
fviz_dend(da_cluster_aveg, k = 2, cex = 2,
          k_colors = pal_nejm("default")(2), 
          color_labels_by_k = TRUE, ggtheme = theme_void())

fviz_dend(da_cluster_aveg, k = 3, cex = 2,
          k_colors = pal_nejm("default")(3), 
          color_labels_by_k = TRUE, ggtheme = theme_void())

fviz_dend(da_cluster_aveg, k = 4, cex = 2,
          k_colors = pal_nejm("default")(4), 
          color_labels_by_k = TRUE, ggtheme = theme_void())

fviz_dend(da_cluster_aveg, k = 5, cex = 2,
          k_colors = pal_nejm("default")(5), 
          color_labels_by_k = TRUE, ggtheme = theme_void())

fviz_dend(da_cluster_aveg, k = 6, cex = 2,
          k_colors = pal_nejm("default")(6), 
          color_labels_by_k = TRUE, ggtheme = theme_void())

fviz_dend(da_cluster_aveg, k = 7, cex = 2,
          k_colors = pal_nejm("default")(7), 
          color_labels_by_k = TRUE, ggtheme = theme_void())

fviz_dend(da_cluster_aveg, k = 8, cex = 2,
          k_colors = pal_nejm("default")(8), 
          color_labels_by_k = TRUE, ggtheme = theme_void())

# end ---------------------------------------------------------------------