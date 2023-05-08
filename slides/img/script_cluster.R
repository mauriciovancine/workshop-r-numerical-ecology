library(tidyverse)
library(ggdendro)

da <- readr::read_csv("dados_livro.csv")
da

da_dist <- da[, -1]
rownames(da_dist) <- da[, 1]
da_dist

dist <- vegan::vegdist(da_dist, method = "jaccard")
dist

cluster_max <- hclust(d = dist, method = "complete")
cluster_min <- hclust(d = dist, method = "single")
cluster_mea <- hclust(d = dist, method = "average")

plot(cluster_max, hang = -1)
plot(cluster_min, hang = -1)
plot(cluster_mea, hang = -1)

cluster_max_data <- dendro_data(as.dendrogram(cluster_max), type = "rectangle")
cluster_min_data <- dendro_data(as.dendrogram(cluster_min), type = "rectangle")
cluster_mea_data <- dendro_data(as.dendrogram(cluster_mea), type = "rectangle")

ggplot(cluster_max_data$segments) + 
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), color = "#157353", size = 1.5)+
    geom_text(data = dend_data$labels, color = "#157353",
              aes(x, y, label = label), vjust = 1.2, size = 8) +
    ylim(0, 1) +
    labs(title = "Complete", x = "", y = "") +
    theme_classic(base_size = 20) +
    theme(title = element_text(size = 30),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
ggsave("slides/img/cluster_19.png", width = 30, height = 20, units = "cm", dpi = 300)

ggplot(cluster_min_data$segments) + 
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), color = "#157353", size = 1.5)+
    geom_text(data = dend_data$labels, color = "#157353",
              aes(x, y, label = label), vjust = 1.2, size = 8) +
    ylim(0, 1) +
    labs(title = "Single", x = "", y = "") +
    theme_classic(base_size = 20) +
    theme(title = element_text(size = 30),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
ggsave("slides/img/cluster_20.png", width = 30, height = 20, units = "cm", dpi = 300)

ggplot(cluster_mea_data$segments) + 
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), color = "#157353", size = 1.5)+
    geom_text(data = dend_data$labels, color = "#157353",
              aes(x, y, label = label), vjust = 1.2, size = 8) +
    ylim(0, 1) +
    labs(title = "Average", x = "", y = "") +
    theme_classic(base_size = 20) +
    theme(title = element_text(size = 30),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
ggsave("slides/img/cluster_21.png", width = 30, height = 20, units = "cm", dpi = 300)

ggplot(cluster_max_data$segments) + 
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), color = "#157353", size = 1.5)+
    geom_text(data = dend_data$labels, color = "#157353",
              aes(x, y, label = label), vjust = 1.2, size = 8) +
    geom_hline(yintercept = .88, linetype = 2) +
    annotate("text", x = 6.9, y = .91, label = "2 grupos", size = 6) +
    geom_hline(yintercept = .6, linetype = 2) +
    annotate("text", x = 6.9, y = .63, label = "3 grupos", size = 6) +
    geom_hline(yintercept = .38, linetype = 2) +
    annotate("text", x = 6.9, y = .41, label = "4 grupos", size = 6) +
    ylim(0, 1) +
    labs(title = "", x = "", y = "") +
    theme_classic(base_size = 20) +
    theme(title = element_text(size = 30),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
ggsave("slides/img/cluster_22.png", width = 30, height = 20, units = "cm", dpi = 300)
