########################################################################
###########                Mini- Curso - SIGM                ###########
###########               Jennifer Luz Lopes                 ###########
###########                  11/11/2024                      ###########
########################################################################

# Análise de PCA ----------------------------------------------------------

# Instale o pacman (caso não esteja instalado)

if (!require("pacman")) install.packages("pacman")
library(pacman)

# Carregue e instale os pacotes necessários com pacman::p_load

p_load(metan, dplyr, ggplot2, ggthemes, factoextra, DT)


# Importando os dados -----------------------------------------------------

# Completo - Com duas testemunhas

completo <- read_delim("Scripts/Multivariada/dados/completo.txt", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE)

# Incompleto- Sem as testemunhas

incompleto <- read_delim("Scripts/Multivariada/dados/incompleto.txt", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)

# Pré-processamento dos dados ---------------------------------------------

data_pca <- completo %>%
  metan::means_by(gen) %>%
  column_to_rownames("gen")

# Realizar a PCA com padronização das variáveis

res.pca <- prcomp(data_pca, scale. = TRUE)

# Visualizar autovalores e variância explicada

eig.val <- get_eigenvalue(res.pca)

datatable(eig.val)

# Resumo da PCA

summary(res.pca)

# Visualização da PCA (Biplot)

pca_plot <- fviz_pca_biplot(res.pca, repel = TRUE, ggtheme = theme_base(), title = "")
plot(pca_plot)

# Gráficos de autovalores e variância acumulada

p1 <- fviz_eig(res.pca)
p2 <- fviz_eig(
  res.pca,
  addlabels = TRUE,
  geom = "bar",
  barfill = "orange",
  barcolor = "black",
  xlab = "Componentes Principais",
  ylab = "Percentagem da variância explicada",
  main = "")


# Salvar o gráfico do Biplot
ggsave("pca_biplot.tiff", plot = pca_plot, width = 12, height = 8)
print(p1)
print(p2)

# Visualização da contribuição das variáveis

fviz_pca_var(res.pca, col.var = "black")

# Salvar gráficos adicionais

ggsave("biplot.tiff", plot = pca_plot, width = 12, height = 8)

#####################################################################################

# Análise de Correlação ---------------------------------------------------

# Exemplo 1 ---------------------------------------------------------------

corr_1 <- incompleto %>% 
  select(-gen) %>% 
  corr_coef()

plot(corr_1, legend.title = "Correlação de Pearson")

# Exemplo 2 ---------------------------------------------------------------

corr_2 <- incompleto %>% 
  select(-gen) %>% 
  corr_plot(
    shape.point = 19,
    size.point = 2,
    alpha.point = 0.5,
    alpha.diag = 0,
    pan.spacing = 0,
    col.sign = "gray",
    alpha.sign = 0.3,
    axis.labels = FALSE,
    progress = FALSE)

corr_2

# Salvar gráfico

ggplot2::ggsave("correlation.tiff", width = 12, height = 8)

