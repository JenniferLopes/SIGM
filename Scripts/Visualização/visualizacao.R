########################################################################
###########                Mini- Curso - SIGM                ###########
###########               Jennifer Luz Lopes                 ###########
###########                  11/11/2024                      ###########
########################################################################

# Visualização de dados ---------------------------------------------------

# Instale o pacman (caso não esteja instalado)
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# Carregue e instale os pacotes necessários com pacman::p_load
pacman::p_load(
  tidyverse,  # Manipulação e visualização de dados
  DT,         # Tabelas dinâmicas
  readxl,     # Importação de arquivos Excel
  patchwork,  # Layouts de gráficos combinados
  viridis,    # Paleta de cores para gráficos
  plotly,     # Gráficos interativos
  ggridges,   # Visualizações de densidade em cristas
  janitor)    # Limpeza de dados
library(tibble)
library(dplyr)
library(ggplot2)
library(forcats)
library(glue)
library(ggtext)

# Argumentos das funções --------------------------------------------------------------

# Aspectos estéticos()

# color= altera a cor de formas que não tem área (pontos e retas)
# fill=  altera a cor de formas com área (barras, caixas, densidades, áreas)
# size=  altera o tamanho de formas
# shape=  altera o tipo da forma, geralmente usada para pontos (use de 0 a 20)
# alpha= altera a transparência da forma

# Aspectos de geometria (geom)

# geom_point(): gráficos de dispersão
# geom_line(): gráficos de linhas
# geom_bar(): gráficos de barras
# geom_boxplot(): gráficos de caixa
# geom_histogram(): histogramas
# geom_col(): gráficos de coluna
# geom_smooth(): linha de tendência

# Facetas

# facet_wrap( ~ nome da variável): Para mostrar um painel diferente para cada nível de uma variável única
# facet_grid(): Utilizado quando se deseja trazer uma segunda variável para a disposição das facetas

# Importação dos data= dados ----------------------------------------------------

dados <- read_excel("Scripts/Visualização/visualizacao.xlsx")

# geom_point() -------------------------------------------------------

# 1- Primeiro passo: argumento (data)

ggplot(data= dados)

# 2- Passo: argumento (aes)

ggplot(data= dados, 
       aes(x = dap, y = h))

# 3-Passo: argumento (geom)

ggplot(data= dados, 
       aes(x = dap, y = h)) +
  geom_point()

# shape = 2 

ggplot(data= dados,  
       aes(x = dap, y = h)) +
  geom_point(shape = 2)

# size = 3, color e alpha

(g1 <- ggplot(data= dados,  
                  aes(x = dap, y = h)) +
    geom_point(color = "black", size = 3, alpha= 0.6))


# Linha vertical/horizontal

mean(dados$dap, na.rm = T)

g1 +
  geom_hline(yintercept = 14, color= "red")

# geom_jitter = gráfico de pontos -----------------------------------------

(g2 <- ggplot(data = dados,
       aes(x = dap,
           y = h,
           fill = clone)) +
  
  geom_jitter(size = 3,
              alpha = 0.5,
              shape = 21) +
  
  labs(
    title = "Relação DAP X ALTURA",
    subtitle = "De 7 Clones",
    caption = "SIGM, 2024.",
    x = "DAP",
    y = "Altura") +
  
  theme_light())

# geom_smooth() -------------------------------------------------------

# lm é uma função em R que significa "linear model" (modelo linear); 
# É usada para ajustar uma linha reta (ou modelo linear) aos dados;
# Encontrando a melhor linha que minimiza a soma dos quadrados das diferenças; 
# verticais entre os pontos de dados observados e os valores previstos pela linha.

# geom_smooth(): linha de tendência no gráfico
# method: "lm" podemos utilizar outros (glm, LOESS)
# se = F: não mostra a área cinza-escura ao redor da linha suave (Intervalo de confiança)

(g3 <- ggplot(data= dados,
              aes(x = dap, y = h)) +
   geom_point(aes(color = clone)) +
   geom_smooth(method = "lm") +      # se = F
   labs(x= "DAP (m)", y= "Altura (m)"))

# Histograma  --------------------------------------------------------------

# Argumentos: binwidth | fill | color | alpha | labs | tag | cap ------------

(g4 <- ggplot(data = dados, aes(x = dap)) +
    geom_histogram(
      binwidth = 2,
      fill = "#073b4c",
      color = "#e9ecef",
      alpha = 0.8) +
    xlim(4, 24) +
    labs(
      title = "Distribuição do DAP de Clones",
      x = "DAP (m)",
      y = " ",
      tag = "Plot1",
      caption = "SIGM, 2024.") +
    
    theme_minimal())

# Facet_wrap

(g44 <- ggplot(data= dados, 
       mapping = aes(x = dap,
                     fill= clone)) +
  geom_histogram(position = "dodge") +
  labs(title = "Relação DAP X Altura de clones ",
       x= "Idade",
       y= "Altura", 
       tag = "A") +
  theme_bw() +
  facet_wrap(~ clone))

# Gráfico de densidade ----------------------------------------------------

# geom_density_ridges_gradient()
# Estima a densidades dos dados e então as desenha usando ridgelines.

(g5 <- ggplot(dados, aes(x = dap,
                  y = clone,
                  fill = stat(x))) +
  ggridges::geom_density_ridges_gradient(scale = 3,               # achatamento da curva
                                         rel_min_height = 0.01) + # suavidade da curva
  scale_fill_viridis_c(name = "DAP",                              # escala contínua
                       option = "D") +                            # teste cores (A,B,C,D,E,F,G,H)
  labs(
    title = "Distrubuição de densidade do DAP para 7 clones",
    subtitle = "Função do pacote ggridges",
    x = "DAP",
    y = "Clones ",
    caption = "SIGM, 2024."))

# box_plot() --------------------------------------------------------------

(g6 <- dados %>%
    ggplot(aes(x=dap, y=h, fill=clone)) +
    geom_boxplot(outlier.color = NA, show.legend = F) +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    labs(title = "Relação DAP x Altura de Clones",
         x= "DAP (m)",
         y= "Altura (m)",
         caption = "SIGM, 2024.") +
    theme_minimal())


(g7 <- dados %>%
    ggplot( aes(x=clone,
                y=dap,
                fill=clone)) +
    geom_boxplot() +
    stat_summary(
      fun = mean,
      geom = "point",
      color = "black",
      size = 3))

(g8 <-dados %>%
    ggplot(aes(x=dap,
               y=h,
               fill=clone)) +
    geom_boxplot() +
    
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    
    labs(title = "Relação DAP x Altura de Clones",
         x= "DAP (m)",
         y= "Altura (m)",
         caption = "SIGM, 2024.") +
    theme_minimal())


# Gráfico de barras -------------------------------------------------------

data <- tibble(
  nome = c("VAR1", "VAR2", "VAR4", "VAR5", "VAR6", "VAR7", "VAR8", "VAR9"),
  cod = c("cod 1", "cod 2", "cod 3", "cod 4", "cod 5", "cod 6", "cod 7", "cod 8"),
  valor = c(-0.5, 1, 2, 3, 4, 5, 6, 7),
  color = c("#009E73", "#A3B18A", "#3A5A40", "#588157", "#344E41", "#588157", "#656d4a", "#414833"))

data %>%
  mutate(
    nome = glue("<i style='color:{color}'>{nome}</i> ({cod})"),
    nome = fct_reorder(nome, valor)) %>%
  
  ggplot(aes(x = valor, y = nome, fill = color)) +
  geom_col(alpha = 0.5) +
  scale_fill_identity() +
  labs(x= "Valor", y="Nome da variável",
    caption = "Mini-Curso de R. SIGM, 2024. Pacote: **ggtext.com**<br>") +
  theme_minimal() +  
  theme(
    axis.text.y = element_markdown(),
    plot.caption = element_markdown(lineheight = 3),
    panel.grid.major.y = element_blank())
