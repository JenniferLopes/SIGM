# Script_experimentação
# Delineamento em Alpha-lattice
# Autor: Jennifer Lopes

# ------------------------------------------------------
# Instalação e carregamento dos pacotes
# ------------------------------------------------------

# Se ainda não tiver o pacman() instalado, instale com:
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# Carrega os pacotes necessários
pacman::p_load(lmerTest, lme4, broom, plotly, tidyverse, desplot, broom.mixed, emmeans, multcomp, 
               multcompView, ggpubr, writexl)

# ------------------------------------------------------
# Importando os dados
# ------------------------------------------------------

alpha_lattice <- read_excel("Scripts/Experimentação/dados/alpha_lattice.xlsx")
dados <- alpha_lattice

# Sobre os dados
# - `rep`: representa a repetição do experimento.
# - `inc.bloco`: representa os blocos incompletos dentro de cada repetição.
# - `gen`: representa os genótipos.
# - `prod`: representa a variável de resposta.
# - `row`: localização espacial do `GEN` na linha.
# - `col`: localização espacial do `GEN` na coluna.

# Visualizando estrutura dos dados
dplyr::glimpse(dados)

# ------------------------------------------------------
# Transformando efeitos em fatores
# ------------------------------------------------------

# Separação por níveis e a variável será considerada categórica;
# Cada nível do fator pode ter seu próprio efeito estimado.

dados$gen <- as.factor(dados$gen)
dados$rep <- as.factor(dados$rep)
dados$inc.bloco <- as.factor(dados$inc.bloco)

# ------------------------------------------------------
# Croqui de campo com ggplot2
# ------------------------------------------------------

croqui <- dados %>%
  ggplot(aes(x = col, y = row, fill = inc.bloco)) +
  geom_tile(color = "black") +
  geom_text(aes(label = gen)) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~ rep, scales = "free_x") +
  labs(title = "Croqui de campo",
       x = "Colunas", y = "Linhas",
       caption = "SIGM, 2024.")
croqui

# ------------------------------------------------------
# Croqui de campo com desplot
# ------------------------------------------------------

# Pacote desplot- Acesse documentação em pdf em: https://cloud.r-project.org/web/packages/desplot/desplot.pdf

desplot::desplot(data = dados,
                 form = gen ~ col + row | rep,          
                 text = gen, cex = 0.7, shorten = "no", 
                 out1 = rep, out2 = inc.bloco,                      
                 main = "Croqui de campo - SIGM 2024.", show.key = F)

# ------------------------------------------------------
# Análise descritiva
# ------------------------------------------------------

# Calcula todas as estatísticas principais de uma só vez (CV, máximo, média, mediana, etc.)
dados %>% metan::desc_stat(prod, hist = TRUE, stats = "main")

# ------------------------------------------------------
# Análise da repetição e dos blocos incompletos
# ------------------------------------------------------

# Análise da repetição
dados %>% group_by(rep) %>%
  summarize(mean = mean(prod), std.dev = sd(prod), cv = std.dev/mean)

# Análise dos blocos incompletos
dados %>% group_by(rep, inc.bloco) %>%
  summarize(mean = mean(prod), std.dev = sd(prod), cv = std.dev/mean)

# ------------------------------------------------------
# Gráficos de distribuição por genótipo e repetição
# ------------------------------------------------------

# Gráfico de distribuição da produtividade por genótipo
dados %>% ggplot(aes(x = gen, y = prod, fill = gen)) +
  geom_boxplot(show.legend = F) +
  theme_classic() +
  labs(title = "Distribuição da produtividade por genótipo", x= "Genótipo", y= "Produção", caption = "SIGM 2024.")

# Gráfico de distribuição da produtividade por repetição
dados %>% ggplot(aes(x = rep,  y = prod, fill = rep)) +
  geom_boxplot(outliers = F) +
  theme_classic() +
  labs(title = "Distribuição da produtividade por repetição", x= "Repetição", y= "Produção", caption = "SIGM 2024.")

# ------------------------------------------------------
# Modelagem - Modelos com efeitos fixos e aleatórios
# ------------------------------------------------------

# Modelo com gen como efeito fixo
mod.fg <- lmer(prod ~ gen + rep + (1 | rep:inc.bloco), data = dados)

# Modelo com gen como efeito aleatório
mod.rg <- lmer(prod ~ (1 | gen) + rep + (1 | rep:inc.bloco), data = dados)

# ------------------------------------------------------
# Análise dos modelos e comparação
# ------------------------------------------------------

# Resumo do modelo fixo
summary(mod.fg, ddf = "Kenward-Roger")

# Resumo do modelo aleatório
summary(mod.rg, ddf = "Kenward-Roger")

# ANOVA para efeitos fixos
mod.fg %>% anova(ddf = "Kenward-Roger")

# ANOVA para efeitos aleatórios
mod.rg %>% anova(ddf = "Kenward-Roger")

# RANOVA para efeitos aleatórios
mod.fg %>% ranova()
mod.rg %>% ranova()

# ------------------------------------------------------
# Estimativas BLUEs e BLUPs
# ------------------------------------------------------

# BLUEs (Best Linear Unbiased Estimators)
BLUEs <- emmeans::emmeans(mod.fg, ~ gen) %>%
  as.data.frame() %>%
  transmute(gen, BLUE = emmean, std.error_BLUE = SE)

# BLUPs (Best Linear Unbiased Predictors) para o modelo com gen como aleatório
mu_manual <- fixef(mod.rg)[1] + sum(fixef(mod.rg)[2:3])/3
BLUPs <- augment(ranef(mod.rg)) %>%
  filter(grp == "gen") %>%
  transmute(gen = level, BLUP = mu_manual + estimate, std.error_BLUP = std.error)

# ------------------------------------------------------
# Herdabilidade
# ------------------------------------------------------

# Herdabilidade clássica: h^2 = V_g / (V_g + V_e / n)
vcomps <- as.data.frame(VarCorr(mod.rg))
vc.g <- vcomps[vcomps$grp == "gen", "vcov"]
vc.e <- vcomps[vcomps$grp == "Residual", "vcov"]
nreps <- 3
hc <- vc.g / (vc.g + vc.e / nreps)
head(hc)

# ------------------------------------------------------
# Análise de grupamento
# ------------------------------------------------------

# Cálculo da distância euclidiana e agrupamento hierárquico
dist_blups <- dist(BLUPs$BLUP, method = "euclidean")
hc <- hclust(dist_blups, method = "average")

# Determinação do ponto de corte usando valor de Mojena
c_value <- 1.25
cut_height <- mean(hc$height) + c_value * sd(hc$height)
plot(hc, main = "Dendrograma", xlab = "Genótipos", ylab = "Distância", cex = 0.8, sub = "")
abline(h = cut_height, col = "red", lty = 2, lwd = 1.5)

# ------------------------------------------------------
# Regressão entre BLUE e BLUP
# ------------------------------------------------------

pvals <- merge(BLUEs, BLUPs, by = "gen")

ggplot(pvals, aes(x = BLUE, y = BLUP)) +
  geom_smooth(
    se = F,
    color = "red",
    size = 0.8,
    method = "lm") + 
  
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "black",
    size = 0.8,
    linetype = 2) +
  
  geom_point(size = 3, alpha = 0.5) +
  stat_regline_equation() +
  theme_bw() +
  coord_fixed()
    
    ggreg
              