# Script_experimentação
# Delineamento em Alpha-lattice
# Autor: Jennifer Lopes

# Efeitos Fixos: Representam fatores de interesse específico que se quer avaliar diretamente.
# - Genótipos como efeitos fixos permitem avaliar o impacto de cada um na variável de resposta (ex.: rendimento).
# - As estimativas associadas a efeitos fixos são chamadas de BLUEs (Best Linear Unbiased Estimates).

# Efeitos Aleatórios: Representam fatores vistos como amostra de uma população maior, focando na variabilidade entre os níveis.
# - Genótipos como efeitos aleatórios produzem BLUPs (Best Linear Unbiased Predictions).
# - Esse modelo ajuda a entender a variabilidade entre genótipos e prever a resposta esperada de um genótipo.

# O que queremos responder com esse experimento:
# 1. Qual é o efeito dos genótipos sobre o rendimento?
# 2. Quais genótipos têm melhores performances ajustadas (BLUEs) ou predições (BLUPs)?
# 3. Qual é a herdabilidade associada aos genótipos?
# 4. Análise de agrupamento hierárquico para similaridade genética.
# 5. Qual a correlação entre estimativas BLUE e BLUP?

# Instalação e carregamento dos pacotes
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  broom,
  broom.mixed,
  desplot,
  emmeans,
  ggpubr,
  lme4,
  lmerTest,
  multcomp,
  multcompView,
  plotly,
  tidyverse,
  writexl)

# Importando os dados

dados <- read_excel("Scripts/Experimentação/dados/alpha_lattice.xlsx")

# Visão geral dos dados

dplyr::glimpse(dados)

# Estrutura dos dados:
# - `rep`: repetição do experimento
# - `inc.bloco`: blocos incompletos dentro de cada repetição
# - `gen`: genótipos
# - `prod`: variável de resposta
# - `row` e `col`: localização espacial do genótipo

# Convertendo efeitos em fatores
dados$gen <- as.factor(dados$gen)
dados$rep <- as.factor(dados$rep)
dados$inc.bloco <- as.factor(dados$inc.bloco)

# Croqui de campo com ggplot2
croqui <- dados %>%
  ggplot(aes(x = col, y = row, fill = inc.bloco)) +
  geom_tile(color = "black") +
  geom_text(aes(label = gen)) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid = element_blank()) +
  facet_wrap(~ rep, scales = "free_x") +
  labs(title = "Croqui de campo", x = "Colunas", y = "Linhas", caption = "SIGM, 2024.")
croqui

# Croqui de campo com desplot
desplot::desplot(
  data = dados, form = gen ~ col + row | rep,
  text = gen, cex = 0.7, shorten = "no", out1 = rep, out2 = inc.bloco,
  main = "Croqui de campo - SIGM 2024", show.key = TRUE)

# Análise descritiva
dados %>% metan::desc_stat(prod, hist = TRUE, stats = "main")

# Análise exploratória por repetição
dados %>%
  group_by(rep) %>%
  summarize(mean = mean(prod), std.dev = sd(prod), cv = std.dev/mean)

# Análise dos blocos incompletos
dados %>%
  group_by(rep, inc.bloco) %>%
  summarize(mean = mean(prod), std.dev = sd(prod), cv = std.dev/mean, median = median(prod))

# Distribuição de produtividade por genótipo
dados %>%
  ggplot(aes(x = gen, y = prod, fill = gen)) +
  geom_boxplot(show.legend = FALSE) +
  theme_classic() +
  labs(title = "Distribuição da produtividade por genótipo", x = "Genótipo", y = "Produção", caption = "SIGM 2024.")

# Modelagem
# Modelo com gen como efeito fixo
mod.fg <- lmer(prod ~ gen + rep + (1 | rep:inc.bloco), data = dados)

# Modelo com gen como efeito aleatório
mod.rg <- lmer(prod ~ (1 | gen) + rep + (1 | rep:inc.bloco), data = dados)

# Análise dos modelos e comparação usando REML
# REML maximiza a verossimilhança residual e permite comparações entre efeitos de variância.
summary(mod.fg, ddf = "Kenward-Roger")
summary(mod.rg, ddf = "Kenward-Roger")

# ANOVA para efeitos fixos e aleatórios
mod.fg %>% anova(ddf = "Kenward-Roger")
mod.rg %>% anova(ddf = "Kenward-Roger")

# Comparação RANOVA para componentes de variância
mod.fg %>% ranova()
mod.rg %>% ranova()

# Estimativa dos Componentes da Variância
# sdcor: desvio padrão de cada componente de variância.
as.data.frame(VarCorr(mod.fg))
as.data.frame(VarCorr(mod.rg))

# Estimativas BLUEs e BLUPs
# BLUE: estimativa para gen como fixo (media marginal)
BLUEs <- emmeans::emmeans(mod.fg, ~ gen) %>%
  as.data.frame() %>%
  transmute(gen, BLUE = emmean, std.error_BLUE = SE)

# BLUP: predição para gen como aleatório
mu_manual <- fixef(mod.rg)[1] + sum(fixef(mod.rg)[2:3])/3
BLUPs <- augment(ranef(mod.rg)) %>%
  filter(grp == "gen") %>%
  transmute(gen = level, BLUP = mu_manual + estimate, std.error_BLUP = std.error)

# Herdabilidade clássica
vcomps <- as.data.frame(VarCorr(mod.rg))
vc.g <- vcomps[vcomps$grp == "gen", "vcov"]
vc.e <- vcomps[vcomps$grp == "Residual", "vcov"]
nreps <- 3
hc <- vc.g / (vc.g + vc.e / nreps)

# Análise de grupamento dos BLUPs
blup_values <- BLUPs$BLUP
names(blup_values) <- BLUPs$gen
dist_blups <- dist(blup_values, method = "euclidean")
hc <- hclust(dist_blups, method = "average")
c_value <- 1.25
cut_height <- mean(hc$height) + c_value * sd(hc$height)
cluster_groups <- cutree(hc, h = cut_height)

# Adiciona informações de cluster ao data frame
BLUPs <- BLUPs %>% mutate(cluster = factor(cluster_groups[gen]))

# Regressão entre BLUE e BLUP
pvals <- merge(BLUEs, BLUPs, by = "gen")
ggreg <- pvals %>%
  ggplot(aes(x = BLUE, y = BLUP)) +
  geom_smooth(se = FALSE, color = "red", size = 0.8, method = "lm", formula = y ~ x) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = 2) +
  geom_point(size = 3, alpha = 0.5) +
  theme_bw() +
  coord_fixed()
ggreg
