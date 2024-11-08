# Instalação e carregamento dos pacotes ------------------------------------

# Instala o pacman se não estiver instalado

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# Carrega os pacotes necessários

pacman::p_load(
  lmerTest, lme4, broom, plotly, tidyverse, desplot, broom.mixed, 
  emmeans, multcomp, multcompView, ggpubr, writexl)

# Importando os dados ------------------------------------------------------

dados <- read_excel("Scripts/Experimentação/dados/alpha_lattice.xlsx")

# Exibindo informações sobre os dados

dplyr::glimpse(dados)

# Transformando efeitos em fatores -----------------------------------------

dados$gen <- as.factor(dados$gen)
dados$rep <- as.factor(dados$rep)
dados$inc.bloco <- as.factor(dados$inc.bloco)

# Croqui de campo com ggplot2 ----------------------------------------------

croqui <- dados %>% 
  ggplot(aes(x = col, y = row, fill = inc.bloco)) +
  geom_tile(color = "black") +
  geom_text(aes(label = gen)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  facet_wrap(~ rep, scales = "free_x") +
  labs(
    title = "Croqui de campo",
    x = "Colunas", y = "Linhas",
    caption = "SIGM, 2024.")

# Croqui de campo com desplot ----------------------------------------------

desplot(data = dados, flip = FALSE,
        form = gen ~ col + row | rep,
        text = gen, cex = 0.7, shorten = "no",
        out1 = rep, out2 = inc.bloco,
        main = "Croqui de campo - SIGM 2024.", show.key = TRUE)

# Análise descritiva -------------------------------------------------------

dados %>%
  desc_stat(prod, hist = TRUE, stats = "all")

# Análise da repetição -----------------------------------------------------

dados %>% 
  group_by(rep) %>% 
  summarize(
    mean = mean(prod),
    std.dev = sd(prod),
    cv = std.dev / mean)

# Análise dos blocos incompletos -------------------------------------------

dados %>% 
  group_by(rep, inc.bloco) %>% 
  summarize(
    mean = mean(prod),
    std.dev = sd(prod),
    cv = std.dev / mean)

# Cálculo da média por genótipo --------------------------------------------

plotdata <- dados %>% 
  group_by(gen) %>% 
  mutate(mean_prod = mean(prod)) %>%
  ungroup() %>%
  mutate(gen = fct_reorder(gen, mean_prod))

# Gráfico da produtividade por genótipo ------------------------------------

ggplot(data = plotdata, aes(x = gen)) +
  geom_point(aes(y = prod, shape = rep)) +  
  geom_point(aes(y = mean_prod), color = "cornflowerblue") + 
  ylim(0, NA) +
  labs(
    caption = "Pontos em azul representam a média por genótipo.",
    x = "Genótipo", y = "Produção") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Boxplots -----------------------------------------------------------------

dados %>% 
  ggplot(aes(x = gen, y = prod, fill = gen)) +
  geom_boxplot() +
  theme_classic() +
  labs(
    title = "Distribuição da produtividade por genótipo", 
    x = "Genótipo", y = "Produção",
    caption = "SIGM 2024.")

dados %>% 
  ggplot(aes(x = rep, y = prod, fill = rep)) +
  geom_boxplot() +
  theme_classic() +
  labs(
    title = "Distribuição da produtividade por repetição", 
    x = "Repetição", y = "Produção",
    caption = "SIGM 2024.")

# Modelagem ----------------------------------------------------------------

mod.fg <- lmer(prod ~ gen + rep + (1 | rep:inc.bloco), data = dados)

mod.rg <- lmer(prod ~ (1 | gen) + rep + (1 | rep:inc.bloco), data = dados)

# Análise dos modelos e comparação -----------------------------------------

summary(mod.fg, ddf = "Kenward-Roger")

summary(mod.rg, ddf = "Kenward-Roger")

anova(mod.fg, ddf = "Kenward-Roger")

anova(mod.rg, ddf = "Kenward-Roger")

ranova(mod.fg)

ranova(mod.rg)

# Estimativa dos Componentes da Variância ----------------------------------

# Exibe a variância dos componentes para mod.fg e mod.rg

as.data.frame(VarCorr(mod.fg))

as.data.frame(VarCorr(mod.rg))

# Estimativas BLUEs e BLUPs ------------------------------------------------

# BLUEs (Best Linear Unbiased Estimators) ----------------------------------

# Calcula as médias ajustadas dos genótipos para o modelo com efeito fixo

BLUEs <- emmeans::emmeans(mod.fg, ~ gen) %>%
  as.data.frame() %>%
  transmute(gen, BLUE = emmean, std.error_BLUE = SE)

# Exibe as primeiras linhas de BLUEs

head(BLUEs)

# Gráfico dos BLUEs com intervalo de confiança de 95%

ggplot2::ggplot(BLUEs, aes(x = gen, y = BLUE)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = BLUE - 1.96 * std.error_BLUE, ymax = BLUE + 1.96 * std.error_BLUE), 
                width = 0.2, color = "gray") +
  labs(title = "Estimativas BLUEs",
       x = "Genótipos", 
       y = "BLUE (Best Linear Unbiased Estimator)",
       caption = "SIGM,2024.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# BLUPs (Best Linear Unbiased Predictors) ----------------------------------

# Calcula as estimativas BLUPs dos genótipos para o modelo com efeito aleatório

mu_manual <- fixef(mod.rg)[1] + sum(fixef(mod.rg)[2:3])/3

BLUPs <- augment(ranef(mod.rg)) %>%
  filter(grp == "gen") %>%
  transmute(gen = level, BLUP = mu_manual + estimate, std.error_BLUP = std.error)

# Herdabilidade -------------------------------------------------------------

# Extração das Componentes da Variância para cálculo da herdabilidade

vcomps <- as.data.frame(VarCorr(mod.rg))
vc.g <- vcomps[vcomps$grp == "gen", "vcov"]
vc.e <- vcomps[vcomps$grp == "Residual", "vcov"]

# Cálculo da herdabilidade clássica

nreps <- 3
hc <- vc.g / (vc.g + vc.e / nreps)

# Exibe as primeiras linhas da herdabilidade

head(hc)

# Plotando os BLUPs --------------------------------------------------------

# Cálculo do intervalo de confiança (95%)

nivel_confianca <- 0.95
z_value <- qnorm((1 + nivel_confianca) / 2)

BLUPs <- BLUPs %>%
  mutate(
    IC_inferior = BLUP - z_value * std.error_BLUP,
    IC_superior = BLUP + z_value * std.error_BLUP)

# Média dos BLUPs

media_blup <- mean(BLUPs$BLUP)
head(media_blup)

# Ordena os BLUPs e define a ordem dos níveis de gen

BLUPs <- BLUPs %>%
  arrange(desc(BLUP)) %>%
  mutate(gen = factor(gen, levels = gen))

# Gráfico dos BLUPs com intervalo de confiança

ggplot(BLUPs, aes(x = gen, y = BLUP)) +
  geom_bar(stat = "identity", fill = "#588157", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = IC_inferior, ymax = IC_superior), width = 0.2) +
  geom_hline(yintercept = media_blup, color = "red", linetype = "dashed", size = 0.8) +
  theme_minimal() +
  labs(title = "",
       x = "Genótipos", y = "Valor Predito",
       caption = "SIGM, 2024.") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Clusterização hierárquica com distância euclidiana ------------------------

# Calcula a distância euclidiana entre os BLUPs

blup_values <- BLUPs$BLUP
names(blup_values) <- BLUPs$gen
dist_blups <- dist(blup_values, method = "euclidean")

# Clusterização hierárquica usando UPGMA (método da média)

hc <- hclust(dist_blups, method = "average")

# Valor de Mojena para o ponto de corte e plotagem do dendrograma

c_value <- 1.25
heights <- hc$height
cut_height <- mean(heights) + c_value * sd(heights)

plot(hc, main = "Dendrograma", 
     xlab = "Genótipos", ylab = "Distância", cex = 0.8, sub = "")
abline(h = cut_height, col = "red", lty = 2, lwd = 1.5)

# Definindo os clusters com o ponto de corte e adicionando ao data frame BLUPs

cluster_groups <- cutree(hc, h = cut_height)
BLUPs <- BLUPs %>% mutate(cluster = factor(cluster_groups[gen]))

# Exibe o data frame com os clusters

head(BLUPs)

# Calcula a correlação cofenética para verificar a qualidade da clusterização

cophenetic_dist <- cophenetic(hc)
correlation_cophenetic <- cor(dist_blups, cophenetic_dist)
print(paste("Correlação cofenética:", round(correlation_cophenetic, 4)))

# Regressão ----------------------------------------------------------------

# Estatísticas descritivas por genótipo

raw_m <- dados %>% 
  group_by(gen) %>% 
  summarize(mean = mean(prod),
            std.dev = sd(prod),
            cv = std.dev / mean, 
            n = n(),
            n_mis = sum(is.na(prod)))
raw_m

# Merge das estimativas BLUEs e BLUPs com os dados descritivos

pvals <- merge(BLUEs, BLUPs, by = "gen")
pvals <- merge(pvals, raw_m, by = "gen")

# Gráfico de dispersão entre BLUEs e BLUPs com linha de regressão

pvals %>% 
  ggplot(aes(x = BLUE, y = BLUP)) +
  geom_smooth(se = FALSE, color = "red", size = 0.8, method = "lm", formula = y ~ x) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, linetype = 2) +
  geom_point(size = 3, alpha = 0.5) +
  stat_regline_equation() +
  theme_bw() +
  coord_fixed()
