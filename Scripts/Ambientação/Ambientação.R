########################################################################
###########                Mini- Curso - SIGM                ###########
###########               Jennifer Luz Lopes                 ###########
###########                  11/11/2024                      ###########
########################################################################

# Ambientação no RStudio --------------------------------------------------

# File> New File > R Script.

# Atalho Ctrl + Shift + N.

# Conhecendo os 4 ambientes do RStudio

# (editor, environment, console, output)

# Quais os principais menus?


# Funções iniciais --------------------------------------------------------

# Citando R
citation() 

# Pedindo ajuda
?hist 
help(hist)

# Ajuda sobre um pacote
help(package= "dplyr")

# Ajuda sobre uma estatística
help.search("tukey")

# Visualizar os pacotes do R Base
getOption("defaultPackages")

# Ver todos os pacotes disponíveis em sua biblioteca
library()

# Ver todos os pacotes carregados
(.packages())

# citando pacote
citation("tidyverse")

# Instalação dos pacotes --------------------------------------------------

install.packages("pacman")

# Carregando os pacotes ---------------------------------------------------

library(pacman)

# Outra opção para instalação e carregamento dos pacotes -----------------------

pacman::p_load(pacman,
               tidyverse,
               janitor,
               ggplot2,
               dplyr,
               readxl,
               readr,
               DT,
               writexl,
               openxlsx)

# Importando dados -------------------------------------------------------------

# xlsx

dados_xlsx <- readxl::read_excel("Scripts/Ambientação/dados/dados.xlsx", sheet = "dados_2")
dados_xlsx2 <- readxl::read_excel("Scripts/Ambientação/dados/dados.xlsx", sheet = "dados_1")

# Unindo arquivos xlsx

dados_unidos <- dplyr::left_join(dados_xlsx, dados_xlsx2, by = "TRAT")

# web

dados_csv <- readr::read_csv("https://raw.githubusercontent.com/JenniferLopes/index_selection/refs/heads/main/selections_by_index.csv")


###################################################################################################################

# Operações básicas -------------------------------------------------------

2+2
3-1
2*3 
4/2
2^2

sqrt(100)

4<7	  
2<=3 	
5>2	  
4>=3	
2==3	
6!=7	

#######################################################################################################


# Criando objetos ---------------------------------------------------------

planta <- 31 
conta <- 36*30    
x <- 25
X1 <- 32
meu_objeto <- 35
meu.objeto <- 25

# Criando um data frame ---------------------------------------------------
# paste 

genotipos <- paste0("GEN", 1:10)

altura_planta <- c(70, 75, 80, 85, 90, 98, 85, 80, 83, 100)

df <- data.frame(genotipos, altura_planta)

#######################################################################################################


# Baixando/salvando dados -------------------------------------------------

# Exportação individual

write_xlsx(dados_xlsx, "Scripts/Ambientação/output/dados_final.xlsx")

# lista_dfs: É uma lista onde cada elemento é um data frame, e o nome de cada elemento será o nome da aba no arquivo Excel.
# addWorksheet(): Adiciona uma nova aba no arquivo Excel com o nome fornecido.
# writeData(): Escreve o conteúdo de cada data frame na aba correspondente.
# saveWorkbook(): Salva o arquivo Excel no diretório especificado com o nome definido

# Exportação de multiplos arquivos - Função

exportar_multiplos_dfs <- function(lista_dados, nome_arquivo) {
  # Cria um novo arquivo Excel
  workbook <- createWorkbook()
  
  # Itera sobre a lista de data frames
  for (nome in names(lista_dados)) {
    # Adiciona uma nova aba (sheet) com o nome do data frame
    addWorksheet(workbook, sheetName = nome)
    
    # Escreve o data frame na aba correspondente
    writeData(workbook, sheet = nome, lista_dados[[nome]])
  }
  
  # Salva o arquivo Excel
  saveWorkbook(workbook, file = nome_arquivo, overwrite = TRUE)
}

# Criando uma lista com os data frames

lista_dados <- list(arquivo1 = dados_csv, arquivo2 = dados_xlsx)

# Exportando todos os data frames para um arquivo Excel com múltiplas abas

exportar_multiplos_dfs(lista_dados, "dados_completos.xlsx")
