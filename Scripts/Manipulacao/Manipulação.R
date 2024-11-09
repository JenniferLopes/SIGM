########################################################################
###########                Mini- Curso - SIGM                ###########
###########               Jennifer Luz Lopes                 ###########
###########                  11/11/2024                      ###########
########################################################################

# Manipulação de dados ----------------------------------------------------

# Instalação e carregamento dos pacotes ----------------------------------

# install.packages("pacman")
# library(pacman)

pacman::p_load(tidyverse, readr, metan, DT)

# Importação dos dados ----------------------------------------------------

dados <- read_excel("Scripts/Manipulacao/dados/dados.xlsx")

glimpse(dados)

# Transformando colunas em numéricas

dados <- dados %>%
  mutate(across(-c(AMB, HIB, REP), ~ as.numeric(as.character(.))))

# Print Table -------------------------------------------------------------

# Função 1: Print personalizado de tabela ---------------------------------

print_table_jeni <- function(table, rownames = FALSE, digits = 3, 
                             filter_cols = FALSE, custom_digits = list(), ...) {
  
  options_list <- list(
    scrollX = TRUE,
    dom = '<<t>Bp>',
    buttons = c('copy', 'excel', 'pdf', 'print'))
  
  if (filter_cols) {
    options_list$searching <- TRUE
    options_list$searchCols <- lapply(seq_along(table), function(i) NULL)
  }
  
  df <- datatable(
    table,
    rownames = rownames,
    extensions = 'Buttons',
    options = options_list,...)
  
  # Formatando colunas numéricas
  num_cols <- which(sapply(table, is.numeric))
  if (length(num_cols) > 0) {
    df <- formatSignif(df, columns = num_cols, digits = digits)
  }
  
  # Formatação personalizada de colunas
  if (length(custom_digits) > 0) {
    for (col_name in names(custom_digits)) {
      if (col_name %in% names(table)) {
        df <- formatSignif(df, columns = col_name, digits = custom_digits[[col_name]])
      }
    }
  }
  
  df
}


# Manipulações- Pacote dplyr ----------------------------------------------

# Função select() ---------------------------------------------------------

dados %>%
  dplyr::select(AMB, HIB, APLA)

dados %>%
  dplyr::select(AMB, APLA:CESP, everything()) %>% 
  print_table_jeni()

dados %>%
  dplyr::select(- c(AMB, REP)) %>%
  print_table_jeni()

# Função mutate() ---------------------------------------------------------

dados %>%
  dplyr::mutate(NOVA_VAR = NGRA/100) %>%
  dplyr::select(AMB, HIB, REP, NOVA_VAR, everything()) %>%
  print_table_jeni()

dados_class <- dados %>%
  select(HIB, MMG, everything()) %>%
  mutate(MMG_class = case_when(
    MMG < 200 ~ "baixa",
    MMG <= 350 ~ "média",
    TRUE ~ "alto"
  )) %>%
  mutate(MMG_class = factor(MMG_class, levels = c("baixa", "média", "alto")))

dados_class %>% 
  print_table_jeni()

# Função filter() ---------------------------------------------------------

dados %>%
  dplyr::filter(MMG > 300) %>%
  dplyr::select(HIB, MMG, everything()) %>%
  print_table_jeni()

dados %>%
  dplyr::filter(APLA > 2.5 | AIES > 1.3 | CESP < 15) %>%
  dplyr::select(HIB, APLA, AIES, CESP) %>%
  print_table_jeni()


# Função summarise() ------------------------------------------------------

TESTE <- dados %>% 
   dplyr::filter(AMB == "3") %>% 
   dplyr::mutate(NOVA_var = MMG * 1000) %>% 
   dplyr::group_by(HIB) %>% 
   dplyr::summarize(MMG_media = mean(MMG))


##############################################################################################

# Função-Estrutura --------------------------------------------------------

nome_da_funcao <- function(argumentos) {
  
  # Corpo da função: conjunto de operações
  
  resultado <- alguma_operacao(argumentos)
  
  return(resultado)  # Retorno opcional
}


# Função 2: Agrupar variáveis --------------------------------------------

group_by_jeni <- function(data, ..., summary_funcs) {
  data %>%
    dplyr::group_by(across(c(...))) %>%
    dplyr::summarise(across(where(is.numeric), summary_funcs, .names = "{.col}_{.fn}"))
}


group_by_jeni(dados, HIB, AMB, summary_funcs = list(sd = sd))

group_by_jeni(dados, AMB, summary_funcs = list(mean = mean, sd = sd))


# Função 3: Reordenar colunas ---------------------------------------------

reorder_colls_jeni <- function(data, cols_to_move, ref_col, position = c("antes", "depois")) {
  position <- match.arg(position)
  col_names <- colnames(data)
  
  # Verifica se as colunas para mover e a coluna de referência estão no dataframe
  if (!all(cols_to_move %in% col_names) || !ref_col %in% col_names) {
    stop("As colunas para mover ou a coluna de referência não existem no dataframe.")
  }
  
  # Determina a nova ordem de colunas com base na posição
  new_order <- if (position == "antes") {
    append(setdiff(col_names, cols_to_move), cols_to_move, after = which(col_names == ref_col) - 1)
  } else {
    append(setdiff(col_names, cols_to_move), cols_to_move, after = which(col_names == ref_col))
  }
  
  # Seleciona as colunas na nova ordem e imprime a tabela
  data <- data %>% dplyr::select(all_of(new_order))
}

dados1 <- reorder_colls_jeni(dados, c("AMB", "HIB", "REP"), ref_col = "NGRA", position = "depois")
dados1 <- reorder_colls_jeni(dados, c("AMB", "HIB", "REP"), ref_col = "NGRA", position = "antes")


# Joins são vida -------------------------------------------------------------------

# Importação de dados

dados_inner1 <- read_excel("Scripts/Manipulacao/dados/dados.xlsx", sheet = "inner_data1")
dados_inner2 <- read_excel("Scripts/Manipulacao/dados/dados.xlsx", sheet = "inner_data2")

# Tipos de joins() ------------------------------------------------------------

# inner_join(): retorna apenas as linhas que têm correspondências em ambas as tabelas

resultado_inner <- dados_inner1 %>%
  inner_join(dados_inner2, by = "AMB")

# left_join() 
# retorna todas as linhas de dados_inner1 e adiciona as colunas de dados_inner2 
# onde houver correspondência, preenchendo com NA para as linhas de dados_inner1 sem correspondência.

resultado_left <- dados_inner1 %>%
  left_join(dados_inner2, by= "AMB")

# right_join() 
# retorna todas as linhas de dados_inner2 e adiciona as colunas de dados_inner1 onde houver correspondência, 
# preenchendo com NA onde dados_inner1 não tiver correspondência.

resultado_right <- dados_inner1 %>%
  right_join(dados_inner2, by = "AMB")

# full_join() 
# retorna todas as linhas de ambas as tabelas e preenche com NA onde não há correspondência.

resultado_full <- dados_inner1 %>%
  full_join(dados_inner2, by = "AMB")


# retorna apenas as linhas de dados_inner1 que têm correspondências em dados_inner2, 
# sem adicionar colunas de dados_inner2.

resultado_semi <- dados_inner1 %>%
  semi_join(dados_inner2, by = "AMB")

# anti_join() 
# retorna apenas as linhas de dados_inner1 que não têm correspondências em dados_inner2.

resultado_anti <- dados_inner1 %>%
  anti_join(dados_inner2, by = "AMB")
