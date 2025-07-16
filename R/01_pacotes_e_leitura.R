# R/01_pacotes_e_leitura.R

carregar_pacotes <- function() {
  cli::cli_h2("Carregando Pacotes Necessários")
  suppressPackageStartupMessages({
    library(tidyverse)
    library(arrow)
    library(scales)
    library(skimr)
    library(sf)
    library(psych)
    library(DT)
    library(effsize)
    library(cli)
  })
  cli::cli_alert_success("Pacotes carregados com sucesso!")
}

ler_dados <- function(num_linhas_amostras) {
  cli::cli_h2("Lendo Bases de Dados")
  dados_lazy <- open_dataset("dados/servico_agua_escoto.parquet")
  estados <- readRDS("dados/estados.rds")
  cli::cli_alert_info(paste("Carregadas", num_linhas_amostras, "linhas da base de dados de serviço de água e esgoto."))
  cli::cli_alert_info("Carregado o shapefile de estados.")
  
  return(list(dados_lazy = dados_lazy, estados = estados))
}