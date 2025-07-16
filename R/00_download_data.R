# R/00_download_data.R
# (Este é o conteúdo que você deve copiar para o arquivo)

# Função para baixar e salvar os dados
baixar_e_salvar_dados <- function() {
  cli::cli_h1("ETAPA 1 - DOWNLOAD DE DADOS")
  
  ## ===================== 1. Pacotes ============================================
  # Carrega pacotes necessários para download, queries e salvamento
  cli::cli_h2("Carregando Pacotes para Download")
  suppressPackageStartupMessages({
    library(basedosdados)   # Acessar e executar queries SQL no BigQuery
    library(bigrquery)      # Autenticação de login e credenciais no Google Cloud
    library(arrow)          # Leitura e escrita rápida de arquivos Parquet
    library(geobr)          # Download de shapefiles prontos do Brasil (IBGE)
    library(sf)             # Manipulação de dados espaciais (sf objects)
    library(cli)            # Para títulos de seção no console
  })
  cli::cli_alert_success("Pacotes de download carregados com sucesso!")
  
  # -----------------------------------------------------------------------------
  # 1.0 Autenticação no Google Cloud
  # -----------------------------------------------------------------------------
  
  cli::cli_h2("Autenticando no Google Cloud")
  # Definir o email de forma segura
  EMAIL <- Sys.getenv("EMAIL")
  
  # Definir o billing_id de forma segura
  billing_id <- Sys.getenv("GCP_BILLING_ID") # Defina no seu .Renviron
  if (billing_id == "") {
    cli::cli_alert_danger("GCP_BILLING_ID não encontrado no .Renviron! Por favor, defina-o para autenticação.")
    stop("Autenticação falhou: GCP_BILLING_ID ausente.")
  }
  
  # Abre janela de login do Google para autenticação na BasedosDados
  # Esta linha pode exigir interação do usuário na primeira vez
  bq_auth(email = EMAIL) # Definir "EMAIL" em .Renviron
  
  # Define o projeto de billing no Google Cloud (substitua pelo seu ID real)
  set_billing_id(billing_id)
  cli::cli_alert_success("Autenticação no Google Cloud concluída.")
  
  # -----------------------------------------------------------------------------
  # 1.1 Cria pasta local "dados/" se não existir
  # -----------------------------------------------------------------------------
  
  cli::cli_h2("Verificando e Criando Diretório de Dados")
  dir.create("dados", showWarnings = FALSE) # Cria a pasta "dados" caso não exista
  cli::cli_alert_success("Diretório 'dados/' verificado/criado.")
  
  # -----------------------------------------------------------------------------
  # 1.2 Download dos microdados de Serviço de Água e Esgoto
  # -----------------------------------------------------------------------------
  
  cli::cli_h2("Baixando Dados de Serviço de Água e Esgoto do BigQuery")
  # Define a query SQL que será executada na BasedosDados via BigQuery
  query <- "
SELECT
    dados.ano as ano,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.populacao_urbana as populacao_urbana,
    dados.populacao_urbana_atendida_agua as populacao_urbana_atendida_agua,
    dados.populacao_urbana_atendida_esgoto as populacao_urbana_atendida_esgoto,
    dados.volume_agua_produzido as volume_agua_produzido,
    dados.volume_agua_tratada_eta as volume_agua_tratada_eta,
    dados.volume_agua_consumido as volume_agua_consumido
FROM basedosdados.br_mdr_snis.municipio_agua_esgoto AS dados
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM basedosdados.br_bd_diretorios_brasil.municipio) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM basedosdados.br_bd_diretorios_brasil.uf) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
WHERE dados.ano BETWEEN 2012 AND 2021
"
  
  # Executa a query SQL no BigQuery e salva o resultado como Parquet
  tryCatch({
    basedosdados::read_sql(query, billing_project_id = basedosdados::get_billing_id()) |>
      write_parquet("dados/servico_agua_escoto.parquet")
    cli::cli_alert_success("Dados de serviço de água e esgoto salvos em 'dados/servico_agua_escoto.parquet'.")
  }, error = function(e) {
    cli::cli_alert_danger(paste("Erro ao baixar dados de serviço de água e esgoto:", e$message))
    stop("Download de dados de serviço de água e esgoto falhou.")
  })
  
  # -----------------------------------------------------------------------------
  # 1.3 Download do shapefile de estados (para mapas)
  # -----------------------------------------------------------------------------
  
  cli::cli_h2("Baixando Shapefile de Estados")
  # Baixa shapefile de todos os estados do Brasil (ano 2020) via geobr
  tryCatch({
    estados <- geobr::read_state(code_state = "all", year = 2020)
    saveRDS(estados, "dados/estados.rds")
    cli::cli_alert_success("Shapefile de estados salvo em 'dados/estados.rds'.")
  }, error = function(e) {
    cli::cli_alert_danger(paste("Erro ao baixar shapefile de estados:", e$message))
    stop("Download do shapefile de estados falhou.")
  })
  
  # -----------------------------------------------------------------------------
  # Mensagem final de conclusão
  # -----------------------------------------------------------------------------
  
  cli::cli_h1("Etapa 1 Concluída!")
  cli::cli_alert_success("Bases salvas em 'dados/' (servico_agua_escoto.parquet e estados.rds).")
}