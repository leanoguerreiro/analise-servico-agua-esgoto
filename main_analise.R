# main_analise.R

# Define o número de linhas para a amostra (ou defina NULL para carregar tudo)
num_linhas_para_analise <- 57037 

# --- Carregar todas as funções dos arquivos ---
# A ordem é importante: o download deve vir primeiro
source("R/00_download_data.R") # NOVO: Carrega a função de download
source("R/01_pacotes_e_leitura.R")
source("R/02_preparacao_dados.R")
source("R/03_eda_e_outliers.R")
source("R/04_visualizacoes_mapas.R")
source("R/05_testes_estatisticos.R")
source("R/06_graficos_adicionais.R")
source("R/07_reflexoes_finais.R")

# --- Função Principal para Orquestrar a Análise ---
executar_analise_completa_norte <- function(num_linhas_amostras = 57037) {
  
  # 1. Executa a etapa de download (se necessário)
  # Você pode adicionar uma lógica aqui para perguntar ao usuário se deseja baixar
  # ou verificar se os arquivos já existem antes de baixar novamente.
  # Por enquanto, ele sempre tentará baixar.
  
  if (!dir.exists("dados/")) {
    baixar_e_salvar_dados()
  }else{
    message("Diretório 'dados/' já existe. Pulando o download.")
  }
  
  # 2. Carrega pacotes para a análise (os pacotes de download já foram carregados)
  carregar_pacotes() 
  
  # 3. Lê os dados agora que eles estão salvos localmente
  dados_carregados <- ler_dados(num_linhas_amostras)
  dados_lazy <- dados_carregados$dados_lazy
  estados <- dados_carregados$estados
  
  # 4. Prepara geometrias e regiões
  preparacao <- preparar_geometrias_e_regioes(estados, dados_lazy)
  regioes_sf <- preparacao$regioes_sf
  dados_lazy_com_regiao <- preparacao$dados_lazy
  
  # 5. Realiza EDA e tratamento de NAs
  servico_agua_esgoto_coletado <- realizar_eda_e_tratar_nas(dados_lazy_com_regiao, num_linhas_amostras)
  
  # 6. Filtra para a Região Norte e EDA detalhada
  filtragem_e_eda_results <- filtrar_e_eda_norte(servico_agua_esgoto_coletado)
  servico_agua_esgoto_norte <- filtragem_e_eda_results$servico_agua_esgoto_norte
  dados_regiao_2021 <- filtragem_e_eda_results$dados_regiao_2021
  
  # 7. Gera visualizações da Região Norte
  gerar_visualizacoes_norte(servico_agua_esgoto_norte)
  
  # 8. Detecta outliers
  servico_agua_esgoto_norte_com_outliers <- detectar_outliers_norte(servico_agua_esgoto_norte)
  
  # 9. Gera mapas coropléticos
  gerar_mapas_coropleticos(regioes_sf, dados_regiao_2021, estados, servico_agua_esgoto_norte_com_outliers)
  
  # 10. Realiza testes de hipótese e Cohen's d
  realizar_testes_e_cohen_d(servico_agua_esgoto_norte_com_outliers)
  
  # 11. Gera análises agregadas e gráficos adicionais
  gerar_analises_agregadas_e_graficos_adicionais(servico_agua_esgoto_norte_com_outliers)
  
  # 12. Exibe reflexões finais
  exibir_reflexoes_finais()
  
  cli::cli_h1("Análise Completa da Região Norte Concluída!")
}

# --- Executa a análise ---
executar_analise_completa_norte(num_linhas_para_analise)
