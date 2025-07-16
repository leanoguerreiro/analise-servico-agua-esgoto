# R/02_preparacao_dados.R

preparar_geometrias_e_regioes <- function(estados, dados_lazy) {
  cli::cli_h2("Preparando Geometrias das Regiões para Mapeamento")
  estados_com_regiao <- estados |>
    mutate(regiao = case_when(
      abbrev_state %in% c("AM", "RR", "AP", "PA", "RO", "AC", "TO") ~ "Norte",
      abbrev_state %in% c("MT", "MS", "GO", "DF") ~ "Centro-Oeste",
      abbrev_state %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "Nordeste",
      abbrev_state %in% c("SP", "RJ", "MG", "ES") ~ "Sudeste",
      abbrev_state %in% c("PR", "SC", "RS") ~ "Sul",
      TRUE ~ "Outras"
    ))
  
  regioes_sf <- estados_com_regiao |>
    group_by(regiao) |>
    summarise(geometry = sf::st_union(geom)) |>
    ungroup()
  
  cli::cli_alert_success("Geometrias de Regiões criadas.")
  
  cli::cli_h2("Criando Coluna de Região nos Dados Principais")
  dados_lazy <- dados_lazy |>
    mutate(regiao = case_when(
      sigla_uf %in% c("AM", "RR", "AP", "PA", "RO", "AC", "TO") ~ "Norte",
      sigla_uf %in% c("MT", "MS", "GO", "DF") ~ "Centro-Oeste",
      sigla_uf %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "Nordeste",
      sigla_uf %in% c("SP", "RJ", "MG", "ES") ~ "Sudeste",
      sigla_uf %in% c("PR", "SC", "RS") ~ "Sul",
      TRUE ~ "Outras"
    ))
  cli::cli_alert_success("Coluna 'regiao' adicionada aos dados.")
  
  return(list(regioes_sf = regioes_sf, dados_lazy = dados_lazy))
}