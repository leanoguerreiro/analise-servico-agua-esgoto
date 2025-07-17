# R/04_visualizacoes_mapas.R

gerar_visualizacoes_norte <- function(servico_agua_esgoto_norte) {
  
  p_boxplot_perc_agua_uf_norte <- ggplot(servico_agua_esgoto_norte, aes(sigla_uf, perc_agua_atendida, fill = sigla_uf)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Percentual de População Urbana Atendida por Água por UF (Região Norte)", x = "UF", y = "Percentual Atendido (%)") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"))
  print(p_boxplot_perc_agua_uf_norte)
  ggsave("boxplot_perc_agua_uf_norte.png", plot = p_boxplot_perc_agua_uf_norte, width = 10, height = 6, dpi = 300)
  cli::cli_alert_success("Boxplot de percentual de água atendida por UF (Norte) salvo.")
  
  p_boxplot_perc_esgoto_uf_norte <- ggplot(servico_agua_esgoto_norte, aes(sigla_uf, perc_esgoto_atendido, fill = sigla_uf)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Percentual de População Urbana Atendida por Esgoto por UF (Região Norte)", x = "UF", y = "Percentual Atendido (%)") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"))
  print(p_boxplot_perc_esgoto_uf_norte)
  ggsave("boxplot_perc_esgoto_uf_norte.png", plot = p_boxplot_perc_esgoto_uf_norte, width = 10, height = 6, dpi = 300)
  cli::cli_alert_success("Boxplot de percentual de esgoto atendido por UF (Norte) salvo.")
  
  cli::cli_h2("Distribuição por Variáveis Categóricas - Região Norte")
  p_dist_uf_norte <- ggplot(servico_agua_esgoto_norte, aes(x = fct_infreq(sigla_uf_nome), fill = sigla_uf_nome)) +
    geom_bar() +
    labs(title = "Distribuição de Registros por UF (Região Norte)", x = "UF", y = "Contagem") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white")) + guides(fill = "none")
  print(p_dist_uf_norte)
  ggsave("dist_uf_norte.png", plot = p_dist_uf_norte, width = 10, height = 6, dpi = 300)
  cli::cli_alert_success("Gráfico de distribuição de registros por UF (Norte) salvo.")
}

gerar_mapas_coropleticos <- function(regioes_sf, dados_regiao_2021, estados, servico_agua_esgoto_norte) {
  cli::cli_h1("Mapas Coropléticos de Percentuais por Região (Ano 2021) com Rótulos")
  
  mapa_dados_regiao <- regioes_sf |>
    left_join(dados_regiao_2021, by = "regiao")
  
  mapa_dados_regiao_com_labels <- mapa_dados_regiao |>
    mutate(
      centroide = sf::st_centroid(geometry),
      label_perc_agua = paste0(regiao, "\n", round(media_perc_agua_regiao, 1), "%"),
      label_perc_esgoto = paste0(regiao, "\n", round(media_perc_esgoto_regiao, 1), "%")
    )
  
  p_mapa_perc_agua_regiao_melhorado <- ggplot(mapa_dados_regiao_com_labels) +
    geom_sf(aes(fill = media_perc_agua_regiao), color = "black", linewidth = 0.4) +
    scale_fill_viridis_c(na.value = "grey80", option = "viridis", name = "Média de Atendimento de Água (%)", labels = label_number(suffix = "%"), breaks = c(20, 40, 60, 80, 100), limits = c(0, 100)) +
    geom_sf_text(aes(geometry = centroide, label = label_perc_agua), stat = "sf_coordinates", color = "white", size = 4, fontface = "bold", bg.color = "black", bg.r = 0.05, check_overlap = TRUE) +
    labs(title = "Cobertura Média de Água Urbana por Região – Brasil (2021)", subtitle = "Percentual da População Urbana Atendida") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)), plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 15)), legend.position = "bottom", legend.title = element_text(size = 12), legend.text = element_text(size = 10), panel.background = element_rect(fill = "lightblue", color = NA), plot.background = element_rect(fill = "lightblue", color = NA))
  print(p_mapa_perc_agua_regiao_melhorado)
  ggsave("mapa_perc_agua_regiao_2021_melhorado.png", plot = p_mapa_perc_agua_regiao_melhorado, width = 12, height = 9, dpi = 300)
  cli::cli_alert_success("Mapa de percentual de água por região (2021) salvo.")
  
  p_mapa_perc_esgoto_regiao_melhorado <- ggplot(mapa_dados_regiao_com_labels) +
    geom_sf(aes(fill = media_perc_esgoto_regiao), color = "black", linewidth = 0.4) +
    scale_fill_viridis_c(na.value = "grey80", option = "magma", name = "Média de Atendimento de Esgoto (%)", labels = label_number(suffix = "%"), breaks = c(20, 40, 60, 80, 100), limits = c(0, 100)) +
    geom_sf_text(aes(geometry = centroide, label = label_perc_esgoto), stat = "sf_coordinates", color = "white", size = 4, fontface = "bold", bg.color = "black", bg.r = 0.05, check_overlap = TRUE) +
    labs(title = "Cobertura Média de Esgoto Urbano por Região – Brasil (2021)", subtitle = "Percentual da População Urbana Atendida") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)), plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 15)), legend.position = "bottom", legend.title = element_text(size = 12), legend.text = element_text(size = 10), panel.background = element_rect(fill = "lightblue", color = NA), plot.background = element_rect(fill = "lightblue", color = NA))
  print(p_mapa_perc_esgoto_regiao_melhorado)
  ggsave("mapa_perc_esgoto_regiao_2021_melhorado.png", plot = p_mapa_perc_esgoto_regiao_melhorado, width = 12, height = 9, dpi = 300)
  cli::cli_alert_success("Mapa de percentual de esgoto por região (2021) salvo.")
  
  cli::cli_h1("Mapas Coropléticos da Região Norte")
  
  servico_agua_esgoto_norte_for_map <- servico_agua_esgoto_norte
  anos_disponiveis_mapa_norte <- servico_agua_esgoto_norte_for_map |> distinct(ano) |> pull(ano)
  
  if (length(anos_disponiveis_mapa_norte) == 0) {
    cli::cli_alert_warning("Não há anos disponíveis nos dados da Região Norte para gerar mapas por UF.")
    return(NULL)
  }
  ano_mapa_norte <- max(anos_disponiveis_mapa_norte, na.rm = TRUE)
  
  agua_esgoto_por_uf_norte <- servico_agua_esgoto_norte_for_map |>
    filter(ano == ano_mapa_norte) |>
    group_by(sigla_uf, sigla_uf_nome, regiao) |>
    summarise(
      media_pop_agua = mean(populacao_urbana_atendida_agua, na.rm = TRUE),
      media_pop_esgoto = mean(populacao_urbana_atendida_esgoto, na.rm = TRUE),
      media_perc_agua = mean(perc_agua_atendida, na.rm = TRUE),
      media_perc_esgoto = mean(perc_esgoto_atendido, na.rm = TRUE),
      .groups = 'drop'
    ) |> ungroup()
  
  estados_mapa_servicos_norte <- estados |>
    filter(abbrev_state %in% c("AM", "RR", "AP", "PA", "RO", "AC", "TO")) |>
    left_join(agua_esgoto_por_uf_norte, by = c("abbrev_state" = "sigla_uf"))
  
  p_mapa_agua_norte <- ggplot(estados_mapa_servicos_norte) +
    geom_sf(aes(fill = media_pop_agua), color = "black", linewidth = 0.2) +
    scale_fill_viridis_c(na.value = "grey80", option = "viridis", name = "Média Pop. Atendida Água", labels = label_number(scale_cut = cut_short_scale())) +
    labs(title = paste("Média de População Urbana Atendida por Água por UF (Região Norte) – Ano", ano_mapa_norte), subtitle = "Dados do ano mais recente disponível") +
    theme_void() + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), plot.subtitle = element_text(hjust = 0.5, size = 10), legend.position = "bottom", panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"))
  print(p_mapa_agua_norte)
  ggsave("mapa_pop_agua_atendida_norte.png", plot = p_mapa_agua_norte, width = 10, height = 8, dpi = 300)
  cli::cli_alert_success("Mapa de população atendida por água (UF, Norte) salvo.")
  
  p_mapa_esgoto_norte <- ggplot(estados_mapa_servicos_norte) +
    geom_sf(aes(fill = media_pop_esgoto), color = "black", linewidth = 0.2) +
    scale_fill_viridis_c(na.value = "grey80", option = "magma", name = "Média Pop. Atendida Esgoto", labels = label_number(scale_cut = cut_short_scale())) +
    labs(title = paste("Média de População Urbana Atendida por Esgoto por UF (Região Norte) – Ano", ano_mapa_norte), subtitle = "Dados do ano mais recente disponível") +
    theme_void() + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), plot.subtitle = element_text(hjust = 0.5, size = 10), legend.position = "bottom", panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"))
  print(p_mapa_esgoto_norte)
  ggsave("mapa_pop_esgoto_atendida_norte.png", plot = p_mapa_esgoto_norte, width = 10, height = 8, dpi = 300)
  cli::cli_alert_success("Mapa de população atendida por esgoto (UF, Norte) salvo.")
  
  p_mapa_perc_agua_norte <- ggplot(estados_mapa_servicos_norte) +
    geom_sf(aes(fill = media_perc_agua), color = "black", linewidth = 0.2) +
    scale_fill_viridis_c(na.value = "grey80", option = "cividis", name = "Média % Atendido Água", labels = label_number(scale_cut = cut_short_scale())) +
    labs(title = paste("Média de Percentual de População Urbana Atendida por Água por UF (Região Norte) – Ano", ano_mapa_norte), subtitle = "Dados do ano mais recente disponível") +
    theme_void() + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), plot.subtitle = element_text(hjust = 0.5, size = 10), legend.position = "bottom", panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"))
  print(p_mapa_perc_agua_norte)
  ggsave("mapa_perc_agua_atendida_norte.png", plot = p_mapa_perc_agua_norte, width = 10, height = 8, dpi = 300)
  cli::cli_alert_success("Mapa de percentual de água atendida (UF, Norte) salvo.")
}