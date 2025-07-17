# R/06_graficos_adicionais.R

gerar_analises_agregadas_e_graficos_adicionais <- function(servico_agua_esgoto_norte) {
  cli::cli_h1("Análises Agregadas e Gráficos Adicionais - Região Norte")

  # Cria a pasta 'graficos' se ela não existir
  dir.create("graficos", showWarnings = FALSE)
  cli::cli_alert_info("Verificado/criado o diretório 'graficos/'.")
  
  # 1. Total de População Urbana Atendida por Água por ano na Região Norte
  pop_agua_ano_norte <- servico_agua_esgoto_norte |>
    group_by(ano) |>
    summarise(total_pop_agua = sum(populacao_urbana_atendida_agua, na.rm = TRUE)) |>
    collect()
  
  p_pop_agua_ano_norte <- ggplot(pop_agua_ano_norte, aes(x = ano, y = total_pop_agua)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_point(color = "red", size = 2) +
    labs(title = "Distribuição da População Urbana Atendida por Água por Ano (Região Norte)", x = "Ano", y = "População Atendida") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    scale_x_continuous(breaks = unique(pop_agua_ano_norte$ano)) +
    theme_minimal() + theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"))
  print(p_pop_agua_ano_norte)
  ggsave("graficos/total_pop_agua_ano_norte.png", plot = p_pop_agua_ano_norte, width = 10, height = 6, dpi = 300)
  cli::cli_alert_success("Gráfico de total de população atendida por água (Norte) por ano salvo.")
  
  # 2. Média de Volume de Água Produzido por UF na Região Norte
  volume_uf_norte <- servico_agua_esgoto_norte |>
    group_by(sigla_uf_nome) |>
    summarise(media_volume_produzido = mean(volume_agua_produzido, na.rm = TRUE)) |>
    arrange(desc(media_volume_produzido)) |>
    collect()
  
  p_volume_uf_norte <- ggplot(volume_uf_norte,
                              aes(x = reorder(sigla_uf_nome, -media_volume_produzido), y = media_volume_produzido)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Média de Volume de Água Produzido por UF (Região Norte)", x = "UF", y = "Média de Volume (m³)") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"))
  print(p_volume_uf_norte)
  ggsave("graficos/volume_agua_uf_norte.png", plot = p_volume_uf_norte, width = 10, height = 6, dpi = 300)
  cli::cli_alert_success("Gráfico de média de volume de água produzido por UF (Norte) salvo.")

  # NOVO GRÁFICO: Média de Volume de Água Produzido vs. Consumido por UF na Região Norte
  cli::cli_h2("Comparação da Média de Volume de Água Produzido vs. Consumido por UF (Região Norte)")
  
  volume_produzido_consumido_uf_norte <- servico_agua_esgoto_norte |>
    group_by(sigla_uf_nome) |>
    summarise(
      media_volume_produzido = mean(volume_agua_produzido, na.rm = TRUE),
      media_volume_consumido = mean(volume_agua_consumido, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # Pivotar para formato longo para facilitar a plotagem com ggplot2
    pivot_longer(
      cols = starts_with("media_volume"),
      names_to = "tipo_volume",
      values_to = "valor_volume"
    ) |>
    mutate(
      tipo_volume = case_when(
        tipo_volume == "media_volume_produzido" ~ "Produzido",
        tipo_volume == "media_volume_consumido" ~ "Consumido",
        TRUE ~ tipo_volume # Garante que outros tipos, se houver, sejam mantidos
      ),
      # Reordenar as UFs com base na soma total do volume (para empilhamento horizontal)
      sigla_uf_nome = fct_reorder(sigla_uf_nome, valor_volume, .fun = sum, .desc = FALSE) # Ordem inversa para que o maior fique em cima
    ) |>
    collect()

  p_volume_prod_cons_uf_norte <- ggplot(volume_produzido_consumido_uf_norte, 
                                        aes(y = sigla_uf_nome, x = valor_volume, fill = tipo_volume)) + # ALTERADO: x e y invertidos
    geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.2) +
    labs(
      title = "Média de Volume de Água Produzido vs. Consumido por UF (Região Norte)",
      y = "UF", # ALTERADO: y-axis label
      x = "Média de Volume (m³)", # ALTERADO: x-axis label
      fill = "Tipo de Volume"
    ) +
    scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) + # ALTERADO: escala no eixo x
    scale_fill_manual(values = c("Produzido" = "darkgreen", "Consumido" = "darkblue")) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(angle = 0, hjust = 1), # ALTERADO: ajuste para texto no eixo Y
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      legend.position = "bottom"
    )
  print(p_volume_prod_cons_uf_norte)
  ggsave("graficos/volume_produzido_consumido_uf_norte.png", plot = p_volume_prod_cons_uf_norte, width = 12, height = 7, dpi = 300)
  cli::cli_alert_success("Gráfico de comparação de volume produzido vs. consumido por UF (Norte) salvo.")

  # 3. Média de Percentual de População Atendida por Esgoto por UF na Região Norte
  perc_esgoto_uf_norte <- servico_agua_esgoto_norte |>
    group_by(sigla_uf_nome) |>
    summarise(media_perc_esgoto = mean(perc_esgoto_atendido, na.rm = TRUE)) |>
    arrange(desc(media_perc_esgoto)) |>
    collect()
  
  p_perc_esgoto_uf_norte <- ggplot(perc_esgoto_uf_norte,
                                   aes(x = reorder(sigla_uf_nome, -media_perc_esgoto), y = media_perc_esgoto, fill = sigla_uf_nome)) +
    geom_bar(stat = "identity") +
    labs(title = "Média do Percentual de População Atendida por Esgoto por UF (Região Norte)", x = "UF", y = "Média % Atendido") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white")) + guides(fill = "none")
  print(p_perc_esgoto_uf_norte)
  ggsave("graficos/mean_perc_esgoto_uf_norte.png", plot = p_perc_esgoto_uf_norte, width = 10, height = 6, dpi = 300)
  cli::cli_alert_success("Gráfico de média de percentual de esgoto atendido por UF (Norte) salvo.")
  
  # 4. Evolução do Percentual de População Atendida por Água por UF na Região Norte
  perc_agua_uf_evolucao_norte <- servico_agua_esgoto_norte |>
    filter(!is.na(perc_agua_atendida), !is.na(sigla_uf)) |>
    group_by(ano, sigla_uf) |>
    summarise(media_perc_agua = mean(perc_agua_atendida, na.rm = TRUE), .groups = "drop") |>
    collect()
  
  p_perc_agua_uf_evolucao_norte <- ggplot(perc_agua_uf_evolucao_norte,
                                          aes(x = ano, y = media_perc_agua, color = sigla_uf)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(title = "Evolução do Percentual de População Urbana Atendida por Água por UF (Região Norte)", x = "Ano", y = "Média % Atendido") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    scale_x_continuous(breaks = unique(perc_agua_uf_evolucao_norte$ano)) +
    theme_minimal() + theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"))
  print(p_perc_agua_uf_evolucao_norte)
  ggsave("graficos/evolution_perc_agua_uf_norte.png", plot = p_perc_agua_uf_evolucao_norte, width = 10, height = 6, dpi = 300)
  cli::cli_alert_success("Gráfico de evolução do percentual de água atendida por UF (Norte) salvo.")
  
  # 5. Gráfico de pizza por UF na Região Norte (participação da população urbana total)
  pop_urbana_uf_pizza_norte <- servico_agua_esgoto_norte |>
    group_by(sigla_uf_nome) |>
    summarise(total_pop_urbana = sum(populacao_urbana, na.rm = TRUE)) |>
    mutate(perc = total_pop_urbana / sum(total_pop_urbana) * 100,
           label = paste0(sigla_uf_nome, " (", round(perc, 1), "%)")) |>
    arrange(desc(total_pop_urbana)) |>
    collect()
  
  p_pop_urbana_uf_pizza_norte <- ggplot(pop_urbana_uf_pizza_norte, aes(x = "", y = total_pop_urbana, fill = sigla_uf_nome)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = "Participação da População Urbana Total por UF (Região Norte)") +
    theme_void() + theme(legend.position = "right", panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white")) + geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "black", size = 3)
  print(p_pop_urbana_uf_pizza_norte)
  ggsave("graficos/pie_chart_pop_urbana_uf_norte.png", plot = p_pop_urbana_uf_pizza_norte, width = 10, height = 8, dpi = 300)
  cli::cli_alert_success("Gráfico de pizza da população urbana por UF (Norte) salvo.")
}
