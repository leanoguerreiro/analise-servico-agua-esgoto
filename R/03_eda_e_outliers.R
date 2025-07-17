# R/03_eda_e_outliers.R

realizar_eda_e_tratar_nas <- function(dados_lazy, num_linhas_amostras) {
  cli::cli_h1("Exploração Inicial e Análise de NAs")
  
  # Cria a pasta 'graficos' se ela não existir. Necessário aqui pois esta função
  # é a primeira a salvar gráficos como na_count_plot.png
  dir.create("graficos", showWarnings = FALSE)
  cli::cli_alert_info("Verificado/criado o diretório 'graficos/'.")
  
  amostra <- dados_lazy |> head(num_linhas_amostras) |> collect()
  
  contagem_na <- amostra |>
    summarise(across(everything(), ~sum(is.na(.)))) |>
    pivot_longer(cols = everything(), names_to = "coluna", values_to = "quantidade_na") |>
    arrange(desc(quantidade_na))
  
  cli::cli_alert_info("Contagem de valores NA por coluna:")
  print(contagem_na)
  
  contagem_na_plot <- contagem_na |> filter(quantidade_na > 0)
  
  if (nrow(contagem_na_plot) > 0) {
    p_na_count <- ggplot(contagem_na_plot, aes(x = reorder(coluna, -quantidade_na), y = quantidade_na)) +
      geom_bar(stat = "identity", fill = "salmon", color = "black") +
      labs(title = "Quantidade de Valores Ausentes (NA) por Coluna",
           x = "Coluna", y = "Número de NAs") +
      theme_minimal() +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"))
    print(p_na_count)
    # Caminho corrigido para salvar na pasta 'graficos'
    ggsave("graficos/na_count_plot.png", plot = p_na_count, width = 10, height = 6, dpi = 300)
    cli::cli_alert_success("Gráfico de contagem de NAs salvo como 'graficos/na_count_plot.png'.")
  } else {
    cli::cli_alert_info("Nenhuma coluna com valores NA encontrada para plotagem.")
  }
  
  na_agua_por_ano <- dados_lazy |>
    filter(is.na(populacao_urbana_atendida_agua)) |>
    group_by(ano) |>
    summarise(
      quantidade_na_agua = n(),
      quantidade_na_esgoto_quando_agua_na = sum(is.na(populacao_urbana_atendida_esgoto), na.rm = TRUE)
    ) |>
    collect() |> arrange(ano)
  
  if (nrow(na_agua_por_ano) > 0 && sum(na_agua_por_ano$quantidade_na_agua) > 0) {
    cli::cli_alert_info("Contagem de NAs na coluna 'populacao_urbana_atendida_agua' (e 'populacao_urbana_atendida_esgoto' nesses casos) por ano:")
    print(na_agua_por_ano)
    p_na_agua_ano <- ggplot(na_agua_por_ano, aes(x = as.factor(ano), y = quantidade_na_agua)) +
      geom_bar(stat = "identity", fill = "orangered", color = "black") +
      labs(title = "NAs em 'populacao_urbana_atendida_agua' por Ano",
           subtitle = "Contagem de registros onde a população atendida por água urbana não foi especificada",
           x = "Ano", y = "Número de Registros com NA") +
      theme_minimal() + scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"))
    print(p_na_agua_ano)
    # Caminho corrigido para salvar na pasta 'graficos'
    ggsave("graficos/na_agua_por_ano_plot.png", plot = p_na_agua_ano, width = 10, height = 6, dpi = 300)
    cli::cli_alert_success("Gráfico de NAs em 'populacao_urbana_atendida_agua' por ano salvo como 'graficos/na_agua_por_ano_plot.png'.")
    
    na_link_check <- dados_lazy |>
      filter(is.na(populacao_urbana_atendida_agua)) |>
      summarise(
        total_registros_agua_na = n(),
        total_esgoto_na_quando_agua_na = sum(is.na(populacao_urbana_atendida_esgoto), na.rm = TRUE),
        percentual_esgoto_na = total_esgoto_na_quando_agua_na / total_registros_agua_na * 100
      ) |> collect()
    cli::cli_alert_info("Verificação da ligação entre NA em 'populacao_urbana_atendida_agua' e 'populacao_urbana_atendida_esgoto':")
    print(na_link_check)
    cli::cli_alert_info("ATENÇÃO: Verifique a concentração de NAs nas colunas de serviço de água e esgoto por ano. Isso é um ponto crítico para a análise.")
  } else {
    cli::cli_alert_info("Nenhum NA encontrado na coluna 'populacao_urbana_atendida_agua' para esta análise.")
  }
  
  cli::cli_h2("Análise 2: Presença/Ausência de Reporte de Dados de Água/Esgoto por Ano e UF")
  
  all_ufs <- dados_lazy |> filter(!is.na(sigla_uf)) |> distinct(sigla_uf) |> collect() |> pull(sigla_uf)
  all_anos <- dados_lazy |> filter(!is.na(ano)) |> distinct(ano) |> collect() |> arrange(ano) |> pull(ano)
  
  if (length(all_ufs) > 0 && length(all_anos) > 0) {
    complete_grid <- expand_grid(ano = all_anos, sigla_uf = all_ufs)
    reported_data_by_year_uf <- dados_lazy |>
      filter(!is.na(populacao_urbana_atendida_agua) & !is.na(ano) & !is.na(sigla_uf)) |>
      group_by(ano, sigla_uf) |> summarise(num_registros = n(), .groups = "drop") |> collect()
    
    data_presence_by_year_uf <- complete_grid |>
      left_join(reported_data_by_year_uf, by = c("ano", "sigla_uf")) |>
      mutate(presente_com_registros = !is.na(num_registros) & num_registros > 0)
    
    absent_data_uf <- data_presence_by_year_uf |>
      filter(!presente_com_registros) |> select(ano, sigla_uf) |> arrange(sigla_uf, ano)
    
    if (nrow(absent_data_uf) > 0) {
      cli::cli_alert_info("Amostra de anos em que UFs NÃO tiveram registros de 'populacao_urbana_atendida_agua':")
      print(head(absent_data_uf, 10))
      summary_absent_uf <- absent_data_uf |>
        group_by(sigla_uf) |>
        summarise(anos_ausente = paste(sort(unique(ano)), collapse = ", "),
                  num_anos_ausente = n_distinct(ano)) |> arrange(desc(num_anos_ausente))
      cli::cli_alert_info("Resumo de UFs e o número de anos em que estiveram ausentes de registros de 'populacao_urbana_atendida_agua':")
      print(head(summary_absent_uf, 10))
    } else {
      cli::cli_alert_info("Todos os dados de 'populacao_urbana_atendida_agua' foram reportados em todos os anos para todas as UFs.")
    }
    
    num_uf_heatmap <- min(20, nrow(all_ufs))
    if (num_uf_heatmap > 0) {
      uf_counts_overall <- reported_data_by_year_uf |>
        group_by(sigla_uf) |>
        summarise(total_registros_geral = sum(num_registros, na.rm = TRUE)) |> arrange(desc(total_registros_geral))
      
      top_n_uf_for_heatmap <- uf_counts_overall |> slice_head(n = num_uf_heatmap) |> pull(sigla_uf)
      heatmap_data <- data_presence_by_year_uf |> filter(sigla_uf %in% top_n_uf_for_heatmap)
      
      p_heatmap_data_presence <- ggplot(heatmap_data, aes(x = as.factor(ano), y = factor(sigla_uf, levels = rev(top_n_uf_for_heatmap)), fill = presente_com_registros)) +
        geom_tile(color = "white", linewidth = 0.5) +
        scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "lightgrey"), name = "Reportado") +
        labs(title = "Presença/Ausência de Reporte de Dados de Água por UF e Ano",
             subtitle = paste("Para as", num_uf_heatmap, "UFs com mais registros no geral. Verde = Reportado, Cinza = Não Reportado."),
             x = "Ano", y = "UF") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom", panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"))
      print(p_heatmap_data_presence)
      # Caminho corrigido para salvar na pasta 'graficos'
      ggsave("graficos/data_presence_heatmap.png", plot = p_heatmap_data_presence, width = 12, height = 8, dpi = 300)
      cli::cli_alert_success("Heatmap de presença/ausência de dados salvo como 'graficos/data_presence_heatmap.png'.")
    } else {
      cli::cli_alert_info("Não há dados suficientes de UFs para gerar o heatmap.")
    }
  } else {
    cli::cli_alert_info("Não foi possível obter a lista de todas as UFs ou todos os anos para a Análise 2.")
  }
  
  # Coleta do dataset completo para análises que exigem os dados em memória
  servico_agua_esgoto <- dados_lazy |> head(num_linhas_amostras) |> collect()
  
  # Crie as colunas de percentual de atendimento AQUI, no dataframe completo
  servico_agua_esgoto <- servico_agua_esgoto |>
    mutate(
      perc_agua_atendida = ifelse(populacao_urbana > 0, (populacao_urbana_atendida_agua / populacao_urbana) * 100, 0),
      perc_esgoto_atendido = ifelse(populacao_urbana > 0, (populacao_urbana_atendida_esgoto / populacao_urbana) * 100, 0)
    )
  cli::cli_alert_success("Colunas de percentuais de atendimento criadas no dataset completo.")
  
  # Tratamento de outliers superiores para percentuais acima de 100%
  servico_agua_esgoto <- servico_agua_esgoto |>
    mutate(
      perc_agua_atendida = ifelse(perc_agua_atendida > 100, 100, perc_agua_atendida),
      perc_esgoto_atendido = ifelse(perc_esgoto_atendido > 100, 100, perc_esgoto_atendido)
    )
  cli::cli_alert_success("Valores de percentual acima de 100% corrigidos.")
  
  # DIAGNÓSTICO: Verificar colunas antes de retornar
  cli::cli_alert_info("Colunas em 'servico_agua_esgoto' antes de retornar de 'realizar_eda_e_tratar_nas':")
  print(colnames(servico_agua_esgoto))
  
  return(servico_agua_esgoto)
}

filtrar_e_eda_norte <- function(servico_agua_esgoto) { # servico_agua_esgoto agora JÁ TEM as colunas de percentual
  cli::cli_h1("Filtrando dados para a Região Norte")
  
  # DIAGNÓSTICO: Verificar colunas no início de filtrar_e_eda_norte
  cli::cli_alert_info("Colunas em 'servico_agua_esgoto' no início de 'filtrar_e_eda_norte':")
  print(colnames(servico_agua_esgoto))
  
  servico_agua_esgoto_norte <- servico_agua_esgoto |>
    filter(regiao == "Norte")
  
  if (nrow(servico_agua_esgoto_norte) == 0) {
    cli::cli_alert_warning("Nenhum dado encontrado para a Região Norte. Verifique a coluna 'regiao' e os dados.")
    stop("Análise interrompida: Sem dados para a Região Norte.")
  } else {
    cli::cli_alert_info(paste("Dados filtrados para a Região Norte. Total de registros:", nrow(servico_agua_esgoto_norte)))
  }
  
  cli::cli_h1("Análise Exploratória de Dados (EDA) Detalhada - Região Norte")
  cli::cli_alert_info("Estatísticas descritivas gerais da base da Região Norte:")
  skim(servico_agua_esgoto_norte) |> print()
  
  # Esta parte agora funcionará corretamente, pois 'servico_agua_esgoto'
  # (o dataset completo passado como argumento) já contém as colunas de percentual.
  dados_regiao_2021 <- servico_agua_esgoto |>
    filter(ano == 2021) |>
    group_by(regiao) |>
    summarise(
      media_perc_agua_regiao = mean(perc_agua_atendida, na.rm = TRUE),
      media_perc_esgoto_regiao = mean(perc_esgoto_atendido, na.rm = TRUE),
      .groups = 'drop'
    ) |> collect()
  
  cli::cli_alert_info("Média de Percentuais de Água e Esgoto por Região (2021):")
  print(dados_regiao_2021)
  
  cli::cli_h2("Estatísticas Descritivas de População Urbana Atendida por Água por UF na Região Norte")
  describeBy(servico_agua_esgoto_norte$populacao_urbana_atendida_agua, group = servico_agua_esgoto_norte$sigla_uf, mat = TRUE) |> print()
  
  cli::cli_h2("Estatísticas Descritivas de População Urbana Atendida por Esgoto por UF na Região Norte")
  describeBy(servico_agua_esgoto_norte$populacao_urbana_atendida_esgoto, group = servico_agua_esgoto_norte$sigla_uf, mat = TRUE) |> print()
  
  cli::cli_h2("Estatísticas Descritivas de Percentual de População Atendida por Água por UF na Região Norte")
  describeBy(servico_agua_esgoto_norte$perc_agua_atendida, group = servico_agua_esgoto_norte$sigla_uf, mat = TRUE) |> print()
  
  cli::cli_h2("Estatísticas Descritivas de Percentual de População Atendida por Esgoto por UF na Região Norte")
  describeBy(servico_agua_esgoto_norte$perc_esgoto_atendido, group = servico_agua_esgoto_norte$sigla_uf, mat = TRUE) |> print()
  
  return(list(servico_agua_esgoto_norte = servico_agua_esgoto_norte, dados_regiao_2021 = dados_regiao_2021))
}

detectar_outliers_norte <- function(servico_agua_esgoto_norte) {
  cli::cli_h1("Análise de Outliers (Critério IQR) - Região Norte")
  
  # Debugging: Print column names to confirm what's available
  cli::cli_alert_info("Column names in servico_agua_esgoto_norte before outlier detection:")
  print(colnames(servico_agua_esgoto_norte)) 
  
  # Verify column existence before proceeding
  if (!"populacao_urbana_atendida_agua" %in% colnames(servico_agua_esgoto_norte)) {
    cli::cli_alert_danger("Erro crítico: Coluna 'populacao_urbana_atendida_agua' não encontrada no dataframe para detecção de outliers. Verifique a origem dos dados.")
    stop("Coluna essencial 'populacao_urbana_atendida_agua' ausente.")
  }
  if (!"volume_agua_produzido" %in% colnames(servico_agua_esgoto_norte)) {
    cli::cli_alert_danger("Erro crítico: Coluna 'volume_agua_produzido' não encontrada no dataframe para detecção de outliers. Verifique a origem dos dados.")
    stop("Coluna essencial 'volume_agua_produzido' ausente.")
  }
  
  # Usando [[ ]] para acesso mais robusto à coluna
  q_agua_norte <- quantile(servico_agua_esgoto_norte[["populacao_urbana_atendida_agua"]], c(.25, .75), na.rm = TRUE)
  iqr_agua_norte <- diff(q_agua_norte)
  lim_agua_norte <- list(inf = q_agua_norte[1] - 1.5 * iqr_agua_norte, sup = q_agua_norte[2] + 1.5 * iqr_agua_norte)
  
  servico_agua_esgoto_norte <- servico_agua_esgoto_norte |>
    mutate(outlier_agua_atendida = case_when(
      .data[["populacao_urbana_atendida_agua"]] < lim_agua_norte$inf ~ "Inferior", # Usando .data[[]] para garantir o escopo
      .data[["populacao_urbana_atendida_agua"]] > lim_agua_norte$sup ~ "Superior",
      TRUE ~ "Normal"
    ))
  
  n_out_agua_norte <- servico_agua_esgoto_norte |> filter(outlier_agua_atendida != "Normal") |> nrow()
  cli::cli_alert_info(paste0("Total de outliers (População Atendida por Água - Região Norte): ", n_out_agua_norte,
                             " (", round(100 * n_out_agua_norte / nrow(servico_agua_esgoto_norte), 2), "% dos dados )"))
  
  q_volume_norte <- quantile(servico_agua_esgoto_norte[["volume_agua_produzido"]], c(.25, .75), na.rm = TRUE)
  iqr_volume_norte <- diff(q_volume_norte)
  lim_volume_norte <- list(inf = q_volume_norte[1] - 1.5 * iqr_volume_norte, sup = q_volume_norte[2] + 1.5 * iqr_volume_norte)
  
  servico_agua_esgoto_norte <- servico_agua_esgoto_norte |>
    mutate(outlier_volume_produzido = case_when(
      .data[["volume_agua_produzido"]] < lim_volume_norte$inf ~ "Inferior",
      .data[["volume_agua_produzido"]] > lim_volume_norte$sup ~ "Superior",
      TRUE ~ "Normal"
    ))
  
  n_out_volume_norte <- servico_agua_esgoto_norte |> filter(outlier_volume_produzido != "Normal") |> nrow()
  cli::cli_alert_info(paste0("Total de outliers (Volume de Água Produzido - Região Norte): ", n_out_volume_norte,
                             " (", round(100 * n_out_volume_norte / nrow(servico_agua_esgoto_norte), 2), "% dos dados )"))
  
  cli::cli_h2("Tabela de Observações Consideradas Outliers (População Atendida por Água - Região Norte)")
  outliers_table <- servico_agua_esgoto_norte |>
    filter(outlier_agua_atendida != "Normal") |>
    select(ano, sigla_uf, sigla_uf_nome, populacao_urbana_atendida_agua, outlier_agua_atendida, regiao)
  
  if (nrow(outliers_table) > 0) {
    print(datatable(outliers_table, options = list(pageLength = 5),
                    caption = "Observações consideradas outliers (População Atendida por Água - Região Norte)"))
  } else {
    cli::cli_alert_info("Nenhum outlier de população atendida por água encontrado na Região Norte.")
  }
  
  return(servico_agua_esgoto_norte)
}