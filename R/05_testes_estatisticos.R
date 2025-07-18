# R/05_testes_estatisticos.R

realizar_testes_e_cohen_d <- function(servico_agua_esgoto_norte) {
  cli::cli_h1("Testes de Hipótese e Tamanho de Efeito (Cohen's d) - Região Norte")
  
  teste_completo <- function(x, y, grupo1, grupo2, medida = "Média") {
    cat(paste0("\n### ", grupo1, " × ", grupo2, " (Comparação de ", medida, ")\n"))
    
    x <- na.omit(x)
    y <- na.omit(y)
    
    if (length(x) == 0 || length(y) == 0) {
      cat("Erro: Um dos grupos está vazio após remoção de NAs. Impossível realizar o teste.\n")
      return(NULL)
    }
    
    t_res <- t.test(x, y)
    ic_x <- t.test(x)$conf.int
    ic_y <- t.test(y)$conf.int
    
    cat("n =", length(x), "/", length(y), "\n")
    cat(medida, " ", grupo1, " =", round(mean(x), 2),
        "IC95% [", round(ic_x[1], 2), ";", round(ic_x[2], 2), "]\n")
    cat(medida, " ", grupo2, " =", round(mean(y), 2),
        "IC95% [", round(ic_y[1], 2), ";", round(ic_y[2], 2), "]\n")
    cat("Diferença de ", medida, " IC95%: [", round(t_res$conf.int[1], 2), ";", round(t_res$conf.int[2], 2), "]\n")
    cat("t =", round(t_res$statistic, 3), "p =", round(t_res$p.value, 4),
        ifelse(t_res$p.value < .05, "→ REJEITA H0", "→ não rejeita H0"), "\n")
    
    f_res <- var.test(x, y)
    cat("F =", round(f_res$statistic, 3), "p =", round(f_res$p.value, 4),
        ifelse(f_res$p.value < .05, "→ REJEITA H0", "→ não rejeita H0"), "\n")
    
    d <- effsize::cohen.d(x, y)$estimate
    classe_d <- ifelse(abs(d) < 0.2, "muito pequeno",
                       ifelse(abs(d) < 0.5, "pequeno",
                              ifelse(abs(d) < 0.8, "médio", "grande")))
    cat("Cohen's d =", round(d, 2), "→ efeito", classe_d, "\n")
    
    # Adicionando a interpretação do teste de hipótese e do tamanho do efeito
    cat("  **Interpretação**: ")
    if (t_res$p.value < 0.05) {
      if (mean(x) > mean(y)) {
        cat(paste0("Existe uma diferença estatisticamente significativa no ", tolower(medida), " entre ", grupo1, " e ", grupo2, ", com ", grupo1, " apresentando um valor médio maior. "))
      } else {
        cat(paste0("Existe uma diferença estatisticamente significativa no ", tolower(medida), " entre ", grupo1, " e ", grupo2, ", com ", grupo2, " apresentando um valor médio maior. "))
      }
    } else {
      cat(paste0("Não há diferença estatisticamente significativa no ", tolower(medida), " entre ", grupo1, " e ", grupo2, ". "))
    }
    cat(paste0("O tamanho do efeito é ", classe_d, " (Cohen's d = ", round(d, 2), "), indicando que a diferença prática é ", classe_d, ". "))
    
    if (f_res$p.value < 0.05) {
      cat("As variâncias dos dois grupos também são significativamente diferentes.\n")
    } else {
      cat("As variâncias dos dois grupos não são significativamente diferentes.\n")
    }
    cat("\n") # Adiciona uma linha em branco para melhor separação
  }
  
  ufs_norte_com_dados <- servico_agua_esgoto_norte |>
    group_by(sigla_uf) |>
    summarise(n = n_distinct(id_municipio_nome), .groups = "drop") |>
    filter(n > 50) |> # Critério para ter dados suficientes para comparação
    pull(sigla_uf)
  
  if (length(ufs_norte_com_dados) >= 2) {
    # Gera todas as combinações de 2 UFs para comparação
    pares_ufs_para_comparacao <- combn(ufs_norte_com_dados, 2)
    for (i in 1:ncol(pares_ufs_para_comparacao)) {
      uf1 <- pares_ufs_para_comparacao[1, i]
      uf2 <- pares_ufs_para_comparacao[2, i]
      
      # Comparação para Percentual Atendido Água
      cli::cli_h2(paste0("Teste: Percentual de População Atendida por Água - ", uf1, " vs. ", uf2, " (Região Norte)"))
      data_uf1_perc_agua <- servico_agua_esgoto_norte |> filter(sigla_uf == uf1) |> pull(perc_agua_atendida)
      data_uf2_perc_agua <- servico_agua_esgoto_norte |> filter(sigla_uf == uf2) |> pull(perc_agua_atendida)
      teste_completo(data_uf1_perc_agua, data_uf2_perc_agua, uf1, uf2, "Percentual Atendido Água")
      
      # Comparação para Volume de Água Produzido
      cli::cli_h2(paste0("Teste: Volume de Água Produzido - ", uf1, " vs. ", uf2, " (Região Norte)"))
      data_uf1_volume <- servico_agua_esgoto_norte |> filter(sigla_uf == uf1) |> pull(volume_agua_produzido)
      data_uf2_volume <- servico_agua_esgoto_norte |> filter(sigla_uf == uf2) |> pull(volume_agua_produzido)
      teste_completo(data_uf1_volume, data_uf2_volume, uf1, uf2, "Volume de Água Produzido")
    }
  } else {
    cli::cli_alert_info("Não há UFs suficientes na Região Norte com dados para realizar testes comparativos entre elas.")
    cli::cli_alert_info("Considere ajustar o critério de 'n > 50' ou realizar outros tipos de testes se apropriado.")
  }
}