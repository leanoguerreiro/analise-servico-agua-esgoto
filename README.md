# Análise do Serviço de Água e Esgoto na Região Norte do Brasil

Este repositório contém um projeto de análise de dados focado na cobertura e gestão dos serviços de abastecimento de água e esgotamento sanitário na Região Norte do Brasil, utilizando dados do período de 2012 a 2021. O objetivo principal é explorar as disparidades na cobertura de saneamento, analisar volumes produzidos e consumidos, identificar outliers e determinar a significância estatística das diferenças observadas entre as Unidades da Federação (UFs) da região.

---

## 📑 Sumário

- [Visão Geral](#visão-geral)
- [Estrutura do Projeto](#estrutura-do-projeto)
- [Dados](#dados)
- [Metodologia](#metodologia)
- [Como Executar o Projeto](#como-executar-o-projeto)
- [Resultados Principais](#resultados-principais)
- [Contribuições](#contribuições)
- [Licença](#licença)

---

## 📌 Visão Geral

O saneamento básico é crucial para a saúde pública e o desenvolvimento socioeconômico. A Região Norte do Brasil apresenta desafios únicos devido às suas particularidades geográficas e demográficas. Este projeto visa fornecer insights valiosos para a formulação de políticas públicas e o direcionamento de investimentos em saneamento, especificamente para essa região.

A análise combina:

- **Análise Exploratória de Dados (EDA)** para compreender a estrutura e características do dataset.
- **Visualizações Geográficas (Mapas Coropléticos)** para identificar padrões espaciais.
- **Detecção de Outliers** para identificar observações atípicas que podem representar grandes infraestruturas ou anomalias.
- **Testes de Hipótese e Cálculo do Tamanho de Efeito (Cohen's d)** para mensurar a significância e magnitude das diferenças entre as UFs.

---

## 📁 Estrutura do Projeto

```
leanoguerreiro-analise-servico-agua-esgoto/
├── main_analise.R                # Script principal
├── relatorio_final.Rmd           # Relatório em R Markdown
├── servicos_agua_esgoto.Rproj    # Projeto RStudio
├── dados/
│   ├── estados.rds               # Shapefile dos estados
│   └── servico_agua_escoto.parquet # Dados brutos
└── R/
    ├── 00_download_data.R
    ├── 01_pacotes_e_leitura.R
    ├── 02_preparacao_dados.R
    ├── 03_eda_e_outliers.R
    ├── 04_visualizacoes_mapas.R
    ├── 05_testes_estatisticos.R
    ├── 06_graficos_adicionais.R
    └── 07_reflexoes_finais.R
```

---

## 📊 Dados

Fonte: **Sistema Nacional de Informações sobre Saneamento (SNIS)** via **Base dos Dados (BigQuery)**.

### Principais Variáveis:

- `ano`
- `id_municipio`, `id_municipio_nome`
- `sigla_uf`, `sigla_uf_nome`
- `populacao_urbana`, `populacao_urbana_atendida_agua`, `populacao_urbana_atendida_esgoto`
- `volume_agua_produzido`, `volume_agua_tratada_eta`, `volume_agua_consumido`

Também é utilizado um **shapefile de estados brasileiros** para visualizações.

---

## ⚙️ Metodologia

1. **Download e Carregamento de Dados** via BigQuery (Parquet) + shapefile com `geobr`.

2. **Preparação de Dados**:
   - Geometrias agregadas por região.
   - Adição da coluna `regiao`.

3. **EDA**:
   - Identificação de NAs, especialmente em `populacao_urbana_atendida_esgoto`.
   - Cálculo de percentuais corrigidos.

4. **Filtragem Região Norte** e estatísticas por UF.

5. **Visualizações**:
   - Boxplots de cobertura.
   - Mapas coropléticos nacionais e regionais.

6. **Outliers**:
   - Método IQR para detectar anomalias.

7. **Testes de Hipótese** e **Cohen's d**:
   - Comparações entre pares de UFs.

8. **Gráficos Adicionais**:
   - Séries temporais.
   - Comparações de volume produzido vs. consumido.
   - Participação da população urbana por UF.

9. **Conclusão** no arquivo `relatorio_final.Rmd`.

---

## ▶️ Como Executar o Projeto

### Pré-requisitos

- **R e RStudio**
- **Variáveis de Ambiente** em `.Renviron`:

```r
EMAIL="seu_email@gmail.com"
GCP_BILLING_ID="seu-projeto-de-faturamento-no-gcp"
```

### Passos

```bash
git clone https://github.com/leanoguerreiro/analise-servico-agua-esgoto.git
cd analise-servico-agua-esgoto
```

- Abra `servicos_agua_esgoto.Rproj` no RStudio.

- Instale os pacotes:

```r
install.packages(c("tidyverse", "arrow", "scales", "skimr", "sf", "psych", 
                   "DT", "effsize", "cli", "gtools", "basedosdados", 
                   "bigrquery", "geobr"))
```

- Execute a análise:

```r
source("main_analise.R")
```

- Para gerar o relatório:

```r
rmarkdown::render("relatorio_final.Rmd")
```

---

## 📈 Resultados Principais

- **Dados Ausentes**: NAs impactam a confiabilidade, especialmente em `populacao_urbana_atendida_esgoto`.

- **Disparidades Regionais**: Região Norte tem a **menor cobertura nacional**.

- **Disparidades Intra-regionais**: 
  - **AP** e **PA** com piores indicadores.
  - **RR** e **TO** apresentam melhores desempenhos.

- **Eficiência Hídrica**: Grandes perdas observadas entre produção e consumo, especialmente no **AM**.

- **Outliers**: Centros urbanos se destacam em `população atendida` e `volume produzido`.

- **Significância Estatística**: Diferenças entre UFs confirmadas com **testes de hipótese** e **Cohen’s d**.

---

## 🤝 Contribuições

Contribuições são bem-vindas! Sinta-se à vontade para abrir issues ou pull requests para:

- Melhorias no código
- Análises adicionais
- Correções de bugs

---

## 📄 Licença

Este projeto está licenciado sob a **Licença MIT**. Veja o arquivo `LICENSE` para mais detalhes.
