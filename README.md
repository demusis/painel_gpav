# Dashboard: Análise de Laudos Periciais

**Versão 6 – Métricas, SLA e Predição de Atrasos**


---

## Objetivo

Este aplicativo interativo foi criado para **análise exploratória e preditiva de laudos periciais**. Ele permite:

- Visualização e filtragem de dados de laudos;
- Cálculo de métricas temporais como SLA e atraso;
- Visualizações gráficas por categorias;
- Predição de atraso na entrega dos laudos utilizando modelos estatísticos.

---

## Estrutura do App

O app foi desenvolvido em **R** utilizando o framework **Shiny**, com estrutura baseada em `shinydashboard`.

### 1. Interface

A interface contém três abas principais:

- **Dados carregados/calculados**: Visualização da base de dados com filtros dinâmicos;
- **Visualizações**: Gráficos de frequência e percentuais por categoria;
- **Análises**: Modelos estatísticos para predição de atrasos e avaliação de SLA.

### 2. Funcionalidades principais

- **Upload de arquivos** `.csv`, `.xls`, `.xlsx`;
- **Limpeza e padronização automática dos dados**;
- **Filtros dinâmicos** de texto e datas;
- **Cálculo automático de métricas**:
  - `tempo_dedicado_ao_laudo`
  - `tempo_estimado_total`
  - `atraso_na_entrega`
- **Visualizações gráficas** (barras, boxplots);
- **Cálculo do SLA** (% de laudos entregues no prazo);
- **Modelos preditivos**:
  - Regressão logística (`glm`)
  - Árvore de decisão (`rpart`)

---

## Requisitos

- R (>= 4.0)
- Pacotes necessários:

```r
shiny, shinydashboard, dplyr, readxl, lubridate, ggplot2,
DT, tidyr, janitor, rpart, rpart.plot, broom, scales
