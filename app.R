###############################################################################
#  DASHBOARD: Análise de Laudos Periciais
#  v6  –  cálculo robusto de métricas  •  SLA  •  predição de atraso
#  Autor: Carlo
###############################################################################

# ─────────────────────────  PACOTES  ─────────────────────────
library(shiny);  library(shinydashboard)
library(dplyr);  library(readxl);      library(lubridate)
library(ggplot2);library(DT);          library(tidyr);     library(janitor)
library(rpart);  library(rpart.plot);  library(broom);     library(scales)

# ─────────────────────────  UTILIDADES  ──────────────────────
get_sober_blue_palette <- function(n){
  if(n<=2)       return(c("#2C3E50","#34495E"))
  else if(n<=5)  return(c("#2C3E50","#34495E","#4A607C","#5C7691","#6F8DA6"))
  colorRampPalette(c("#2C3E50","#6F8DA6"))(n)
}
pretty_label <- function(col) stringr::str_to_sentence(gsub("_"," ",col))

colunas_texto <- c(
  "cobranca_da_autoridade","prioridades_legais","status_da_atribuicao",
  "perito","status_do_laudo","perito_revisor_do_laudo_pericial",
  "pendencia_no_caso"
)

# ─────────────────────────  UI  ──────────────────────────────
ui <- dashboardPage(
  dashboardHeader(title = "Análise de Laudos Periciais", titleWidth = 350),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Dados carregados/calculados",      tabName = "dados",         icon = icon("file-upload")),
      menuItem("Visualizações",       tabName = "visualizacoes", icon = icon("chart-bar")),
      menuItem("Análises",  tabName = "avancadas",     icon = icon("chart-line"))
    ),
    fileInput("file_upload", "Escolha o arquivo Excel/CSV",
              accept = c(".xlsx", ".xls", ".csv")),
    tags$hr(), h4("Filtros de Texto e Categoria"), uiOutput("ui_filtros_texto"),
    tags$hr(), h4("Filtros de Data"),
    uiOutput("filtro_data_atribuicao"), uiOutput("filtro_data_entrega")
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .main-header .logo{font-weight:bold;font-size:20px;color:#2C3E50;}
      .main-header .navbar{background:#34495E;}
      .sidebar-menu li a{color:#ecf0f1;}
      .sidebar-menu li.active a{border-left-color:#1ABC9C;}
      .skin-blue .main-sidebar{background:#2C3E50;}
      .box.box-solid.box-primary>.box-header{color:#fff;background:#34495E;}
      .box.box-solid.box-primary{border-color:#34495E;}"))),
    tabItems(
      tabItem(tabName="dados",
              h2("Visualização dos Dados Carregados"), DTOutput("tabela_dados")),
      tabItem(tabName="visualizacoes",
              h2("Visualizações Gráficas"),
              fluidRow(
                box("Frequência: Prioridades Legais",  status="primary", solidHeader=TRUE,
                    width=6, plotOutput("plot_prioridades_legais_abs")),
                box("Percentual: Prioridades Legais", status="primary", solidHeader=TRUE,
                    width=6, plotOutput("plot_prioridades_legais_perc"))
              ),
              fluidRow(
                box("Frequência: Status da Atribuição",  status="primary", solidHeader=TRUE,
                    width=6, plotOutput("plot_status_atribuicao_abs")),
                box("Percentual: Status da Atribuição", status="primary", solidHeader=TRUE,
                    width=6, plotOutput("plot_status_atribuicao_perc"))
              ),
              fluidRow(
                box("Frequência: Status do Laudo",       status="primary", solidHeader=TRUE,
                    width=6, plotOutput("plot_status_laudo_abs")),
                box("Percentual: Status do Laudo",      status="primary", solidHeader=TRUE,
                    width=6, plotOutput("plot_status_laudo_perc"))
              ),
              fluidRow(
                box("Cobrança da Autoridade × Status da Atribuição", status="primary",
                    solidHeader=TRUE, width=12, plotOutput("plot_cobranca_status_atribuicao"))
              ),
              fluidRow(
                box("Boxplot: Tempo dedicado × Complexidade", status="primary",
                    solidHeader=TRUE, width=6, plotOutput("box_tempo_dedicado"))
              ),
              fluidRow(valueBoxOutput("sla_value", width = 4))
      ),
      tabItem(tabName="avancadas",
              h2("Previsão de Atraso na Entrega"),
              fluidRow(
                box("Logistic Regression (Resumo)", width=6, status="primary",
                    solidHeader=TRUE, verbatimTextOutput("logreg_summary")),
                box("Árvore de Decisão (rpart)",    width=6, status="primary",
                    solidHeader=TRUE, plotOutput("tree_plot"))
              ),
              fluidRow(
                box("Probabilidade de Atraso por Caso", width=12, status="primary",
                    solidHeader=TRUE, DTOutput("tabela_risco_atraso"))
              )
      )
    )
  )
)

# ─────────────────────────  SERVER  ──────────────────────────
server <- function(input, output, session){
  
  dados_brutos <- reactiveVal()
  dados_processados <- reactiveVal()
  
  # ---------- 1. Upload ---------------------------------------------------
  observeEvent(input$file_upload, {
    req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    df <- switch(ext,
                  "csv" = read.csv(input$file_upload$datapath, sep=",", header=TRUE,
                                    stringsAsFactors=FALSE, encoding="UTF-8"),
                  "xls" = read_excel(input$file_upload$datapath),
                  "xlsx" = read_excel(input$file_upload$datapath),
                  NULL)
    validate(need(!is.null(df), "Falha ao ler o arquivo."))
    
    # 1.1 limpar nomes
    df <- clean_names(df, ascii = TRUE)
    
    # 1.2 converter datas
    date_cols <- c("data_da_atribuicao",
                   "data_de_entrega_do_laudo_pericial",
                   "data_prevista_de_termino_da_pericia")
    for(dc in intersect(date_cols, names(df))) df[[dc]] <- as.Date(df[[dc]])
    
    # 1.3 Natureza -> colunas booleanas
    if ("natureza_da_pericia" %in% names(df)) {
      nats <- df |> pull(natureza_da_pericia) |> strsplit(",") |>
        unlist() |> trimws() |> unique()
      for(nat in nats){
        col_bool <- paste0("natureza_da_pericia_", gsub(" ", "_", tolower(nat)))
        df[[col_bool]] <- grepl(nat, df$natureza_da_pericia, ignore.case=TRUE)
      }
    }
    
    # 1.4 Recalcular sempre atraso_na_entrega (nova regra + checagem NA)
    df <- df |>
      mutate(
        atraso_na_entrega = case_when(
          status_do_laudo == "Concluído" &
            !is.na(data_de_entrega_do_laudo_pericial) &
            !is.na(data_prevista_de_termino_da_pericia) &
            data_de_entrega_do_laudo_pericial > data_prevista_de_termino_da_pericia
          ~ "Sim",
          status_do_laudo != "Concluído" &
            !is.na(data_prevista_de_termino_da_pericia) &
            Sys.Date() > data_prevista_de_termino_da_pericia
          ~ "Sim",
          status_do_laudo == "Concluído" ~ "Não",
          TRUE ~ NA_character_
        )
      )
    
    # 1.5 Métricas de tempo (criam, mesmo se ficarem NA)
    df <- df |>
      mutate(
        tempo_dedicado_ao_laudo = case_when(
          status_do_laudo == "Concluído" &
            !is.na(data_de_entrega_do_laudo_pericial) &
            !is.na(data_da_atribuicao) ~
            as.numeric(data_de_entrega_do_laudo_pericial - data_da_atribuicao),
          TRUE ~ NA_real_
        ),
        tempo_estimado_total = ifelse(
          !is.na(data_prevista_de_termino_da_pericia) &
            !is.na(data_da_atribuicao),
          as.numeric(data_prevista_de_termino_da_pericia - data_da_atribuicao),
          NA_real_
        ),
        tempo_estimado_restante = ifelse(
          !is.na(data_prevista_de_termino_da_pericia),
          as.numeric(data_prevista_de_termino_da_pericia - Sys.Date()),
          NA_real_
        )
      )
    
    dados_brutos(df)
  })
  
  observe({ req(dados_brutos()); dados_processados(dados_brutos()) })
  
  # ---------- 2. Filtros --------------------------------------------------
  output$ui_filtros_texto <- renderUI({
    df <- dados_processados(); req(df)
    tagList(lapply(intersect(colunas_texto, names(df)), \(col){
      checkboxGroupInput(paste0("filtro_", col),
                         label = paste("Filtrar por", pretty_label(col)),
                         choices = sort(unique(df[[col]])),
                         selected = sort(unique(df[[col]]))) # <--- MANTER ESTA LINHA: SELECIONA TODOS POR PADRÃO
    }))
  })
  
  observeEvent(dados_processados(), {
    df <- dados_processados(); req(df)
    if ("data_da_atribuicao" %in% names(df)){
      output$filtro_data_atribuicao <- renderUI({
        dateRangeInput("filtro_data_atribuicao", "Data da Atribuição",
                       min(df$data_da_atribuicao, na.rm=TRUE),
                       max(df$data_da_atribuicao, na.rm=TRUE)) # <--- MANTER ESTAS LINHAS: DEFINE O INTERVALO MÁXIMO
      })
    }
    if ("data_de_entrega_do_laudo_pericial" %in% names(df)){
      output$filtro_data_entrega <- renderUI({
        dateRangeInput("filtro_data_entrega", "Data de Entrega",
                       min(df$data_de_entrega_do_laudo_pericial, na.rm=TRUE),
                       max(df$data_de_entrega_do_laudo_pericial, na.rm=TRUE)) # <--- MANTER ESTAS LINHAS: DEFINE O INTERVALO MÁXIMO
      })
    }
  }, ignoreNULL=FALSE)
  
  # ---------- 3. Dados filtrados -----------------------------------------
  dados_filtrados <- reactive({
    df <- dados_processados(); req(df)
    # Aplica filtros de texto/categoria
    for (col in colunas_texto) {
      id <- paste0("filtro_", col)
      if (!is.null(input[[id]]) && length(input[[id]]) > 0) {
        df <- df |> filter(
          is.na(.data[[col]]) |                       # mantém linhas sem valor
            .data[[col]] %in% input[[id]]               # ou que batem no checkbox
        )
      }
    }
    
    
    
    # Adicionei uma condição para garantir que, se os filtros de data não forem renderizados (e, portanto, input$filtro_data_atribuicao seja NULL), não haverá erro e nenhum filtro de data será aplicado.
    # Se os filtros de data forem renderizados, eles já vêm com o intervalo máximo por padrão, não restringindo.
    # dentro de dados_filtrados()
    if (!is.null(input$filtro_data_atribuicao)) {
      df <- df |> filter(
        is.na(data_da_atribuicao) |                          # mantém NAs
          (data_da_atribuicao >= input$filtro_data_atribuicao[1] &
             data_da_atribuicao <= input$filtro_data_atribuicao[2])
      )
    }
    
    if (!is.null(input$filtro_data_entrega)) {
      df <- df |> filter(
        is.na(data_de_entrega_do_laudo_pericial) |           # mantém NAs
          (data_de_entrega_do_laudo_pericial >= input$filtro_data_entrega[1] &
             data_de_entrega_do_laudo_pericial <= input$filtro_data_entrega[2])
      )
    }
    
    df
  })
  




  # ---------- 4. Tabela ---------------------------------------------------
  output$tabela_dados <- renderDT({
    datatable(dados_filtrados(), options = list(pageLength=10, scrollX=TRUE))
  })
  
  # ---------- 5. Funções de gráfico --------------------------------------
  render_freq_plots <- function(col, titulo, data){
    dfp <- data |> count(.data[[col]]) |>
      mutate(pct = n/sum(n)*100, lbl = paste0(round(pct,1),"%"))
    pal <- get_sober_blue_palette(nrow(dfp))
    p_abs <- ggplot(dfp, aes(reorder(.data[[col]], -n), n, fill=.data[[col]])) +
      geom_col() + geom_text(aes(label=n), vjust=-0.5) +
      scale_fill_manual(values=pal) +
      labs(title=paste(titulo,"(Absoluta)"), x="", y="Frequência") +
      theme_minimal() + theme(legend.position="none",
                              axis.text.x=element_text(angle=45,hjust=1))
    p_pct <- ggplot(dfp, aes(reorder(.data[[col]], -pct), pct, fill=.data[[col]])) +
      geom_col() + geom_text(aes(label=lbl), vjust=-0.5) +
      scale_fill_manual(values=pal) +
      labs(title=paste(titulo,"(Percentual)"), x="", y="Percentual") +
      theme_minimal() + theme(legend.position="none",
                              axis.text.x=element_text(angle=45,hjust=1))
    list(abs=p_abs, perc=p_pct)
  }
  
  output$plot_prioridades_legais_abs  <- renderPlot({
    render_freq_plots("prioridades_legais","Prioridades Legais",dados_filtrados())$abs })
  output$plot_prioridades_legais_perc <- renderPlot({
    render_freq_plots("prioridades_legais","Prioridades Legais",dados_filtrados())$perc })
  
  output$plot_status_atribuicao_abs  <- renderPlot({
    render_freq_plots("status_da_atribuicao","Status da Atribuição",dados_filtrados())$abs })
  output$plot_status_atribuicao_perc <- renderPlot({
    render_freq_plots("status_da_atribuicao","Status da Atribuição",dados_filtrados())$perc })
  
  output$plot_status_laudo_abs  <- renderPlot({
    render_freq_plots("status_do_laudo","Status do Laudo",dados_filtrados())$abs })
  output$plot_status_laudo_perc <- renderPlot({
    render_freq_plots("status_do_laudo","Status do Laudo",dados_filtrados())$perc })
  
  output$plot_cobranca_status_atribuicao <- renderPlot({
    dfp <- dados_filtrados() |> count(cobranca_da_autoridade, status_da_atribuicao)
    ggplot(dfp, aes(cobranca_da_autoridade, n, fill=status_da_atribuicao)) +
      geom_col(position=position_dodge(.8), width=.7) +
      geom_text(aes(label=n), position=position_dodge(.8), vjust=-0.5) +
      scale_fill_manual(values=get_sober_blue_palette(length(unique(dfp$status_da_atribuicao)))) +
      labs(x="Cobrança da Autoridade", y="Frequência",
           title="Cobrança × Status", fill="Status") +
      theme_minimal() + theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  # ---------- 6. Boxplot --------------------------------------------------
  output$box_tempo_dedicado <- renderPlot({
    df <- dados_filtrados()
    validate(need("tempo_dedicado_ao_laudo" %in% names(df),
                  "Coluna tempo_dedicado_ao_laudo ausente no dataset."))
    df <- df |> filter(!is.na(tempo_dedicado_ao_laudo),
                       !is.na(complexidade_do_caso))
    validate(need(nrow(df) > 0, "Sem dados para o boxplot."))
    ggplot(df, aes(complexidade_do_caso, tempo_dedicado_ao_laudo,
                   fill=complexidade_do_caso)) +
      geom_boxplot(outlier.shape=21, alpha=.6) +
      scale_fill_manual(values=get_sober_blue_palette(length(unique(df$complexidade_do_caso)))) +
      labs(x="Complexidade", y="Tempo dedicado (dias)",
           title="Tempo dedicado × Complexidade") +
      theme_minimal() + theme(legend.position="none")
  })
  
  # ---------- 7. SLA ------------------------------------------------------
  output$sla_value <- renderValueBox({
    df <- dados_filtrados() |> filter(status_do_laudo=="Concluído",
                                      !is.na(atraso_na_entrega))
    if(nrow(df)==0)
      return(valueBox("—","Sem laudos concluídos",icon=icon("info"),color="light"))
    taxa <- mean(df$atraso_na_entrega=="Não")
    valueBox(
      scales::percent(taxa, .1), "SLA - % no prazo",
      icon  = icon(ifelse(taxa>=.9,"thumbs-up","exclamation-triangle")),
      color = ifelse(taxa>=.9,"teal", ifelse(taxa>=.75,"yellow","red"))
    )
  })
  
  # ---------- 8. Modelos --------------------------------------------------
  modelo_atraso <- reactive({
    df <- dados_processados(); req(df)
    mod_df <- df |>
      select(atraso_na_entrega,
             complexidade_do_caso,
             # quantidade_de_arquivos, 
             duracao_dos_arquivos_em_minutos,
             tempo_estimado_total) |> na.omit()
    if(nrow(mod_df) < 10 || length(unique(mod_df$atraso_na_entrega)) < 2) return(NULL)

    mod_df$atraso_na_entrega <- factor(mod_df$atraso_na_entrega)
    mod_df$complexidade_do_caso = factor(mod_df$complexidade_do_caso)
    
    list(glm  = glm(atraso_na_entrega~., data=mod_df, family=binomial),
         # tree = rpart(atraso_na_entrega~., data=mod_df, method="class"))
    
         tree = rpart(atraso_na_entrega~., data=mod_df, method="class",
                      control = rpart.control(cp = 0.01, minsplit = 5, minbucket = 3)))
  })
  
  output$logreg_summary <- renderPrint({
    mods <- modelo_atraso()
    if(is.null(mods)){
      cat("Dados insuficientes para ajustar o modelo.\n",
          "Necessário ≥ 10 registros com 'Sim' e 'Não'.")
    } else {
      print(broom::tidy(mods$glm, exponentiate=TRUE
                        , conf.int=TRUE
                        ))
    }
  })
  
  output$tree_plot <- renderPlot({
    mods <- modelo_atraso()
    validate(need(!is.null(mods), "Dados insuficientes para a árvore."))
    rpart.plot(mods$tree, extra=104, box.palette="GnBu", shadow.col="gray")
  })
  
  output$tabela_risco_atraso <- renderDT({
    mods <- modelo_atraso()
    df   <- dados_filtrados()
    if(is.null(mods) || nrow(df)==0)
      return(datatable(data.frame(Mensagem="Modelo ou dados insuficientes."),
                       options=list(dom='t')))
    pred <- unname(predict(mods$glm, newdata=df, type="response"))
    datatable(cbind(df, risco_atraso_percent = percent(pred, 0.1)),
              options=list(pageLength=10, scrollX=TRUE))
  })
}

# ─────────────────────────  RUN APP  ─────────────────────────
shinyApp(ui, server)
