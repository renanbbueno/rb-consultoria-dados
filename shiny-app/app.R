library(shiny)

score_label <- function(score) {
  if (score < 35) return("Base inicial")
  if (score < 65) return("Em estruturacao")
  if (score < 85) return("Boa maturidade")
  "Alta maturidade"
}

score_class <- function(score) {
  if (score < 35) return("score-low")
  if (score < 65) return("score-mid")
  if (score < 85) return("score-good")
  "score-high"
}

recommendations_for <- function(score, organizacao, qualidade, visualizacao, automacao, entregas) {
  recs <- character()

  if (organizacao < 50) {
    recs <- c(recs, "Priorizar organizacao da base: padronizar colunas, unidades, identificadores e dicionario de dados.")
  }
  if (qualidade < 50) {
    recs <- c(recs, "Executar checagem de qualidade: dados ausentes, inconsistencias, outliers e validacao por regra tecnica.")
  }
  if (visualizacao < 50) {
    recs <- c(recs, "Criar visualizacoes iniciais para comunicar padroes, comparacoes e tendencias de forma objetiva.")
  }
  if (automacao < 50) {
    recs <- c(recs, "Automatizar etapas repetitivas para reduzir retrabalho e tornar a analise reproduzivel.")
  }
  if ("Dashboard" %in% entregas) {
    recs <- c(recs, "Planejar um dashboard com indicadores principais, filtros e leitura rapida para decisao.")
  }
  if ("Relatorio tecnico" %in% entregas) {
    recs <- c(recs, "Estruturar um relatorio tecnico com metodologia, resultados, graficos e interpretacao aplicada.")
  }
  if (score >= 85) {
    recs <- c(recs, "A base parece madura para analises preditivas, modelos comparativos e produtos automatizados.")
  }
  if (!length(recs)) {
    recs <- c(recs, "O caminho recomendado e uma revisao rapida da base seguida de uma analise exploratoria orientada ao objetivo final.")
  }

  unique(recs)
}

ui <- fluidPage(
  tags$head(
    tags$title("Diagnostico Interativo - RB Consultoria em Dados"),
    tags$style(HTML("
      :root{
        --verde-claro:#99CD85;
        --verde-suave:#CFE0BC;
        --verde-medio:#7FA653;
        --verde-escuro:#63783D;
        --verde-profundo:#0B2F25;
        --off:#F7FAF4;
        --texto:#1D241F;
        --cinza:#6C756E;
      }
      body{
        margin:0;
        background:linear-gradient(135deg,#F7FAF4 0%,#EAF2E4 100%);
        color:var(--texto);
        font-family:'Segoe UI',Arial,sans-serif;
      }
      .container-fluid{padding:0;}
      .app-header{
        padding:34px 6vw;
        color:white;
        background:linear-gradient(135deg,var(--verde-profundo),var(--verde-escuro));
      }
      .app-header h1{
        margin:0;
        font-family:Georgia,'Times New Roman',serif;
        font-size:clamp(2rem,4vw,4.2rem);
        line-height:1.05;
      }
      .app-header p{max-width:820px;margin:18px 0 0;color:rgba(255,255,255,.84);font-size:1.05rem;line-height:1.65;}
      .app-wrap{padding:34px 6vw 54px;}
      .grid{display:grid;grid-template-columns:minmax(300px,430px) 1fr;gap:24px;align-items:start;}
      .panel{
        background:rgba(255,255,255,.92);
        border:1px solid rgba(99,120,61,.12);
        border-radius:18px;
        box-shadow:0 18px 42px rgba(23,55,35,.10);
        padding:26px;
      }
      .panel h2,.panel h3{margin-top:0;color:var(--verde-profundo);}
      label{font-weight:800;color:#2F3A33;}
      .form-control,.selectize-input{border-radius:10px;border-color:#d9e4d1;box-shadow:none;}
      .irs--shiny .irs-bar,.irs--shiny .irs-single{background:var(--verde-medio);border-color:var(--verde-medio);}
      .score-card{display:grid;grid-template-columns:190px 1fr;gap:22px;align-items:center;margin-bottom:22px;}
      .score-circle{
        width:170px;height:170px;border-radius:50%;display:grid;place-items:center;
        color:white;font-weight:900;font-size:3rem;box-shadow:0 18px 36px rgba(23,55,35,.18);
      }
      .score-low{background:#B55B4D;}.score-mid{background:#B18A37;}.score-good{background:var(--verde-medio);}.score-high{background:var(--verde-profundo);}
      .score-copy strong{display:block;font-size:1.35rem;color:var(--verde-profundo);margin-bottom:8px;}
      .metric-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin:20px 0;}
      .metric{background:#F4F8F0;border:1px solid #DDEBD2;border-radius:14px;padding:15px;}
      .metric span{display:block;color:var(--cinza);font-size:.82rem;font-weight:700;}
      .metric strong{display:block;color:var(--verde-profundo);font-size:1.4rem;margin-top:6px;}
      .recommendations{display:grid;gap:10px;margin-top:16px;}
      .recommendation{padding:14px 16px;border-radius:12px;background:#EDF7E8;border-left:5px solid var(--verde-medio);line-height:1.5;}
      .notice{font-size:.9rem;line-height:1.55;color:var(--cinza);background:#F4F8F0;border-radius:12px;padding:14px;margin-top:16px;}
      .mailto{display:inline-flex;margin-top:18px;padding:13px 18px;border-radius:10px;background:linear-gradient(135deg,var(--verde-medio),var(--verde-escuro));color:white;text-decoration:none;font-weight:900;}
      .mailto:hover{color:white;text-decoration:none;transform:translateY(-2px);}
      @media(max-width:980px){.grid,.score-card{grid-template-columns:1fr}.metric-grid{grid-template-columns:repeat(2,1fr)}.score-circle{width:140px;height:140px;font-size:2.4rem}}
    "))
  ),
  div(class = "app-header",
    h1("Diagnostico interativo de dados"),
    p("Uma primeira leitura da maturidade da sua base e do tipo de entrega que melhor combina com a sua demanda. O resultado e orientativo e nao substitui uma avaliacao tecnica completa.")
  ),
  div(class = "app-wrap",
    div(class = "grid",
      div(class = "panel",
        h2("Dados da demanda"),
        textInput("nome", "Nome ou projeto", placeholder = "Ex.: Inventario florestal 2026"),
        selectInput("perfil", "Perfil", c("Empresa", "Pesquisador(a)", "Estudante", "Produtor(a)", "Outro")),
        selectInput("demanda", "Principal objetivo", c("Organizar e limpar dados", "Analisar estatisticamente", "Criar dashboard", "Gerar relatorio tecnico", "Modelagem preditiva", "Ainda nao sei")),
        numericInput("variaveis", "Numero aproximado de variaveis/colunas", value = 12, min = 1, max = 500, step = 1),
        sliderInput("organizacao", "Organizacao da base", min = 0, max = 100, value = 45),
        sliderInput("qualidade", "Qualidade/confiabilidade dos dados", min = 0, max = 100, value = 55),
        sliderInput("visualizacao", "Nivel atual de visualizacao", min = 0, max = 100, value = 35),
        sliderInput("automacao", "Automacao/reprodutibilidade", min = 0, max = 100, value = 25),
        checkboxGroupInput("entregas", "Entregas desejadas", c("Analise exploratoria", "Dashboard", "Relatorio tecnico", "Modelo preditivo", "Limpeza de dados"), selected = c("Analise exploratoria", "Relatorio tecnico")),
        textAreaInput("observacoes", "Contexto da demanda", placeholder = "Descreva rapidamente o problema, prazo e resultado esperado.", rows = 4),
        div(class = "notice", "Privacidade: esta versao nao grava respostas, nao envia dados para banco e nao armazena uploads. O resumo so e montado na sessao atual.")
      ),
      div(class = "panel",
        h2("Resultado do diagnostico"),
        uiOutput("score_card"),
        div(class = "metric-grid",
          div(class = "metric", span("Organizacao"), strong(textOutput("m_org", inline = TRUE))),
          div(class = "metric", span("Qualidade"), strong(textOutput("m_qual", inline = TRUE))),
          div(class = "metric", span("Visualizacao"), strong(textOutput("m_vis", inline = TRUE))),
          div(class = "metric", span("Automacao"), strong(textOutput("m_auto", inline = TRUE)))
        ),
        plotOutput("maturity_plot", height = "260px"),
        h3("Recomendacoes"),
        uiOutput("recommendations"),
        h3("Resumo para contato"),
        verbatimTextOutput("summary"),
        uiOutput("mailto"),
        downloadButton("download_summary", "Baixar resumo")
      )
    )
  )
)

server <- function(input, output, session) {
  maturity_score <- reactive({
    round((input$organizacao * 0.28) + (input$qualidade * 0.32) + (input$visualizacao * 0.22) + (input$automacao * 0.18))
  })

  output$score_card <- renderUI({
    score <- maturity_score()
    div(class = "score-card",
      div(class = paste("score-circle", score_class(score)), paste0(score, "%")),
      div(class = "score-copy",
        strong(score_label(score)),
        p("Quanto maior a pontuacao, mais preparada a base parece estar para analises avancadas, dashboards e entregas tecnicas reproduziveis.")
      )
    )
  })

  output$m_org <- renderText(paste0(input$organizacao, "%"))
  output$m_qual <- renderText(paste0(input$qualidade, "%"))
  output$m_vis <- renderText(paste0(input$visualizacao, "%"))
  output$m_auto <- renderText(paste0(input$automacao, "%"))

  output$maturity_plot <- renderPlot({
    values <- c(
      Organizacao = input$organizacao,
      Qualidade = input$qualidade,
      Visualizacao = input$visualizacao,
      Automacao = input$automacao
    )
    old_par <- par(mar = c(5, 4, 2, 1))
    on.exit(par(old_par))
    barplot(values, ylim = c(0, 100), col = c("#63783D", "#7FA653", "#99CD85", "#CFE0BC"), border = NA, ylab = "Pontuacao")
    abline(h = c(35, 65, 85), col = "#D8E3D0", lty = 2)
  })

  output$recommendations <- renderUI({
    recs <- recommendations_for(maturity_score(), input$organizacao, input$qualidade, input$visualizacao, input$automacao, input$entregas)
    div(class = "recommendations", lapply(recs, function(item) div(class = "recommendation", item)))
  })

  summary_text <- reactive({
    paste(
      "Diagnostico RB Consultoria em Dados",
      paste0("Projeto: ", ifelse(nzchar(input$nome), input$nome, "Nao informado")),
      paste0("Perfil: ", input$perfil),
      paste0("Objetivo: ", input$demanda),
      paste0("Variaveis aproximadas: ", input$variaveis),
      paste0("Pontuacao de maturidade: ", maturity_score(), "% - ", score_label(maturity_score())),
      paste0("Entregas desejadas: ", paste(input$entregas, collapse = ", ")),
      paste0("Contexto: ", ifelse(nzchar(input$observacoes), input$observacoes, "Nao informado")),
      sep = "\n"
    )
  })

  output$summary <- renderText(summary_text())

  output$mailto <- renderUI({
    subject <- URLencode("Diagnostico interativo - RB Consultoria em Dados", reserved = TRUE)
    body <- URLencode(summary_text(), reserved = TRUE)
    tags$a(class = "mailto", href = paste0("mailto:renanbbueno.rb@gmail.com?subject=", subject, "&body=", body), "Enviar resumo por e-mail")
  })

  output$download_summary <- downloadHandler(
    filename = function() "diagnostico-rb-consultoria.txt",
    content = function(file) writeLines(summary_text(), file, useBytes = TRUE)
  )
}

shinyApp(ui, server)
