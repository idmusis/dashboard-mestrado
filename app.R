library(tidyverse)
library(shiny)
library(plotly)
library(rlang)
library(ggplot2)
library(bs4Dash)
library(highcharter)
library(shinyWidgets)
library(data.table)
library(fresh)
library(waiter)

library(DT)
library(boot)
library(bestglm)
library(caret)
library(pROC)
library(purrr)

library(shinyjs)
source("funcoes dashboard.R")


lang <- getOption("highcharter.lang")
lang$decimalPoint <- ","
lang$downloadCSV <- "Baixar CSV"
lang$downloadXLS <- "Baixar XLS"
lang$downloadPNG <- "Baixar PNG"
lang$downloadPDF <- "Baixar PDF"
lang$viewData <- "Ver tabela de dados"
lang$hideData <- "Esconder tabela de dados"
lang$exportInProgress <- "Exportando..."
lang$noData <- "Sem dados para exibir"
lang$loading <- "Carregando..."
lang$printChart <- "Imprimir gráfico"
lang$downloadJPEG <- "Baixar JPEG"
lang$exportData$categoryHeader <- "Categoria"
options(highcharter.lang = lang)


# Dados descritiva
data <- arrow::read_feather("dados_feather.feather")


# Dados regressão
dados_numericos<-arrow::read_feather("dados-dummy.feather") %>% select_if(is.numeric)%>% mutate_all(~ replace(., is.na(.), 0))

variavel_resposta <- dados_numericos[, "evadido"]

ajuste_nulo <- glm(evadido ~ 1, data = dados_numericos, family = "binomial")


modelo_step_padrao <- readRDS("regressao/modelo_step.rds")
info_criteria_padrao <- readRDS("regressao/info_criteria.rds")
coeficientes_df_padrao <- readRDS("regressao/coeficientes_df_tornado.rds")
resultados_bootstrap_padrao <- readRDS("regressao/resultados_bootstrap.rds")
roc_padrao <- readRDS("regressao/roc.rds")


custom_theme <- create_theme(
  bs4dash_layout(
    main_bg = "#F1F1F1",  # Light background color for the body
  sidebar_width="300px"
    ),
  bs4dash_status(
    primary = "#1C4E80",
    danger="#F1F1F1"
  ),
  bs4dash_font(
    size_base="1rem",
    family_sans_serif = "'Open Sans', -apple-system, BlinkMacSystemFont, 'San Francisco', 'Segoe UI', Roboto, 'Helvetica Neue', sans-serif",
    family_base = "'Open Sans', -apple-system, BlinkMacSystemFont, 'San Francisco', 'Segoe UI', Roboto, 'Helvetica Neue', sans-serif"
  )
)



modelo_step <- readRDS("regressao/modelo_step.rds")
info_criteria <- readRDS("regressao/info_criteria.rds")
tornado_data <- readRDS("regressao/coeficientes_df_tornado.rds")
resultados_bootstrap <- readRDS("regressao/resultados_bootstrap.rds")

# ui ---- 

ui <- dashboardPage(
  freshTheme = custom_theme,
  preloader = list(html = tagList(spin_loader()), color = transparent(.9)),
  scrollToTop=TRUE,
  help=NULL,
  title = "Dashboard - Fatores que influenciam a permanência e a evasão nos cursos de pós-graduação stricto sensu da UFMT",
  
  ## Cabeçalho ----
  header = dashboardHeader(
    navbarMenu(
      navbarTab(
        "Visão geral",
        tabName = "resumo"
      ),
      navbarTab(
        "Análise de Regressão",
        tabName = "logistic"
      )
    ),
  #  actionButton(inputId = "controlbarToggle", label = "Filtros", icon=icon("filter"),class = "mx-2")
  controlbarIcon=icon("gear")
  ),
  
  # ## Barra lateral ----
  sidebar = dashboardSidebar(disable = TRUE),
  # sidebar = bs4DashSidebar(
  #   status = "primary",
  #   title = "Menu",
  #   skin="dark",
  #   elevation=2,
  #   collapsed=TRUE,
  #   bs4SidebarMenu(
  #     bs4SidebarMenuItem(
  #       "Visão geral",
  #       tabName = "resumo",
  #       icon = icon("chart-pie")
  #     ),
  #     bs4SidebarMenuItem(
  #       "Análise de Regressão",
  #       tabName = "logistic",
  #       icon = icon("chart-line")
  #     )
  #    )
  # ),
  
  ## Control bar ----
  
  controlbar=dashboardControlbar(
    id="controlbar",
    pinned=FALSE,overlay=FALSE,
    controlbarMenu(type="hidden",
      column(width=12,
             radioGroupButtons(
               inputId = "comparar",
               justified=TRUE,
               choiceNames = c("Comparação", "Total"),
               choiceValues=c("comparar","total"),
               selected="comparar"#,
              # status = "primary"
             ),
          teste_box(
            title="Status",
          width=12,headerBorder = TRUE, collapsible = TRUE,collapsed=FALSE,
            column(width = 12, offset = 0,
                   prettyCheckboxGroup(
                     inputId = paste0("evadido_checkbox", "_selectall"),
                     label = NULL,
                     choiceValues = "selectall",
                     choiceNames = "Selecionar tudo",
                     shape="curve",
                     selected = "selectall",
                     status = "primary",
                   #  icon = icon("check"),
                     animation = "smooth"
                   ),
                   prettyCheckboxGroup(
                     inputId = "evadido_checkbox",
                     label = NULL,
                     choiceValues = c(0,1),
                     shape="curve",
                     choiceNames = c("Concluíntes","Desvinculados"),
                     selected = c(0,1),  
                     status = "primary",
                    # icon = icon("check"),
                     animation = "smooth"
                   )
            )),
        lapply(list(
          #list("evadido_checkbox", "Status", c("Concluíntes"=0, "Desvinculados"=1)),
          list("nivel_checkbox", "Nível acadêmico", unique(data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %>% na.omit())),
          list("campus_checkbox", "Câmpus", unique(data$`Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?` %>% na.omit())),
          list("genero_checkbox", "Identidade de Gênero", unique(data$`Identidade de gênero`)),
          list("etnia_checkbox", "Raça/Etnia", unique(data$`Qual opção melhor descreve sua raça ou etnia?`)),
          list("areaConhecimento_checkbox", "Área de Conhecimento", unique(data$`area_conhecimento_curso`)),
               list("curso_checkbox", "Curso", unique(data$`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?`))
        ), function(args) {
          teste_box(
            title=args[[2]],
            width = 12, headerBorder = TRUE, collapsible = TRUE,
              column(width=12,offset=0,
                     prettyCheckboxGroup(inputId = paste0(args[[1]], "_selectall"),
                                         label = NULL,#args[[2]],#"Selecionar tudo",
                                         choiceValues="selectall",
                                         choiceNames="Selecionar tudo",
                                         selected="selectall",
                                         shape="curve",
                                         status = "primary",
                                        # icon = icon("check"),
                                         animation="smooth"),
                     
                     prettyCheckboxGroup(
                       inputId = args[[1]],
                       label = NULL,#args[[2]],
                       choiceValues=args[[3]],
                       width="30px",
                       choiceNames = args[[3]] %>% stringr::str_trunc(width = 30), #stringr::str_wrap(width = 60) %>% stringr::str_replace_all("\\n", "<br>") %>% lapply(HTML),
                       selected = args[[3]],  # Select all by default, modify as needed
                       shape="curve",
                       status = "primary",
                     #  icon = icon("check"),
                       animation="smooth"
                     )
            ))
        }
        )
    )
    
  )
  ),
  
  
  ## Corpo ----
  body = 
    bs4DashBody(
      autoWaiter(html=spin_3(),color=transparent(.9)),
    tags$script(src = "https://code.highcharts.com/mapdata/countries/br/br-all.js"),
    tags$link(rel = "stylesheet", type="text/css", href="style.css"
  #   tags$head(
  #     tags$style(HTML("
  #       .scroll-box {
  #         overflow-y: auto;  /* Enables vertical scrollbar if needed */
  #         height: 150px;    /* Set fixed height */
  #         max-height: 150px; /* Adjust height as needed */
  #         font-size: 0.8em;  /* Smaller font size */
  #       }
  #       
  #       .smalltext {
  #         font-size: 0.8em;  /* Smaller font size */
  #       }
  #       
  #       .pretty .state label, .pretty .state label:after {
  #       font-weight: normal !important; /* Override to normal weight */
  #       
  # 
  #        .pretty .state label::after, .pretty .state label::before, .icon {
  #                   margin-top: 20px;
  #               }
  #               
  #               .pretty .state label {
  #                   margin-left: 10px;
  #               }
  #               
  #       
  #       .scroll-box .form-group { margin-bottom: 0px !important; } /* Reduce space between groups */
  #       
  #       .card-body {
  #          padding: 0px;
  #       }
  #       
  #       
  #       .no-border {
  #   border: none !important;
  #   box-shadow: none !important;
  #       }
  # 
  # 
  # /* Set a fixed height or maximum height */
  #     .control-sidebar {
  #       height: 100%; /* Use a percentage or a fixed height like 400px */
  #       max-height: 100%;
  #       overflow-y: auto; /* Enable vertical scrolling */
  #     }
  #     .control-sidebar-content {
  #       height: 100%; /* Ensure the content takes full height */
  #       overflow-y: auto; /* Enable vertical scrolling */
  #     }
  # 
  # 
  #     "))
  ),
    
    ### Página 1 ----
    bs4TabItems(
      bs4TabItem(
        tabName = "resumo",
        
        ##### filtros ----
        fluidPage(
          fluidRow(actionButton(inputId = "controlbarToggle", label = "Filtros", icon=icon("filter"),class = "mx-2")),h3(""),
        fluidRow(
        
            bs4ValueBoxOutput("contagemRespondentes"),
             bs4ValueBoxOutput("contagemConcluintes"),
             bs4ValueBoxOutput("contagemDesvinculados")
          ),
        #### gráficos gerais ----
       # h4("Aspectos individuais"),
        tabsetPanel(type="pills",
                    tabPanel("Aspectos individuais",
                             h3(""),
        fluidRow(
          column(
            width = 4,
            bs4Card(
             # title="Plot 7",
             # status="primary",
              headerBorder=FALSE,
              width = 12,
              maximizable = FALSE,
              collapsible = FALSE,
              highchartOutput("plot7", height = "700px")
            ) #%>% sortable(width = 12)
          ),
          column(
            width = 4,
                bs4Card(
               #   title = "Plot 18",
                  width = 12,
                  headerBorder = FALSE,
                  maximizable = FALSE, collapsible = FALSE,
                  highchartOutput("plot18", height = "300px")
                ) , #%>% sortable(width = 12),
                bs4Card(
              #    title = "Plot 16",
                  width = 12,
                  headerBorder = FALSE,
               #   status="info",
                  maximizable = FALSE, collapsible = FALSE,
                  highchartOutput("plot16", height = "300px")
                ) #%>% sortable(width = 12)
              ),
              column(
                width = 4,
                    bs4Card(
                     # title = "Plot 21",
                      width = 12,
                    #  status="secondary",
                    headerBorder = FALSE,
                      maximizable = FALSE, collapsible = FALSE,
                      highchartOutput("plot21", height = "300px")
                    ) , #%>% sortable(width = 12),
                    bs4Card(
                     # title = "Plot 17",
                      width = 12,
                      headerBorder = FALSE,
                      maximizable = FALSE, collapsible = FALSE,
                      highchartOutput("plot17", height = "300px")
                    ) #%>% sortable(width = 12)
                  )
        ),
        fluidRow(
            column(
              width = 7,
              bs4Card(
               # title = "Plot 19",
                width = 12,
                maximizable = FALSE, collapsible = FALSE,
                headerBorder = FALSE,
                highchartOutput("plot19", height = "450px")
              ) , #%>% sortable(width = 12),
              bs4Card(
               # title = "Plot 20",
                width = 12,
                maximizable = FALSE, collapsible = FALSE,
                headerBorder = FALSE,
                highchartOutput("plot20", height = "350px")
              ) #%>% sortable(width = 12)
            ),
            column(
              width = 5,
              bs4Card(
               # title="Plot 13",
                headerBorder=FALSE,
                width = 12,
                maximizable = FALSE, collapsible = FALSE,
                highchartOutput("plot13", height = "500px")
              ) , #%>% sortable(width = 12),
              bs4Card(
               # title="Plot 15",
                headerBorder=FALSE,
                width = 12,
                maximizable = FALSE, collapsible = FALSE,
                highchartOutput("plot15", height = "300px")
              ) , #%>% sortable(width = 12),
            )
          ),
        h4("Sobre o trabalho"), #### Sobre o trabalho ----
        fluidRow(
          column(
            width = 5,
            bs4Card(
             # title = "Plot 24",
              width = 12,
              maximizable = FALSE, collapsible = FALSE,
              headerBorder = FALSE,
              highchartOutput("plot24", height = "300px")
            ) , #%>% sortable(width = 12),
            bs4Card(
             # title = "Plot 25",
              width = 12,
              maximizable = FALSE, collapsible = FALSE,
              headerBorder = FALSE,
              highchartOutput("plot25", height = "300px")
            ) #%>% sortable(width = 12)
          ),
          column(
            width = 7,
            bs4Card(
             # title = "Plot 26",
              width = 12,
              maximizable = FALSE, collapsible = FALSE,
              headerBorder = FALSE,
              highchartOutput("plot26", height = "300px")
            ) , #%>% sortable(width = 12),
            bs4Card(
             # title = "Plot 27",
              width = 12,
              maximizable = FALSE, collapsible = FALSE,
              headerBorder = FALSE,
              highchartOutput("plot27", height = "300px")
            ) #%>% sortable(width = 12)
          )
        )
        ),
        tabPanel("Informações acadêmicas",
                 h3(""),
      #  h4("Informações acadêmicas"), #### Informações acadêmicas ----
          fluidRow(
            column(
              width = 4,
              bs4Card(
               # title = "Plot 1",
                width = 12,
                maximizable = FALSE, collapsible = FALSE,
                headerBorder = FALSE,
                highchartOutput("plot1", height = "300px")
              ) #%>% sortable(width = 12)
              ),
              column(
                width = 4,
                bs4Card(
                 # title = "Plot 4",
                  width = 12,
                  maximizable = FALSE, collapsible = FALSE,
                  headerBorder = FALSE,
                  highchartOutput("plot4", height = "300px")
                ) #%>% sortable(width = 12)
                ),
            column(
              width = 4,
              bs4Card(
               # title = "Plot 11",
                width = 12,
                maximizable = FALSE, collapsible = FALSE,
                headerBorder = FALSE,
                highchartOutput("plot11", height = "300px")
              ) #%>% sortable(width = 12)
            ),
            column(
              width = 6,
              bs4Card(
               # title = "Plot 3",
                width = 12,
                maximizable = FALSE, collapsible = FALSE,
                headerBorder = FALSE,
                highchartOutput("plot3", height = "300px")
              ) , #%>% sortable(width = 12),
              bs4Card(
               # title = "Plot 9",
                width = 12,
                maximizable = FALSE, collapsible = FALSE,
                headerBorder = FALSE,
                highchartOutput("plot9", height = "300px")
              ) #%>% sortable(width = 12)
          ),
          column(
            width = 3,
            bs4Card(
             # title = "Plot 12",
              width = 12,
              maximizable = FALSE, collapsible = FALSE,
              headerBorder = FALSE,
              highchartOutput("plot12", height = "300px")
            ) , #%>% sortable(width = 12),
            bs4Card(
             # title = "Plot 8",
              width = 12,
              maximizable = FALSE, collapsible = FALSE,
              headerBorder = FALSE,
              highchartOutput("plot8", height = "300px")
            ) #%>% sortable(width = 12)
            ),
          column(width=3,
            bs4Card(
             # title = "Plot 5",
              width = 12,
              maximizable = FALSE, collapsible = FALSE,
              headerBorder = FALSE,
              highchartOutput("plot5", height = "300px")
            ) , #%>% sortable(width = 12),
            bs4Card(
             # title = "Plot 10",
              width = 12,
              maximizable = FALSE, collapsible = FALSE,
              headerBorder = FALSE,
              highchartOutput("plot10", height = "300px")
            ) #%>% sortable(width = 12)
            )
          ),
          h4("Sobre o desligamento"),
        fluidRow(
          column(
            width = 6,
            bs4Card(
             # title = "Plot 23",
              width = 12,
              maximizable = FALSE, collapsible = FALSE,
              headerBorder = FALSE,
              highchartOutput("plot23", height = "310px")
            ) #%>% sortable(width = 12)
          ),
          column(
            width = 6,
            bs4Card(
             # title = "Plot 22",
              width = 12,
              maximizable = FALSE, collapsible = FALSE,
              headerBorder = FALSE,
              highchartOutput("plot22", height = "310px")
            ) #%>% sortable(width = 12)
          ))
              ),
        tabPanel("Aspectos acadêmicos",
                 h3(""),
       # h4("Aspectos acadêmicos"), #### Aspectos acadêmicos ----
        fluidRow(
            bs4Card(
             # title="Plot 29",
              headerBorder=FALSE,
              width = 4,
              maximizable = FALSE, collapsible = FALSE,
              highchartOutput("plot29", height = "400px")
            ), #%>% sortable(width = 4),
                bs4Card(
                 # title = "Plot 30",
                  width = 4,
                  maximizable = FALSE, collapsible = FALSE,
                  headerBorder = FALSE,
                  highchartOutput("plot30", height = "400px")
                ), #%>% sortable(width = 4),
                bs4Card(
                 # title = "Plot 31",
                  width = 4,
                  maximizable = FALSE, collapsible = FALSE,
                  headerBorder = FALSE,
                  highchartOutput("plot31", height = "400px")
                ) #%>% sortable(width = 4)
            ),
       fluidRow(
                bs4Card(
                 # title = "Plot 32",
                  width = 4,
                  maximizable = FALSE, collapsible = FALSE,
                  headerBorder = FALSE,
                  highchartOutput("plot32", height = "400px")
                ), #%>% sortable(width = 4),
                bs4Card(
                 # title = "Plot 33",
                  headerBorder = FALSE,
                  width = 4,
                  maximizable = FALSE, collapsible = FALSE,
                  highchartOutput("plot33", height = "400px")
                ), #%>% sortable(width = 4),
                    bs4Card(
                     # title = "Plot 34",
                      width = 4,
                      maximizable = FALSE, collapsible = FALSE,
                      headerBorder = FALSE,
                      highchartOutput("plot34", height = "400px")
                    ) #%>% sortable(width = 4)
                  )
                ),
      tabPanel("Aspectos sociais",
               h3(""),
        #### Aspectos sociais ----
       # h4("Aspectos sociais"),
        fluidRow(
          bs4Card(
           # title = "Plot 35",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot35",height="400px")
          ), #%>% sortable(width = 4),
          bs4Card(
           # title = "Plot 36",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot36",height="400px")
          ), #%>% sortable(width = 4),
          bs4Card(
           # title = "Plot 37",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot37",height="400px")
          ), #%>% sortable(width = 4),
          bs4Card(
           # title = "Plot 38",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot38",height="400px")
          ), #%>% sortable(width = 4),
          bs4Card(
           # title = "Plot 39",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot39",height="400px")
          ), #%>% sortable(width = 4),
          bs4Card(
           # title = "Plot 40",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot40",height="400px")
          ) #%>% sortable(width = 4)
        )
        ),
      tabPanel("Aspectos institucionais",
               h3(""),
        #### Aspectos institucionais ----
        #h4("Aspectos institucionais"),
        fluidRow(
          bs4Card(
           # title = "Plot 41",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot41",height="400px")
          ), #%>% sortable(width = 4),
          bs4Card(
           # title = "Plot 42",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot42",height="400px")
          ), #%>% sortable(width = 4),
          bs4Card(
           # title = "Plot 43",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot43",height="400px")
          ), #%>% sortable(width = 4),
          bs4Card(
           # title = "Plot 44",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot44",height="400px")
          ), #%>% sortable(width = 4),
          bs4Card(
           # title = "Plot 45",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot45",height="400px")
          ), #%>% sortable(width = 4),
          bs4Card(
           # title = "Plot 46",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot46",height="400px")
          ) #%>% sortable(width = 4)
        )
        ),
        tabPanel("Aspectos de carreira",
                 h3(""),
        #### Aspectos de carreira ----
       # h4("Aspectos de carreira"),
        fluidRow(
          bs4Card(
           # title = "Plot 47",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot47",height="400px")
          ), #%>% sortable(width = 4),
          bs4Card(
           # title = "Plot 48",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot48",height="400px")
          ), #%>% sortable(width = 4),
          bs4Card(
           # title = "Plot 49",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot49",height="400px")
          ), #%>% sortable(width = 4),
          bs4Card(
           # title = "Plot 50",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot50",height="400px")
          ), #%>% sortable(width = 4),
          bs4Card(
           # title = "Plot 51",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot51",height="400px")
          ), #%>% sortable(width = 4),
          bs4Card(
           # title = "Plot 52",
            width = 4,
            maximizable = FALSE, collapsible = FALSE,
            headerBorder = FALSE,
            highchartOutput("plot52",height="400px")
          ) #%>% sortable(width = 4)
        )
        ))
            
      )), ### Página 2 ----
      bs4TabItem(
        tabName = "logistic",
        bs4Card(
          title = "Configurações avançadas",
          solidHeader = TRUE,
          collapsible = TRUE,
          width=12,
          collapsed = FALSE,
          prettyCheckbox("include_bootstrap", label= "Performar validação bootstrap", value = FALSE,
                         animation="smooth",status="primary"),
          # Conditional panel for sliders
          conditionalPanel(
            condition = "input.include_bootstrap == true",
            sliderInput("proporcao_treino", "Partição de treino", min = 50, max = 95, value = 80),
            sliderInput("slider1", "Repetições bootstrap", min = 5, max = 10000, value = 100)
          ),
          prettyRadioButtons("criteria", "Critério de seleção de variáveis", choices = c("BIC", "AIC"), selected = "BIC",
                             animation="smooth"),
          prettyRadioButtons("direction", "Direção de seleção stepwise", choiceNames = c("Eliminação bidirecional","Seleção direta", "Eliminação reversa"), choiceValues=c("both","forward","backward"),selected = "both",
                             animation="smooth")
        ),
        actionButton("analyze", "Processar",
                   style = "bordered",status="primary"),
        # actionBttn("save_results", "Salvar Resultados",
        #            style = "bordered", color="success"),
        actionButton("restore_default", "Restaurar Modelo Padrão",
                   style = "bordered", status = "warning"),
        h3(""),
        tabsetPanel(type="pills",
                    id = "results_tabs",
                    
                    tabPanel(
           # tabName = "stepwise_models",
            title = "Modelo de regressão",
            textOutput("configuracoes_modelo"),
            verbatimTextOutput("stepwise")
          ),
          tabPanel(
            title = "LogLik e Critérios de informação",
            verbatimTextOutput("info_criteria")
          ),
          tabPanel(
            title = "Plots",
            plotOutput("residualsPlot"),
            plotOutput("qqPlot"),
            plotOutput("scaleLocationPlot"),
            plotOutput("cooksDistancePlot"),
            plotOutput("rocPlot"),
            h3("Tornado Plot"),
            plotOutput("tornadoPlot"),
          ),
          tabPanel(
            title = "Bootstrap",
            h4("Estatísticas"),
            DT::DTOutput("boot"),
            h4("Coeficientes"),
            DT::DTOutput("bootCoef"),
            h4("Odds Ratio"),
            DT::DTOutput("bootOR")
            
          )
        )
      )
)
)
)

    
  

# server ----

# Server logic
server <- function(input, output,session) {

  
  observeEvent(input$controlbarToggle, {
    updateControlbar(id = "controlbar")
  })
  
  ##########definindo função
  
  plot_bar <- function(data, category_col, category_order = NULL, category_labels = NULL, xlab="",ylab="",tipo="column",titulo=category_col,
                       na.omit=TRUE,
                       percent=TRUE,
                       comparar=req(input$comparar),group_col="evadido",group_labels=NULL) {
    # Convertendo nome da coluna categoria em símbolo para uso no dplyr
    category_sym <- rlang::sym(category_col)
    group_sym <- rlang::sym(group_col)
    
    # Tratando valores NA
    if (na.omit) {
      data <- data %>% filter(!is.na(!!category_sym))
    } else {
      data <- data %>% mutate(!!category_sym := ifelse(is.na(!!category_sym), "NA", !!category_sym))
    }
    
    if (comparar=="comparar") {
      # Preparando os dados
      data_grouped <- data %>%
        dplyr::count(!!group_sym, !!category_sym) %>%
        group_by(!!group_sym) %>%
        mutate(percent = n / sum(n) * 100) %>%
        ungroup()
    }else if (comparar=="total"){
      # Preparando os dados
      data_grouped <- data %>%
        dplyr::count(!!category_sym) %>%
        mutate(percent = n / sum(n) * 100)
    }
    
    if (!is.null(category_order)) {
      data_grouped <- data_grouped %>%
        mutate(!!category_sym := factor(!!category_sym, levels = category_order)) %>% arrange(!!category_sym)
    } else{
      data_grouped <- data_grouped %>%
        mutate(!!category_sym := factor(!!category_sym)) 
    }
    
    if (comparar=="comparar"){
      # Aplicando rótulos específicos para 'evadido'
      if (group_col == "evadido") {
        data_grouped <- data_grouped %>%
          mutate(!!group_sym := factor(!!group_sym, levels = c(0, 1), labels = c("Concluíntes", "Desvinculados")))
      }
      
      # Criando o gráfico
      hc <- highchart() %>%
        hc_chart(lang = list(decimalPoint = ',', thousandsSep = '.')) %>%
        hc_title(text=titulo) %>%
        hc_add_series(data_grouped, type = tipo, hcaes(x = !!category_sym, y = percent, group = !!group_sym)) %>%
        hc_xAxis(categories = if (is.null(category_labels)) levels(data_grouped[[category_col]]) else category_labels, title = list(text = xlab)) %>%
        hc_yAxis(title = list(text = ""),labels = list(format = '{value}%')) %>%
        # hc_tooltip(shared=FALSE,useHTML = TRUE, headerFormat = '<span style="font-size: 0.8em">{point.key}</span><br/>',
        #            pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.n}</b> ({point.percent:.1f}% do total do grupo)<br/>') %>%
        hc_tooltip(shared = FALSE, useHTML = TRUE, headerFormat = '<span style="font-size: 0.8em">{point.key}</span><br/>',
                   pointFormat = '<b>{series.name}</b><br><span style="color:{point.color}">\u25AA</span>Frequência (%): <b>{point.n}</b> ({point.percent:.1f}%)<br/>') #%>%
        #hc_colors(c("#7BB1E4","#404C57"))
      
      # Configurando rótulos de grupo, se fornecidos
      if (!is.null(group_labels)) {
        hc <- hc %>% hc_xAxis(categories = group_labels)
      }
    }else if (comparar=="total"){
      if (percent){
        hc <- highchart() %>%
          hc_chart(lang = list(decimalPoint = ',', thousandsSep = '.')) %>%
          hc_title(text=titulo,
                   style = list(fontSize = "16px")) %>%
          hc_add_series(data_grouped, type = tipo, hcaes(x = !!category_sym, y = percent)) %>%
          hc_xAxis(categories = if (is.null(category_labels)) levels(data_grouped[[category_col]]) else category_labels, title = list(text = xlab)) %>%
          hc_yAxis(title = list(text = ylab), labels = list(format = '{value}%')) %>%
          hc_tooltip(shared = TRUE, useHTML = TRUE, headerFormat = '<span style="font-size: 0.8em">{point.key}</span><br/>',
                     pointFormat = '<span style="color:{point.color}">\u25AA</span> Frequência (%): <b>{point.n} ({point.y:.1f}%)</b><br/>') %>%
          hc_legend(enabled = FALSE)  #%>% 
         # hc_colors("#ED9368")
      }else{
        # Criando o gráfico
        hc <- highchart() %>%
          hc_chart(lang = list(decimalPoint = ',', thousandsSep = '.')) %>%
          hc_title(text=titulo) %>%
          hc_add_series(data_grouped, type = tipo, hcaes(x = !!category_sym, y = n)) %>%
          hc_xAxis(categories = if (is.null(category_labels)) levels(data_grouped[[category_col]]) else category_labels, title = list(text = xlab)) %>%
          hc_yAxis(title = list(text = ylab)) %>%
          hc_tooltip(shared = TRUE, useHTML = TRUE, headerFormat = '<span style="font-size: 0.8em">{point.key}</span><br/>',
                     pointFormat = '<span style="color:{point.color}">\u25AA</span> Frequência (%): <b>{point.y}</b> ({point.percent:.1f}%)<br/>') %>%
          hc_legend(enabled = FALSE)  %>% 
          hc_colors("#ED9368")
          
      }
    }
    hc<- hc %>%
      hc_exporting(enabled=TRUE,
                   buttons = list(
                     contextButton = list(
                       menuItems = botoes_menu
                     )
                   )
      )
    return(hc)
    
  }
  ###
  forceRedraw <- reactiveVal(FALSE)
  
## tema ----
  observe({
    observeEvent(input$dark_mode, {
      ignoreNULL=FALSE
      if (input$dark_mode) {
        #print("Dark mode")
        # options(highcharter.theme = hc_theme_darkunica())
        options(highcharter.theme = hc_theme(
          # colors=c("#f2a65a","#6a7b9d","#8abfce"),
          chart = list(
          backgroundColor = 'transparent',
          style = list(color = '#E0E0E3',
                       fontFamily="Open Sans")
        ),
        title = list(style = list(color = '#E0E0E3',fontSize = "16px")),
        subtitle = list(style = list(color = '#E0E0E3')),
        xAxis = list(
          labels = list(style = list(color = '#E0E0E3')),
          lineColor = '#707073',
          minorGridLineColor = '#505053',
          tickColor = '#707073'
        ),
        yAxis = list(
          labels = list(style = list(color = '#E0E0E3')),
          lineColor = '#707073',
          minorGridLineColor = '#505053',
          tickColor = '#707073',
          title = list(style = list(color = '#E0E0E3'))
        ),
        legend = list(
          backgroundColor = 'transparent',
          itemStyle = list(color = '#E0E0E3'),
          itemHoverStyle = list(color = '#FFF'),
          itemHiddenStyle = list(color = '#606063')
        ),
        credits = list(style = list(color = '#666')),
        plotOptions = list(
          pie = list(
            dataLabels = list(
              style = list(color = '#E0E0E3')
            )
          )
        ))
        )
        
        forceRedraw(!forceRedraw())  # Toggle the value to trigger reactivity
} else {
        options(highcharter.theme = hc_theme(
          # colors=c("#f2a65a","#6a7b9d","#8abfce"),
          chart = list(
          backgroundColor = 'transparent',
          style = list(color = '#333333',
                       fontFamily="Open Sans")
        ),
        title = list(style = list(color = '#333333',fontSize = "16px")),
        subtitle = list(style = list(color = '#666666')),
        xAxis = list(
          labels = list(style = list(color = '#333333')),
          lineColor = '#ccc',
          minorGridLineColor = '#f2f2f2',
          tickColor = '#ccc'
        ),
        yAxis = list(
          labels = list(style = list(color = '#333333')),
          lineColor = '#ccc',
          minorGridLineColor = '#f2f2f2',
          tickColor = '#ccc',
          title = list(style = list(color = '#333333'))
        ),
        legend = list(
          backgroundColor = 'transparent',
          itemStyle = list(color = '#333333'),
          itemHoverStyle = list(color = '#000'),
          itemHiddenStyle = list(color = '#ccc')
        ),
        credits = list(style = list(color = '#333'))
        )
        )
        forceRedraw(!forceRedraw())
      }
    })
  })
  
  observeEvent(input$reload, {
    forceRedraw(!forceRedraw())  # Toggle the value to trigger reactivity
  })
  
# 
# #### filtros ----
    
    
  observe({
    print(input$evadido_checkbox_selectall)
    updatePrettyCheckboxGroup(
      inputId= "evadido_checkbox", 
      selected = if(!is.null(input$evadido_checkbox_selectall)) unique(data$evadido %>% na.omit()) else character(0)
    )
    
  })
  
    observe({
      updatePrettyCheckboxGroup(
        inputId= "nivel_checkbox", 
        selected = if(!is.null(input$nivel_checkbox_selectall)) unique(data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %>% na.omit()) else character(0)
      )

    })
    
    # Observer for Curso

    observe({
      updatePrettyCheckboxGroup(
        inputId = "curso_checkbox", 
        selected = if (!is.null(input$curso_checkbox_selectall)) unique(data$`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?`) else character(0)
      )
    })
    
    # Observer for Câmpus

    observe({
      updatePrettyCheckboxGroup(
        inputId = "campus_checkbox", 
        selected = if (!is.null(input$campus_checkbox_selectall)) unique(data$`Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?` %>% na.omit()) else character(0)
      )
    })
    
    # Observer for Gênero
    observe({
      updatePrettyCheckboxGroup(
        inputId = "genero_checkbox", 
        selected = if (!is.null(input$genero_checkbox_selectall)) unique(data$`Identidade de gênero`) else character(0)
      )
    })
    
    # Observer for Raça/Etnia
    observe({
      updatePrettyCheckboxGroup(
        inputId = "etnia_checkbox", 
        selected = if (!is.null(input$etnia_checkbox_selectall)) unique(data$`Qual opção melhor descreve sua raça ou etnia?`) else character(0)
      )
    })
    
    # Observer for Área de Conhecimento
    observe({
      updatePrettyCheckboxGroup(
        inputId = "areaConhecimento_checkbox", 
        selected = if (!is.null(input$areaConhecimento_checkbox_selectall)) unique(data$`area_conhecimento_curso`) else character(0)
      )
    })

  
  dadosFiltrados <- reactive({
   # input$dark_mode
   # Aplicar filtros baseados nas escolhas das checkboxes
    filtered <- data
    
    # if (!is.null(input$evadido_checkbox)) {
    #   data <- data[data$status %in% input$evadido_checkbox, ]
    # }
    
    # if (!is.null(input$curso_checkbox)) {
    filtered <- filtered %>% filter(evadido %in% input$evadido_checkbox)
    
      filtered <- filtered %>% filter(`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?` %in% input$curso_checkbox)
      
    # }
    #print(input$nivel_checkbox)
      filtered <- filtered %>% filter(`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %in% input$nivel_checkbox)
      
    # 
    # if (!is.null(input$campus_checkbox)) {
      filtered <- filtered %>% filter(`Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?` %in% input$campus_checkbox)
    # }
    # if (!is.null(input$genero_checkbox)) {
      filtered <- filtered %>% filter(`Identidade de gênero` %in% input$genero_checkbox)
    # }
    # if (!is.null(input$etnia_checkbox)) {
      filtered <- filtered %>% filter(`Qual opção melhor descreve sua raça ou etnia?` %in% input$etnia_checkbox)
    # }
    # if (!is.null(input$areaConhecimento_checkbox)) {
      filtered <- filtered %>% filter(`area_conhecimento_curso` %in% input$areaConhecimento_checkbox)
    # }
    force(forceRedraw())
    filtered
  })
  
  # Output para contagem de respondentes
  output$contagemRespondentes <- renderbs4ValueBox({
    valuebox2(
      subtitle= "Nº de respondentes",
      value=nrow(dadosFiltrados()),
      icon=icon("users"))
  })
  output$contagemConcluintes <- renderbs4ValueBox({
    valuebox2(
      subtitle = "Nº de Concluintes",
      value = nrow(subset(dadosFiltrados(), evadido == 0)),
      icon = icon("user-graduate"),
      color = "info"
    )
  })
  
  output$contagemDesvinculados <- renderbs4ValueBox({
    valuebox2(
      subtitle = "Nº de Desvinculados",
      value = nrow(subset(dadosFiltrados(), evadido == 1)),
      icon = icon("user-times"),
      color = "gray-dark",
    )
  })
  
## plots ----
  hc <- function(funcao) {
    renderHighchart({
      force(forceRedraw())  # Ensure dependency on forceRedraw to trigger reactivity
      funcao  # Execute the Highcharter function passed as argument
    })
  }
  
  output$plot1<-renderHighchart({
    force(forceRedraw())
    plot_pie(dadosFiltrados(),category_col = "evadido",
                         category_order = c(0,1), 
                         category_labels = c("Concluínte","Desvinculado"),
                         titulo="Status")})

  output$plot2<-renderHighchart({
    force(forceRedraw())
    plot_bar(dadosFiltrados(),category_col = "Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?",titulo="Curso")})
  
  # output$plot3<-renderHighchart({
  #   force(forceRedraw())
  #   plot_bar(dadosFiltrados(),category_col = "Em que ano você iniciou seu último curso de pós-graduação stricto sensu na UFMT?",tipo="line",titulo="Ano de início do curso") })
  plot3_filter <- reactiveVal("total")
  
  plot3_data<- reactive({
    if (plot3_filter() == "total") {
      dadosFiltrados() %>% 
        filter(!is.na(`Em que ano você iniciou seu último curso de pós-graduação stricto sensu na UFMT?`)) %>%
        dplyr::count(`Em que ano você iniciou seu último curso de pós-graduação stricto sensu na UFMT?`) %>%
        mutate(percent = n / sum(n) * 100)
      
    } else {
      dadosFiltrados() %>% 
        filter(!is.na(`Em que ano você iniciou seu último curso de pós-graduação stricto sensu na UFMT?`) & evadido == plot3_filter()) %>%
        dplyr::count(`Em que ano você iniciou seu último curso de pós-graduação stricto sensu na UFMT?`) %>%
        mutate(percent = n / sum(n) * 100)
    }
    
  })

  output$plot3<-renderHighchart({
    
    force(forceRedraw())
    
    highchart() %>%
      hc_chart(lang = list(decimalPoint = ',', thousandsSep = '.')) %>%
      hc_title(text="Ano de Início e Conclusão do Curso") %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = '{value}%')) %>%
      # hc_tooltip(shared = TRUE, useHTML = TRUE, headerFormat = '<span style="font-size: 0.8em">{point.key}</span><br/>') %>%
      hc_tooltip(shared = TRUE, useHTML = TRUE, headerFormat = '<span style="font-size: 0.8em">{point.key}</span><br/>',
                 pointFormat = '<span style="color:{point.color}">\u25AA</span> Frequência (%): <b>{point.n} ({point.y:.1f}%)</b><br/>') %>%
      hc_legend(enabled = TRUE)%>%
      hc_add_series(
        data = plot3_data(),
        type = "line",
        hcaes(x = `Em que ano você iniciou seu último curso de pós-graduação stricto sensu na UFMT?`, y = percent),
        name = "Ano de início do curso"
      )%>%
      hc_add_series(
        data = dadosFiltrados() %>% 
          filter(!is.na(`Em que ano concluiu seu curso de pós-graduação? Caso tenha sido desligado, informe o ano em que ocorreu o desligamento`)) %>%
          dplyr::count(`Em que ano concluiu seu curso de pós-graduação? Caso tenha sido desligado, informe o ano em que ocorreu o desligamento`) %>%
          mutate(percent = n / sum(n) * 100),
        type = "line",
        hcaes(x = `Em que ano concluiu seu curso de pós-graduação? Caso tenha sido desligado, informe o ano em que ocorreu o desligamento`, y = percent),
        name = "Ano de conclusão ou desligamento"
      )%>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = botoes_menu
          ),
          customButtonTotal = list(
            text = 'Total',
            onclick = JS("function() {
              Shiny.onInputChange('plot3_filter', 'total');
            }"),
            align = 'right',     # Align buttons to the left
            verticalAlign = 'top',  # Align buttons to the top
            width=100,
            # x = 160,  # Horizontal offset from the left
            y = 30 ,  # Vertical offset from the top
            theme=theme_buttons
          ),
          customButton0 = list(
            text = 'Concluíntes',
            onclick = JS("function() {
              Shiny.onInputChange('plot3_filter', 0);
            }"),
            align = 'right',     # Align buttons to the left
            verticalAlign = 'top',  # Align buttons to the top
            # x = 160,  # Horizontal offset from the left
            y = 30,   # Vertical offset to place below the previous button
            theme=theme_buttons
          ),
          customButton1 = list(
            text = 'Desvinculados',
            onclick = JS("function() {
              Shiny.onInputChange('plot3_filter', 1);
            }"),
            align = 'right',     # Align buttons to the left
            verticalAlign = 'top',  # Align buttons to the top
            # x = 160,  # Horizontal offset from the left
            y = 30,  # Vertical offset to place below the previous button
            theme=theme_buttons
          )
        )
      )
    })
  
  observeEvent(input$plot3_filter, {
    plot3_filter(input$plot3_filter)
  })
  
  # output$plot4<-plot_bar(dadosFiltrados(),category_col = "Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?") %>% renderHighchart()
   output$plot4<-renderHighchart({
     force(forceRedraw())
     plot_bar(dadosFiltrados(),category_col = "Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?",titulo="Nível acadêmico")})
   
  output$plot5<-
    renderHighchart({
      force(forceRedraw())
      plot_bar(
    dadosFiltrados(),category_col = "Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?",titulo="Câmpus")
    })
  
  output$plot6<-renderHighchart({
    force(forceRedraw())
    plot_bar(dadosFiltrados(),category_col = "Em que ano concluiu seu curso de pós-graduação? Caso tenha sido desligado, informe o ano em que ocorreu o desligamento",tipo="line",titulo="Ano de conclusão/desligamento do curso")
  })
#  output$plot7<-plot_box(dadosFiltrados(),category_col = "Idade no ingresso") %>% renderHighchart()

  ############## piramide etaria
categorize_age <- function(age) {
  breaks <- c(seq(0, 95, 5), 100, Inf)
  labels <- c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44',
              '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85-89', '90-94',
              '95-99', '100 +')
  label <- findInterval(age, vec = breaks)
  return(labels[label])
}
# processed_data<-data %>%
#   filter(`Identidade de gênero` %in% c("Masculina", "Feminina")) %>%
#   select("Identidade de gênero","Idade no ingresso") %>%
#   na.omit() %>%
#   mutate(age_group = sapply(`Idade no ingresso`, categorize_age)) %>%
#   group_by(age_group, `Identidade de gênero`) %>%
#   summarize(count = n(), .groups = 'drop') %>%
#   mutate(count = if_else(`Identidade de gênero` == "Feminina", -count, count)) #%>%
#  # arrange(`Identidade de gênero`, match(age_group, categories))%>%
#   #arrange(match(age_group, categories))
# 
# categories <- processed_data %>%
#   select(age_group) %>%
#   distinct() %>%
#   arrange(match(age_group, unique(processed_data$age_group))) %>%
#   pull(age_group)
    
plot7_filter <- reactiveVal("total")

filtered_data <- reactive({
  if (plot7_filter() == "total") {
    data_intermed <- dadosFiltrados() %>%
      filter(`Identidade de gênero` %in% c("Masculina", "Feminina")) %>%
      select("Identidade de gênero", "Idade no ingresso") %>%
      na.omit() %>%
      mutate(age_group = sapply(`Idade no ingresso`, categorize_age)) %>%
      group_by(age_group, `Identidade de gênero`) %>%
      summarize(count = n(), .groups = 'drop') %>%
      mutate(count = if_else(`Identidade de gênero` == "Feminina", count, -count))
    
    total_population <- sum(abs(data_intermed$count))
    
    data_intermed %>%
      mutate(percentage = count / total_population * 100)
  } else {
    data_intermed <- dadosFiltrados() %>%
      filter(`Identidade de gênero` %in% c("Masculina", "Feminina") & evadido == plot7_filter()) %>%
      select("Identidade de gênero", "Idade no ingresso") %>%
      na.omit() %>%
      mutate(age_group = sapply(`Idade no ingresso`, categorize_age)) %>%
      group_by(age_group, `Identidade de gênero`) %>%
      summarize(count = n(), .groups = 'drop') %>%
      mutate(count = if_else(`Identidade de gênero` == "Feminina", count, -count)) %>%
      mutate(percentage = count / sum(abs(count)) * 100)
    
    total_population <- sum(abs(data_intermed$count))
    
    data_intermed %>%
      mutate(percentage = count / total_population * 100)
  }
})
categories <- c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69")

theme_buttons<-list(
 # align="center",
 # fill = '#1C4E80',  # Bootstrap primary color
  stroke = '#F1F1F1',  # Darker border color
 # 'stroke-width' = 1,
  r = 1,  # Border radius for rounded corners
 # width = 60,  # Fixed width for buttons
  height = 30,  # Fixed height for buttons
  style = list(
  #  color = '#F1F1F1',  # White text color
  #  fontWeight = 'bold',
    padding = '15px',  # Padding for a more bubble-like appearance
   #textAlign = 'center',  # Center align text
  #  borderRadius = '5px',  # Ensuring text aligns within the bubble
    boxShadow = '2px 4px 6px rgba(0, 0, 0, 0.1)'  # Adding a subtle shadow

  )
)
  
output$plot7 <- renderHighchart({
  force(forceRedraw())
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_title(text = "Idade no ingresso por gênero") %>%
    hc_xAxis(
      list(
        categories = categories,
        reversed = FALSE
      ),
      list(
        opposite = TRUE,
        reversed = FALSE,
        linkedTo = 0,
        categories = categories
      )) %>%
    hc_yAxis(labels = list(formatter = JS("function () { return Math.abs(this.value) + '%'; }")), 
             title = list(text = "")) %>%
    hc_plotOptions(series = list(stacking = 'normal')) %>%
    hc_tooltip(formatter = JS("
    function () {
      var percentage = Math.abs(this.point.y);
      var count = Math.abs(this.point.count);
      return '<span style=\"font-size: 0.8em\">' + this.point.category + '</span><br/>' +
             '<span style=\"color:' + this.point.color + '\">\u25AA</span> ' + this.series.name + ': <b>' + count +
             '</b> (' + Highcharts.numberFormat(percentage, 1) + '%)<br/>';
    }")) %>%
    hc_add_series(name = "Masculino", data = filtered_data() %>% filter(`Identidade de gênero` == "Masculina") %>% arrange(match(age_group, categories)) %>% select(y = percentage, count = count)) %>%
    hc_add_series(name = "Feminino", data = filtered_data() %>% filter(`Identidade de gênero` == "Feminina") %>% arrange(match(age_group, categories)) %>% select(y = percentage, count = count)) %>%
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = botoes_menu
        ),
        customButtonTotal = list(
          text = 'Total',
          onclick = JS("function() {
              Shiny.onInputChange('plot7_filter', 'total');
            }"),
          align = 'right',     # Align buttons to the left
          verticalAlign = 'top',  # Align buttons to the top
          width=100,
          # x = 160,  # Horizontal offset from the left
         y = 30 ,  # Vertical offset from the top
          theme=theme_buttons
        ),
        customButton0 = list(
          text = 'Concluíntes',
          onclick = JS("function() {
              Shiny.onInputChange('plot7_filter', 0);
            }"),
          align = 'right',     # Align buttons to the left
          verticalAlign = 'top',  # Align buttons to the top
          # x = 160,  # Horizontal offset from the left
           y = 30,   # Vertical offset to place below the previous button
        theme=theme_buttons
        ),
        customButton1 = list(
          text = 'Desvinculados',
          onclick = JS("function() {
              Shiny.onInputChange('plot7_filter', 1);
            }"),
          align = 'right',     # Align buttons to the left
          verticalAlign = 'top',  # Align buttons to the top
          # x = 160,  # Horizontal offset from the left
           y = 30,  # Vertical offset to place below the previous button
         theme=theme_buttons
        )
      )
    )
})

observeEvent(input$plot7_filter, {
  plot7_filter(input$plot7_filter)
  #forceRedraw(!forceRedraw())  # Toggle the value to trigger reactivity
})
    



plot13_filter <- reactiveVal("total")

plot13_data<- reactive({
  if (plot13_filter() == "total") {
           dadosFiltrados() %>%
            filter(!is.na(`Estado de nascimento`)) %>%
            count(`Estado de nascimento`)%>%
            mutate(percent = n / sum(n) * 100)
  } else {
          dadosFiltrados() %>%
            filter(!is.na(`Estado de nascimento`) & evadido == plot13_filter()) %>%
            count(`Estado de nascimento`)%>%
            mutate(percent = n / sum(n) * 100)
  }
})



output$plot13<-renderHighchart({
  force(forceRedraw())
  hcmap("countries/br/br-all", download_map_data =FALSE,
        data = plot13_data(), value = "n", joinBy = c("name","Estado de nascimento"),
        name = "Estado de Nascimento",
        dataLabels = list(enabled = TRUE, format = '{point.name}'),
        tooltip = list()) %>%
    hc_title(text = "Estado de Nascimento") %>%
    hc_legend(layout = "vertical", align = "right", verticalAlign = "middle") %>%
    hc_tooltip(shared = TRUE, useHTML = TRUE, headerFormat = '<span style="font-size: 0.8em">{point.key}</span><br/>',
               pointFormat = '<span style="color:{point.color}">\u25AA</span> Frequência (%): <b>{point.value}</b> ({point.percent:.1f}%)<br/>') %>%
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = botoes_menu
        ),
        customButtonTotal = list(
          text = 'Total',
          onclick = JS("function() {
              Shiny.onInputChange('plot13_filter', 'total');
            }"),
          align = 'right',     # Align buttons to the left
          verticalAlign = 'top',  # Align buttons to the top
         # width=100,
         # x = -160,  # Horizontal offset from the left
          y = 30 ,  # Vertical offset from the top
          theme=theme_buttons
        ),
        customButton0 = list(
          text = 'Concluíntes',
          onclick = JS("function() {
              Shiny.onInputChange('plot13_filter', 0);
            }"),
          align = 'right',     # Align buttons to the left
          verticalAlign = 'top',  # Align buttons to the top
          #x = -160,  # Horizontal offset from the left
          y = 30,   # Vertical offset to place below the previous button
          theme=theme_buttons
        ),
        customButton1 = list(
          text = 'Desvinculados',
          onclick = JS("function() {
              Shiny.onInputChange('plot13_filter', 1);
            }"),
          align = 'right',     # Align buttons to the left
          verticalAlign = 'top',  # Align buttons to the top
          #x = -160,  # Horizontal offset from the left
          y = 30,  # Vertical offset to place below the previous button
          theme=theme_buttons
        )
      )
    )
})

observeEvent(input$plot13_filter, {
  plot13_filter(input$plot13_filter)
})


output$plot8<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col =  "Você fez uso de algum período de prorrogação de prazo para o término do seu curso de pós-graduação? Se sim, qual foi o período de prorrogação utilizado?",
           titulo="Uso de período de prorrogação") 
})

output$plot9<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col =  "area_conhecimento_curso",titulo="Área de conhecimento do curso") 
})
output$plot10<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Você trancou sua matrícula no programa de pós-graduação? Se sim, por quanto tempo?",
           titulo="Trancamento de matrícula")
})

output$plot11<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Você recebeu bolsa de estudos ou auxílio financeiro durante o curso de pós-graduação? Se sim, por quanto tempo?",titulo="Recebimento de bolsa de estudos/auxílio financeiro",category_order=c("Não recebi bolsa ou auxílio financeiro","Sim, recebi por menos de 1 ano","Sim, recebi entre 1 a 2 anos","Sim, recebi por mais de 2 anos (para cursos com duração superior a 2 anos)","Sim, recebi por todo o período do curso"))
})

output$plot12<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Você precisou mudar de cidade para realizar o curso de pós-graduação?")
})




output$plot15<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Estado civil ao iniciar o curso de pós-graduação",
           category_order=c("Solteiro(a)","Casado(a)","Divorciado(a)","Separado(a) Judicialmente","Viúvo(a)","União Estável"))
})

output$plot16<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Identidade de gênero")
})

output$plot17<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Como você se identifica em relação à sua orientação sexual?",titulo="Orientação sexual",
          # category_order=c("Heterossexual","Gay ou Lésbica","Bissexual","Pansexual","Assexual","Estou questionando minha orientação sexual","Prefiro não responder"))
          category_order=c("Heterossexual","Gay ou Lésbica","Bissexual/Pansexual","Outros"))
})

output$plot18<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Qual opção melhor descreve sua raça ou etnia?",titulo="Raça/Etnia",category_order = c("Branca","Preta","Parda","Amarela","Indígena"))
})


output$plot19<-renderHighchart({
  force(forceRedraw())
  hchart(
  dadosFiltrados() %>% 
    separate_rows(`Apresenta alguma deficiência ou transtorno? Marque todos que se aplicam`, sep = ";") %>% 
    filter(`Apresenta alguma deficiência ou transtorno? Marque todos que se aplicam` != "Não, nenhuma deficiência ou transtorno") %>% 
    count(`Apresenta alguma deficiência ou transtorno? Marque todos que se aplicam`, sort = TRUE) %>% 
    mutate(percent = n / sum(n) * 100), 
  "column", hcaes(x = `Apresenta alguma deficiência ou transtorno? Marque todos que se aplicam`, y = percent)
) %>%
  hc_yAxis(labels = list(format = "{value}%"),title = list(text = "")) %>%
    hc_xAxis(title = list(text = "")) %>%
  hc_title(text = "Tipos de deficiência/transtorno") %>%
  hc_tooltip(
    shared = TRUE, 
    useHTML = TRUE, 
    headerFormat = '<span style="font-size: 0.8em">{point.key}</span><br/>',
    pointFormat = '<span style="color:{point.color}">\u25AA</span> Frequência (%): <b>{point.n} ({point.y:.1f}%)</b><br/>'
  ) %>%
  hc_add_series(
    data = dadosFiltrados() %>%
      mutate(tem_deficiencia = ifelse(grepl("Não, nenhuma deficiência ou transtorno", `Apresenta alguma deficiência ou transtorno? Marque todos que se aplicam`), 
                                      "Nenhuma deficiência ou transtorno", "Possui alguma deficiência ou transtorno")) %>%
      mutate(tem_deficiencia = factor(tem_deficiencia, levels = c("Possui alguma deficiência ou transtorno", "Nenhuma deficiência ou transtorno"))) %>%
      count(tem_deficiencia) %>%
      mutate(percent = n / sum(n) * 100) %>%
      list_parse2(),
    type = "pie",
    name = "Proporção de Deficiências",
    center = c('75%', '10%'),
    dataLabels=list(enabled=FALSE),
    size = 50,
    showInLegend = TRUE,
    tooltip = list(
      pointFormat = '<span style="color:{point.color}">\u25AA</span>Frequência (%): <b>{point.y}</b> ({point.percentage:.1f}%)<br/>'
    )
  ) %>%
    
    hc_annotations(
      list(
        labels = list(
          list(
            point = list(xAxis = 1, yAxis = 1, x = 750, y = 10),
            text = "Proporção de Deficiências"
          #  style = list(fontSize = "14px", fontWeight = "bold")
          )
        )
      )
    )
})


output$plot20<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Qual era o rendimento per capita familiar durante o período em que realizou o curso de pós-graduação?",titulo="Rendimento per capita familiar durante o curso",
           category_order=c(
             "Sem renda",
             "Até 1 salário-mínimo",
             "De 1 até 2 salários-mínimos",
             "De 2 até 3 salários-mínimos",
             "De 3 até 5 salários-mínimos",
             "De 5 até 8 salários-mínimos",
             "Acima de 8 salários-mínimos",
             "Prefiro não responder"
           ))
})

output$plot21<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Você cursou o ensino superior (graduação) predominantemente em",titulo="Ensino superior (graduação) cursado em",category_order=c("Instituição pública","Instituição particular","Alternância entre instituições públicas e particulares"))
})

output$plot22<-renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(),category_col = "Antes de seu desligamento do curso de pós-graduação, chegou a realizar exame de qualificação?")
})

output$plot23<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Após o desligamento nesse programa, você concluiu ou está cursando outro curso de pós-graduação stricto sensu?",category_order = c("Não concluí e não estou cursando","Cursando na UFMT","Cursando em outra instituição","Concluí em outra instituição"))
})

output$plot24<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Você trabalhou enquanto estava cursando pós-graduação?")
})

output$plot25<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Especifique a natureza da jornada de trabalho durante a pós-graduação")
})

output$plot26<-renderHighchart({
  force(forceRedraw())
    dadosFiltrados () %>%
      separate_rows("Caso tenha mantido vínculo empregatício (trabalhando ou não), qual categoria melhor descreve seu ambiente de trabalho? Marque todas que se aplicam", sep = ";") %>%
      mutate_all(~ if(is.character(.)) na_if(trimws(.), "") else .) %>%
  plot_bar(.,category_col = "Caso tenha mantido vínculo empregatício (trabalhando ou não), qual categoria melhor descreve seu ambiente de trabalho? Marque todas que se aplicam",titulo="Categoria do ambiente de trabalho",category_order=c("Iniciativa privada","Setor público","Organizações Não Governamentais (ONGs)","Parceria público-privada","Trabalhador autônomo","Desempregado","Não teve vínculo (bolsista)"))
})

output$plot27<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Por que motivo não trabalhou durante o período cursado?")
})

output$plot29 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Como você classificaria seu nível de desempenho acadêmico ao longo do curso de pós-graduação?")
})

output$plot30 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Qual o seu nível de satisfação com a didática dos docentes em classe?")
})

output$plot31 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Como você classificaria a disponibilidade dos docentes para responder às dúvidas dos discentes durante o curso de pós-graduação?")
})

output$plot32 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "De forma geral, como você avalia o nível de suporte recebido do seu orientador durante a realização de sua dissertação/tese de pós-graduação?")
})

output$plot33 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Qual o seu nível de satisfação com a acessibilidade, qualidade e eficácia dos materiais didáticos e recursos de aprendizagem oferecidos em seu curso de pós-graduação?")
})

output$plot34 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Como você descreveria sua frequência de participação em atividades acadêmicas extracurriculares (como seminários, workshops e outros eventos) durante o seu curso de pós-graduação?")
})

output$plot35 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Como você avalia as oportunidades de colaboração entre alunos, como trabalhos em grupo ou projetos de pesquisa, no seu programa de pós-graduação?")
})

output$plot36 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Como você avalia seu nível de interação com colegas dentro do ambiente acadêmico do seu programa de pós-graduação?")
})

output$plot37 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Como você avalia seu nível de integração e participação em atividades sociais com colegas do seu programa de pós-graduação, realizadas fora do ambiente acadêmico?")
})

output$plot38 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Como você avalia a liberdade de expressar suas opiniões no ambiente acadêmico do seu programa de pós-graduação?")
})

output$plot39 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Durante o seu curso de pós-graduação, com que frequência você socializou com colegas em momentos de lanches e refeições?")
})

output$plot40 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "No decorrer do seu programa de pós-graduação, com que frequência você testemunhou ou sofreu alguma forma de assédio, entendido como condutas indesejadas de natureza física, verbal ou psicológica, que visaram ou resultaram em ofensa ou humilhação?")
})

output$plot41 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Em que medida a escolha desta instituição específica se alinhou com seus planos de carreira?")
})

output$plot42 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Em que medida a oferta de cursos nesta área influenciou sua escolha por esta instituição?")
})

output$plot43 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Como você avaliaria os custos indiretos (moradia, transporte, material didático, custo de vida) associados a estudar nesta instituição, em comparação com outras escolhas?")
})

output$plot44 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Qual a probabilidade de você recomendar esta instituição para futuros estudantes de pós-graduação?")
})

output$plot45 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Qual o seu nível de satisfação com a eficácia da comunicação entre a coordenação do curso e os estudantes?")
})

output$plot46 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Como você avalia a qualidade e adequação da infraestrutura física da instituição (prédios, laboratórios, salas de aula, espaços de convivência) para atender às necessidades acadêmicas dos alunos?")
})

output$plot47 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Durante o seu curso de pós-graduação, como você classificaria sua intenção de buscar futuras oportunidades educacionais, como doutorado, pós-doutorado ou formações adicionais em sua área?")
})

output$plot48 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Durante o curso de pós-graduação, como descreveria sua disposição ou interesse em trabalhar na área do curso?")
})

output$plot49 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Em que nível o programa de pós-graduação atendeu às suas expectativas no desenvolvimento de suas competências?")
})

output$plot50 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "No início dos estudos, qual era seu nível de intenção de completar o curso?")
})

output$plot51 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "Durante a pós-graduação, como você avaliaria a relevância do curso para sua carreira profissional?")
})

output$plot52 <- renderHighchart({
  force(forceRedraw())
  likert_chart(dadosFiltrados(), category_col = "No decorrer do seu programa de pós-graduação, com que frequência você testemunhou ou sofreu alguma forma de assédio, entendido como condutas indesejadas de natureza física, verbal ou psicológica, que visaram ou resultaram em ofensa ou humilhação?")
})


## regressão ----

rv <- reactiveValues(
  modelo_step = NULL,
  resultados_bootstrap = NULL,
  coeficientes_df = NULL,
  info_criteria = NULL,
  configuracoes_modelo=NULL,
)

padrao <- reactiveVal(TRUE)  # padrao starts as TRUE

observe({
  if (padrao()) {
    rv$modelo_step <- modelo_step_padrao
    rv$info_criteria <- info_criteria_padrao
    rv$coeficientes_df <- coeficientes_df_padrao
    rv$resultados_bootstrap <- resultados_bootstrap_padrao
    rv$roc <- roc_padrao
    
    updateUIWithModel()  # Update the UI with the default model
  }
})

# Function to update UI with the model
updateUIWithModel <- function() {
  
  output$stepwise <- renderPrint({
    list(Modelo = rv$modelo_step)
  })
  
  output$info_criteria <- renderPrint({
    rv$info_criteria
  })
  
  output$residualsPlot <- renderPlot({
    plot(rv$modelo_step, which = 1)
  })
  
  output$qqPlot <- renderPlot({
    plot(rv$modelo_step, which = 2)
  })
  output$scaleLocationPlot <- renderPlot({
    plot(rv$modelo_step, which = 3)
  })
  output$cooksDistancePlot <- renderPlot({
    plot(rv$modelo_step, which = 4)
  })
  
  output$tornadoPlot <- renderPlot({
    ggplot2::ggplot(rv$coeficientes_df, ggplot2::aes(x = Variable, y = Estimate)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Tornado Plot", x = "Variable", y = "Coefficient Estimates") +
      ggplot2::scale_x_discrete(labels = scales::label_wrap(45))
  })
  
  output$configuracoes_modelo <- renderText({
    paste0("Configurações do modelo: \n",
    "Include Bootstrap: ", input$include_bootstrap, "\n",
    if (input$include_bootstrap) {
      paste0(
        "Proporção Treino: ", input$proporcao_treino, "%\n",
        "Repetições Bootstrap: ", input$slider1, "\n"
      )
    } else "",
    "Critério: ", input$criteria, "\n",
    "Direção: ", input$direction
  )
    })
  
  ###
  if(!is.null(rv$resultados_bootstrap)){
    
    print("bootstrap is not null") #debug 
    
    shinyjs::show("boot")
    shinyjs::show("bootCoef")
    shinyjs::show("bootOR")
    
    output$boot <- DT::renderDT({
      dfBoot <- rv$resultados_bootstrap$metrics
      rownames(dfBoot) <- NULL
      dfBoot <- as.data.frame(dfBoot)
      boottable<-calcular_ic(dfBoot)
      
      names(boottable)<-c("Variável","Média","2,5%","97,5%","N válidos")
      boottable<-boottable %>%
        mutate(across(everything(), ~ formatC(.x, format = "f", digits = 2, decimal.mark = ",")))
      # DT::datatable(boottable, rownames = FALSE, 
      #               extensions = 'Buttons',
      #               options = list(
      #                 dom = 'Bfrtip',
      #                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      #                 language = list(
      #                   url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
      #                 )
      #               )
      # )
      tabela_dt(boottable)
    })
    
    output$bootCoef <- DT::renderDT({
      dfBootCoef <- rv$resultados_bootstrap$coef
      rownames(dfBootCoef) <- NULL
      dfBootCoef <- as.data.frame(dfBootCoef)
      auxc <- calcular_ic(dfBootCoef) %>%
        mutate(across(everything(), ~ formatC(.x, format = "f", digits = 2, decimal.mark = ",")))
      names(auxc)<-c("Variável","Média","2,5%","97,5%","N válidos")
      
      tabela_dt(auxc)
      
    })
    
    
    output$bootOR <- DT::renderDT({
      
      dfBootOR <- rv$resultados_bootstrap$odds_ratios
      rownames(dfBootOR) <- NULL
      dfBootOR <- as.data.frame(dfBootOR)
      auxc <- calcular_ic(dfBootOR)%>%
        mutate(across(everything(), ~ formatC(.x, format = "f", digits = 2, decimal.mark = ",")))
      names(auxc)<-c("Variável","Média","2,5%","97,5%","N válidos")
      
      
      tabela_dt(auxc)
    })
  }
  else{
    print("bootstrap is null") #debug 
    
    shinyjs::hide("boot")
    shinyjs::hide("bootCoef")
    shinyjs::hide("bootOR")
    
  }
}

## Funções ----

calcular_ic <- function(df) {
  calcular_valores <- function(coluna) {
    coluna <- na.omit(coluna)
    media <- mean(coluna)
    q2.5 <- quantile(coluna, 0.025)
    q97.5 <- quantile(coluna, 0.975)
    n_validos <- length(coluna)
    return(data.frame("Média" = media, "2,5%" = q2.5, "97,5%" = q97.5, "N válidos" = n_validos))
  }
  resultado <- purrr::map_dfr(df, calcular_valores, .id = "Variável")
  return(resultado)
}

calcular_metricas <- function(modelo, dados, resposta) {
  pred_prob <- predict(modelo, type = "response")
  pred <- ifelse(pred_prob > 0.5, 1, 0)
  cm <- caret::confusionMatrix(factor(pred), factor(resposta))
  metrics <- data.frame(
    Accuracy = cm$overall["Accuracy"],
    Sensitivity = cm$byClass["Sensitivity"],
    Specificity = cm$byClass["Specificity"],
    VPP = cm$byClass["Pos Pred Value"],
    VPN = cm$byClass["Neg Pred Value"]
  )
  return(metrics)
}

# Função para ajustar modelos e calcular métricas de avaliação
ajustar_modelos <- function(dados_treino, dados_teste) {
  # Ajustar o modelo
  
  modelo_step <- glm(fstep, data=dados_treino,family="binomial")
  
  # Avaliação do modelo no conjunto de teste
  predicoes <- predict(modelo_step, newdata = dados_teste, type = "response")
  pred_class <- ifelse(predicoes > 0.5, 1, 0)
  conf_matrix <- confusionMatrix(factor(pred_class), factor(dados_teste$evadido))
  
  # Coletar coeficientes e odds ratios
  coef <- coef(modelo_step)
  odds_ratios <- exp(coef)
  
  # Métricas de avaliação
  metrics <- data.frame(
    Acurácia = conf_matrix$overall['Accuracy'],
    Sensibilidade = conf_matrix$byClass['Sensitivity'],
    Especificidade = conf_matrix$byClass['Specificity']
  )
  
  
  list(
    metrics = metrics,
    coef = coef,
    odds_ratios = odds_ratios
  )
}


bootstrap_modelos <- function(dados, n_bootstrap) {
  resultados <- replicate(n_bootstrap, {
    num_individuos <- nrow(dados)
    
    # Número de indivíduos no conjunto de treino
    num_treino <- round(input$proporcao_treino * num_individuos/100)
    
    # Amostra aleatória, sem reposição, de índices para o conjunto de treino
    indices_treino <- sample(num_individuos, num_treino, replace = FALSE)
    
    # Indivíduos restantes para o conjunto de teste
    indices_teste <- setdiff(1:num_individuos, indices_treino)
    
    # Amostra bootstrap
    indices_bootstrap_treino <- sample(num_treino, num_treino, replace = TRUE)
    indices_bootstrap_teste <- sample(num_individuos-num_treino, 
                                      num_individuos-num_treino, 
                                      replace = TRUE)
    
    dados_treino <- dados[indices_treino[indices_bootstrap_treino], ]
    dados_teste <- dados[indices_teste[indices_bootstrap_teste], ]
    
    # Ajustar modelos e calcular métricas
    ajustar_modelos(dados_treino, dados_teste)
  }, simplify = FALSE)
  
  # Coletar resultados
  metrics <- do.call(rbind, lapply(resultados, function(x)
    x$metrics))
  coef <- do.call(rbind, lapply(resultados, function(x)
    x$coef))
  odds_ratios <- do.call(rbind, lapply(resultados, function(x)
    x$odds_ratios))
  
  list(
    metrics = metrics,
    coef = coef,
    odds_ratios = odds_ratios
  )
}

## input$analyze ----
w <- Waiter$new(
  html = spin_3(), # Choose your preferred spinner
  color = transparent(.2) # Set a semi-transparent white overlay
)

observeEvent(input$analyze, {
  
  #shinybusy::show_modal_progress_circle()
  w$show()
  
  padrao(FALSE)  # Set padrao to FALSE when analyze is triggered
  
  # Resetando rv
  rv$modelo_step <- NULL
  rv$info_criteria <- NULL
  rv$coeficientes_df <- NULL
  rv$resultados_bootstrap <- NULL
  rv$roc <- NULL
  
  
  dados_regressao<-reactive({
    dados_numericos
  })
  
  # Model with all covariates
  ajuste <- glm(evadido ~ ., data = dados_regressao(), family = "binomial")
  
  if (input$criteria == "AIC") {
    modelo_step <- step(ajuste_nulo, scope = formula(ajuste), direction = input$direction)
  }
  
  if (input$criteria == "BIC") {
    modelo_step <- step(ajuste_nulo, scope = formula(ajuste), direction = input$direction, k = log(nrow(dados_regressao())))
  }
  
  rv$modelo_step <- modelo_step
  
  fstep <<- modelo_step$formula
  modelo_final_step <- formula(modelo_step)
  vfstep <- all.vars(modelo_final_step)[-1]
  
  
  if (input$include_bootstrap) {
    resultados_bootstrap <<- bootstrap_modelos(dados_regressao(), n_bootstrap = input$slider1)
    
    rv$resultados_bootstrap <- resultados_bootstrap
  } else {
    rv$resultados_bootstrap<-NULL
  }
  
  coeficientes <- summary(modelo_step)$coefficients
  coeficientes_df <- as.data.frame(coeficientes)
  coeficientes_df <- coeficientes_df[-1, ]
  coeficientes_df$Variable <- rownames(coeficientes_df)
  coeficientes_df <- coeficientes_df %>%
    arrange(Estimate) %>%
    mutate(Variable = factor(Variable, levels = Variable))
  
  rv$coeficientes_df<-coeficientes_df
  
  
  prob <- predict(modelo_step, type = "response")
  roc <- pROC::roc(dados_regressao()$evadido, prob)
  rv$roc<-roc
  
  
  rv$info_criteria<-list(
    LogLik_Stepwise = logLik(modelo_step),
    AIC_Stepwise = AIC(modelo_step),
    BIC_Stepwise = BIC(modelo_step),
    LogLik_Nulo = logLik(ajuste_nulo),
    AIC_Nulo = AIC(ajuste_nulo)
  )
  
  # Chamar a função para atualizar a interface
  updateUIWithModel()
  
  
  w$hide()
  #shinybusy::remove_modal_spinner()
  
  # Salvando modelo ----
  observeEvent(input$save_results, {
    tryCatch({
      saveRDS(modelo_step, "modelo_step.rds")
      saveRDS(list(
        LogLik_Stepwise = logLik(modelo_step),
        AIC_Stepwise = AIC(modelo_step),
        BIC_Stepwise = BIC(modelo_step),
        LogLik_Nulo = logLik(ajuste_nulo),
        AIC_Nulo = AIC(ajuste_nulo)
      ),"info_criteria.rds")
      
      coeficientes <- summary(modelo_step)$coefficients
      coeficientes_df <- as.data.frame(coeficientes)
      coeficientes_df <- coeficientes_df[-1, ]
      coeficientes_df$Variable <- rownames(coeficientes_df)
      coeficientes_df <- coeficientes_df %>%
        arrange(Estimate) %>%
        mutate(Variable = factor(Variable, levels = Variable))
      
      saveRDS(coeficientes_df, "coeficientes_df_tornado.rds")
      
      saveRDS(resultados_bootstrap,"resultados_bootstrap.rds")
      
      saveRDS(resultados_bootstrap,"resultados_bootstrap.rds")
      
      prob <- predict(modelo_step, type = "response")
      # pROC::roc(dados_numericos$evadido, prob)
      roc <- pROC::roc(dados_regressao()$evadido, prob)
      saveRDS(roc,"regressao/roc.rds")
      showNotification("Resultados salvos com sucesso!", type = "message")
    }, error = function(e) {
      showNotification(paste("Erro ao salvar resultados:", e$message), type = "error")
    })
  })
})

# Restore the default model when the restore button is clicked ----
observeEvent(input$restore_default, {
  padrao(TRUE)
  #  rv$modelo_step <- modelo_step_padrao
  #  rv$info_criteria <- info_criteria_padrao
  #  rv$coeficientes_df <- coeficientes_df_padrao
  #  rv$resultados_bootstrap <- resultados_bootstrap_padrao
  #  rv$roc <- roc_padrao
  # updateUIWithModel()
  showNotification("Modelo padrão restaurado!", type = "message")
})

}

# Executa o aplicativo
shinyApp(ui, server)
