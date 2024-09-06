library(tidytable)
library(data.table) 
library(DT)
library(bs4Dash)
library(shiny)
library(shinyWidgets)
library(highcharter)
library(shinyWidgets)
library(fresh) 
library(waiter) 
library(dplyr)
library(tidyr)

source("funcoes dashboard.R")

options(highcharter.lang = lang)

# Dados descritiva ------

data <- data.table(arrow::read_feather("dados_feather.feather"))


# Dados regressão -----------

dados_numericos<-data.table(arrow::read_feather("dados-dummy.feather"))


modelo_step_padrao <- readRDS("regressao/modelo_step.rds")
info_criteria_padrao <- readRDS("regressao/info_criteria.rds")
coeficientes_df_padrao <- readRDS("regressao/coeficientes_df_tornado.rds")
resultados_bootstrap_padrao <- readRDS("regressao/resultados_bootstrap.rds")
roc_padrao <- readRDS("regressao/roc.rds")
configuracoes_modelo_padrao <- readRDS("regressao/configuracoes_modelo_padrao.rds")

## Mapeamento de variáveis dummy

dummy_vars <- names(dados_numericos)[stringr::str_detect(names(dados_numericos), stringr::coll("_"))]
unique_vars <- unique(stringr::str_remove(dummy_vars, "_.+"))

dummy_mapping <- lapply(unique_vars, function(var) {
  dummy_vars_for_var <- dummy_vars[stringr::str_detect(dummy_vars, stringr::coll(paste0(var, "_")))]
  choices <- setNames(dummy_vars_for_var, stringr::str_replace(dummy_vars_for_var, stringr::coll(paste0(var, "_")), ""))
  return(list(var = var, choices = choices))
})
variaveis_filtradas <- list(
  "Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?" = list(label = "Nível Acadêmico", id = "nivelacademico_reg"),
  "Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?" = list(label = "Campus", id = "campus_reg"),
  "Identidade de gênero" = list(label = "Identidade de gênero", id = "genero_reg"),
  "Qual opção melhor descreve sua raça ou etnia?" = list(label = "Raça/etnia", id = "raca_reg"),
  "Área de Conhecimento do curso" = list(label = "Área de conhecimento", id = "area_reg")
)


# Tema --------------

custom_theme <- create_theme(
  bs4dash_layout(
    main_bg = "#F2F2F2",
    sidebar_width="300px"
  ),
  bs4dash_status(
    primary = "#373799",
    info = "#7CB5EC",
    danger="#434348",
    warning="#DB8325"
  ),
  bs4dash_font(
    size_base="1rem",
    family_sans_serif = "'Open Sans', -apple-system, BlinkMacSystemFont, 'San Francisco', 'Segoe UI', Roboto, 'Helvetica Neue', sans-serif",
    family_base = "'Open Sans', -apple-system, BlinkMacSystemFont, 'San Francisco', 'Segoe UI', Roboto, 'Helvetica Neue', sans-serif"
  ),
  bs4dash_status(dark = "#24262D")
)


# ui ----------------------------------------------------------

ui <- dashboardPage(
  freshTheme = custom_theme,
  preloader = list(html = tagList(spin_loader()), color = transparent(.9)),
  scrollToTop=TRUE,
  help=NULL,
  title = "Dashboard - Fatores que influenciam a permanência e a evasão nos cursos de pós-graduação stricto sensu da UFMT",
  
  ## Cabeçalho  ----------------------------------------------------------
  
  header = dashboardHeader(title = tagList(
    img(src = "logo.png", height = "30px", style = "margin-left: 10px;")
  ),
  navbarMenu(
    id="tabs",
    navbarTab(
      "Sobre o dashboard",
      # tabName = "sobre" 
      navbarTab(
        "Informações Gerais",
        tabName = "sobre"
      ),
      navbarTab(
        "Metodologia",
        tabName = "metodologia"
      )
    ),
    navbarTab(
      "Visão geral",
      tabName = "resumo"
    ),
    navbarTab(
      "Análise de Regressão",
      tabName = "logistic"
    )
  ),
  controlbarIcon=icon("filter")
  ),
  ## Barra lateral  ----------------------------------------------------------
  
  sidebar = dashboardSidebar(disable = TRUE),
  
  ## Control bar ----------------------------------------------------------
  
  controlbar=dashboardControlbar(
    id="controlbar",
    pinned=FALSE,overlay=FALSE,
    
    ### Resumo -----
    conditionalPanel(
      condition = "input.tabs=='resumo'",
      
      controlbarMenu(type="hidden",
                     column(width=12,
                            h6("Filtros - Visão geral", class = "text-center"),
                            radioGroupButtons(
                              inputId = "comparar",
                              justified=TRUE,
                              choiceNames = c("Comparação", "Total"),
                              choiceValues=c("comparar","total"),
                              selected="comparar"
                            ),
                            filtro_box(
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
                                       animation = "smooth"
                                     )
                              )),
                            lapply(list(
                              #list("evadido_checkbox", "Status", c("Concluíntes"=0, "Desvinculados"=1)),
                              list("nivel_checkbox", "Nível acadêmico", unique(data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %>% na.omit())),
                              list("campus_checkbox", "Câmpus", unique(data$`Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?` %>% na.omit())),
                              list("genero_checkbox", "Identidade de gênero", unique(data$`Identidade de gênero`)),
                              list("etnia_checkbox", "Raça/Etnia", unique(data$`Qual opção melhor descreve sua raça ou etnia?`)),
                              list("areaConhecimento_checkbox", "Área de conhecimento", unique(data$`area_conhecimento_curso`)),
                              list("curso_checkbox", "Curso", unique(data$`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?`))
                            ), function(args) {
                              filtro_box(
                                title=args[[2]],
                                width = 12, headerBorder = TRUE, collapsible = TRUE,
                                column(width=12,offset=0,
                                       prettyCheckboxGroup(inputId = paste0(args[[1]], "_selectall"),
                                                           label = NULL,
                                                           choiceValues="selectall",
                                                           choiceNames="Selecionar tudo",
                                                           selected="selectall",
                                                           shape="curve",
                                                           status = "primary",
                                                           animation="smooth"),
                                       
                                       prettyCheckboxGroup(
                                         inputId = args[[1]],
                                         label = NULL,
                                         choiceValues=args[[3]],
                                         width="30px",
                                         choiceNames = args[[3]] %>% stringr::str_trunc(width = 25), 
                                         selected = args[[3]],  
                                         shape="curve",
                                         status = "primary",
                                         animation="smooth"
                                       )
                                ))
                            }
                            )
                     )
                     
      )
    ),
    ### Regressão ----
    conditionalPanel(
      condition = "input.tabs=='logistic'",
      controlbarMenu(type="hidden",
                     column(width=12,
                            h6("Filtros - Regressão", class = "text-center"),
                            uiOutput("contagemregressao"),
                            h4(" "),
                            lapply(seq_along(variaveis_filtradas), function(i) {
                              var <- names(variaveis_filtradas)[i]
                              # Encontrar o mapeamento correto com base no campo `var` dentro de cada elemento da lista
                              mapping <- Filter(function(m) m$var == var, dummy_mapping)
                              
                              if (length(mapping) > 0) {
                                mapping <- mapping[[1]]
                                
                                filtro_box(width = 12, headerBorder = TRUE, collapsible = TRUE, collapsed= if(i==1) FALSE else TRUE,
                                           title = variaveis_filtradas[[var]]$label,
                                           column(width = 12, offset = 0,
                                                  prettyCheckboxGroup(
                                                    inputId = paste0(variaveis_filtradas[[var]]$id, "_selectall"), 
                                                    label = NULL,
                                                    choiceValues = "selectall",
                                                    choiceNames = "Selecionar tudo",
                                                    selected = "selectall",
                                                    shape="curve",
                                                    status = "primary",
                                                    animation="smooth"
                                                  ),
                                                  prettyCheckboxGroup(
                                                    inputId = variaveis_filtradas[[var]]$id,
                                                    label = NULL,
                                                    choices = mapping$choices,
                                                    selected = mapping$choices,
                                                    shape="curve",
                                                    status = "primary",
                                                    animation="smooth"
                                                  )
                                           )
                                )
                              }
                            })
                            
                     )
      )
    ),
    ### Outro -------------------
    conditionalPanel(
      condition = "input.tabs != 'resumo' && input.tabs != 'logistic'",
      h6("Não existem filtros a serem aplicados nesta página.", class = "text-center")
    )
  ),
  ## Rodapé -----
  bs4DashFooter(left=HTML("&copy; 2024 Adriana A. Musis")
  ),
  
  ## Corpo ----
  body = 
    bs4DashBody(
      tags$head(
        tags$style(HTML("
    .control-sidebar {
      overflow-y: scroll; 
      scrollbar-width: 10px;
    }

.control-sidebar::-webkit-scrollbar{
  width: 10px;
}

.control-sidebar::-webkit-scrollbar-thumb{
  height:20px;
  background-color: #D6D6D6;  
  border-radius: 10px; 
}
body.dark-mode .main-header {
        background-color: #24262D !important; /* Cor do fundo da navbar no modo escuro */
      }
.dark-mode .nav-pills .nav-link.active {
    color: #fff !important;
}

.dark-mode .nav-pills .nav-link:hover {
    color: #fff !important;
}

.nav-item a.nav-link {
      white-space: nowrap; /* Impede que o texto quebre para a próxima linha */
}
  "))
      ),
      autoWaiter(html=spin_3(),color=transparent(.9)),
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = TRUE),
      tags$link(href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;400;600&display=swap", rel = "stylesheet"),
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
      
      ## Página: Visão geral -------------------------------
      bs4TabItems(
        bs4TabItem(
          tabName = "resumo",
          
          fluidPage(
            fluidRow(actionButton(inputId = "controlbarToggle", label = "Filtros", icon=icon("filter"),class = "mx-2")),h3(""),
            fluidRow(
              
              bs4ValueBoxOutput("contagemRespondentes"),
              bs4ValueBoxOutput("contagemConcluintes"),
              bs4ValueBoxOutput("contagemDesvinculados")
            ),
            ### gráficos gerais ----
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
                                 #### Informações acadêmicas --------------------
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
                                 #### Aspectos acadêmicos ------------
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
            
          )), ## Página: Regressão ----
        bs4TabItem(
          tabName = "logistic",
          bs4Card(
            title = "Configurações avançadas",
            solidHeader = TRUE,
            collapsible = TRUE,
            width=12,
            collapsed = FALSE,
            ### Configurações -----------------------
            fluidRow(actionButton(inputId = "controlbarToggle", label = "Filtros", icon=icon("filter"),class = "mx-2")),h3(""),
            prettyRadioButtons("criteria", "Critério de seleção do modelo", choices = c("BIC", "AIC"), selected = "BIC",
                               animation="smooth"),
            prettyRadioButtons("direction", "Direção de seleção de variáveis", choiceNames = c("Eliminação bidirecional","Seleção direta", "Eliminação reversa"), choiceValues=c("both","forward","backward"),selected = "both",
                               animation="smooth"),
            prettyCheckbox("include_bootstrap", label= "Performar validação bootstrap", value = FALSE,
                           animation="smooth",status="primary"),
            conditionalPanel(
              condition = "input.include_bootstrap == true",
              sliderInput("proporcao_treino", "Partição de treino", min = 50, max = 95, value = 80),
              sliderInput("rep_bootstrap", "Repetições bootstrap", min = 20, max = 10000, value = 100)
            )
          ),
          actionButton("analyze", "Processar",
                       status="primary"),
          # actionBttn("save_results", "Salvar Resultados (debug)",            style = "bordered", color="success"),
          actionButton("restore_default", "Restaurar Modelo Padrão",
                       status = "warning"),
          h1(" "),
          tabsetPanel(type="pills",
                      id = "results_tabs",
                      ### Modelo  -------------------
                      tabPanel(
                        title = "Modelo de regressão",
                        h3(),
                        bs4Card(
                          title="Configurações do modelo",
                          headerBorder=FALSE,collapsible=FALSE,width=12,
                          uiOutput("configuracoes_modelo"),
                          uiOutput("filtered_vars")
                        ),
                        bs4Card(
                          title="Coeficientes",
                          headerBorder=FALSE,collapsible=FALSE,width=12,
                          DT::DTOutput("coef")
                        ),
                        bs4Card(
                          title="Informações do modelo",
                          headerBorder=FALSE,collapsible=FALSE,width=12,
                          uiOutput("info_modelo")
                        ),
                        
                        bs4Card(
                          title="Critérios de informação",
                          headerBorder=FALSE,collapsible=FALSE,width=12,
                          DTOutput("info_criteria")
                        )
                      ),
                      ### Gráficos  ----------------
                      tabPanel(
                        title = "Gráficos",
                        h3(),
                        fluidRow(
                          bs4Card(
                            width = 6,
                            maximizable = FALSE, collapsible = FALSE,
                            headerBorder = FALSE,
                            highchartOutput("tornadoPlot")
                          ),
                          bs4Card(
                            width = 6,
                            maximizable = FALSE, collapsible = FALSE,
                            headerBorder = FALSE,
                            highchartOutput("rocPlot")
                          ) 
                        ),
                        fluidRow(
                          bs4Card(
                            width = 6,
                            maximizable = FALSE, collapsible = FALSE,
                            headerBorder = FALSE,
                            highchartOutput("residualsPlot")
                          ),
                          bs4Card(
                            width = 6,
                            maximizable = FALSE, collapsible = FALSE,
                            headerBorder = FALSE,
                            highchartOutput("qqPlot")
                          ) 
                        ),
                        fluidRow(
                          bs4Card(
                            width = 6,
                            maximizable = FALSE, collapsible = FALSE,
                            headerBorder = FALSE,
                            highchartOutput("scaleLocationPlot")
                          ),
                          bs4Card(
                            width = 6,
                            maximizable = FALSE, collapsible = FALSE,
                            headerBorder = FALSE,
                            highchartOutput("cooksDistancePlot")
                          ) 
                        )
                      ),
                      ### Bootstrap  ------------------
                      tabPanel(
                        title = "Validação bootstrap",
                        h3(),
                        conditionalPanel(
                          condition = "output.bootstrap == true",
                          bs4Card(
                            title="Estatísticas",
                            headerBorder=FALSE,collapsible=FALSE,width=12,
                            DT::DTOutput("boot")),
                          bs4Card(
                            title="Coeficientes",
                            headerBorder=FALSE,collapsible=FALSE,width=12,
                            DT::DTOutput("bootCoef")),
                          bs4Card(
                            title="Odds Ratio",
                            headerBorder=FALSE,collapsible=FALSE,width=12,
                            DT::DTOutput("bootOR"))
                        ),
                        conditionalPanel(
                          condition = "output.bootstrap == false",
                          bs4Card(
                            title=NULL,
                            headerBorder=FALSE,collapsible=FALSE,width=12,
                            h6("Não foi realizada validação bootstrap."))
                        )
                      )
          )
        ),
        ## Página: Sobre o dashboard -------------------
        
        bs4TabItem(
          tabName = "sobre",
          fluidPage(
            bs4Card(
              title = "Dashboard - Fatores que influenciam a permanência e a evasão nos cursos de pós-graduação stricto sensu da UFMT",
              width = 12,
              collapsible = FALSE,
              HTML("
    <p>Este aplicativo foi elaborado como parte dos requisitos para a conclusão do Mestrado Profissional em Administração na Universidade Federal Fluminense (UFF) de <strong>Adriana A. Musis</strong>, sob orientação do Professor Dr. André Ferreira. O dashboard oferece uma análise visual dos fatores que influenciam a permanência e a evasão nos cursos de pós-graduação stricto sensu da <strong>Universidade Federal de Mato Grosso (UFMT)</strong>.</p>

    <h5>Objetivo do Aplicativo</h5>
    <p>O objetivo desta plataforma é comparar as trajetórias acadêmicas dos estudantes que concluíram os cursos com aquelas dos que não finalizaram. Através dessa comparação, buscamos colaborar com os gestores na identificação de padrões de evasão, objetivando o aprimoramento de estratégias de retenção de estudantes.</p>

    <h5>Como Usar o Dashboard</h5>
    <p>Para explorar os dados, utilize as <b>páginas disponíveis na barra superior</b> e os <b>filtros disponíveis na barra lateral</b>. Esses filtros permitem ajustar as visualizações e personalizar os resultados de acordo com as suas necessidades.</p>

    <h6>Funcionalidades Principais:</h6>
    <ul>
      <li><b>Visão Geral:</b> Explore informações descritivas e gráficos interativos que proporcionam uma visão geral dos respondentes.</li>
      <li><b>Análise de Regressão:</b> Realize uma análise preditiva sobre os fatores associados à evasão nos cursos de pós-graduação.</li>
    </ul>
    
        <h5>Base de Dados</h5>
    <p>As informações apresentadas são baseadas nas respostas de ex-discentes dos cursos de pós-graduação da UFMT, obtidas por meio de questionário enviado por e-mail.</p>
    <p>Os tópicos do questionário foram selecionados com base em relevância teórica, com foco nas contribuições de Vincent Tinto.</p>
    <p><b>Período de aplicação do questionário:</b> 06 a 19/06/2024.</p>
    
        <h5>Links</h5>
    <p>O código-fonte deste dashboard está disponível em: <a href='https://github.com/musiss/dashboard-mestrado' target='_blank' class='code-link'>https://github.com/musiss/dashboard-mestrado</a>.</p>

"),
              # Botão para ir à página "resumo"
              actionButton("go_to_resumo", "Ir para Visão Geral", 
                           class = "btn btn-primary", 
                           style = "position: absolute; bottom: 10px; right: 10px;")
            )
          )
        ),
        
        ### Metodologia --------------
        
        bs4TabItem(
          tabName = "metodologia",
          bs4Card(
            title = "Metodologia - Análise de regressão logística",
            width = 12,
            collapsible = FALSE,
            HTML("
<p>A análise de regressão logística foi desenvolvida para identificar os fatores que influenciam a probabilidade de evasão nos cursos de pós-graduação stricto sensu da UFMT. Esta técnica estatística permite modelar a relação entre a evasão (evadiu ou não evadiu) e diversas variáveis explicativas. A variável resposta, <strong>evasão</strong>, foi definida da seguinte maneira:</p>
    <ul>
        <li><b>0:</b> Estudantes que concluíram o curso.</li>
        <li><b>1:</b> Estudantes que evadiram.</li>
    </ul>
    <p>Entre outras informações apresentadas sobre o modelo, os coeficientes do modelo fornecem uma representação do impacto de cada variável na probabilidade de evasão:</p>
    <ul>
        <li><b>Coeficiente Positivo:</b> Um aumento na variável aumenta a probabilidade de evasão.</li>
        <li><b>Coeficiente Negativo:</b> Um aumento na variável reduz a probabilidade de evasão.</li>
    </ul>

    <h5>Direção de seleção de variáveis</h5>
    <p>Para escolher o modelo mais adequado, são testadas várias combinações de variáveis. O usuário pode escolher entre três opções:</p>
    <ul>
      <li><b>Seleção direta:</b> Método que começa com um modelo vazio e adiciona as variáveis mais relevantes uma a uma, com base em sua contribuição estatística.</li>
      <li><b>Eliminação reversa:</b> Método que começa com todas as variáveis no modelo e remove aquelas que não têm relevância significativa.</li>
      <li><b>Eliminação bidirecional:</b> Combina os dois métodos, adicionando e removendo variáveis conforme necessário para encontrar o melhor modelo.
    </ul>
    <p>Esses métodos ajudam a garantir que apenas as variáveis mais significativas fiquem no modelo final.</p>


    <h5>Critérios de seleção do modelo</h5>
    <p>O método de seleção de variáveis leva em consideração um dos seguintes critérios estatísticos, conforme escolha do usuário:</p>
    <ul>
      <li><b>AIC (Critério de Informação de Akaike):</b> Busca selecionar um modelo que equilibra a simplicidade e a capacidade de explicar os dados.</li>
      <li><b>BIC (Critério de Informação Bayesiano):</b> Um critério mais rigoroso que favorece modelos mais simples.</li>
    </ul>

    <h5>Validação bootstrap</h5>
    <p>O usuário também pode optar por aplicar a validação bootstrap, um processo que consiste em repetir a análise diversas vezes com diferentes amostras dos dados, para verificar a consistência das estimativas.</p>
    <p>Foi utilizada uma validação cruzada <i>holdout</i>, onde os dados são divididos em duas partes: Uma porção dos dados é utilizada para 'treinar' o modelo, enquanto o resto é utilizado para 'testar' sua precisão.</p>
    <p>Os parâmetros da validação bootstrap são customizáveis:</p>
     <ul>
      <li><b>Tamanho da partição:</b> Define a proporção de dados usados para treinar e testar o modelo (ex: 80% para treinar, 20% para testar).</li>
      <li><b>Número de Repetições:</b> O número de vezes em que o processo bootstrap é repetido. Quanto mais repetições forem feitas, maior será a confiabilidade dos resultados. </li>
    </ul>   

"
            )
          )
        )
      )
    )
)




# server ----

server <- function(input, output,session) {
  
  
  observeEvent(input$controlbarToggle, {
    updateControlbar(id = "controlbar")
  })
  
  observeEvent(input$go_to_resumo, {
    updateTabItems(session, "tabs", "resumo")
  })
  
  ########## Definindo funções ----
  
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
        
        hc_tooltip(shared = FALSE, useHTML = TRUE, headerFormat = '<span style="font-size: 0.8em">{point.key}</span><br/>',
                   pointFormat = '<b>{series.name}</b><br><span style="color:{point.color}">\u25AA</span>Frequência (%): <b>{point.n}</b> ({point.percent:.1f}%)<br/>') 
      
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
          hc_legend(enabled = FALSE)  %>% 
          hc_colors("#6886C3")
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
          hc_legend(enabled = FALSE) %>% 
          hc_colors("#6886C3")
        
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
  
  
  likert_chart <- function(data, category_col, group_col="evadido",
                           levels_order = c("Muito baixo(a)", "Baixo(a)", "Neutro", "Alto(a)", "Muito alto(a)") %>% rev(),
                           titulo=category_col,comparar=req(input$comparar)) {
    
    # Verificar se as colunas existem no banco de dados
    if (!(category_col %in% colnames(data)) | !(group_col %in% colnames(data))) {
      stop("Coluna(s) especificada(s) não encontrada(s) no banco de dados.")
    }
    
    # Transformar os valores da coluna de pergunta se forem de 1 a 5
    if (all(data[[category_col]] %in% 1:5)) {
      data[[category_col]] <- factor(data[[category_col]], levels = 5:1, labels = levels_order)
    } else {
      data[[category_col]] <- factor(data[[category_col]], levels = levels_order)
    }
    
    if (comparar == "comparar") {
      # Renomear valores de evadido para "Concluínte" e "Desvinculado"
      data[[group_col]] <- factor(data[[group_col]], levels = c(0, 1), labels = c("Concluínte", "Desvinculado"))
    }
    else if (comparar == "total") {
      data[[group_col]] <- factor(data[[group_col]], levels = c(0,1),labels=c("Total","Total"))
    }
    
    # Contar as respostas por categoria
    data_summary <- data %>%
      group_by(!!sym(group_col), !!sym(category_col)) %>%
      summarise(Contagem = n()) %>%
      ungroup() %>%
      mutate(!!sym(group_col) := as.factor(!!sym(group_col)))
    
    # Calcular porcentagens para a tooltip
    data_summary <- data_summary %>%
      group_by(!!sym(group_col)) %>%
      mutate(Percentual = Contagem / sum(Contagem) * 100) %>%
      ungroup()
    
    # Converter dados para o formato longo para o gráfico
    data_long <- data_summary %>%
      pivot_wider(names_from = !!sym(category_col), values_from = c(Contagem, Percentual), values_fill = 0) %>%
      pivot_longer(cols = -!!sym(group_col), names_to = c(".value", "Resposta"), names_sep = "_")
    
    # Ajustar a ordem dos fatores na coluna "Resposta" após a transformação para longo
    data_long$Resposta <- factor(data_long$Resposta, levels = levels_order)
    
    
    # Criar o gráfico de escala Likert
    hc <- hchart(data_long, "bar", hcaes(x = !!sym(group_col), y = Contagem, group = Resposta)) %>%
      hc_chart(type = "bar") %>%
      hc_title(text = titulo) %>%
      hc_xAxis(categories = unique(data_long[[group_col]]),
               title = list(text = "")) %>%
      hc_yAxis(title=list(text=""), labels = list(format = '{value}%'))%>%
      hc_plotOptions(series = list(stacking = "percent")) %>%
      hc_tooltip(useHTML=TRUE, pointFormat = '<span style="color:{point.color}">\u25AA</span>  {point.Resposta}: <b>{point.Contagem} </b>({point.Percentual:.1f}%)<br/>') %>%
      hc_legend(reversed = TRUE) %>%
      hc_colors(c("#d73027", "#fc8d59", "#ECEADA", "#91bfdb", "#4575b4") %>% rev())
    
    # Retornar o gráfico
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
  
  
  forceRedraw <- reactiveVal(FALSE)
  
  ## tema ----
  observe({
    observeEvent(input$dark_mode, {
      ignoreNULL=FALSE
      if (input$dark_mode) {
        
        options(highcharter.theme = hc_theme(
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
        
        forceRedraw(!forceRedraw())  
      } else {
        options(highcharter.theme = hc_theme(
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
  
  
  ## filtros ----
  
  filtros_descritiva <- list(
    list(id = "evadido_checkbox", var = "evadido", choices = c("Concluíntes" = 0, "Desvinculados" = 1)),
    list(id = "nivel_checkbox", var = "Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?", choices = unique(data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %>% na.omit())),
    list(id = "curso_checkbox", var = "Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?", choices = unique(data$`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?`)),
    list(id = "campus_checkbox", var = "Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?", choices = unique(data$`Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?` %>% na.omit())),
    list(id = "genero_checkbox", var = "Identidade de gênero", choices = unique(data$`Identidade de gênero`)),
    list(id = "etnia_checkbox", var = "Qual opção melhor descreve sua raça ou etnia?", choices = unique(data$`Qual opção melhor descreve sua raça ou etnia?`)),
    list(id = "areaConhecimento_checkbox", var = "area_conhecimento_curso", choices = unique(data$`area_conhecimento_curso`))
  )
  
  observe({
    lapply(filtros_descritiva, function(variable) {
      id_select_all <- paste0(variable$id, "_selectall")
      id_checkbox <- variable$id
      
      # Atualizar o grupo de checkboxes com base na seleção do "Selecionar Tudo"
      updatePrettyCheckboxGroup(
        inputId = id_checkbox,
        selected = if (!is.null(input[[id_select_all]])) variable$choices else character(0)
      )
    })
  })
  
  
  dadosFiltrados <- reactive({
    # Garantindo ser data table
    if (!is.data.table(data)) {
      filtered <- as.data.table(data)
    } else {
      filtered <- data
    }
    
    filtered <- filtered %>%
      tidytable::filter(evadido %in% input$evadido_checkbox) %>%
      tidytable::filter(`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?` %in% input$curso_checkbox) %>%
      tidytable::filter(`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %in% input$nivel_checkbox) %>%
      tidytable::filter(`Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?` %in% input$campus_checkbox) %>%
      tidytable::filter(`Identidade de gênero` %in% input$genero_checkbox) %>%
      tidytable::filter(`Qual opção melhor descreve sua raça ou etnia?` %in% input$etnia_checkbox) %>%
      tidytable::filter(area_conhecimento_curso %in% input$areaConhecimento_checkbox)
    
    return(filtered)
    
    # Forçar o redesenho dos plots
    force(forceRedraw())
  })
  
  # Output para contagem de respondentes
  output$contagemRespondentes <- renderbs4ValueBox({
    valuebox2(
      subtitle= "Nº de respondentes",
      value=nrow(dadosFiltrados()),
      icon=icon("users"),
      color="white")
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
      color = "danger",
    )
  })
  
  ## plots ----
  
  output$plot1<-renderHighchart({
    force(forceRedraw())
    plot_pie(dadosFiltrados(),category_col = "evadido",
             category_order = c(0,1), 
             category_labels = c("Concluínte","Desvinculado"),
             titulo="Status")})
  
  output$plot2<-renderHighchart({
    force(forceRedraw())
    plot_bar(dadosFiltrados(),category_col = "Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?",titulo="Curso")})
  
  
  plot3_filter <- reactiveVal("total")
  
  plot3_cores <- reactiveVal(c("#6886C3", "#24427F"))  # Valores padrão para "Total"
  
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
      hc_colors(plot3_cores()) %>%
      hc_chart(lang = list(decimalPoint = ',', thousandsSep = '.')) %>%
      hc_title(text="Ano de Início e Conclusão do Curso") %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = '{value}%')) %>%
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
            align = 'right',
            verticalAlign = 'top',  
            width=100,
            y = 30 , 
            theme=theme_buttons
          ),
          customButton0 = list(
            text = 'Concluíntes',
            onclick = JS("function() {
              Shiny.onInputChange('plot3_filter', 0);
            }"),
            align = 'right',  
            verticalAlign = 'top', 
            y = 30,   
            theme=theme_buttons
          ),
          customButton1 = list(
            text = 'Desvinculados',
            onclick = JS("function() {
              Shiny.onInputChange('plot3_filter', 1);
            }"),
            align = 'right',     
            verticalAlign = 'top',  
            y = 30,  
            theme=theme_buttons
          )
        )
      )
  })
  
  observeEvent(input$plot3_filter, {
    plot3_filter(input$plot3_filter)
    plot3_cores(cores_botoes(plot3_filter()))
  })
  
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
  
  ############## piramide etaria
  categorize_age <- function(age) {
    breaks <- c(seq(0, 95, 5), 100, Inf)
    labels <- c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44',
                '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85-89', '90-94',
                '95-99', '100 +')
    label <- findInterval(age, vec = breaks)
    return(labels[label])
  }
  
  plot7_filter <- reactiveVal("total")
  plot7_cores <- reactiveVal(c("#6886C3", "#24427F"))  # Valores padrão para "Total"
  
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
    stroke = '#F1F1F1',  
    r = 1,  
    height = 30,  
    style = list(
      padding = '15px', 
      boxShadow = '2px 4px 6px rgba(0, 0, 0, 0.1)'
    )
  )
  
  output$plot7 <- renderHighchart({
    force(forceRedraw())
    highchart() %>%
      hc_colors(plot7_cores()) %>%
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
            align = 'right',    
            verticalAlign = 'top', 
            width=100,
            y = 30 , 
            theme=theme_buttons
          ),
          customButton0 = list(
            text = 'Concluíntes',
            onclick = JS("function() {
              Shiny.onInputChange('plot7_filter', 0);
            }"),
            align = 'right', 
            verticalAlign = 'top', 
            y = 30,  
            theme=theme_buttons
          ),
          customButton1 = list(
            text = 'Desvinculados',
            onclick = JS("function() {
              Shiny.onInputChange('plot7_filter', 1);
            }"),
            align = 'right',  
            verticalAlign = 'top', 
            y = 30,  
            theme=theme_buttons
          )
        )
      )
  })
  
  observeEvent(input$plot7_filter, {
    plot7_filter(input$plot7_filter)
    plot7_cores(cores_botoes(plot7_filter()))
  })
  
  plot13_filter <- reactiveVal("total")
  plot13_cores <- reactiveVal("#031772")  # Valores padrão para "Total"
  
  
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
      
      hc_colorAxis(mincolor="#FFFFFF",maxColor=plot13_cores()) %>%
      
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
            align = 'right',    
            verticalAlign = 'top',  
            y = 30 , 
            theme=theme_buttons
          ),
          customButton0 = list(
            text = 'Concluíntes',
            onclick = JS("function() {
              Shiny.onInputChange('plot13_filter', 0);
            }"),
            align = 'right',   
            verticalAlign = 'top',
            y = 30,   
            theme=theme_buttons
          ),
          customButton1 = list(
            text = 'Desvinculados',
            onclick = JS("function() {
              Shiny.onInputChange('plot13_filter', 1);
            }"),
            align = 'right',  
            verticalAlign = 'top', 
            y = 30, 
            theme=theme_buttons
          )
        )
      )
  })
  
  observeEvent(input$plot13_filter, {
    plot13_filter(input$plot13_filter)
    plot13_cores(cores_botoes(plot13_filter(),total="#031772",evadido="#252528",concluinte="#316276",outro="#97979A"))
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
             category_order=c("Heterossexual","Gay ou Lésbica","Bissexual/Pansexual","Outros"))
  })
  
  output$plot18<-renderHighchart({
    force(forceRedraw())
    plot_bar(dadosFiltrados(),category_col = "Qual opção melhor descreve sua raça ou etnia?",titulo="Raça/Etnia",category_order = c("Branca","Preta","Parda","Amarela","Indígena"))
  })
  
  
  output$plot19<-renderHighchart({
    if (req(input$comparar)=="total"){
      
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
        hc_xAxis(title = list(text = ""),
                 categories=list("Deficiência Física",
                                 "Deficiência Visual",
                                 "Deficiência Auditiva",
                                 "Deficiência Intelectual",
                                 "Transtorno do Espectro Autista",
                                 "Transtorno Afetivo Bipolar",
                                 "Transtorno do Déficit de Atenção com Hiperatividade",
                                 "Transtorno de Ansiedade Generalizada",
                                 "Outro")) %>%
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
        )%>% 
        hc_colors(c("#6886C3","#071D41"))
      
    } else if (req(input$comparar)=="comparar"){
      
      force(forceRedraw())
      
      dados_comparar <- dadosFiltrados() %>%
        mutate(tem_deficiencia = ifelse(grepl("Não, nenhuma deficiência ou transtorno", `Apresenta alguma deficiência ou transtorno? Marque todos que se aplicam`),
                                        "Nenhuma deficiência ou transtorno", "Possui alguma deficiência ou transtorno")) %>%
        mutate(tem_deficiencia = factor(tem_deficiencia, levels = c("Possui alguma deficiência ou transtorno", "Nenhuma deficiência ou transtorno"))) %>%
        mutate(evadido := factor(evadido, levels = c(0, 1), labels = c("Concluíntes", "Desvinculados")))
      
      
      evadidos <- dados_comparar %>%
        filter(evadido == "Desvinculados") %>%
        count(tem_deficiencia) %>%
        mutate(percent = n / sum(n) * 100)
      
      concluintes <- dados_comparar %>%
        filter(evadido == "Concluíntes") %>%
        count(tem_deficiencia) %>%
        mutate(percent = n / sum(n) * 100)
      
      
      dados_barra<-dados_comparar %>% 
        separate_rows(`Apresenta alguma deficiência ou transtorno? Marque todos que se aplicam`, sep = ";") %>% 
        mutate(`Apresenta alguma deficiência ou transtorno? Marque todos que se aplicam` := factor(`Apresenta alguma deficiência ou transtorno? Marque todos que se aplicam`, levels = c(
          "Deficiência Física",
          "Deficiência Visual",
          "Deficiência Auditiva",
          "Deficiência Intelectual",
          "Transtorno do Espectro Autista",
          "Transtorno Afetivo Bipolar",
          "Transtorno do Déficit de Atenção com Hiperatividade",
          "Transtorno de Ansiedade Generalizada",
          "Outro",
          "Não, nenhuma deficiência ou transtorno"))) %>% arrange(`Apresenta alguma deficiência ou transtorno? Marque todos que se aplicam`) %>%
        filter(`Apresenta alguma deficiência ou transtorno? Marque todos que se aplicam` != "Não, nenhuma deficiência ou transtorno")%>% 
        count(evadido, `Apresenta alguma deficiência ou transtorno? Marque todos que se aplicam`) %>%  # Conta os casos agrupados por Status e Deficiência
        group_by(evadido) %>%  # Agrupa por Status (Concluinte ou Evadido)
        mutate(percent = n / sum(n) * 100) %>%  # Calcula a porcentagem dentro de cada grupo
        ungroup() 
      
      hchart(
        dados_barra,  
        "column", hcaes(x = `Apresenta alguma deficiência ou transtorno? Marque todos que se aplicam`, y = percent,group=evadido)
      ) %>%
        hc_yAxis(labels = list(format = "{value}%"), title = list(text = "")) %>%
        hc_xAxis(title = list(text = ""),
                 categories=list("Deficiência Física",
                                 "Deficiência Visual",
                                 "Deficiência Auditiva",
                                 "Deficiência Intelectual",
                                 "Transtorno do Espectro Autista",
                                 "Transtorno Afetivo Bipolar",
                                 "Transtorno do Déficit de Atenção com Hiperatividade",
                                 "Transtorno de Ansiedade Generalizada",
                                 "Outro")) %>%
        hc_title(text = "Tipos de deficiência/transtorno") %>%
        # hc_colors(c("#315AAD")) %>%  # Cor do gráfico de barras
        hc_legend(enabled = TRUE) %>%
        hc_tooltip(
          shared = FALSE,
          useHTML = TRUE,
          headerFormat = '<span style="font-size: 0.8em">{point.key}</span><br/>',
          pointFormat = '<span style="color:{point.color}">\u25AA</span> Frequência (%): <b>{point.n} ({point.y:.1f}%)</b><br/>'
        ) %>%
        hc_add_series(
          data = list_parse2(evadidos),
          type = "pie",
          name = "Proporção de Deficiências - Evadidos",
          center = c('75%', '10%'),
          size=75,
          startAngle = -90,
          innerSize='50%',
          endAngle = 90,
          dataLabels = list(enabled = FALSE),
          size = 100,
          showInLegend = FALSE,
          colors = c("#97979A", "#252528"),  # Cores para evadidos
          tooltip = list(
            pointFormat = '<span style="color:{point.color}">\u25AA</span>Frequência (%): <b>{point.y}</b> ({point.percentage:.1f}%)<br/>'
          )
        ) %>%
        hc_add_series(
          data = list_parse2(concluintes),
          type = "pie",
          name = "Proporção de Deficiências - Concluintes",
          center = c('90%', '10%'),
          size=75,
          startAngle = -90,
          endAngle = 90,
          innerSize='50%',
          dataLabels = list(enabled = FALSE),
          size = 100,
          showInLegend = FALSE,
          colors = c("#B7DBF4", "#456882"),  # Cores para concluintes
          tooltip = list(
            pointFormat = '<span style="color:{point.color}">\u25AA</span>Frequência (%): <b>{point.y}</b> ({point.percentage:.1f}%)<br/>'
          )
        )
    }
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
    roc= NULL,
    configuracoes_modelo=list(
      criterios = NULL,
      direcao = NULL,
      proporcao_treino = NULL,
      repeticoes_bootstrap = NULL,
      variaveis_filtradas = NULL
    )
  )
  
  padrao <- reactiveVal(TRUE)  # padrao starts as TRUE
  
  observe({
    if (padrao()) {
      rv$modelo_step <- modelo_step_padrao
      rv$info_criteria <- info_criteria_padrao
      rv$coeficientes_df <- coeficientes_df_padrao
      rv$resultados_bootstrap <- resultados_bootstrap_padrao
      rv$roc <- roc_padrao
      rv$configuracoes_modelo<-configuracoes_modelo_padrao
      
      atualizarUI()  # Update the UI with the default model
    }
  })
  
  
  show_filters <- reactiveVal(FALSE)  # Visualização de filtros
  
  observeEvent(input$toggle_filters, {
    show_filters(!show_filters())  
  })
  
  # Função para atualizar a UI com o modelo processado
  atualizarUI <- function() {
    
    output$stepwise <- renderPrint({
      list(Modelo = rv$modelo_step)
    })
    
    
    output$info_criteria <- renderDT({
      info_criteria_df <-    data.frame(
        Modelo = c("Modelo Escolhido", "Modelo Nulo"),
        AIC = c(rv$info_criteria$AIC_Stepwise, rv$info_criteria$AIC_Nulo),
        BIC = c(rv$info_criteria$BIC_Stepwise, rv$info_criteria$BIC_Nulo),
        LogLik = c(
          paste0(formatC(as.numeric(rv$info_criteria$LogLik_Stepwise), format = "f", digits = 2, decimal.mark = ","), 
                 " (g.l=", attr(rv$info_criteria$LogLik_Stepwise, "df"), ")"),
          paste0(formatC(as.numeric(rv$info_criteria$LogLik_Nulo), format = "f", digits = 2, decimal.mark = ","), 
                 " (g.l=", attr(rv$info_criteria$LogLik_Nulo, "df"), ")")
        )
      )  %>%
        mutate(across(everything(), ~ formatC(.x, format = "f", digits = 2, decimal.mark = ",", big.mark=" ")))
      
      tabela_dt(info_criteria_df,pesquisa=FALSE)
    })
    
    
    output$residualsPlot <- renderHighchart({
      force(forceRedraw())
      plot_lm_highchart(rv$modelo_step,which=1)
    })
    
    output$qqPlot <- renderHighchart({
      force(forceRedraw())
      plot_lm_highchart(rv$modelo_step, which = 2)
    })
    output$scaleLocationPlot <- renderHighchart({
      force(forceRedraw())
      plot_lm_highchart(rv$modelo_step, which = 3)
    })
    output$cooksDistancePlot <- renderHighchart({
      force(forceRedraw())
      plot_lm_highchart(rv$modelo_step, which = 4)
    })
    
    output$rocPlot <- renderHighchart({
      force(forceRedraw())
      roc_obj<-rv$roc
      
      sp <- sort(roc_obj$specificities, decreasing = FALSE)
      se <- sort(roc_obj$sensitivities, decreasing = TRUE)
      
      auc_value <- pROC::auc(roc_obj)
      
      highchart() %>%
        hc_chart(
          events = list(
            render = JS(
              "function() {
          const chart = this,
          yAxisZero = chart.yAxis[0].translate(0);
          chart.xAxis[0].axisGroup.translate(0, -yAxisZero);
          chart.xAxis[0].labelGroup.translate(0, -yAxisZero + 15);
          
          // Move xAxis title
          const xAxisTitle = chart.xAxis[0].axisTitle;
          xAxisTitle.translate(0, -yAxisZero + 55); 
        }"
            )
          )
        ) %>%
        hc_title(text = "Curva ROC") %>%
        hc_xAxis(title = list(text = "Especificidade"),
                 min = -0.1, max = 1.1, 
                 tickPositions = c(0, 0.25, 0.5, 0.75, 1), 
                 reversed = TRUE,
                 endOnTick = FALSE,
                 startOnTick = FALSE) %>%
        hc_yAxis(title = list(text = "Sensibilidade"),
                 min = -0.1, max = 1.1, 
                 tickPositions = c(0, 0.25, 0.5, 0.75, 1),
                 endOnTick = FALSE,
                 startOnTick = FALSE) %>%
        hc_add_series(
          data = data.frame(x = sp, y = se),
          type = "line",
          name = "Curva ROC",
          color = "#24427F",
          marker = list(enabled = FALSE)
        ) %>%
        hc_add_series(
          data = data.frame(x = seq(from=1,to=0,length.out=100), y = seq(from=0, to=1,length.out=100)),
          type = "line",
          name = "Linha de referência",
          color = "gray",
          dashStyle = "Dash"
        )%>%
        hc_tooltip(
          formatter = JS("function () {
      var tooltip;
      tooltip = '<span style=\"font-size: 0.8em\">' +
                '<span style=\"color:' + this.point.color + '\">\u25AA</span> ' + 
                this.series.name + '</span><br>' +
                'Especificidade: <b>' + Highcharts.numberFormat(this.x, 2, ',') + '</b><br>' +
                'Sensibilidade: <b>' + Highcharts.numberFormat(this.y, 2, ',') + '</b>';
      return tooltip;
    }")) %>%
        hc_legend(enabled = FALSE)%>%
        hc_exporting(enabled=TRUE,
                     buttons = list(
                       contextButton = list(
                         menuItems = botoes_menu
                       )
                     )
        )
      
    })
    
    
    output$tornadoPlot <- renderHighchart({
      
      coeficientes_ordenados <- rv$coeficientes_df %>%
        dplyr::arrange(desc(Estimate)) %>%  
        dplyr::mutate(
          Variable = gsub("`", "", Variable)) %>%
        dplyr::mutate(
          Pergunta = stringr::str_extract(Variable, "^[^_]+"),  
          Categoria = stringr::str_replace(Variable, "^[^_]+_", ""),
          y = Estimate
        )
      
      # Criar o gráfico tornado
      highchart() %>%
        hc_chart(type = "bar", marginLeft=300) %>%
        hc_title(text = "Tornado") %>%
        hc_xAxis(
          categories = paste(coeficientes_ordenados$Pergunta, coeficientes_ordenados$Categoria, sep = " - ")
        ) %>%
        hc_yAxis(title = list(text = "Estimativas dos Coeficientes")) %>%
        
        hc_add_series(data = coeficientes_ordenados, name = "Estimativa", color="#24427F") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_tooltip(
          formatter = JS("function () {
                  var tooltip;
                  tooltip = '<span style=\"font-size: 0.8em\">' +
                            '<span style=\"color:' + this.point.color + '\">\u25AA </span>' +
                            this.point.Pergunta + '</span><br>' +
                            'Categoria: <b>' + this.point.Categoria + '</b><br>' +
                            'Estimativa: <b>' + Highcharts.numberFormat(this.point.Estimate, 1, ',') + '</b><br>';
                  return tooltip;
                }")
        )%>%
        hc_legend(enabled = FALSE)%>%
        hc_exporting(enabled=TRUE,
                     buttons = list(
                       contextButton = list(
                         menuItems = botoes_menu
                       )
                     )
        )
    })
    
    output$configuracoes_modelo <- renderUI({
      HTML(paste0(
        "<b>Critério de seleção:</b> ", rv$configuracoes_modelo$criterios, "<br>",
        "<b>Direção de seleção:</b> ", switch(rv$configuracoes_modelo$direcao,
                                              "both" = "Eliminação bidirecional",
                                              "forward" = "Seleção direta",
                                              "backward" = "Eliminação reversa",
                                              rv$configuracoes_modelo$direcao), "<br>",
        "<b>Validação Bootstrap:</b> ", 
        if (!is.null(rv$resultados_bootstrap)) {
          paste0("Sim (", actionLink("go_to_bootstrap", "Visualizar resultados",style = "color: #428BCA; text-decoration: underline;"), ")")
        } else {
          "Não"
        }, "<br>",
        if (!is.null(rv$resultados_bootstrap)) {
          paste0(
            "<b>Partição de treino:</b> ", 
            if(!is.null(rv$configuracoes_modelo$proporcao_treino)) paste0(rv$configuracoes_modelo$proporcao_treino, "%<br>") else "Não especificado<br>",
            "<b>Repetições Bootstrap:</b> ", rv$configuracoes_modelo$repeticoes_bootstrap
          )
        } else {
          ""
        }
      ))
    })
    
    output$coef<-renderDT({
      coef_df <- as.data.frame(summary(rv$modelo_step)$coefficients)
      coef_df$Term <- rownames(coef_df)
      
      # Remover crases e separar Pergunta e Categoria
      coef_df <- coef_df %>%
        mutate(Term = gsub("`", "", Term),
               Pergunta = gsub("_.*", "", Term),
               Categoria = gsub(".*_", "", Term)) %>%
        mutate(Categoria = ifelse(Pergunta == Categoria, "", Categoria)) %>%
        mutate(Pergunta = ifelse(Pergunta == "(Intercept)", "(Intercepto)", Pergunta)) %>%
        select(Pergunta, Categoria, Estimate, `Std. Error`, `z value`, `Pr(>|z|)`)
      
      rownames(coef_df) <- NULL
      names(coef_df)<-c("Pergunta","Categoria","Estimativa","Erro padrão","Valor z","Valor p")
      
      coef_df<- coef_df  %>%
        mutate(across(everything(), ~ formatC(.x, format = "f", digits = 2, decimal.mark = ",", big.mark=" ")))
      
      
      tabela_dt(coef_df)
    })
    
    # Exibir graus de liberdade e deviance
    
    output$info_modelo <- renderUI({
      summary_modelo<-summary(rv$modelo_step)
      HTML(paste0(
        "<h5>Graus de liberdade</h5> <br>",
        "  <b>Total (Nulo):</b> ", summary_modelo$df.null, "<br>",
        "  <b>Residual:</b> ", summary_modelo$df.residual, "<br><br>",
        "<h5>Deviance</h5> <br>",
        "  <b>Nula:</b> ", summary_modelo$null.deviance %>% formatC(format = "f", digits = 2, decimal.mark = ","), "<br>",
        "  <b>Residual:</b> ", summary_modelo$deviance %>% formatC(format = "f", digits = 2, decimal.mark = ","), "<br>"
        
      ))
    })
    
    
    output$filtered_vars <- renderUI({
      if(show_filters()) {
        # Renderizar o HTML com as variáveis filtradas
        HTML(paste0(
          actionLink("toggle_filters", "Esconder variáveis filtradas", style = "color: #428BCA; text-decoration: underline;"),  # Link para esconder
          "<br><br>Foram selecionados respondentes correspondentes às seguintes categorias:<br><br>",
          
          paste(rv$configuracoes_modelo$variaveis_filtradas, collapse = "<br>"),
          "<br><br>Totalizando ",length(rv$modelo_step$y)," indivíduos (",sum(rv$modelo_step$y == 0, na.rm = TRUE), " concluíntes; ",sum(rv$modelo_step$y == 1, na.rm = TRUE), " desvinculados)."
          
        ))
        
      } else {
        # Caso esteja escondendo as variáveis
        actionLink("toggle_filters", "Visualizar variáveis filtradas", style = "color: #428BCA; text-decoration: underline;")  # Link para visualizar
      }
    })
    
    ### Bootstrap ----------------
    
    output$bootstrap <- reactive({
      !is.null(rv$resultados_bootstrap)  # Retorna TRUE se não for nulo, FALSE se for nulo
    })
    
    outputOptions(output, "bootstrap", suspendWhenHidden = FALSE)
    if(!is.null(rv$resultados_bootstrap)){
      
      output$boot <- DT::renderDT({
        dfBoot <- rv$resultados_bootstrap$metrics
        rownames(dfBoot) <- NULL
        dfBoot <- as.data.frame(dfBoot)
        boottable<-calcular_ic(dfBoot)
        
        names(boottable)<-c("Variável","Média","2,5%","97,5%","N válidos")
        boottable<-boottable %>%
          mutate(across(everything(), ~ formatC(.x, format = "f", digits = 2, decimal.mark = ",", big.mark=" ")))
        tabela_dt(boottable,pesquisa=FALSE)
      })
      
      output$bootCoef <- DT::renderDT({
        dfBootCoef <- rv$resultados_bootstrap$coef
        rownames(dfBootCoef) <- NULL
        dfBootCoef <- as.data.frame(dfBootCoef)
        auxc <- calcular_ic(dfBootCoef) %>%
          mutate(Variável = ifelse(Variável == "(Intercept)", "(Intercepto)", Variável)) %>%
          mutate(across(everything(), ~ formatC(.x, format = "f", digits = 2, decimal.mark = ",", big.mark=" ")))
        
        auxc <- auxc %>%
          mutate(Variável = gsub("`", "", Variável),
                 Pergunta = gsub("_.*", "", Variável),
                 Categoria = gsub(".*_", "", Variável)) %>%
          mutate(Categoria = ifelse(Pergunta == Categoria, "", Categoria)) %>%
          select(Pergunta, Categoria, Média, `X2.5.`, `X97.5.`, `N.válidos`)
        
        
        names(auxc)<-c("Pergunta","Categoria","Média","2,5%","97,5%","N válidos")
        
        tabela_dt(auxc)
        
      })
      
      
      output$bootOR <- DT::renderDT({
        
        dfBootOR <- rv$resultados_bootstrap$odds_ratios
        rownames(dfBootOR) <- NULL
        dfBootOR <- as.data.frame(dfBootOR)
        auxc <- calcular_ic(dfBootOR)%>%
          mutate(Variável = ifelse(Variável == "(Intercept)", "(Intercepto)", Variável)) %>%
          mutate(across(everything(), ~ formatC(.x, format = "f", digits = 2, decimal.mark = ",", big.mark=" ")))
        
        auxc <- auxc %>%
          mutate(Variável = gsub("`", "", Variável),
                 Pergunta = gsub("_.*", "", Variável),
                 Categoria = gsub(".*_", "", Variável)) %>%
          mutate(Categoria = ifelse(Pergunta == Categoria, "", Categoria)) %>%
          select(Pergunta, Categoria, Média, `X2.5.`, `X97.5.`, `N.válidos`)
        
        
        names(auxc)<-c("Pergunta","Categoria","Média","2,5%","97,5%","N válidos")
        tabela_dt(auxc)
      })
    } else {
      # print("bootstrap is null") #debug 
      
      output$boot <- NULL
      output$bootCoef <- NULL
      output$bootOR <- NULL
    }
    
  }
  
  # Observa o clique no link e atualiza o painel de abas
  observeEvent(input$go_to_bootstrap, {
    updateTabsetPanel(session, "results_tabs", selected = "Validação bootstrap")
  })
  
  ### Filtros ----
  
  observe({
    lapply(names(variaveis_filtradas), function(var) {
      mapping <- Filter(function(m) m$var == var, dummy_mapping)
      
      if (length(mapping) > 0) {
        mapping <- mapping[[1]]
        id_select_all <- paste0(variaveis_filtradas[[var]]$id, "_selectall")
        id_checkbox <- variaveis_filtradas[[var]]$id
        # Atualizar o grupo de checkboxes com base na seleção do "Selecionar Tudo"
        updatePrettyCheckboxGroup(
          inputId = id_checkbox,
          selected = if (!is.null(input[[id_select_all]])) unname(mapping$choices) else character(0)
        )
      }
    })
  })
  
  ### Funções ------
  
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
  
  output$contagemregressao <- renderUI({
    df <- dados_regressao()  # Obter os dados filtrados
    total<-nrow(df)
    desv <- sum(df$evadido == 1, na.rm = TRUE)
    concluintes <- sum(df$evadido == 0, na.rm = TRUE)
    
    HTML(paste0( 
      "<span style='font-size: 12px;'>",
      "Foram selecionados ", total, " respondentes <br>(",
      concluintes, " concluíntes; ", desv, " desvinculados)",
      "</span>"
    ))
  })
  
  dados_regressao <- reactive({
    dados_numericos
    filtered_df <- dados_numericos
    
    for (var in names(variaveis_filtradas)) {
      # Recuperar o ID associado à variável
      id_checkbox <- variaveis_filtradas[[var]]$id
      selected_vals <- input[[id_checkbox]]  # Capturar as seleções do checkboxGroupInput
      
      # Remover o valor "selectall" se ele estiver presente
      if (!is.null(selected_vals) && "selectall" %in% selected_vals) {
        selected_vals <- setdiff(selected_vals, "selectall")
      }
      
      # Filtrar os dados com base nas seleções
      filtered_df <- filtered_df %>%
        filter(rowSums(across(all_of(selected_vals), ~ . == 1)) > 0)
      
    }
    if (!is.data.table(filtered_df)) {
      return(as.data.table(filtered_df))
    } else {
      return(filtered_df)
    }
  })
  
  ## input$analyze ----
  
  w <- Waiter$new(
    html = spin_3(), # Spinner
    color = transparent(.2) # Overlay semi-transparente
  )
  
  
  
  observeEvent(input$analyze, {
    
    w$show()
    
    padrao(FALSE)  # Padrão é Falso quando apertado o botão de processar
    
    # Resetando rv
    rv$modelo_step <- NULL
    rv$info_criteria <- NULL
    rv$coeficientes_df <- NULL
    rv$resultados_bootstrap <- NULL
    rv$roc <- NULL
    rv$configuracoes_modelo<- NULL
    
    ajuste_nulo <- glm(evadido ~ 1, data = dados_regressao(), family = "binomial")
    ajuste <- glm(evadido ~ ., data = dados_regressao(), family = "binomial")
    
    if (input$criteria == "AIC") {
      modelo_step <- step(ajuste_nulo, scope = formula(ajuste), direction = input$direction)
    }
    
    if (input$criteria == "BIC") {
      modelo_step <- step(ajuste_nulo, scope = formula(ajuste), direction = input$direction, k = log(nrow(dados_regressao())))
    }
    
    rv$modelo_step <- modelo_step
    
    rv$configuracoes_modelo$variaveis_filtradas <-  lapply(names(variaveis_filtradas), function(var) {
      id_checkbox <- variaveis_filtradas[[var]]$id
      selected_vals <- input[[id_checkbox]]  # Capturar as seleções
      
      selected_vals <- setdiff(selected_vals, "selectall") # Remover "selectall" se estiver presente
      
      mapping <- Filter(function(m) m$var == var, dummy_mapping)[[1]] # Mapear os valores selecionados para seus rótulos 
      
      selected_labels <- names(mapping$choices)[mapping$choices %in% selected_vals]
      
      # Retorna a label da variável com os valores filtrados
      if (!is.null(selected_labels) && length(selected_labels) > 0) {
        return(paste0("<b>",variaveis_filtradas[[var]]$label, ": </b>", paste(unname(selected_labels), collapse = "; ")))
      }
      return(NULL)  # Se não houver seleção
    })
    
    print(modelo_step)
    
    fstep <<- modelo_step$formula
    modelo_final_step <- formula(modelo_step)
    vfstep <- all.vars(modelo_final_step)[-1]
    
    
    if (input$include_bootstrap) {
      resultados_bootstrap <<- bootstrap_modelos(dados_regressao(), n_bootstrap = input$rep_bootstrap)
      
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
      AIC_Nulo = AIC(ajuste_nulo),
      BIC_Nulo=BIC(ajuste_nulo)
    )
    
    rv$configuracoes_modelo$criterios <- input$criteria
    rv$configuracoes_modelo$direcao <- switch(input$direction,
                                              "both" = "Eliminação bidirecional",
                                              "forward" = "Seleção direta",
                                              "backward" = "Eliminação reversa",
                                              input$direction)
    rv$configuracoes_modelo$proporcao_treino <- input$proporcao_treino
    rv$configuracoes_modelo$repeticoes_bootstrap <- input$rep_bootstrap %>% formatC(format = "f", digits = 0, decimal.mark = ",", big.mark=" ")
    
    
    
    # Chamar a função para atualizar a interface
    atualizarUI()
    
    
    w$hide()
    
    # Salvando modelo ----
    
    # observeEvent(input$save_results, {
    #   tryCatch({
    #     saveRDS(modelo_step, "modelo_step.rds")
    #     saveRDS(list(
    #       LogLik_Stepwise = logLik(modelo_step),
    #       AIC_Stepwise = AIC(modelo_step),
    #       BIC_Stepwise = BIC(modelo_step),
    #       LogLik_Nulo = logLik(ajuste_nulo),
    #       AIC_Nulo = AIC(ajuste_nulo)
    #     ),"info_criteria.rds")
    #     
    #     coeficientes <- summary(modelo_step)$coefficients
    #     coeficientes_df <- as.data.frame(coeficientes)
    #     coeficientes_df <- coeficientes_df[-1, ]
    #     coeficientes_df$Variable <- rownames(coeficientes_df)
    #     coeficientes_df <- coeficientes_df %>%
    #       arrange(Estimate) %>%
    #       mutate(Variable = factor(Variable, levels = Variable))
    #     
    #     saveRDS(coeficientes_df, "coeficientes_df_tornado.rds")
    #     
    #     saveRDS(resultados_bootstrap,"resultados_bootstrap.rds")
    #     
    #     saveRDS(resultados_bootstrap,"resultados_bootstrap.rds")
    #     
    #     prob <- predict(modelo_step, type = "response")
    #     roc <- pROC::roc(dados_regressao()$evadido, prob)
    #     saveRDS(roc,"regressao/roc.rds")
    #     showNotification("Resultados salvos com sucesso!", type = "message")
    #   }, error = function(e) {
    #     showNotification(paste("Erro ao salvar resultados:", e$message), type = "error")
    #   })
    # })
    
  })
  
  # Restaurar modelo padrão ----
  observeEvent(input$restore_default, {
    padrao(TRUE)
    
    showNotification("Modelo padrão restaurado!", type = "message")
  })
  
}

# Executa o aplicativo
shinyApp(ui, server)
