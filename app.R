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
source("funcoes dashboard.R")


#getOption("highcharter.theme") %>% View()

lang <- getOption("highcharter.lang")
lang$decimalPoint <- ","
options(highcharter.lang = lang)

# Dados exemplo
#data <- readxl::read_xlsx("dados.xlsx")
data <- arrow::read_feather("dados_feather.feather")

custom_theme <- create_theme(
  bs4dash_layout(
    main_bg = "#F1F1F1"  # Light background color for the body
  ),
  bs4dash_status(
    primary = "#1C4E80",
    danger="#F1F1F1"
  ),
  bs4dash_font(
    family_sans_serif = "'Open Sans', -apple-system, BlinkMacSystemFont, 'San Francisco', 'Segoe UI', Roboto, 'Helvetica Neue', sans-serif",
    family_base = "'Open Sans', -apple-system, BlinkMacSystemFont, 'San Francisco', 'Segoe UI', Roboto, 'Helvetica Neue', sans-serif"
  )
)

# ui ---- 

ui <- dashboardPage(
  freshTheme = custom_theme,
  preloader = list(html = tagList(spin_3()), color = transparent(.2)),
  scrollToTop=TRUE,
  help=NULL,
  title = "Dashboard - Fatores que influenciam a permanência e a evasão nos cursos de pós-graduação stricto sensu da UFMT",
  
  # Cabeçalho
  header = bs4DashNavbar(
    # title = bs4DashBrand(
  #   title = "Fatores que influenciam a permanência e a evasão nos cursos de pós-graduação stricto sensu da UFMT",
  
   # status = "primary"
    #   image = NULL
    # )
  ),
  
  ## Barra lateral ----
  sidebar = bs4DashSidebar(
    status = "primary",
    title = "Menu",
    skin="dark",
    elevation=2,
    collapsed=TRUE,
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        "Resumo",
        tabName = "resumo",
        icon = icon("chart-pie")
      ),
      bs4SidebarMenuItem(
        "Página 2",
        tabName = "pagina2",
        icon = icon("chart-bar")
      ),
      bs4SidebarMenuItem(
        "Filtros",
        icon = icon("filter"),
        radioButtons("filtroEvadido", "Filtrar Status:",
                     choices = list("Todos" = "todos",
                                    "Apenas Evadidos" = "evadidos",
                                    "Apenas Concluintes" = "concluintes"),
                     selected = "todos") %>% bs4SidebarMenuSubItem()#,
        # awesomeCheckboxGroup(
        #   inputId = "nivel_checkbox",
        #   label = "Nível acadêmico",
        #   choices = c("Selecionar tudo",unique(data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %>% na.omit())),
        #   selected =c("Selecionar tudo",unique(data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %>% na.omit()))
        # )%>% bs4SidebarMenuSubItem()
      ),
      bs4SidebarMenuItem(
        "Filtros",
        tabName = "filtros",
        icon = icon("filter")
      ),
      bs4SidebarMenuItem(
        actionButton("reload", "Atualizar gráficos")
      )
    )
  ),
  
  ## Corpo ----
  body = 
    bs4DashBody(
      autoWaiter(html=spin_3(),color=transparent(.5)),
      # use_theme(create_theme(
      #   bs4dash_status(
      #     danger="#d2d6de"
      #   ))),

      tags$head(
      tags$script(
        "$(function() {
              $('[data-card-widget=\"maximize\"]').on('click', function() {
                setTimeout(function() {
                  var isMaximized = $('html').hasClass('maximized-card');
                  if (isMaximized) {
                    $('#plot1').css('height', '100%');
                    $('#plot2').css('height', '100%');
                    $('#plot3').css('height', '100%');
                  } else {
                    $('#plot1').css('height', '400px');
                    $('#plot2').css('height', '400px');
                    $('#plot3').css('height', '400px');
                  }
                }, 300);
                $('#plot1').trigger('resize');
                $('#plot2').trigger('resize');
                $('#plot3').trigger('resize');
              });
            });
            "
      )
    ),
    tags$script(src = "https://code.highcharts.com/mapdata/countries/br/br-all.js"),
    tags$head(
      tags$style(HTML("
        .scroll-box {
          overflow-y: auto;  /* Enables vertical scrollbar if needed */
          height: 150px;    /* Set fixed height */
          max-height: 150px; /* Adjust height as needed */
          font-size: 0.8em;  /* Smaller font size */
        }
        
        .pretty .state label, .pretty .state label:after {
        font-weight: normal !important; /* Override to normal weight */
        
         .checkbox-group .pretty { margin-top: 0px !important; margin-bottom: 0px !important; }
        .checkbox-group .pretty.p-default:not(.p-smooth) .state label:after { top: 0px; }
        
                /* Custom color for the primary status */
        .pretty.p-primary .state label:after {
          background-color: #23518d !important; /* Custom color for the check background */
          border-color: #23518d !important;     /* Custom color for the border */
        }
        
        
        .scroll-box .form-group { margin-bottom: 0px !important; } /* Reduce space between groups */
        
        .card-body {
           padding: 0px;
        }
      "))),
    
    ### Página 1 ----
    bs4TabItems(
      bs4TabItem(
        tabName = "resumo",
        ##### filtros ----
        fluidPage(
         # bs4CardLayout(type="group",
          fluidRow(
            bs4Card(
              width = 3, headerBorder = FALSE, collapsible = FALSE, overflow = TRUE, status = "secondary", title = NULL,
              tags$div(
                class = "scroll-box",
                column(width = 12, offset = 0,
                       prettyCheckboxGroup(
                         inputId = paste0("evadido_checkbox", "_selectall"),
                         label = "Status",
                         choiceValues = "selectall",
                         choiceNames = "Selecionar tudo",
                         shape="curve",
                         selected = "selectall",
                         status = "primary",
                         icon = icon("check"),
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
                         icon = icon("check"),
                         animation = "smooth"
                       )
                ))),
            lapply(list(
              #list("evadido_checkbox", "Status", c("Concluíntes"=0, "Desvinculados"=1)),
              list("nivel_checkbox", "Nível acadêmico", unique(data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %>% na.omit())),
              list("curso_checkbox", "Curso", unique(data$`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?`)),
              list("campus_checkbox", "Câmpus", unique(data$`Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?` %>% na.omit())),
              list("genero_checkbox", "Gênero", unique(data$`Identidade de gênero`)),
              list("etnia_checkbox", "Raça/Etnia", unique(data$`Qual opção melhor descreve sua raça ou etnia?`)),
              list("areaConhecimento_checkbox", "Área de Conhecimento", unique(data$`area_conhecimento_curso`))
            ), function(args) {
              bs4Card(
                width = 3, headerBorder = FALSE, collapsible = FALSE,overflow=TRUE,status="secondary",title=NULL,
                tags$div(
                  class = "scroll-box",
                  column(width=12,offset=0,
                 prettyCheckboxGroup(inputId = paste0(args[[1]], "_selectall"),
                                label = args[[2]],#"Selecionar tudo",
                                choiceValues="selectall",
                                choiceNames="Selecionar tudo",
                                selected="selectall",
                                shape="curve",
                                status = "primary",
                                icon = icon("check"),
                                animation="smooth"),

                  prettyCheckboxGroup(
                    inputId = args[[1]],
                    label = NULL,#args[[2]],
                    choices = args[[3]],
                    selected = args[[3]],  # Select all by default, modify as needed
                     shape="curve",
                     status = "primary",
                     icon = icon("check"),
                     animation="smooth"
                  ))
                ))
              }
            ), 
            radioGroupButtons(
              inputId = "comparar",
              choiceNames = c("Comparação", "Total"),
              choiceValues=c("comparar","total"),
              selected="comparar",
              status = "primary"
            )), 
        fluidRow(
        
            bs4InfoBoxOutput("contagemRespondentes"),
             bs4InfoBoxOutput("contagemConcluintes"),
             bs4InfoBoxOutput("contagemDesvinculados")
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
               #   status="secondary",
                  maximizable = FALSE,
                  highchartOutput("plot18", height = "300px")
                ) , #%>% sortable(width = 12),
                bs4Card(
              #    title = "Plot 16",
                  width = 12,
              #    solidHeader = TRUE,
               #   status="info",
                  maximizable = FALSE,
                  highchartOutput("plot16", height = "300px")
                ) #%>% sortable(width = 12)
              ),
              column(
                width = 4,
                    bs4Card(
                      title = "Plot 21",
                      width = 12,
                    #  status="secondary",
                      maximizable = FALSE,
                      highchartOutput("plot21", height = "300px")
                    ) , #%>% sortable(width = 12),
                    bs4Card(
                      title = "Plot 17",
                      width = 12,
                      solidHeader = TRUE,
                      status="info",
                      maximizable = FALSE,
                      highchartOutput("plot17", height = "300px")
                    ) #%>% sortable(width = 12)
                  )
        ),
        fluidRow(
            column(
              width = 7,
              bs4Card(
                title = "Plot 19",
                width = 12,
                maximizable = FALSE,
                highchartOutput("plot19", height = "450px")
              ) , #%>% sortable(width = 12),
              bs4Card(
                title = "Plot 20",
                width = 12,
                maximizable = FALSE,
                highchartOutput("plot20", height = "350px")
              ) #%>% sortable(width = 12)
            ),
            column(
              width = 5,
              bs4Card(
                title="Plot 13",
                headerBorder=FALSE,
                width = 12,
                maximizable = FALSE,
                highchartOutput("plot13", height = "500px")
              ) , #%>% sortable(width = 12),
              bs4Card(
                title="Plot 15",
                headerBorder=FALSE,
                width = 12,
                maximizable = FALSE,
                highchartOutput("plot15", height = "300px")
              ) , #%>% sortable(width = 12),
            )
          ),
        h4("Sobre o trabalho"), #### Sobre o trabalho ----
        fluidRow(
          column(
            width = 5,
            bs4Card(
              title = "Plot 24",
              width = 12,
              maximizable = FALSE,
              highchartOutput("plot24", height = "300px")
            ) , #%>% sortable(width = 12),
            bs4Card(
              title = "Plot 25",
              width = 12,
              maximizable = FALSE,
              highchartOutput("plot25", height = "300px")
            ) #%>% sortable(width = 12)
          ),
          column(
            width = 7,
            bs4Card(
              title = "Plot 26",
              width = 12,
              maximizable = FALSE,
              highchartOutput("plot26", height = "300px")
            ) , #%>% sortable(width = 12),
            bs4Card(
              title = "Plot 27",
              width = 12,
              maximizable = FALSE,
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
                title = "Plot 1",
                width = 12,
                maximizable = FALSE,
                highchartOutput("plot1", height = "300px")
              ) #%>% sortable(width = 12)
              ),
              column(
                width = 4,
                bs4Card(
                  title = "Plot 4",
                  width = 12,
                  maximizable = FALSE,
                  highchartOutput("plot4", height = "300px")
                ) #%>% sortable(width = 12)
                ),
            column(
              width = 4,
              bs4Card(
                title = "Plot 11",
                width = 12,
                maximizable = FALSE,
                highchartOutput("plot11", height = "300px")
              ) #%>% sortable(width = 12)
            ),
            column(
              width = 6,
              bs4Card(
                title = "Plot 3",
                width = 12,
                maximizable = FALSE,
                highchartOutput("plot3", height = "300px")
              ) , #%>% sortable(width = 12),
              bs4Card(
                title = "Plot 9",
                width = 12,
                maximizable = FALSE,
                highchartOutput("plot9", height = "300px")
              ) #%>% sortable(width = 12)
          ),
          column(
            width = 3,
            bs4Card(
              title = "Plot 12",
              width = 12,
              maximizable = FALSE,
              highchartOutput("plot12", height = "300px")
            ) , #%>% sortable(width = 12),
            bs4Card(
              title = "Plot 8",
              width = 12,
              maximizable = FALSE,
              highchartOutput("plot8", height = "300px")
            ) #%>% sortable(width = 12)
            ),
          column(width=3,
            bs4Card(
              title = "Plot 5",
              width = 12,
              maximizable = FALSE,
              highchartOutput("plot5", height = "300px")
            ) , #%>% sortable(width = 12),
            bs4Card(
              title = "Plot 10",
              width = 12,
              maximizable = FALSE,
              highchartOutput("plot10", height = "300px")
            ) #%>% sortable(width = 12)
            )
          ),
          h4("Sobre o desligamento"),
        fluidRow(
          column(
            width = 6,
            bs4Card(
              title = "Plot 23",
              width = 12,
              maximizable = FALSE,
              highchartOutput("plot23", height = "310px")
            ) #%>% sortable(width = 12)
          ),
          column(
            width = 6,
            bs4Card(
              title = "Plot 22",
              width = 12,
              maximizable = FALSE,
              highchartOutput("plot22", height = "310px")
            ) #%>% sortable(width = 12)
          ))
              ),
        tabPanel("Aspectos acadêmicos",
                 h3(""),
       # h4("Aspectos acadêmicos"), #### Aspectos acadêmicos ----
        fluidRow(
            bs4Card(
              title="Plot 29",
              headerBorder=FALSE,
              width = 12,
              maximizable = FALSE,
              highchartOutput("plot29", height = "400px")
            ) %>% sortable(width = 4),
                bs4Card(
                  title = "Plot 30",
                  width = 12,
                  maximizable = FALSE,
                  highchartOutput("plot30", height = "400px")
                ) %>% sortable(width = 4),
                bs4Card(
                  title = "Plot 31",
                  width = 12,
                  maximizable = FALSE,
                  highchartOutput("plot31", height = "400px")
                ) %>% sortable(width = 4)
            ),
       fluidRow(
                bs4Card(
                  title = "Plot 32",
                  width = 12,
                  maximizable = FALSE,
                  highchartOutput("plot32", height = "400px")
                ) %>% sortable(width = 4),
                bs4Card(
                  title = "Plot 33",
                  width = 12,
                  maximizable = FALSE,
                  highchartOutput("plot33", height = "400px")
                ) %>% sortable(width = 4),
                    bs4Card(
                      title = "Plot 34",
                      width = 12,
                      maximizable = FALSE,
                      highchartOutput("plot34", height = "400px")
                    ) %>% sortable(width = 4)
                  )
                ),
      tabPanel("Aspectos sociais",
               h3(""),
        #### Aspectos sociais ----
       # h4("Aspectos sociais"),
        fluidRow(
          bs4Card(
            title = "Plot 35",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot35",height="400px")
          ) %>% sortable(width = 4),
          bs4Card(
            title = "Plot 36",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot36",height="400px")
          ) %>% sortable(width = 4),
          bs4Card(
            title = "Plot 37",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot37",height="400px")
          ) %>% sortable(width = 4),
          bs4Card(
            title = "Plot 38",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot38",height="400px")
          ) %>% sortable(width = 4),
          bs4Card(
            title = "Plot 39",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot39",height="400px")
          ) %>% sortable(width = 4),
          bs4Card(
            title = "Plot 40",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot40",height="400px")
          ) %>% sortable(width = 4)
        )
        ),
      tabPanel("Aspectos institucionais",
               h3(""),
        #### Aspectos institucionais ----
        #h4("Aspectos institucionais"),
        fluidRow(
          bs4Card(
            title = "Plot 41",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot41",height="400px")
          ) %>% sortable(width = 4),
          bs4Card(
            title = "Plot 42",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot42",height="400px")
          ) %>% sortable(width = 4),
          bs4Card(
            title = "Plot 43",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot43",height="400px")
          ) %>% sortable(width = 4),
          bs4Card(
            title = "Plot 44",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot44",height="400px")
          ) %>% sortable(width = 4),
          bs4Card(
            title = "Plot 45",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot45",height="400px")
          ) %>% sortable(width = 4),
          bs4Card(
            title = "Plot 46",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot46",height="400px")
          ) %>% sortable(width = 4)
        )
        ),
        tabPanel("Aspectos de carreira",
                 h3(""),
        #### Aspectos de carreira ----
       # h4("Aspectos de carreira"),
        fluidRow(
          bs4Card(
            title = "Plot 47",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot47",height="400px")
          ) %>% sortable(width = 4),
          bs4Card(
            title = "Plot 48",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot48",height="400px")
          ) %>% sortable(width = 4),
          bs4Card(
            title = "Plot 49",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot49",height="400px")
          ) %>% sortable(width = 4),
          bs4Card(
            title = "Plot 50",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot50",height="400px")
          ) %>% sortable(width = 4),
          bs4Card(
            title = "Plot 51",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot51",height="400px")
          ) %>% sortable(width = 4),
          bs4Card(
            title = "Plot 52",
            width = 12,
            maximizable = FALSE,
            highchartOutput("plot52",height="400px")
          ) %>% sortable(width = 4)
        )
        ))
            
      )), ### Página 2 ----
      bs4TabItem(
        tabName = "pagina2",
        fluidRow(
          bs4Card(
            title = "Gráfico 2",
            #width = 12,
            status = "primary",
            solidHeader = TRUE
           # plotlyOutput("plot2")
          )
        )
      ) ### Página 3 (filtros) ----
  #     bs4TabItem(
  #       tabName = "filtros",
  #       fluidRow(
  #         bs4Card(
  #           title = "Filter Options",
  #           width = 12,
  #           radioButtons("filtroEvadido", "Filtrar Status:",
  #                        choices = list("Todos" = "todos", 
  #                                       "Apenas Evadidos" = "evadidos",
  #                                       "Apenas Concluintes" = "concluintes"),
  #                        selected = "todos"),
  #           bs4Card(
  #             title = "Filter Options",
  #             width = 12,
  #             radioButtons("filtroEvadido", "Filtrar Status:",
  #                          choices = list("Todos" = "todos", 
  #                                         "Apenas Evadidos" = "evadidos",
  #                                         "Apenas Concluintes" = "concluintes"),
  #                          selected = "todos"),
  #             awesomeCheckboxGroup(
  #               inputId = "curso_checkbox",
  #               label = "Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?",
  #               choices = unique(data$`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?`),
  #               selected = unique(data$`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?`),
  #               status = "primary"
  #             ),
  #             awesomeCheckboxGroup(
  #               inputId = "nivel_checkbox",
  #               label = "Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?",
  #               choices = unique(data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?`),
  #             #  selected = unique(data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?`),
  #               status = "primary"
  #             ),
  #             awesomeCheckboxGroup(
  #               inputId = "campus_checkbox",
  #               label = "Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?",
  #               choices = unique(data$`Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?`),
  #               selected = unique(data$`Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?`),
  #               status = "primary"
  #             ),
  #             awesomeCheckboxGroup(
  #               inputId = "genero_checkbox",
  #               label = "Identidade de gênero",
  #               choices = unique(data$`Identidade de gênero`),
  #               selected = unique(data$`Identidade de gênero`),
  #               status = "primary"
  #             ),
  #             awesomeCheckboxGroup(
  #               inputId = "etnia_checkbox",
  #               label = "Qual opção melhor descreve sua raça ou etnia?",
  #               choices = unique(data$`Qual opção melhor descreve sua raça ou etnia?`),
  #               selected = unique(data$`Qual opção melhor descreve sua raça ou etnia?`),
  #               status = "primary"
  #             ),
  #             awesomeCheckboxGroup(
  #               inputId = "areaConhecimento_checkbox",
  #               label = "Área de Conhecimento:",
  #               choices = unique(data$`area_conhecimento_curso`),
  #               selected = unique(data$`area_conhecimento_curso`),
  #               status = "primary"
  #             )
  #           )
  #   )
  # )
#)
)
)
)

    
  

# server ----

# Server logic
server <- function(input, output,session) {

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
                   pointFormat = '<b>{series.name}</b><br><span style="color:{point.color}">\u25AA</span>Frequência (%): <b>{point.n}</b> ({point.percent:.1f}%)<br/>') %>%
        hc_colors(c("#7BB1E4","#404C57"))
      
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
          hc_colors("#ED9368")
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
        title = list(style = list(color = '#E0E0E3')),
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
        title = list(style = list(color = '#333333')),
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
  output$contagemRespondentes <- renderbs4InfoBox({
    bs4InfoBox(
      title= "Nº de respondentes",
      value=nrow(dadosFiltrados()),
      icon=icon("users"))
  })
  output$contagemConcluintes <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Nº de Concluintes",
      value = nrow(subset(dadosFiltrados(), evadido == 0)),
      icon = icon("user-graduate"),
      color = "info"
    )
  })
  
  output$contagemDesvinculados <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Nº de Desvinculados",
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
        data = dadosFiltrados() %>% 
          filter(!is.na(`Em que ano você iniciou seu último curso de pós-graduação stricto sensu na UFMT?`)) %>%
          dplyr::count(`Em que ano você iniciou seu último curso de pós-graduação stricto sensu na UFMT?`) %>%
          mutate(percent = n / sum(n) * 100),
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
      )
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

}

# Executa o aplicativo
shinyApp(ui, server)
