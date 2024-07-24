library(tidyverse)
library(shiny)
library(plotly)
library(ggplot2)
library(bs4Dash)
library(highcharter)
library(shinyWidgets)
library(data.table)
library(fresh)
source("funcoes dashboard.R")



lang <- getOption("highcharter.lang")
lang$decimalPoint <- ","
options(highcharter.lang = lang)

# Dados exemplo
#data <- readxl::read_xlsx("dados.xlsx")
data <- arrow::read_feather("dados_feather.feather")

# ui ---- 

ui <- dashboardPage(
  scrollToTop=TRUE,
  help=NULL,
  title = "Dashboard - Fatores que influenciam a permanência e a evasão nos cursos de pós-graduação stricto sensu da UFMT",
  
  # Cabeçalho
  header = bs4DashNavbar(
    title = bs4DashBrand(
      title = "Fatores que influenciam a permanência e a evasão nos cursos de pós-graduação stricto sensu da UFMT",
      color = "primary",
      image = NULL
    )
  ),
  
  ## Barra lateral ----
  sidebar = bs4DashSidebar(
    status = "primary",
    title = "Menu",
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
        
        .scroll-box .form-group { margin-bottom: 0px !important; } /* Reduce space between groups */
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
                         selected = "selectall",
                         status = "primary",
                         icon = icon("check"),
                         animation = "smooth"
                       ),
                       prettyCheckboxGroup(
                         inputId = "evadido_checkbox",
                         label = NULL,
                         choiceValues = c(0,1),
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
                                status = "primary",
                                icon = icon("check"),
                                animation="smooth"),

                  prettyCheckboxGroup(
                    inputId = args[[1]],
                    label = NULL,#args[[2]],
                    choices = args[[3]],
                    selected = args[[3]],  # Select all by default, modify as needed
                    status = "primary",
                    icon = icon("check"),
                    animation="smooth"
                  ))
                ))
              }
            )), 
        fluidRow(
        
            bs4InfoBoxOutput("contagemRespondentes"),
             bs4InfoBoxOutput("contagemConcluintes"),
             bs4InfoBoxOutput("contagemDesvinculados")
          ),
        #### gráficos gerais ----
        h4("Gráficos gerais"),
        fluidRow(
          bs4Card(
            title = "Plot 1",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot1")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 2",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot2")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 3",
            width = 12,
            collapsible = FALSE,
            headerBorder = FALSE,
            maximizable = TRUE,
            highchartOutput("plot3")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 4",
            width = 12,
            collapsible = FALSE,
            headerBorder = FALSE,
            maximizable = TRUE,
            highchartOutput("plot4")
          ) %>% sortable(width = 3)
        ), #%>% box(collapsible=TRUE,width=12,background="danger",elevation=0,solidHeader=TRUE,title="Gráficos gerais",status="primary"),
        fluidRow(
          bs4Card(
            title = "Plot 5",
            width = 12,
            headerBorder = FALSE,
            maximizable = TRUE,
            highchartOutput("plot5")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 6",
            width = 12,
            collapsible = FALSE,
            headerBorder = FALSE,
            maximizable = TRUE,
            highchartOutput("plot6")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 7",
            width = 12,
            collapsible = FALSE,
            headerBorder = FALSE,
            maximizable = TRUE,
            highchartOutput("plot7", height = "600px")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 8",
            width = 12,
            collapsible = FALSE,
            headerBorder = FALSE,
            maximizable = TRUE,
            highchartOutput("plot8")
          ) %>% sortable(width = 3)
        ),
        # Continue this pattern for additional rows (Rows 3 to 6)
        fluidRow(
          bs4Card(
            title = "Plot 9",
            width = 12,
            collapsible = FALSE,
            headerBorder = FALSE,
            maximizable = TRUE,
            highchartOutput("plot9")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 10",
            width = 12,
            collapsible = FALSE,
            headerBorder = FALSE,
            maximizable = TRUE,
            highchartOutput("plot10")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 11",
            width = NULL,
            collapsible = FALSE,
            headerBorder = FALSE,
            maximizable = TRUE,
            height = "350px",
            highchartOutput("plot11")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 12",
            width = NULL,
            collapsible = FALSE,
            headerBorder = FALSE,
            maximizable = TRUE,
            height = "400px",
            highchartOutput("plot12")
          ) %>% sortable(width = 3)
        ),    fluidRow(
          bs4Card(
            title = "Plot 13",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot13")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 14",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot14")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 15",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot15")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 16",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot16")
          ) %>% sortable(width = 3)
        ),
        fluidRow(
          bs4Card(
            title = "Plot 17",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot17")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 18",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot18")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 19",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot19")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 20",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot20")
          ) %>% sortable(width = 3)
        ),
        fluidRow(
          bs4Card(
            title = "Plot 21",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot21")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 22",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot22")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 23",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot23")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 24",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot24")
          ) %>% sortable(width = 3)
        ),
        fluidRow(
          bs4Card(
            title = "Plot 25",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot25")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 26",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot26")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 27",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot27")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 28",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot28")
          ) %>% sortable(width = 3)
        ),
        #### Aspectos acadêmicos ----
        h4("Aspectos acadêmicos"),
        fluidRow(
          bs4Card(
            title = "Plot 29",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot29")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 30",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot30")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 31",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot31")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 32",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot32")
          ) %>% sortable(width = 3)
        ),
        fluidRow(
          bs4Card(
            title = "Plot 33",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot33")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 34",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot34")
          ) %>% sortable(width = 3)
        ),
        
        #### Aspectos sociais ----
        h4("Aspectos sociais"),
        fluidRow(
          bs4Card(
            title = "Plot 35",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot35")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 36",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot36")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 37",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot37")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 38",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot38")
          ) %>% sortable(width = 3)
        ),
        fluidRow(
          bs4Card(
            title = "Plot 39",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot39")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 40",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot40")
          ) %>% sortable(width = 3)
        ),
        
        #### Aspectos institucionais ----
        h4("Aspectos institucionais"),
        fluidRow(
          bs4Card(
            title = "Plot 41",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot41")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 42",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot42")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 43",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot43")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 44",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot44")
          ) %>% sortable(width = 3)
        ),
        fluidRow(
          bs4Card(
            title = "Plot 45",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot45")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 46",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot46")
          ) %>% sortable(width = 3)
        ),
        
        #### Aspectos de carreira ----
        h4("Aspectos de carreira"),
        fluidRow(
          bs4Card(
            title = "Plot 47",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot47")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 48",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot48")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 49",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot49")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 50",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot50")
          ) %>% sortable(width = 3)
        ),
        fluidRow(
          bs4Card(
            title = "Plot 51",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot51")
          ) %>% sortable(width = 3),
          bs4Card(
            title = "Plot 52",
            width = 12,
            maximizable = TRUE,
            highchartOutput("plot52")
          ) %>% sortable(width = 3)
        )
        
            
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

  # Cria um reactive que depende do filtro selecionado
  # dadosFiltrados <- reactive({
  #   if (input$dark_mode) {data}else{data}
  #   
  #   if (input$filtroEvadido == "evadidos") {
  #     subset(data, evadido == 1)
  #   } else if (input$filtroEvadido == "concluintes") {
  #     subset(data, evadido == 0)
  #   } else {
  #     data
  #   }
  # })
  # 
  
  forceRedraw <- reactiveVal(FALSE)
  
  observe({
    observeEvent(input$dark_mode, {
      ignoreNULL=FALSE
      if (input$dark_mode) {
        print("Dark mode")
        # options(highcharter.theme = hc_theme_darkunica())
        options(highcharter.theme = hc_theme(chart = list(
          backgroundColor = 'transparent',
          style = list(color = '#E0E0E3')
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
        options(highcharter.theme = hc_theme(chart = list(
          backgroundColor = 'transparent',
          style = list(color = '#333333')
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
    
  observeEvent(input$evadido_checkbox_selectall,{
    updatePrettyCheckboxGroup(
      inputId= "evadido_checkbox", 
      selected = if(!is.null(input$evadido_checkbox_selectall)) unique(data$evadido %>% na.omit()) else character(0)
    )
    
  })
  
    observeEvent(input$nivel_checkbox_selectall,{
      updatePrettyCheckboxGroup(
        inputId= "nivel_checkbox", 
        selected = if(!is.null(input$nivel_checkbox_selectall)) unique(data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %>% na.omit()) else character(0)
      )

    })
    
    # Observer for Curso

    observeEvent(input$curso_checkbox_selectall,{
      updatePrettyCheckboxGroup(
        inputId = "curso_checkbox", 
        selected = if (!is.null(input$curso_checkbox_selectall)) unique(data$`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?`) else character(0)
      )
    })
    
    # Observer for Câmpus

    observeEvent(input$campus_checkbox_selectall,{
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
    print(input$nivel_checkbox)
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
  
  # filter1_rows <- reactive({
  #   data[`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?` %in% input$curso_checkbox, which = TRUE]
  # })
  # 
  # filter2_rows <- reactive({
  #   data[`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %in% input$nivel_checkbox, which = TRUE]
  # })
  # 
  # filter3_rows <- reactive({
  #   data[`Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?` %in% input$campus_checkbox, which = TRUE]
  # })
  # 
  # filter4_rows <- reactive({
  #   data[`Identidade de gênero` %in% input$genero_checkbox, which = TRUE]
  # })
  # 
  # filter5_rows <- reactive({
  #   data[`Qual opção melhor descreve sua raça ou etnia?` %in% input$etnia_checkbox, which = TRUE]
  # })
  # 
  # filter6_rows <- reactive({
  #   data[`area_conhecimento_curso` %in% input$areaConhecimento_checkbox, which = TRUE]
  # })
  # 
  # dadosFiltrados <- reactive({
  #   final_rows <- intersect(filter1_rows(), filter2_rows())
  #   final_rows <- intersect(final_rows, filter3_rows())
  #   final_rows <- intersect(final_rows, filter4_rows())
  #   final_rows <- intersect(final_rows, filter5_rows())
  #   final_rows <- intersect(final_rows, filter6_rows())
  #   data[final_rows]
  # })
  # dadosFiltrados <- reactive({
  #     subset(data,`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?` %in% input$curso_checkbox)
             # `Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %in% input$nivel_checkbox,
             # `Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?` %in% input$campus_checkbox,
             # `Identidade de gênero` %in% input$genero_checkbox,
             # `Qual opção melhor descreve sua raça ou etnia?` %in% input$etnia_checkbox,
             # `area_conhecimento_curso` %in% input$areaConhecimento_checkbox)
  #})
  # rv <- reactiveValues()
  # rv$data <- data
  # 
  # observe({
  #   # Start with the original data
  #   rv$data <- data
  #   
  #   # Apply filters based on user input
  #   if (!is.null(input$curso) && length(input$curso) > 0) {
  #     rv$data <- rv$data[rv$data$`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?` %in% input$curso,]
  #   }
  #   if (!is.null(input$nivel_checkbox) && length(input$nivel_checkbox) > 0) {
  #     rv$data <- rv$data[rv$data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %in% input$nivel_checkbox,]
  #   }
  #   if (!is.null(input$campus) && length(input$campus) > 0) {
  #     rv$data <- rv$data[rv$data$`Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?` %in% input$campus,]
  #   }
  #   if (!is.null(input$genero) && length(input$genero) > 0) {
  #     rv$data <- rv$data[rv$data$`Identidade de gênero` %in% input$genero,]
  #   }
  #   if (!is.null(input$raca) && length(input$raca) > 0) {
  #     rv$data <- rv$data[rv$data$`Qual opção melhor descreve sua raça ou etnia?` %in% input$raca,]
  #   }
  #   if (!is.null(input$area) && length(input$area) > 0) {
  #     rv$data <- rv$data[rv$data$area_conhecimento_curso %in% input$area,]
  #   }
  # })
  # 
  # dadosFiltrados <- reactive({
  #   rv$data
  # })
  # Cria um reactive que depende do filtro selecionado
  # dadosFiltrados <- reactive({
  #   if (input$dark_mode) {data}else{data}
  #   
  #   if (input$filtroEvadido == "evadidos") {
  #     subset(data, evadido == 1)
  #   } else if (input$filtroEvadido == "concluintes") {
  #     subset(data, evadido == 0)
  #   } else {
  #     data
  #   }
  # })
  # 
  
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
      color = "gray-dark"
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
    plot_bar(dadosFiltrados(),category_col = "Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?")})
  
  output$plot3<-renderHighchart({
    force(forceRedraw())
    plot_bar(dadosFiltrados(),category_col = "Em que ano você iniciou seu último curso de pós-graduação stricto sensu na UFMT?",tipo="line") })

  # output$plot4<-plot_bar(dadosFiltrados(),category_col = "Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?") %>% renderHighchart()
   output$plot4<-renderHighchart({
     force(forceRedraw())
     plot_bar(dadosFiltrados(),category_col = "Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?")})
   
  output$plot5<-
    renderHighchart({
      force(forceRedraw())
      plot_bar(
    dadosFiltrados(),category_col = "Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?")
    })
  
  output$plot6<-renderHighchart({
    force(forceRedraw())
    plot_bar(dadosFiltrados(),category_col = "Em que ano concluiu seu curso de pós-graduação? Caso tenha sido desligado, informe o ano em que ocorreu o desligamento",tipo="line")
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


output$plot7<-renderHighchart({
  force(forceRedraw())
  processed_data <- reactive({
    dadosFiltrados() %>%
      filter(`Identidade de gênero` %in% c("Masculina", "Feminina")) %>%
      select("Identidade de gênero", "Idade no ingresso") %>%
      na.omit() %>%
      mutate(age_group = sapply(`Idade no ingresso`, categorize_age)) %>%
      group_by(age_group, `Identidade de gênero`) %>%
      summarize(count = n(), .groups = 'drop') %>%
      mutate(count = if_else(`Identidade de gênero` == "Feminina", -count, count))
  })
  
  categories <- processed_data() %>%
    select(age_group) %>%
    distinct() %>%
    arrange(match(age_group, unique(processed_data()$age_group))) %>%
    pull(age_group)
  
  highchart() %>%
  hc_chart(type = "bar") %>%
  hc_title(text = "Idade no ingresso por gênero") %>%
  hc_xAxis(categories = categories, opposite = FALSE) %>%
    hc_yAxis(labels = list(formatter = JS("function () { return Math.abs(this.value) + '%'; }")), # For percentage
             title = list(text = "Percentage")) %>% # Update axis title accordingly
    hc_plotOptions(series = list(stacking = 'normal')) %>%
    hc_tooltip(formatter = JS("
    function () {
      var total = this.series.data.map(point => Math.abs(point.y)).reduce((a, b) => a + b, 0);
      var percentage = Math.abs(this.point.y) / total * 100;
      return '<span style=\"font-size: 0.8em\">' + this.point.category + '</span><br/>' +
             '<span style=\"color:' + this.point.color + '\">\u25AA</span> ' + this.series.name + ' (%): <b>' + Highcharts.numberFormat(Math.abs(this.point.y), 0) +
             '</b> (' + Highcharts.numberFormat(percentage, 1) + '%)<br/>';
    }")) %>%
    # hc_tooltip(formatter = JS("function () {
    #   var total = this.series.data.map(point => point.y).reduce((a, b) => Math.abs(a) + Math.abs(b), 0);
    #   var percentage = Math.abs(this.point.y) / total * 100;
    #   return '<b>' + this.series.name + ', Idade no ingresso ' + this.point.category + '</b><br/>' +
    #          'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0) + '<br/>' +
    #          'Percentage: ' + Highcharts.numberFormat(percentage, 2) + '%';
    # }")) %>%
    hc_add_series(name = "Masculino", data = processed_data() %>% filter(`Identidade de gênero` == "Masculina") %>% arrange(match(age_group, categories)) %>% pull(count)) %>%
    hc_add_series(name = "Feminino", data = processed_data() %>% filter(`Identidade de gênero` == "Feminina") %>% arrange(match(age_group, categories)) %>% pull(count))
  
})

output$plot8<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col =  "Você fez uso de algum período de prorrogação de prazo para o término do seu curso de pós-graduação? Se sim, qual foi o período de prorrogação utilizado?") 
})

output$plot9<-renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(),category_col =  "area_conhecimento_curso",titulo="Área de conhecimento do curso",distance=0) 
})
output$plot10<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Você trancou sua matrícula no programa de pós-graduação? Se sim, por quanto tempo?")
})

output$plot11<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Você recebeu bolsa de estudos ou auxílio financeiro durante o curso de pós-graduação? Se sim, por quanto tempo?")
})

output$plot12<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Você precisou mudar de cidade para realizar o curso de pós-graduação?")
})

output$plot13<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Estado de nascimento")
})

# output$plot14<-renderHighchart({
#   force(forceRedraw())
#   plot_bar(dadosFiltrados(),category_col = "Cidade de nascimento")
# })

output$plot15<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Estado civil ao iniciar o curso de pós-graduação")
})

output$plot16<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Identidade de gênero")
})

output$plot17<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Como você se identifica em relação à sua orientação sexual?")
})

output$plot18<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Qual opção melhor descreve sua raça ou etnia?")
})

output$plot19<-renderHighchart({
  force(forceRedraw())
  dadosFiltrados () %>%
    separate_rows("Apresenta alguma deficiência ou transtorno? Marque todos que se aplicam", sep = ";") %>%
    mutate_all(~ if(is.character(.)) na_if(trimws(.), "") else .) %>%
  plot_bar(.,category_col = "Apresenta alguma deficiência ou transtorno? Marque todos que se aplicam")
})

output$plot20<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Qual era o rendimento per capita familiar durante o período em que realizou o curso de pós-graduação?")
})

output$plot21<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Você cursou o ensino superior (graduação) predominantemente em")
})

output$plot22<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Antes de seu desligamento do curso de pós-graduação, chegou a realizar exame de qualificação?")
})

output$plot23<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Após o desligamento nesse programa, você concluiu ou está cursando outro curso de pós-graduação stricto sensu?")
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
  plot_bar(dadosFiltrados(),category_col = "Caso tenha mantido vínculo empregatício (trabalhando ou não), qual categoria melhor descreve seu ambiente de trabalho? Marque todas que se aplicam")
})

output$plot27<-renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(),category_col = "Por que motivo não trabalhou durante o período cursado?")
})

output$plot29 <- renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(), category_col = "Como você classificaria seu nível de desempenho acadêmico ao longo do curso de pós-graduação?")
})

output$plot30 <- renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(), category_col = "Qual o seu nível de satisfação com a didática dos docentes em classe?")
})

output$plot31 <- renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(), category_col = "Como você classificaria a disponibilidade dos docentes para responder às dúvidas dos discentes durante o curso de pós-graduação?")
})

output$plot32 <- renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(), category_col = "De forma geral, como você avalia o nível de suporte recebido do seu orientador durante a realização de sua dissertação/tese de pós-graduação?")
})

output$plot33 <- renderHighchart({
  force(forceRedraw())
  plot_bar(dadosFiltrados(), category_col = "Qual o seu nível de satisfação com a acessibilidade, qualidade e eficácia dos materiais didáticos e recursos de aprendizagem oferecidos em seu curso de pós-graduação?")
})

output$plot34 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Como você descreveria sua frequência de participação em atividades acadêmicas extracurriculares (como seminários, workshops e outros eventos) durante o seu curso de pós-graduação?")
})

output$plot35 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Como você avalia as oportunidades de colaboração entre alunos, como trabalhos em grupo ou projetos de pesquisa, no seu programa de pós-graduação?")
})

output$plot36 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Como você avalia seu nível de interação com colegas dentro do ambiente acadêmico do seu programa de pós-graduação?")
})

output$plot37 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Como você avalia seu nível de integração e participação em atividades sociais com colegas do seu programa de pós-graduação, realizadas fora do ambiente acadêmico?")
})

output$plot38 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Como você avalia a liberdade de expressar suas opiniões no ambiente acadêmico do seu programa de pós-graduação?")
})

output$plot39 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Durante o seu curso de pós-graduação, com que frequência você socializou com colegas em momentos de lanches e refeições?")
})

output$plot40 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "No decorrer do seu programa de pós-graduação, com que frequência você testemunhou ou sofreu alguma forma de assédio, entendido como condutas indesejadas de natureza física, verbal ou psicológica, que visaram ou resultaram em ofensa ou humilhação?")
})

output$plot41 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Em que medida a escolha desta instituição específica se alinhou com seus planos de carreira?")
})

output$plot42 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Em que medida a oferta de cursos nesta área influenciou sua escolha por esta instituição?")
})

output$plot43 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Como você avaliaria os custos indiretos (moradia, transporte, material didático, custo de vida) associados a estudar nesta instituição, em comparação com outras escolhas?")
})

output$plot44 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Qual a probabilidade de você recomendar esta instituição para futuros estudantes de pós-graduação?")
})

output$plot45 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Qual o seu nível de satisfação com a eficácia da comunicação entre a coordenação do curso e os estudantes?")
})

output$plot46 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Como você avalia a qualidade e adequação da infraestrutura física da instituição (prédios, laboratórios, salas de aula, espaços de convivência) para atender às necessidades acadêmicas dos alunos?")
})

output$plot47 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Durante o seu curso de pós-graduação, como você classificaria sua intenção de buscar futuras oportunidades educacionais, como doutorado, pós-doutorado ou formações adicionais em sua área?")
})

output$plot48 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Durante o curso de pós-graduação, como descreveria sua disposição ou interesse em trabalhar na área do curso?")
})

output$plot49 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Em que nível o programa de pós-graduação atendeu às suas expectativas no desenvolvimento de suas competências?")
})

output$plot50 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "No início dos estudos, qual era seu nível de intenção de completar o curso?")
})

output$plot51 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "Durante a pós-graduação, como você avaliaria a relevância do curso para sua carreira profissional?")
})

output$plot52 <- renderHighchart({
  force(forceRedraw())
  plot_pie(dadosFiltrados(), category_col = "No decorrer do seu programa de pós-graduação, com que frequência você testemunhou ou sofreu alguma forma de assédio, entendido como condutas indesejadas de natureza física, verbal ou psicológica, que visaram ou resultaram em ofensa ou humilhação?")
})

}
# Executa o aplicativo
shinyApp(ui, server)
