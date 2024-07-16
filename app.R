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
data <- readxl::read_xlsx("dados.xlsx")

# ui ---- 

ui <- dashboardPage(
  scrollToTop=TRUE,
  help=FALSE,
  title = "Dashboard de Visualização de Dados",
  
  # Cabeçalho
  header = bs4DashNavbar(
    title = bs4DashBrand(
      title = "Dashboard de Dados",
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
                     selected = "todos")
      ),
      bs4SidebarMenuItem(
        "Filtros",
        tabName = "filtros",
        icon = icon("filter")
      ),
      bs4SidebarMenuItem(
        actionButton("reload", "Redraw Plot")
      )
    )
  ),
  
  ## Corpo ----
  body = 
    bs4DashBody(
      use_theme(create_theme(
        bs4dash_status(
          danger="#d2d6de"
        ))),
        
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
          height: 200px;    /* Set fixed height */
          max-height: 200px; /* Adjust height as needed */
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
                            width = 3, headerBorder = FALSE, collapsible = FALSE,
                            div(
                              class = "scroll-box",
                              awesomeCheckboxGroup(
                                inputId = "evadido_checkbox",
                                label = "Status",
                                choices = c("Selecionar tudo","Concluíntes","Desvinculados"),
                                selected =c("Selecionar tudo","Concluíntes","Desvinculados"),
                                status = "primary"
                              )
                            )
                          ),
            bs4Card(
        width = 3, headerBorder = FALSE, collapsible = FALSE,
        div(
          class = "scroll-box",
          awesomeCheckboxGroup(
            inputId = "nivel_checkbox",
            label = "Nível acadêmico",
            choices = c("Selecionar tudo",unique(data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %>% na.omit())),
            selected =c("Selecionar tudo",unique(data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %>% na.omit())),
            status = "primary"
          )
              )
            ),
            bs4Card(
              width = 3,headerBorder = FALSE,collapsible=FALSE,
              div(
                class = "scroll-box", 
              radioButtons("filtroEvadido", "Filtrar Status:",
                           choices = list("Todos" = "todos",
                                          "Apenas Evadidos" = "evadidos",
                                          "Apenas Concluintes" = "concluintes"),
                           selected = "todos"))),
            bs4Card(
              width = 3,headerBorder = FALSE,collapsible=FALSE,
              div(
                class = "scroll-box", 
                # Content that exceeds the height
              
                awesomeCheckboxGroup(
                  inputId = "curso_checkbox",
                  label = "Curso",
                  choices = unique(data$`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?`),
                  selected = unique(data$`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?`),
                  status = "primary")
                )),
            bs4Card(
              width = 3,headerBorder = FALSE,collapsible=FALSE,
              div(
                class = "scroll-box",
                awesomeCheckboxGroup(
                  inputId = "campus_checkbox",
                  label = "Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?",
                  choices = unique(data$`Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?`) %>% na.omit(),
                  selected = unique(data$`Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?`),
                  status = "primary"
                ))),
            bs4Card(
              width = 3,headerBorder = FALSE,collapsible=FALSE,
              div(
                class = "scroll-box",
                awesomeCheckboxGroup(
                  inputId = "genero_checkbox",
                  label = "Identidade de gênero",
                  choices = unique(data$`Identidade de gênero`),
                  selected = unique(data$`Identidade de gênero`),
                  status = "primary"
                ))),
            bs4Card(
              width = 3,headerBorder = FALSE,collapsible=FALSE,
              div(
                class = "scroll-box",
                awesomeCheckboxGroup(
                  inputId = "etnia_checkbox",
                  label = "Qual opção melhor descreve sua raça ou etnia?",
                  choices = unique(data$`Qual opção melhor descreve sua raça ou etnia?`),
                  selected = unique(data$`Qual opção melhor descreve sua raça ou etnia?`),
                  status = "primary"
                ))),
            bs4Card(
              width =3,headerBorder = FALSE,collapsible=FALSE,
              div(
                class = "scroll-box",
              awesomeCheckboxGroup(
                  inputId = "areaConhecimento_checkbox",
                  label = "Área de Conhecimento:",
                  choices = unique(data$`area_conhecimento_curso`),
                  selected = unique(data$`area_conhecimento_curso`),
                  status = "primary"
                )
            )
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
            highchartOutput("plot3", height = 300)
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
server <- function(input, output) {

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
        # Do something when dark mode is on
        print("Dark mode is ON")
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
          backgroundColor = 'rgba(0, 0, 0, 0.5)',
          itemStyle = list(color = '#E0E0E3'),
          itemHoverStyle = list(color = '#FFF'),
          itemHiddenStyle = list(color = '#606063')
        ),
        credits = list(style = list(color = '#666'))
        ))
        
        forceRedraw(!forceRedraw())  # Toggle the value to trigger reactivity
      } else {
        # Do something when dark mode is off
        print("Dark mode is OFF")
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
          backgroundColor = '#f7f7f7',
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
  
  ## Filtrar dados ----
  temp_data <- data # Iniciar com o dataframe completo
  dadosFiltrados <- reactive({
   # input$dark_mode
   # Aplicar filtros baseados nas escolhas das checkboxes
    filtered <- data
    if (!is.null(input$curso_checkbox)) {
      filtered <- filtered %>% filter(`Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?` %in% input$curso_checkbox)
      
    }
    
    if ("Selecionar tudo" %in% input$nivel_checkbox) {
      if (length(input$nivel_checkbox) - 1 < length(unique(data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?`))) {
        # If "Select All" is checked and not all items are selected, select all
        updateAwesomeCheckboxGroup(inputId="nivel_checkbox",
                                   selected = c("Selecionar tudo", unique(data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?`))
        )
        force(forceRedraw())
      }
    } else {
     # if (length(input$nivel_checkbox) == length(unique(data$`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?`)) + 1) {
        # If "Select All" is unchecked and all items are selected, deselect all
        updateAwesomeCheckboxGroup(inputId="nivel_checkbox", selected = character(0))
      force(forceRedraw())
      }
    #}
  

      filtered <- filtered %>% filter(`Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?` %in% input$nivel_checkbox)
      force(forceRedraw())
    
    if (!is.null(input$campus_checkbox)) {
      filtered <- filtered %>% filter(`Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?` %in% input$campus_checkbox)
    }
    if (!is.null(input$genero_checkbox)) {
      filtered <- filtered %>% filter(`Identidade de gênero` %in% input$genero_checkbox)
    }
    if (!is.null(input$etnia_checkbox)) {
      filtered <- filtered %>% filter(`Qual opção melhor descreve sua raça ou etnia?` %in% input$etnia_checkbox)
    }
    if (!is.null(input$areaConhecimento_checkbox)) {
      filtered <- filtered %>% filter(`area_conhecimento_curso` %in% input$areaConhecimento_checkbox)
    }
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
  
  output$plot1<-renderHighchart(plot_pie(dadosFiltrados(),category_col = "evadido",
                         category_order = c(0,1), 
                         category_labels = c("Concluínte","Desvinculado"),
                         titulo="Status"))

  output$plot2<-renderHighchart({
    force(forceRedraw())
    plot_bar(dadosFiltrados(),category_col = "Qual o nome do curso de sua última pós-graduação stricto sensu realizada na UFMT?")})
  
  output$plot3<-renderHighchart({plot_bar(dadosFiltrados(),category_col = "Em que ano você iniciou seu último curso de pós-graduação stricto sensu na UFMT?",tipo="line") })

  # output$plot4<-plot_bar(dadosFiltrados(),category_col = "Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?") %>% renderHighchart()
   output$plot4<-renderHighchart({
     force(forceRedraw())
     plot_bar(dadosFiltrados(),category_col = "Qual é o nível acadêmico do seu curso de pós-graduação stricto sensu mais recente realizado na UFMT?")})
   
  output$plot5<-plot_bar(dadosFiltrados(),category_col = "Em qual campus da UFMT você cursou seu último programa de pós-graduação stricto sensu?") %>% renderHighchart()
  
  output$plot6<-renderHighchart(plot_bar(dadosFiltrados(),category_col = "Em que ano concluiu seu curso de pós-graduação? Caso tenha sido desligado, informe o ano em que ocorreu o desligamento",tipo="line"))
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
  forceRedraw <- reactiveVal(FALSE)
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

output$plot8<-plot_bar(dadosFiltrados(),category_col =  "Você fez uso de algum período de prorrogação de prazo para o término do seu curso de pós-graduação? Se sim, qual foi o período de prorrogação utilizado?") %>% renderHighchart()

output$plot9<-plot_pie(dadosFiltrados(),category_col =  "area_conhecimento_curso",titulo="Área de conhecimento do curso",distance=0) %>% renderHighchart()
}

# Executa o aplicativo
shinyApp(ui, server)
