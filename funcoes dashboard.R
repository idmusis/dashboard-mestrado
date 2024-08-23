botoes_menu <- list(
  "downloadPNG",
  "downloadPDF",
  "separator",
  "downloadCSV",
  "downloadXLS",
  "viewData"
)

################# gráficos highcharter ----

library(highcharter)

plot_grouped_bar <- function(data, category_col, group_col="evadido",category_order = NULL, category_labels = NULL,group_labels=NULL,xlab=NULL,titulo=category_col,tipo="column",na.omit=TRUE) {
  # Convertendo nomes de colunas em símbolos para uso no dplyr
  group_sym <- rlang::sym(group_col)
  category_sym <- rlang::sym(category_col)

  # Tratando valores NA
  if (na.omit) {
    data <- data %>% filter(!is.na(!!category_sym))
  } else {
    data <- data %>% mutate(!!category_sym := ifelse(is.na(!!category_sym), "NA", !!category_sym))
  }
  
  # Preparando os dados
  data_grouped <- data %>%
    dplyr::count(!!group_sym, !!category_sym) %>%
    group_by(!!group_sym) %>%
    mutate(percent = n / sum(n) * 100) %>%
    ungroup()

  
  # Se a ordem das categorias foi especificada, reordenar os dados
  if (!is.null(category_order)) {
    data_grouped <- data_grouped %>%
      mutate(!!category_sym := factor(!!category_sym, levels = category_order))
  } else{
    data_grouped <- data_grouped %>%
      mutate(!!category_sym := factor(!!category_sym))
  }
  
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
               pointFormat = '<b>{series.name}</b><br><span style="color:{point.color}">\u25AA</span>Frequência (%): <b>{point.n}</b> ({point.percent:.1f}%)<br/>')
  
  # Configurando rótulos de grupo, se fornecidos
  if (!is.null(group_labels)) {
    hc <- hc %>% hc_xAxis(categories = group_labels)
  }
  
  return(hc)
}
###########################################################

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

if (comparar){
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
}else{
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
    hc_legend(enabled = FALSE)  # Esconde a legenda
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
    hc_legend(enabled = FALSE)  # Esconde a legenda
}
}
  
  return(hc)

}

plot_bar_hc <- function(data, category_col, category_order = NULL, category_labels = NULL, xlab=NULL,ylab=NULL,tipo="column",titulo=category_col,na.omit=TRUE) {
  
  renderHighchart({
  force(forceRedraw())
  plot_bar(data, category_col, category_order = NULL, category_labels = NULL, xlab=NULL,ylab=NULL,tipo="column",titulo=category_col,na.omit=TRUE) 
  })
  
}
# forceRedraw <- reactiveVal(FALSE)
# hc <- function(funcao) {
#   renderHighchart({
#     force(forceRedraw())  # Ensure dependency on forceRedraw to trigger reactivity
#     funcao  # Execute the Highcharter function passed as argument
#   })
# }

# Exemplo de uso
#plot_bar(data2, "NivelAcademico")

plot_pie <- function(data=data, category_col, category_order = NULL, category_labels = NULL,titulo=category_col,
                       distance="0") {
  # Convertendo nome da coluna categoria em símbolo para uso no dplyr
  category_sym <- rlang::sym(category_col)
  
  # Assegurando que a coluna seja tratada como fator
  if (!is.null(category_order) && !is.null(category_labels)) {
    data <- data %>%
      mutate(!!category_sym := factor(!!category_sym, levels = category_order, labels = category_labels))
  } else {
    data <- data %>%
      mutate(!!category_sym := as.factor(!!category_sym))
  }
  data<-data %>% select(category_col) %>% na.omit()
  # Preparando os dados
  data_grouped <- data %>%
    dplyr::count(!!category_sym) %>%
    mutate(percent = n / sum(n) * 100)
  
  # Criando o gráfico de pizza
  hc <- highchart() %>%
    hc_chart(lang = list(decimalPoint = ',', thousandsSep = '.'), type = "pie") %>%
    hc_title(text=titulo,
             style = list(fontSize = "16px")) %>%
    hc_add_series(data_grouped, name = "Respondentes", type = "pie", hcaes(name = !!category_sym, y = n)) %>%
    hc_tooltip(shared = FALSE, useHTML = TRUE, 
               headerFormat = '<span style="font-size: 0.8em">{point.key}</span><br/>',
               pointFormat = '<span style="color:{point.color}">\u25AA</span> Frequência (%): <b>{point.y}</b> ({point.percent:.1f}%)<br/>') %>%
    hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE, format = '<b>{point.name}</b>: {point.percentage:.1f} %',distance=distance)))
  
  # # Configurando rótulos de categoria, se fornecidos
  # if (!is.null(category_labels)) {
  #   hc <- hc %>% hc_xAxis(categories = category_labels)
  # }
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

plot_box <- function(data, category_col, xlab="", titulo=category_col,tipo="boxplot") {
  # Convertendo nome da coluna em símbolo para uso no dplyr
  data[[category_col]]<-as.numeric(data[[category_col]])
  
  category_sym <- rlang::sym(category_col)
 boxplot_data <- data_to_boxplot(data,!!sym(category_col))
  # Criando o gráfico de boxplot
 hc<-hcboxplot(outliers=TRUE,
               x=data[[category_col]],
               name=xlab)%>%
   hc_chart(type = "column") %>%
   hc_title(text = titulo) %>%
#     hc_add_series(data = boxplot_data) 
   hc_tooltip(shared = TRUE, useHTML = TRUE,
              headerFormat = "",
              pointFormat = 'Q1: <b>{point.q1}</b><br/>Mediana: <b>{point.median}</b><br/>Q3: <b>{point.q3}</b><br/>Mínimo: <b>{point.low}</b><br/>Máximo: <b>{point.high}</b><br/>') %>%
   hc_xAxis(labels = list(enabled = FALSE))  # Desativa as labels no eixo X
   #hc_legend(enabled = FALSE) # Esconde a legenda
  
  return(hc)
}

hcmap2<-function (map = "custom/world", download_map_data = getOption("highcharter.download_map_data",
                                                                      ), 
                  ler_offline=TRUE,
          data = NULL, value = NULL, joinBy = NULL, ...) 
{
  
  
  fix_map_name <- function(x = "custom/world") {
    x <- stringr::str_replace(x, "\\.js$", "")
    x <- stringr::str_replace(x, "https://code\\.highcharts\\.com/mapdata/", "")
    x <- sprintf("%s.js", x)
    x
  }
  
  
  map <- fix_map_name(map)
  hc <- highchart(type = "map")
  if (download_map_data & !ler_offline) {
    mapdata <- download_map_data(map)
  } else if (ler_offline) {
     mapdata <- readRDS("braziljson.RDS")
  }
  else {
    dep <- htmlDependency(name = basename(map), version = "0.1.0", 
                          src = c(href = "https://code.highcharts.com/mapdata"), 
                          script = map)
    hc$dependencies <- c(hc$dependencies, list(dep))
    mapdata <- JS(sprintf("Highcharts.maps['%s']", str_replace(map, 
                                                               "\\.js$", "")))
  }
  if (is.null(data)) {
    hc <- hc %>% hc_add_series(mapData = mapdata, 
                                       ...)
  }
  else {
    data <- rename(data, `:=`(value, value))
    hc <- hc %>% hc_add_series(mapData = mapdata, 
                                       data = list_parse(data), joinBy = joinBy, ...) %>% 
      hc_colorAxis(auxpar = NULL)
  }
  hc %>% hc_credits(enabled = TRUE)
}

likert_chart <- function(data, category_col, group_col="evadido",
                         levels_order = c("Muito baixo(a)", "Baixo(a)", "Neutro", "Alto(a)", "Muito alto(a)") %>% rev(),
                         titulo=category_col) {
  
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
  
  # Renomear valores de evadido para "Concluínte" e "Desvinculado"
  data[[group_col]] <- factor(data[[group_col]], levels = c(0, 1), labels = c("Concluínte", "Desvinculado"))
  
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

####### bs4Dash ----

teste_box<-function (..., title = NULL, footer = NULL, status = NULL, solidHeader = FALSE, 
                     background = NULL, width = 6, height = NULL, collapsible = TRUE, 
                     collapsed = FALSE, closable = FALSE, maximizable = FALSE, 
                     icon = NULL, gradient = FALSE, boxToolSize = "sm", elevation = 0, 
                     headerBorder = TRUE, label = NULL, dropdownMenu = NULL, 
                     sidebar = NULL, id = NULL, noBorder = TRUE, scrollable = FALSE,smalltext=TRUE) 
{
  if (is.null(status)) 
    solidHeader <- TRUE
  validateBoxProps(title = title, label = label, sidebar = sidebar, 
                   dropdownMenu = dropdownMenu, status = status, gradient = gradient, 
                   collapsible = collapsible, collapsed = collapsed, solidHeader = solidHeader, 
                   background = background, elevation = elevation, width = width)
  props <- dropNulls(list(title = if (!is.null(title)) {
    if (inherits(title, "list")) {
      unlist(dropNulls(lapply(title, function(e) {
        if (inherits(e, "shiny.tag.list") || inherits(e, 
                                                      "shiny.tag")) {
          as.character(e)
        }
      })))
    } else {
      as.character(title)
    }
  } else {
    title
  }, status = status, solidHeader = solidHeader, background = background, 
  width = width, height = height, collapsible = collapsible, 
  closable = closable, maximizable = maximizable, gradient = gradient))
  
  cardCl <- setBoxClass(status, solidHeader, collapsible, 
                        collapsed, elevation, gradient, background, sidebar)
  
  # Apply no-border class if noBorder is TRUE
  if (noBorder) {
    cardCl <- paste(cardCl, "no-border")
  }

  if (smalltext) {
    cardCl <- paste(cardCl, "smalltext")
  }
  
  style <- setBoxStyle(height, sidebar)
  
  cardToolTag <- NULL
  if (collapsible || closable || maximizable || !is.null(dropdownMenu) || 
      !is.null(sidebar) || !is.null(label)) {
    cardToolTag <- shiny::tags$div(class = "card-tools float-right")
  }
  cardToolTag <- shiny::tagAppendChildren(cardToolTag, label, 
                                          createBoxTools(collapsible, collapsed, closable, maximizable, 
                                                         sidebar, dropdownMenu, boxToolSize, status, background, 
                                                         solidHeader))
  if (is.null(title) && (maximizable || closable || collapsible || 
                         !is.null(dropdownMenu) || !is.null(sidebar) || !is.null(label))) 
    title <- "‌"
  headerTag <- shiny::tags$div(class = if (headerBorder) 
    "card-header"
    else "card-header border-0", shiny::tags$h3(class = "card-title", 
                                                icon, title))
  headerTag <- shiny::tagAppendChild(headerTag, cardToolTag)
  
  # Add scroll-box class if scrollable is TRUE
  bodyTag <- shiny::tags$div(
    class = paste("card-body", if (scrollable) "scroll-box"),
    style = style,
    ..., sidebar[[2]]
  )
  
  footerTag <- if (!is.null(footer)) {
    shiny::tags$div(class = "card-footer", footer)
  }
  
  cardTag <- shiny::tags$div(class = cardCl, id = id)
  cardTag <- shiny::tagAppendChildren(cardTag, headerTag, 
                                      bodyTag, footerTag)
  shiny::tags$div(class = if (!is.null(width)) 
    paste0("col-sm-", width), cardTag, shiny::tags$script(type = "application/json", 
                                                          `data-for` = id, jsonlite::toJSON(x = props, auto_unbox = TRUE, 
                                                                                            json_verbatim = TRUE)))
}

environment(teste_box) <- asNamespace('bs4Dash')


valuebox2<-function (value, subtitle, icon = NULL, color = NULL, width = 3, 
                     href = NULL, footer = NULL, gradient = FALSE, elevation = NULL) 
{
  if (!is.null(icon)) {
    tagAssert(icon, type = "i")
  }
  if (is.null(color) && gradient) {
    stop("color cannot be NULL when gradient is TRUE. \n         fill cannot be TRUE when color is NULL.")
  }
  if (!is.null(width)) {
    stopifnot(is.numeric(width))
    stopifnot(width <= 12)
    stopifnot(width >= 0)
  }
  if (!is.null(elevation)) {
    stopifnot(is.numeric(elevation))
    stopifnot(elevation < 6)
    stopifnot(elevation >= 0)
  }
  if (!is.null(footer) & !is.null(href)) {
    stop("Choose either href or footer.")
  }
  valueBoxCl <- "small-box"
  if (!is.null(color)) {
    validateStatusPlus(color)
    if (gradient) {
      valueBoxCl <- paste0(valueBoxCl, " bg-gradient-", 
                           color)
    }
    else {
      valueBoxCl <- paste0(valueBoxCl, " bg-", color)
    }
  }
  if (!is.null(elevation)) 
    valueBoxCl <- paste0(valueBoxCl, " elevation-", elevation)
  innerTag <- shiny::tags$div(class = "inner", shiny::tags$strong(value), shiny::tags$p(class = "small-box-subtitle", 
                                                                    subtitle))
  iconTag <- if (!is.null(icon)) {
    shiny::tags$div(class = "icon", icon)
  }
  else {
    NULL
  }
  footerTag <- if (!is.null(footer)) {
    shiny::tags$div(class = "small-box-footer", footer)
  }
  else if (!is.null(href)) {
    shiny::tags$a(href = href, target = "_blank", class = "small-box-footer", 
                  "More info", shiny::icon("circle-arrow-right"))
  }
  else {
    NULL  # This removes the footer entirely if both are NULL
  }
  
  # If there's no footer, add padding-bottom to simulate the space it would have taken
  valueBoxTag <- shiny::tags$div(class = valueBoxCl, 
                                 style = if (is.null(footerTag)) "padding-bottom: 30px;")
  valueBoxTag <- shiny::tagAppendChildren(valueBoxTag, innerTag, iconTag)
  
  # Append the footer only if it was created (i.e., not NULL)
  if (!is.null(footerTag)) {
    valueBoxTag <- shiny::tagAppendChild(valueBoxTag, footerTag)
  }
  
  shiny::tags$div(class = if (!is.null(width)) 
    paste0("col-sm-", width), valueBoxTag)
}

environment(valuebox2) <- asNamespace('bs4Dash')


###### Tabela ----

tabela_dt <- function(tabela) {
  DT::datatable(tabela, rownames = FALSE,
                style = "bootstrap4",
                extensions = 'Buttons',
                options = list(
                  dom = 'Bfrt',
                  buttons = list(
                    list(
                      extend = 'copy',
                      text = 'Copiar'
                    ),
                    list(
                      extend = 'csv',
                      text = 'CSV'
                    ),
                    list(
                      extend = 'excel',
                      text = 'Excel'
                    ),
                    list(
                      extend = 'pdf',
                      text = 'PDF'
                    ),
                    list(
                      extend = 'print',
                      text = 'Imprimir'
                    )
                  ),
                  language = list(
                    url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                  )
                )
  )
}