# criarGraficoPizza <- function(data) {
#   ggplot(data, aes(x = "", y = valores, fill = categoria)) +
#     geom_bar(stat = "identity", width = 1) +
#     coord_polar(theta = "y") +
#     theme_void() +
#     labs(fill = "Categoria")
# }
# 
# 
# library(plotly)
# library(dplyr)
# 
# # Função para criar um gráfico de barras agrupadas interativo
# createGroupedBarChart <- function(data, group_var, xlab=group_var, color_scheme = "Set1") {
#   # Processar dados para criar porcentagens por grupo
#   variable_table <- data %>%
#     group_by(!!sym(group_var), evadido) %>%
#     summarise(Frequencia = n(), .groups = 'drop') %>%
#     group_by(evadido) %>%
#     mutate(Porcentagem = (Frequencia / sum(Frequencia)) * 100)
#   
#   # Mapear 'evadido' para nomes descritivos
#   variable_table$Grupo <- ifelse(variable_table$evadido == 0, "Concluintes", "Evadidos")
#   
#   # Criar gráfico de barras agrupadas com hoverinfo detalhado
#   p <- plot_ly(data = variable_table, x = ~get(group_var), y = ~Porcentagem,
#                type = 'bar', color = ~Grupo, colors = RColorBrewer::brewer.pal(length(unique(variable_table$Grupo)), color_scheme),
#                text = ~paste0("<b>",group_var,":</b>",variable_table[[group_var]],"<br><b>Grupo:</b>", Grupo, "<br><b>Frequência:</b>", Frequencia, "<br><b>Porcentagem:</b>", round(Porcentagem, 2), "%"),
#                hoverinfo = "text") %>%
#     layout(yaxis = list(title = 'Porcentagem do grupo (%)'),
#            xaxis=list(title=xlab),
#            barmode = 'group')
#   
#   # Retornar o gráfico
#   return(p)
# }

# # Exemplo de uso da função
# set.seed(123)
# data <- data.frame(
#   evadido = sample(0:1, 100, replace = TRUE),
#   Graduacao = sample(c("Graduação", "Mestrado", "Doutorado"), 100, replace = TRUE),
#   Valor = rnorm(100)
# )
# 
# # Chamada da função
# p <- createGroupedBarChart(data, "Graduacao")
# p
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
    hc_yAxis(title = list(text = "Porcentagem por grupo")) %>%
    # hc_tooltip(shared=FALSE,useHTML = TRUE, headerFormat = '<span style="font-size: 0.8em">{point.key}</span><br/>',
    #            pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.n}</b> ({point.percent:.1f}% do total do grupo)<br/>') %>%
    hc_tooltip(shared = FALSE, useHTML = TRUE, headerFormat = '<span style="font-size: 0.8em">{point.key}</span><br/>',
               pointFormat = '<span style="color:{point.color}">\u25AA</span>Grupo:<b>{series.name}</b><br><span style="color:{point.color}">\u25AA</span>Frequência:<b>{point.n}</b><br><span style="color:{point.color}">\u25AA</span>Percentual:<b>{point.percent:.1f}%</b> do total do grupo<br/>')
  
  # Configurando rótulos de grupo, se fornecidos
  if (!is.null(group_labels)) {
    hc <- hc %>% hc_xAxis(categories = group_labels)
  }
  # } else if (group_col == "evadido") {
  #   hc <- hc %>% hc_xAxis(categories = c("Concluínte", "Desvinculado"))
  # }
  
  return(hc)
}

plot_bar <- function(data, category_col, category_order = NULL, category_labels = NULL, xlab="",ylab="",tipo="column",titulo=category_col,
                     na.omit=TRUE,
                     percent=TRUE) {
  #force(forceRedraw())
  # Convertendo nome da coluna categoria em símbolo para uso no dplyr
  category_sym <- rlang::sym(category_col)

  # Tratando valores NA
  if (na.omit) {
    data <- data %>% filter(!is.na(!!category_sym))
  } else {
    data <- data %>% mutate(!!category_sym := ifelse(is.na(!!category_sym), "NA", !!category_sym))
  }

  # Preparando os dados
  data_grouped <- data %>%
    dplyr::count(!!category_sym) %>%
    mutate(percent = n / sum(n) * 100)
<<<<<<< HEAD

=======
  
>>>>>>> 3a96898237d6227024f8935fb1bff09572a5d8e2
  if (!is.null(category_order)) {
    data_grouped <- data_grouped %>%
      mutate(!!category_sym := factor(!!category_sym, levels = category_order)) %>% arrange(!!category_sym)
  } else{
    data_grouped <- data_grouped %>%
      mutate(!!category_sym := factor(!!category_sym)) 
  }
<<<<<<< HEAD


if (percent){
  hc <- highchart() %>%
    hc_chart(lang = list(decimalPoint = ',', thousandsSep = '.')) %>%
    hc_title(text=titulo,
             style = list(fontSize = "16px")) %>%
=======
if (percent){
  hc <- highchart() %>%
    hc_chart(lang = list(decimalPoint = ',', thousandsSep = '.')) %>%
    hc_title(text=titulo) %>%
>>>>>>> 3a96898237d6227024f8935fb1bff09572a5d8e2
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
  return(hc)
<<<<<<< HEAD

=======
  
>>>>>>> 3a96898237d6227024f8935fb1bff09572a5d8e2
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


