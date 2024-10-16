library(highcharter)
botoes_menu <- list(
  "downloadPNG",
  "downloadPDF",
  "separator",
  "downloadCSV",
  "downloadXLS",
  "viewData"
)

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

################# gráficos highcharter ----

# Funções que dependiam de inputs do app foram movidas para app.R


# Atualiza as cores com base no filtro selecionado
cores_botoes <- function(input, total = c("#6886C3", "#24427F"),
                         concluinte = c("#B7DBF4", "#456882"),
                         evadido = c("#97979A", "#252528"),
                         outro = c("#6886C3", "#24427F")) {
  input <- as.character(input)
  switch(input,
    "total" = total,
    "0" = concluinte,
    "1" = evadido,
    outro
  ) # Cor padrão caso nenhuma correspondência seja encontrada
}

plot_pie <- function(data = data, category_col, category_order = NULL, category_labels = NULL, titulo = category_col,
                     distance = "0") {
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
  data <- data %>%
    select(category_col) %>%
    na.omit()
  # Preparando os dados
  data_grouped <- data %>%
    dplyr::count(!!category_sym) %>%
    mutate(percent = n / sum(n) * 100)

  # Criando o gráfico de pizza
  hc <- highchart() %>%
    hc_chart(lang = list(decimalPoint = ",", thousandsSep = "."), type = "pie") %>%
    hc_title(
      text = titulo,
      style = list(fontSize = "16px")
    ) %>%
    hc_add_series(data_grouped, name = "Respondentes", type = "pie", hcaes(name = !!category_sym, y = n)) %>%
    hc_tooltip(
      shared = FALSE, useHTML = TRUE,
      headerFormat = '<span style="font-size: 0.8em">{point.key}</span><br/>',
      pointFormat = '<span style="color:{point.color}">\u25AA</span> Frequência (%): <b>{point.y}</b> ({point.percent:.1f}%)<br/>'
    ) %>%
    hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE, format = "<b>{point.name}</b>: {point.percentage:.1f} %", distance = distance)))

  # # Configurando rótulos de categoria, se fornecidos
  # if (!is.null(category_labels)) {
  #   hc <- hc %>% hc_xAxis(categories = category_labels)
  # }
  hc <- hc %>%
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = botoes_menu
        )
      )
    )

  return(hc)
}

# bs4Dash ----------------------

filtro_box <- function(..., title = NULL, footer = NULL, status = NULL, solidHeader = FALSE,
                       background = NULL, width = 6, height = NULL, collapsible = TRUE,
                       collapsed = TRUE, closable = FALSE, maximizable = FALSE,
                       icon = NULL, gradient = FALSE, boxToolSize = "sm", elevation = 0,
                       headerBorder = TRUE, label = NULL, dropdownMenu = NULL,
                       sidebar = NULL, id = NULL, noBorder = TRUE, scrollable = FALSE, smalltext = TRUE) {
  ######## Ajustado de bs4Dash::box()
  if (is.null(status)) {
    solidHeader <- TRUE
  }
  validateBoxProps(
    title = title, label = label, sidebar = sidebar,
    dropdownMenu = dropdownMenu, status = status, gradient = gradient,
    collapsible = collapsible, collapsed = collapsed, solidHeader = solidHeader,
    background = background, elevation = elevation, width = width
  )
  props <- dropNulls(list(
    title = if (!is.null(title)) {
      if (inherits(title, "list")) {
        unlist(dropNulls(lapply(title, function(e) {
          if (inherits(e, "shiny.tag.list") || inherits(
            e,
            "shiny.tag"
          )) {
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
    closable = closable, maximizable = maximizable, gradient = gradient
  ))

  cardCl <- setBoxClass(
    status, solidHeader, collapsible,
    collapsed, elevation, gradient, background, sidebar
  )

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
  cardToolTag <- shiny::tagAppendChildren(
    cardToolTag, label,
    createBoxTools(
      collapsible, collapsed, closable, maximizable,
      sidebar, dropdownMenu, boxToolSize, status, background,
      solidHeader
    )
  )
  if (is.null(title) && (maximizable || closable || collapsible ||
    !is.null(dropdownMenu) || !is.null(sidebar) || !is.null(label))) {
    title <- "‌"
  }
  headerTag <- shiny::tags$div(class = if (headerBorder) {
    "card-header"
  } else {
    "card-header border-0"
  }, shiny::tags$h3(
    class = "card-title",
    icon, title
  ))
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
  cardTag <- shiny::tagAppendChildren(
    cardTag, headerTag,
    bodyTag, footerTag
  )
  shiny::tags$div(class = if (!is.null(width)) {
    paste0("col-sm-", width)
  }, cardTag, shiny::tags$script(
    type = "application/json",
    `data-for` = id, jsonlite::toJSON(
      x = props, auto_unbox = TRUE,
      json_verbatim = TRUE
    )
  ))
}

environment(filtro_box) <- asNamespace("bs4Dash")


valuebox2 <- function(value, subtitle, icon = NULL, color = NULL, width = 3,
                      href = NULL, footer = NULL, gradient = FALSE, elevation = NULL) {
  ###### Ajustado de bs4Dash::valuebox()
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
      valueBoxCl <- paste0(
        valueBoxCl, " bg-gradient-",
        color
      )
    } else {
      valueBoxCl <- paste0(valueBoxCl, " bg-", color)
    }
  }
  if (!is.null(elevation)) {
    valueBoxCl <- paste0(valueBoxCl, " elevation-", elevation)
  }
  innerTag <- shiny::tags$div(class = "inner", shiny::tags$strong(value), shiny::tags$p(
    class = "small-box-subtitle",
    subtitle
  ))
  iconTag <- if (!is.null(icon)) {
    shiny::tags$div(class = "icon", icon)
  } else {
    NULL
  }
  footerTag <- if (!is.null(footer)) {
    shiny::tags$div(class = "small-box-footer", footer)
  } else if (!is.null(href)) {
    shiny::tags$a(
      href = href, target = "_blank", class = "small-box-footer",
      "More info", shiny::icon("circle-arrow-right")
    )
  } else {
    NULL # This removes the footer entirely if both are NULL
  }

  # If there's no footer, add padding-bottom to simulate the space it would have taken
  valueBoxTag <- shiny::tags$div(
    class = valueBoxCl,
    style = if (is.null(footerTag)) "padding-bottom: 30px;"
  )
  valueBoxTag <- shiny::tagAppendChildren(valueBoxTag, innerTag, iconTag)

  # Append the footer only if it was created (i.e., not NULL)
  if (!is.null(footerTag)) {
    valueBoxTag <- shiny::tagAppendChild(valueBoxTag, footerTag)
  }

  shiny::tags$div(class = if (!is.null(width)) {
    paste0("col-sm-", width)
  }, valueBoxTag)
}

environment(valuebox2) <- asNamespace("bs4Dash")


# Tabela -------------------------------------

tabela_dt <- function(tabela, pesquisa = TRUE) {
  dom <- if (pesquisa) {
    "frtB" # Inclui a barra de pesquisa
  } else {
    "rtB" # Remove a barra de pesquisa
  }
  DT::datatable(tabela,
    rownames = FALSE,
    style = "bootstrap4",
    extensions = "Buttons",
    options = list(
      dom = dom,
      buttons = list(
        list(
          extend = "copy",
          text = "Copiar"
        ),
        list(
          extend = "csv",
          text = "CSV"
        ),
        list(
          extend = "excel",
          text = "Excel"
        ),
        list(
          extend = "pdf",
          text = "PDF"
        ),
        list(
          extend = "print",
          text = "Imprimir"
        )
      ),
      language = list(
        url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json"
      )
    )
  )
}

# Regressão  ----------------------------------------------------------

plot_lm_highchart <- function(x, which = c(1L:3L, 5L), id.n = 3,
                              caption = list(
                                "Residuals vs Fitted", "Normal Q-Q",
                                "Scale-Location", "Cook's distance"
                              ),
                              marker = list(
                                symbol = "circle", # Shape of the point
                                radius = 2, # Size of the point (smaller)
                                fillColor = "transparent", # Hollow point (no fill)
                                lineColor = "#24427F", # Border color of the point
                                lineWidth = 2 # Thickness of the border
                              ),
                              ...) {
  ############# Ajustado de stats::plot.lm()

  dropInf <- function(x, h) {
    if (any(isInf <- h >= 1)) {
      warning(
        gettextf(
          "not plotting observations with leverage one:\n  %s",
          paste(which(isInf), collapse = ", ")
        ),
        call. = FALSE,
        domain = NA
      )
      x[isInf] <- NaN
    }
    x
  }


  if (!inherits(x, "lm")) {
    stop("use only with \"lm\" objects")
  }
  if (!is.numeric(which) || any(which < 1) || any(which > 6)) {
    stop("'which' must be in 1:6")
  }
  if ((isGlm <- inherits(x, "glm"))) {
    binomialLike <- family(x)$family == "binomial"
  }
  show <- rep(FALSE, 6)
  show[which] <- TRUE
  r <- if (isGlm) {
    residuals(x, type = "pearson")
  } else {
    residuals(x)
  }
  yh <- predict(x)
  w <- weights(x)
  labels.id <- names(residuals(x))
  if (!is.null(w)) {
    wind <- w != 0
    r <- r[wind]
    yh <- yh[wind]
    w <- w[wind]
    labels.id <- labels.id[wind]
  }
  n <- length(r)

  add.smooth <- getOption("add.smooth")
  panel <- if (add.smooth) {
    function(x, y, ...) {
      panel.smooth(x,
        y,
        iter = iter.smooth, ...
      )
    }
  } else {
    points
  }
  iter.smooth <- if (isGlm) 0 else 3

  if (any(show[2L:6L])) {
    s <- if (inherits(x, "rlm")) {
      x$s
    } else if (isGlm) {
      sqrt(summary(x)$dispersion)
    } else {
      sqrt(deviance(x) / df.residual(x))
    }
    hii <- (infl <- influence(x, do.coef = FALSE))$hat
    if (any(show[4L:6L])) {
      cook <- cooks.distance(x, infl)
    }
  }
  if (any(show[c(2L, 3L, 5L)])) {
    ylab5 <- ylab3 <- if (isGlm) {
      "Res. padronizados de Pearson"
    } else {
      "Resíduos padronizados"
    }
    ylab2 <- if (isGlm) {
      "Res. de deviância padronizados"
    } else {
      ylab3
    }
    rs <- dropInf(if (isGlm) {
      rstandard(x, type = "pearson")
    } else {
      (if (is.null(w)) {
        r
      } else {
        sqrt(w) * r
      }) / (s * sqrt(1 - hii))
    }, hii)
    rds <- if (isGlm) {
      suppressWarnings(dropInf(
        rstandard(x, type = "deviance"),
        hii
      ))
    } else {
      rs
    }
  }
  if (any(show[5L:6L])) {
    r.hat <- range(hii, na.rm = TRUE)
    isConst.hat <- all(r.hat == 0) || diff(r.hat) < 1e-10 *
      mean(hii, na.rm = TRUE)
  }
  if (any(show[c(1L, 3L)])) {
    l.fit <- if (isGlm) {
      "Valores preditos"
    } else {
      "Valores ajustados"
    }
  }
  if (is.null(id.n)) {
    id.n <- 0L
  } else {
    id.n <- as.integer(id.n)
    if (id.n < 0L || id.n > n) {
      stop(gettextf("'id.n' must be in {1,..,%d}", n),
        domain = NA
      )
    }
  }
  if (id.n > 0L) {
    if (is.null(labels.id)) {
      labels.id <- paste(1L:n)
    }
    iid <- 1L:id.n
    show.r <- sort.list(abs(r), decreasing = TRUE)[iid]
    if (any(show[2L:3L])) {
      show.rs <- sort.list(abs(rs), decreasing = TRUE)[iid]
      show.rds <- sort.list(abs(rds), decreasing = TRUE)[iid]
    }
  }

  # Gráfico 1: Residuals vs Fitted
  if (show[1L]) {
    ylim <- range(r, na.rm = TRUE)
    ylim <- extendrange(r = ylim, f = 0.08) # Ajustando os limites do eixo Y

    ylab1 <- if (isGlm) {
      "Resíduos de Pearson"
    } else {
      "Resíduos"
    }


    residuals_vs_fitted <- highchart() %>%
      hc_add_series(
        data = data.frame(x = yh, y = r, id = seq_along(r)),
        type = "scatter", name = "Resíduos", color = marker$lineColor, marker = marker, showInLegend = FALSE
      ) %>%
      hc_xAxis(title = list(text = l.fit)) %>%
      hc_title(text = "Resíduos vs Ajustes") %>%
      hc_yAxis(min = ylim[1], max = ylim[2], title = list(text = ylab1))

    if (id.n > 0) {
      y.id <- r[show.r]
      residuals_vs_fitted <- residuals_vs_fitted %>%
        hc_add_series(
          data = data.frame(x = yh[show.r], y = y.id, id = show.r),
          type = "scatter",
          name = "Resíduos",
          color = marker$lineColor,
          marker = marker,
          dataLabels = list(
            enabled = TRUE,
            format = "{point.id}",
            allowOverlap = TRUE,
            style = list(fontSize = "10px"),
            distance = 30,
            align = "right"
          )
        )
    }


    smooth_data <- lowess(yh, r, iter = iter.smooth)
    smooth_fit <- data.frame(x = smooth_data$x, y = smooth_data$y)


    residuals_vs_fitted <- residuals_vs_fitted %>%
      hc_add_series(
        data = list_parse2(smooth_fit),
        type = "line", color = "#EC7272", name = "Linha suavizada",
        lineWidth = 1
      ) %>%
      hc_tooltip(
        formatter = JS("function () {
                  var tooltip;
                  if (this.series.name === 'Resíduos') {
                    tooltip = '<span style=\"font-size: 0.8em\">' +
                              '<span style=\"color:' + this.point.color + '\">\u25AA </span>' +
                              this.point.id + '</span><br>' +
                              'X: <b>' + Highcharts.numberFormat(this.x, 1, ',') + '</b><br>' +
                              'Y: <b>' + Highcharts.numberFormat(this.y, 1, ',') + '</b>';
                  } else {
                    // Formato para outras séries
                    tooltip = '<span style=\"font-size: 0.8em\">' +
                              '<span style=\"color:' + this.point.color + '\">\u25AA</span> ' +
                              this.series.name + '</span> <br>' +
                              'X: <b>' + Highcharts.numberFormat(this.x, 1, ',') + '</b><br>' +
                              'Y: <b>' + Highcharts.numberFormat(this.y, 1, ',') + '</b>';
                  }
                  return tooltip;
                }")
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = botoes_menu
          )
        )
      )

    return(residuals_vs_fitted)
  }

  # Gráfico 2: Normal Q-Q
  if (show[2L]) {
    qhalfnorm <- function(p) qnorm((p + 1) / 2)
    qqhalfnorm <- function(y,
                           ...) {
      if (has.na <- any(ina <- is.na(y))) {
        yN <- y
        y <- y[!ina]
      }
      if (0 == (n <- length(y))) {
        stop("y is empty or has only NAs")
      }
      x <- qhalfnorm(ppoints(n))[order(order(y))]
      if (has.na) {
        y <- x
        x <- rep.int(NA_real_, length(ina))
        x[!ina] <- y
        y <- yN
      }
      return(list(x = x, y = y))
    }

    absr <- abs(rds)
    ylim <- c(0, max(absr, na.rm = TRUE) * 1.075)

    qq <- qqhalfnorm(absr)

    probs <- c(0.25, 0.75)
    y <- quantile(qq$y, probs, names = FALSE)
    x <- quantile(qq$x, probs, names = FALSE)
    slope <- diff(y) / diff(x)
    intercept <- y[1] - slope * x[1]

    yl <- paste0("|", ylab2, "|")

    qqplot <- highchart() %>%
      hc_add_series(
        data = data.frame(x = qq$x, y = intercept + slope * qq$x),
        type = "line", color = "gray", name = "Linha de referência", dashStyle = "Dash", lineWidth = 1
      ) %>%
      hc_add_series(
        data = data.frame(x = qq$x, y = qq$y, id = seq_along(rds)),
        type = "scatter", name = "Resíduos", color = marker$lineColor, marker = marker
      ) %>%
      hc_yAxis(min = ylim[1], max = ylim[2], title = list(text = yl)) %>%
      hc_xAxis(title = list(text = "Quantis teóricos")) %>%
      hc_title(text = "Q-Q Residual") %>%
      hc_tooltip(
        formatter = JS("function () {
                  var tooltip;
                  if (this.series.name === 'Resíduos') {
                    tooltip = '<span style=\"font-size: 0.8em\">' +
                              '<span style=\"color:' + this.point.color + '\">\u25AA </span>' +
                              this.point.id + '</span><br>' +
                              'X: <b>' + Highcharts.numberFormat(this.x, 1, ',') + '</b><br>' +
                              'Y: <b>' + Highcharts.numberFormat(this.y, 1, ',') + '</b>';
                  } else {
                    // Formato para outras séries
                    tooltip = '<span style=\"font-size: 0.8em\">' +
                              '<span style=\"color:' + this.point.color + '\">\u25AA</span> ' +
                              this.series.name + '</span> <br>' +
                              'X: <b>' + Highcharts.numberFormat(this.x, 1, ',') + '</b><br>' +
                              'Y: <b>' + Highcharts.numberFormat(this.y, 1, ',') + '</b>';
                  }
                  return tooltip;
                }")
      ) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = botoes_menu
          )
        )
      ) %>%
      hc_legend(enabled = FALSE)

    if (id.n > 0) {
      qqplot <- qqplot %>%
        hc_add_series(
          data = data.frame(x = qq$x[show.rds], y = qq$y[show.rds], id = show.rds),
          type = "scatter",
          name = "Resíduos",
          color = marker$lineColor,
          marker = marker,
          dataLabels = list(
            enabled = TRUE,
            format = "{point.id}",
            allowOverlap = TRUE,
            style = list(fontSize = "10px"),
            distance = 30,
            align = "right"
          )
        )
    }

    return(qqplot)
  }

  # Gráfico 3: Scale-Location Plot
  if (show[3L]) {
    sqrtabsr <- sqrt(abs(rs)) # Raiz quadrada do valor absoluto dos resíduos padronizados
    ylim <- c(0, max(sqrtabsr, na.rm = TRUE))
    yl <- paste0("√|", ylab3, "|")
    yhn0 <- if (is.null(w)) {
      yh
    } else {
      yh[w != 0]
    }

    scale_location <- highchart() %>%
      hc_add_series(
        data = data.frame(x = yhn0, y = sqrtabsr, id = seq_along(rs)),
        type = "scatter", name = "Resíduos", color = marker$lineColor, marker = marker
      ) %>%
      hc_yAxis(min = ylim[1], max = ylim[2], title = list(text = yl)) %>%
      hc_xAxis(title = list(text = l.fit)) %>%
      hc_title(text = "Localização-escala")



    if (id.n > 0) {
      scale_location <- scale_location %>%
        hc_add_series(
          data = data.frame(x = yhn0[show.rs], y = sqrtabsr[show.rs], id = show.rs),
          type = "scatter",
          name = "Resíduos",
          color = marker$lineColor,
          marker = marker,
          dataLabels = list(
            enabled = TRUE,
            format = "{point.id}",
            allowOverlap = TRUE,
            style = list(fontSize = "10px"),
            distance = 30,
            align = "right"
          )
        )
    }

    smooth_data <- lowess(yhn0, sqrtabsr, iter = iter.smooth)
    smooth_fit <- data.frame(x = smooth_data$x, y = smooth_data$y)

    scale_location <- scale_location %>%
      hc_add_series(
        data = list_parse2(smooth_fit),
        type = "line", color = "#EC7272", name = "Linha suavizada",
        lineWidth = 1
      ) %>%
      hc_tooltip(
        formatter = JS("function () {
                  var tooltip;
                  if (this.series.name === 'Resíduos') {
                    tooltip = '<span style=\"font-size: 0.8em\">' +
                              '<span style=\"color:' + this.point.color + '\">\u25AA </span>' +
                              this.point.id + '</span><br>' +
                              'X: <b>' + Highcharts.numberFormat(this.x, 1, ',') + '</b><br>' +
                              'Y: <b>' + Highcharts.numberFormat(this.y, 1, ',') + '</b>';
                  } else {
                    // Formato para outras séries
                    tooltip = '<span style=\"font-size: 0.8em\">' +
                              '<span style=\"color:' + this.point.color + '\">\u25AA</span> ' +
                              this.series.name + '</span> <br>' +
                              'X: <b>' + Highcharts.numberFormat(this.x, 1, ',') + '</b><br>' +
                              'Y: <b>' + Highcharts.numberFormat(this.y, 1, ',') + '</b>';
                  }
                  return tooltip;
                }")
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = botoes_menu
          )
        )
      )



    return(scale_location)
  }

  # Gráfico 4: Cook's Distance
  if (show[4L]) {
    if (id.n > 0) {
      show.r <- order(-cook)[iid]
      ymx <- cook[show.r[1L]] * 1.075
    } else {
      ymx <- max(cook, na.rm = TRUE)
    }
    ymx <- unname(ymx)

    cooks_distance <- highchart() %>%
      hc_add_series(
        data = data.frame(
          x = as.numeric(names(cook)), # Índice das observações (Obs. number)
          y = unname(as.numeric(cook)),
          name = "Distâncias",
          id = seq_along(r)
        ),
        type = "column", color = marker$lineColor
      ) %>%
      hc_yAxis(min = 0, max = ymx, title = list(text = "Distância de Cook")) %>%
      hc_xAxis(title = list(text = "Observação")) %>%
      hc_legend(enabled = FALSE) %>%
      hc_tooltip(
        formatter = JS("function () {
                  var tooltip;
                    tooltip = '<span style=\"font-size: 0.8em\">' +
                              '<span style=\"color:' + this.point.color + '\">\u25AA </span>' +
                              this.point.id + '</span><br>' +
                              'Cook: <b>' + Highcharts.numberFormat(this.y, 4, ',') + '</b><br>';
                  return tooltip;
                }")
      ) %>%
      hc_title(text = "Distância de Cook")

    # Adicionar labels ou anotações com IDs
    if (id.n > 0) {
      cooks_distance <- cooks_distance %>%
        hc_add_series(
          data = data.frame(x = show.r, y = cook[show.r], id = show.r),
          type = "column",
          name = "Pontos Destacados",
          color = marker$lineColor,
          dataLabels = list(
            enabled = TRUE,
            format = "{point.id}", # Exibe o ID
            allowOverlap = FALSE,
            style = list(fontSize = "10px"),
            distance = 15
          )
        )
    }

    cooks_distance <- cooks_distance %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = botoes_menu
          )
        )
      )
    return(cooks_distance)
  }
}
environment(plot_lm_highchart) <- asNamespace("stats")

## Cálculos regressão ------

calcular_ic <- function(df) {
  calcular_valores <- function(coluna) {
    coluna <- na.omit(coluna)
    median <- median(coluna)
    q2.5 <- quantile(coluna, 0.025)
    q97.5 <- quantile(coluna, 0.975)
    n_validos <- length(coluna)
    return(data.frame("Mediana" = median, "2,5%" = q2.5, "97,5%" = q97.5, "N válidos" = n_validos))
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
  modelo_step <- glm(fstep, data = dados_treino, family = "binomial")

  # Avaliação do modelo no conjunto de teste
  predicoes <- predict(modelo_step, newdata = dados_teste, type = "response")
  pred_class <- ifelse(predicoes > 0.5, 1, 0)
  conf_matrix <- caret::confusionMatrix(factor(pred_class), factor(dados_teste$evadido))

  # Coletar coeficientes e odds ratios
  coef <- modelo_step$coefficients
  odds_ratios <- exp(coef)

  # Métricas de avaliação
  metrics <- data.frame(
    Acurácia = conf_matrix$overall["Accuracy"],
    Sensibilidade = conf_matrix$byClass["Sensitivity"],
    Especificidade = conf_matrix$byClass["Specificity"]
  )


  list(
    metrics = metrics,
    coef = coef,
    odds_ratios = odds_ratios
  )
}
