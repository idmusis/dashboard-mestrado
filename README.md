
<h1 align="center">dashboard-mestrado</h1>

<p align="center">
	<img src="https://img.shields.io/badge/R-276DC3.svg?style=default&logo=R&logoColor=white" alt="R">
</p>

<br>

Dashboard interativo, contendo uma análise visual dos fatores que influenciam a permanência e a evasão nos cursos de pós-graduação stricto sensu da UFMT.

O dashboard está disponível online no seguinte link: [https://musis.shinyapps.io/dashboard-evasao/](https://musis.shinyapps.io/dashboard-evasao/)


## Funcionalidades

O dashboard oferece as seguintes páginas e funcionalidades:

### 1. Páginas "Sobre o dashboard":
   - **Informações gerais**: Informações sobre o contexto, objetivo e estrutura do projeto.
   - **Metodologia**: Detalhamento da análise de regressão logística e dos critérios usados para modelagem.

### 2. Página de Visão Geral:
Página com estatísticas descritivas fazendo um resumo dos dados coletados.
   - **Filtros**: Os dados podem ser filtrados por categorias como nível acadêmico, câmpus, identidade de gênero, raça/etnia e área de conhecimento.
   - **Visualização de Gráficos**: 
     - **Comparação**: Gráficos de barras agrupadas que comparam os estudantes que evadiram e os que concluíram.
     - **Total**: Gráficos de barras que mostram o total de respondentes.

### 3. Página de Análise de Regressão:
Página para criação e visualização de modelos de regressão logística personalizáveis, com base nos filtros e critérios selecionados. Um modelo pré-processado é exibido inicialmente.
   - **Filtros**: Permite filtrar os respondentes incluídos na análise de regressão com base em diversas variáveis.
   - **Modelos de Regressão Stepwise**: Três métodos de seleção de variáveis:
     - **Seleção direta**: Adiciona as variáveis mais relevantes uma a uma.
     - **Eliminação reversa**: Remove as variáveis menos significativas.
     - **Seleção bidirecional**: Combina os dois métodos anteriores.
   - **Critérios de Seleção do Modelo**:
     - **AIC** (Critério de Informação de Akaike)
     - **BIC** (Critério de Informação Bayesiano)
   - **Validação Bootstrap**: Personalize a validação bootstrap ajustando a proporção do conjunto de treino e o número de repetições.
   - **Resultados Exibidos**:
     - **Coeficientes do modelo**: Impacto das variáveis na evasão.
     - **Critérios de Informação**: AIC, BIC, log-likelihood, grau de liberdade e deviance.
     - **Gráficos**: Tornado, Curva ROC, Resíduos vs Ajustes, Q-Q Residual, Localização-Escala, Distância de Cook.
     - **Bootstrap**: Resultados da validação bootstrap, incluindo coeficientes e odds ratio.


---

## Base de Dados

As informações são baseadas nas respostas de ex-discentes dos cursos de pós-graduação da UFMT, coletadas entre 06 e 19 de junho de 2024. O questionário foi elaborado com base nas contribuições teóricas de Vincent Tinto e aplicado por e-mail.

---

##  Instalação

O dashboard está disponível online no [shinyapps](https://musis.shinyapps.io/dashboard-evasao/). Se desejar instalar localmente, siga estes passos:

Clone o repositório:
 ```bash
git clone https://github.com/musiss/dashboard-mestrado.git
 ```
Navegue até o diretório do projeto:
```bash
cd dashboard-mestrado
```
Instale as dependências:
```R
install.packages(c("arrow", "bs4Dash", "data.table", "dplyr", "DT", "fresh", "highcharter", 
                   "pROC", "rlang", "shiny", "shinyWidgets", "stringr", "tidyr", 
                   "tidytable", "waiter", "caret", "jsonlite", "purrr"))
```

Execute o código:
```R
shiny::runApp("app.R")
```

---

## Licença

Este projeto está licenciado sob a [Licença MIT](https://github.com/musiss/dashboard-mestrado/blob/main/LICENSE).
