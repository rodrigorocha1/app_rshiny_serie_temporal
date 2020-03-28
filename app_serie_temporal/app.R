
library(forecast)
library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Sistema de Análise e Previsão de Séries Temporais"),
    fluidRow(
        column(4, fileInput("arquivo", "Escolha o arquivo: ", multiple = F, accept = c(".csv")),
               helpText("Observação: O arquivo deve conter apenas uma coluna, sem nome e cabeçalho"),
        ),
        column(4, dateRangeInput("datas", label = "Período da Série", format = "mm/yyyy", language = "pt", start = "01/01/2000", end = "12/31/2013", startview = "year", separator = " até "),
               helpText("Observação: para definir mês e ano, selecione um dia qualquer")
        ),
        column(4,
               numericInput("PeriodoPrevisao", "Informe quantos meses quer prever: ", 12, min = 1, max = 48),
               actionButton("Processar", "Processar")
        )
    ),
    fluidRow(
        column(6,
               plotOutput("GrafSerie")
        ),
        column(6,
               plotOutput("GrafHist")
        )
    ),
    fluidRow(
        column(6,
               plotOutput("grafBox")
        ),
        column(6,
               plotOutput("GrafDec")
        )
    ),
    hr(),
    fluidRow(
        column(6, 
               plotOutput("GrafPrev")
        ),
        column(2,
               h1(textOutput("llower")),
               tableOutput("lower")
        ),
        column(2,
               h1(textOutput("lmean")),
               tableOutput("mean")
        ),
        column(2,
               h1(textOutput("lupper")),
               tableOutput("upper")
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$Processar, {
        file1 = input$arquivo
        data = read.csv(file1$datapath, header = F)
        anoinic = as.integer(substr(input$datas[1], 1, 4))
        
        mesinic = as.integer(substr(input$datas[1], 6, 7))
        anofim = as.integer(substr(input$datas[2], 1, 4))
        mesfim = as.integer(substr(input$datas[2], 6, 7))
        
        
        data = ts(data, start = c(anoinic, mesinic), end = c(anofim, mesfim), frequency = 12)
        
        
        output$GrafSerie = renderPlot({autoplot(data, main = "Série Temporal")})
        output$GrafHist = renderPlot({ hist(data, main = "Histograma")})
        output$GrafBox = renderPlot({boxplot(data, main = "boxplot")})
        
        dec = decompose(data)
        output$GrafDec = renderPlot({autoplot(dec, main = "Decomposição")})
        
        
        modelo = auto.arima(data)
        valr = input$PeriodoPrevisao
        
        previsao = forecast(modelo, h = valr)
        
        output$lower = renderTable({previsao$lower})
        output$mean = renderTable({previsao$mean})
        output$upper = renderTable({previsao$upper})
        
        output$llower = renderText({"Lower"})
        output$lmean = renderText({"Mean"})
        output$lupper = renderText({"Upper"})
        
        output$GrafPrev = renderPlot({autoplot(previsao, main = "Previsão da Série")})
        
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
