#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
install.packages("rattle.data")

library(shiny)
library(ggplot2)
#library(rattle.data)
#data(wine)
library(shiny)
library(ggplot2)

# Carregar e preparar os dados
filename <- "./wine.csv"
df <- read.csv(filename, sep=",")
wine <- df[, c("Alcohol", "Malic_Acid", "Total_Phenols", "Flavanoids", "Color_Intensity")]

# Interface do Usuário (UI)
ui <- fluidPage(
  titlePanel("Distribuição de Frequência - Análise de Vinhos"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variavel", "Selecione a variável:",
                  choices = names(wine)[sapply(wine, is.numeric)],
                  selected = "Alcohol"),
      
      radioButtons("tipo_grafico", "Tipo de gráfico:",
                   choices = c("Histograma" = "hist",
                               "Densidade" = "dens",
                               "Boxplot" = "box",
                               "Violino" = "violin"),
                   selected = "hist"),
      
      conditionalPanel(
        condition = "input.tipo_grafico == 'hist'",
        sliderInput("bins", "Número de bins:",
                    min = 5, max = 30, value = 15)
      ),
      
      selectInput("cor", "Cor do gráfico:",
                  choices = c("Azul" = "#1E90FF", 
                              "Vermelho" = "#FF6B6B", 
                              "Verde" = "#4CAF50",
                              "Roxo" = "#9C27B0",
                              "Laranja" = "#FFA500"),
                  selected = "#1E90FF"),
      
      uiOutput("range_slider"),
      
      h4("Limites dos Eixos:"),
      uiOutput("x_limits"),
      uiOutput("y_limits"),
      
      checkboxInput("show_mean", "Mostrar média", TRUE),
      checkboxInput("show_median", "Mostrar mediana", FALSE)
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("estatisticas")
    )
  )
)

# Lógica do Servidor (Server)
server <- function(input, output) {
  
  output$range_slider <- renderUI({
    var_data <- wine[[input$variavel]]
    sliderInput("range", "Filtrar faixa de valores:",
                min = floor(min(var_data, na.rm = TRUE)),
                max = ceiling(max(var_data, na.rm = TRUE)),
                value = range(var_data, na.rm = TRUE))
  })
  
  output$x_limits <- renderUI({
    var_data <- wine[[input$variavel]]
    sliderInput("x_lim", paste("Eixo X (", input$variavel, "):"),
                min = floor(min(var_data, na.rm = TRUE)),
                max = ceiling(max(var_data, na.rm = TRUE)),
                value = range(var_data, na.rm = TRUE))
  })
  
  output$y_limits <- renderUI({
    if(input$tipo_grafico %in% c("hist", "dens")) {
      if(input$tipo_grafico == "hist") {
        max_y <- max(hist(wine[[input$variavel]], breaks = input$bins, plot = FALSE)$counts, na.rm = TRUE)
      } else {
        max_y <- max(density(wine[[input$variavel]], na.rm = TRUE)$y, na.rm = TRUE)
      }
      
      sliderInput("y_lim", "Eixo Y (Frequência/Densidade):",
                  min = 0,
                  max = max_y * 1.1,
                  value = c(0, max_y))
    }
  })
  
  output$distPlot <- renderPlot({
    req(input$range, input$x_lim)
    
    dados <- wine[wine[[input$variavel]] >= input$range[1] & 
                    wine[[input$variavel]] <= input$range[2], ]
    
    p <- ggplot(dados, aes(x = .data[[input$variavel]]))
    
    if (input$tipo_grafico == "hist") {
      p <- p + geom_histogram(bins = input$bins, 
                              fill = input$cor, 
                              color = "white",
                              aes(y = after_stat(count)))
    } else if (input$tipo_grafico == "dens") {
      p <- p + geom_density(fill = input$cor, alpha = 0.6,
                            aes(y = after_stat(density)))
    } else if (input$tipo_grafico == "box") {
      p <- p + geom_boxplot(fill = input$cor, width = 0.2)
    } else if (input$tipo_grafico == "violin") {
      p <- p + geom_violin(fill = input$cor, alpha = 0.6, 
                           trim = FALSE, 
                           scale = "width",
                           aes(y = .data[[input$variavel]]))  # Correção para o violino
    }
    
    if (input$show_mean) {
      p <- p + geom_vline(aes(xintercept = mean(.data[[input$variavel]], na.rm = TRUE)),
                          color = "red", linetype = "dashed", size = 1)
    }
    
    if (input$show_median) {
      p <- p + geom_vline(aes(xintercept = median(.data[[input$variavel]], na.rm = TRUE)),
                          color = "blue", linetype = "dashed", size = 1)
    }
    
    if(input$tipo_grafico %in% c("hist", "dens") && !is.null(input$y_lim)) {
      p <- p + coord_cartesian(xlim = input$x_lim, ylim = input$y_lim)
    } else {
      p <- p + coord_cartesian(xlim = input$x_lim)
    }
    
    p <- p +
      labs(title = paste("Distribuição de", input$variavel),
           x = input$variavel,
           y = ifelse(input$tipo_grafico %in% c("hist"), 
                      "Frequência",
                      ifelse(input$tipo_grafico == "dens",
                             "Densidade", ""))) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
    
    print(p)
  })
  
  output$estatisticas <- renderPrint({
    dados <- wine[wine[[input$variavel]] >= input$range[1] & 
                    wine[[input$variavel]] <= input$range[2], ]
    
    var_data <- dados[[input$variavel]]
    
    cat("Estatísticas Descritivas:\n")
    cat("----------------------------\n")
    cat("Média:", round(mean(var_data, na.rm = TRUE), 2), "\n")
    cat("Mediana:", round(median(var_data, na.rm = TRUE), 2), "\n")
    cat("Desvio Padrão:", round(sd(var_data, na.rm = TRUE), 2), "\n")
    cat("Mínimo:", round(min(var_data, na.rm = TRUE), 2), "\n")
    cat("Máximo:", round(max(var_data, na.rm = TRUE), 2), "\n")
    cat("1º Quartil:", round(quantile(var_data, 0.25, na.rm = TRUE), 2), "\n")
    cat("3º Quartil:", round(quantile(var_data, 0.75, na.rm = TRUE), 2), "\n")
  })
}

shinyApp(ui = ui, server = server)