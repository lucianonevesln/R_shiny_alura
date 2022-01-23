#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# bibliotecas necessarias
library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)

# importando dados externos
dados <- fread('dados_limpos.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Hello Shiny"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # criando um grupo de selecao de caixa
            checkboxGroupInput(inputId = 'select_UF', # identificacao para o output 
                               label = 'Estados:', # rotulo que aparececar para o usuario
                               choices = c('Todos', unique(dados$UF)), selected = 'Todos'), # opcoes que aparecerao para o usuario a partir de uma coluna da base de dados
            
            # criando um range de datas para selecao do usuario
            dateRangeInput(inputId = 'data_abertura', # determina o id que sera usado pela app para identificacao
                           label = 'Data Abertura:', # rotulo de identificacao que aparecera para o usuario
                           format = 'dd-mm-yyyy', # altera o formato de apresentacao padrao para o formato brasileiro
                           start = min(as.Date(dados$DataAbertura)), # data de inicio que aparecera para o usuario
                           end = max(as.Date(dados$DataAbertura))) # data final que aparecera para o usuario
        ),

        # Show a plot of the generated distribution
        mainPanel(
           # verbatimTextOutput(outputId = 'listaNumeros'), # texto para renderizacao
           # verbatimTextOutput(outputId = 'listaUF'), # texto para renderizacao
           plotlyOutput(outputId = 'data'), # grafico com interatividade
           verbatimTextOutput(outputId = 'descData'), # texto para renderizacao
           plotlyOutput(outputId = 'uf'), # grafico com interatividade
           textOutput(outputId = 'descUf'), # texto para renderizacao
           plotlyOutput(outputId = 'atendida'), # grafico com interatividade
           plotlyOutput(outputId = 'atendidaAno') # grafico com interatividade
           # plotOutput(outputId = 'data'), # graficos estaticos
           # plotlyoutput() # graficos interativos
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # armazenando retorno em uma variavel em formato de funcao (reactive) e necessita da sequencia de simbolos ()$
  dados_selecionados <- reactive({
                          # aplicando condicao para que o grafico retorne todos os elementos
                          if (! 'Todos' %in% input$select_UF) {
                            # selecionando dados do data frame
                            dados <- dados %>% 
                                       filter(UF %in% input$select_UF)
                          }
    
                          dados <- dados %>% 
                                     filter(as.Date(DataAbertura) >= input$data_abertura[1] & # associando a data inicial selecionada pelo usuario ao retorno que ocorrera no grafico na posicao 1 do array
                                              as.Date(DataAbertura) <= input$data_abertura[2]) # associando a data final selecionada pelo usuario ao retorno que ocorrera no grafico na posicao 2 do array
                          dados 
                          
                        })
  
  # # criacao do grafico a ser renderizado
  # output$listaUF <- renderPrint({
  #   unique(dados_selecionados()$UF)
  # })
  
  # criacao do grafico a ser renderizado
  output$data <- renderPlotly({
    ggplotly(
      data.frame(table(as.Date(dados_selecionados()$DataArquivamento))) %>% # gerando um data frame a partir de uma tabela, convertendo data original em formato mais legivel
        rename(Data = Var1, Qtd = Freq) %>% # renomeando as colunas geradas no data frame
        ggplot(aes(as.Date(Data), Qtd)) + # definindo eixos x e y
        geom_line(group = 1) + # definindo modelo do grafico
        theme_bw() + # tema definico como branco
        ggtitle('Quantidade de Reclamacoes por Ano-mes') + # define o titulo do grafico
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +# define a angulacao do rotulo de datas
        scale_x_date(date_labels = '%b-%Y', breaks = '6 month') +# escala do eixo x como data, com rotulos das datas e quebra de valores
        xlab('Data') # altera o rotulo do eixo x
    )
  })
  
  # criacao do grafico a ser renderizado
  output$uf <- renderPlotly({
    ggplotly(
      data.frame(table(dados_selecionados()$UF)) %>% # gerando um data frame a partir de uma tabela, baseada no data frame ja utilizado
        rename(UF = Var1, Qtd = Freq) %>% # renomeando colunas geradas na etapa anterior 
        ggplot(aes(x = reorder(UF, Qtd), y = Qtd, text = paste('UF:', UF, '<br>', 'QTD:', Qtd))) + # define o mapeamento dos eixos x e y, ordena o grafico e apresenta texto que aparecera na interacao
        geom_bar(fill = 'blue',  stat = 'identity') + # configura o grafico, definindo a cor e ja que estamos passando o valor da coluna y determina o stat = 'identity'
        coord_flip() + # rotaciona as barras de vertical para horizontal
        xlab('UF') +
        theme_bw() + # tema definico como branco
        ggtitle('Quantidade de Reclamcoes Por UF'), # define o titulo do grafico
      tooltip = 'text'
    )
  })
  
  # criacao do grafico a ser renderizado  
  output$atendida <- renderPlotly({
    ggplotly(
      ggplot(dados_selecionados()) + 
        geom_bar(aes(Atendida), # definindo grafico e eixo x e y 
                 fill = c('red', 'green'), # definindo cores
                 stat = 'count') + # a propria funcao conta a quantidade de registro
        ylab('Quantidade') + # personalizando o rótulo do eixo y
        theme_bw() + # alterando o tema para branco
        ggtitle('Quantidade de Chamados Atendidas') # inserindo um titulo para  grafico
    )
  })
  
  # criacao do grafico a ser renderizado  
  output$atendidaAno <- renderPlotly({
    ggplotly(
      data.frame(table(dados_selecionados()$anocalendario, dados_selecionados()$Atendida)) %>% # criando um data frame com duas colunas a partir de uma tabela 
        rename(Ano = Var1, Atendida = Var2, Qtd = Freq)  %>%  # renomeando as colunas resultado da etapa anterior
        ggplot() + # 
        geom_bar(aes(x = Ano, # define eixo x
                     y = Qtd, # define eixo y
                     fill = Atendida), # define a legenda
                 stat = 'identity', 
                 position = position_dodge2()) + # configura automaticamente a posicao das colunas
        theme_bw() + # tema definico como branco
        ggtitle('Quantidade de Reclamacoes Atendidas (nao) Por Ano') # define o titulo do grafico
    )
  })
  
  # criacao de texto a ser renderizado
  output$descData <- renderText ({
    paste("Grafico com a quantidade de reclamacoes feitas entre:",
          min(dados_selecionados()$DataAbertura),
          '-',
          max(dados_selecionados()$DataAbertura))
  })
  
  # criacao de texto a ser renderizado
  output$descUf <- renderText({
    estados <- paste(unique(dados_selecionados()$UF), # criando um texto dinamico e um vetor de valores unicos
                     collapse = ',') # parametro para apresentar ao usuario em formato texto, cujo separador e a virgula
    paste('Grafico com a quantidade de reclamacoes feitas pelas UF:', estados) # criando um texto estatico
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
