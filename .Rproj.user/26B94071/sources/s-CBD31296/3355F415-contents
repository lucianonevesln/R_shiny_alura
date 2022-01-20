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

# importanto dados externos
dados <- fread('dados_limpos.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Hello Shiny"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Seletor numerico:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           verbatimTextOutput(outputId = 'listaNumeros'),
           verbatimTextOutput(outputId = 'listaUF'),
           plotlyOutput(outputId = 'data'), # grafico com interatividade
           plotlyOutput(outputId = 'uf'), # grafico com interatividade
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
                          # selecionando dados do data frame
                          dados %>% 
                            filter(UF %in% c('DF', 'SC', 'GO'))
                        })
  
  output$listaUF <- renderPrint({
    unique(dados_selecionados()$UF)
  })
  
  output$listaNumeros <- renderPrint({
      seq(1:input$bins)
    })
  
  # renderizacao do grafico
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
