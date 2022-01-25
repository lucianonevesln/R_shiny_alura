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
library(shinydashboard)
library(shinyWidgets)

# importando dados externos
dados <- fread('dados_limpos.csv')# , encoding = 'UTF-8')

# criando uma variavel para armazenar valores de media para apresentacao em dashboard
media_chamados_ano <- dados %>% 
                        group_by(anocalendario) %>% # aplica funcao de agregacao para agrupar conforme coluna escolhida
                        summarise(qtd_chamados = n()) %>% # cria uma nova coluna para armazenar o numero resultado do group_by
                        summarise(medias_chamado_ano = mean(qtd_chamados)) %>% # calcula a media a partir da coluna definida na linha acima
                        as.numeric() # convertendo tudo em tipo numerico

# criando um cabecalho para apresentacao ao usuario
cabecalho <- dashboardHeader(title = 'Dashboard PROCONs')

# criando uma barra lateral para apresentacao ao usuario
barra_lateral <- dashboardSidebar(width = '250px', # tamanho da barra lateral
                                  sidebarMenu( # criando uma menu de barra lateral
                                    menuItem( # item de menu
                                      'Dashboard', # nome que aparecera para o usuario
                                      tabName = 'dashboard', # id de identificacao de elemento
                                      icon = icon('dashboard')), # figura do proprio R, que aparecera para o usuario   
                                    menuItem( # item de menu 
                                      'Informacoes', # nome que aparecera para o usuario
                                      tabName = 'informacoes', # id de identificacao de elemento
                                      icon = icon('info-circle') # figura do proprio R, que aparecera para o usuario
                                    )
                                  ))

# criando uma area para apresentar o corpo dos retornos para o usuario
painel_principal <- dashboardBody(
  
  # criando estrutura que ao ser selecionada pelo usuario, apresenta uma nova pagina para ele
  tabItems(
    tabItem(
      tabName = 'informacoes', # id de identificacao de elemento
      h1('Informacoes'), # nome que aparecera para o usuario
      infoBox(
        title = 'Contato', # nome que aparecera para o usuario
        icon = icon('envelope-square'), # figura do proprio R, que aparecera para o usuario
        subtitle = 'Para mais informacoes e/ou feedback entre em contato: luciano@email.com') # subtitulo que aparecera abaixo do valor)
    ),
    tabItem(
      tabName = 'dashboard',
      # inseriondo uma caixa com valor, uma com informacoes e outra de output para apresentacao ao usuario
      fluidRow( # inserindo uma linha fluida na app
        valueBox(
          subtitle = 'Registros', # subtitulo que aparecera abaixo do valor
          value = nrow(dados), # valor que aparecera para o usuario 
          icon = icon('database')), # figura do proprio R, que aparecera para o usuario 
        infoBox(
          title = '',
          subtitle = 'Reclamacoes por ano', # subtitulo que aparecera abaixo do valor
          value = media_chamados_ano, # valor que aparecera para o usuario
          icon = icon('list')), # figura do proprio R, que aparecera para o usuario
        valueBoxOutput(
          outputId = 'qtduf' # id que sera utilizado pela app para identificacao
        )
      ),
      fluidRow( # inserindo uma linha fluida na app
        column(width = 12, # inserindo uma coluna dentro linha
               # criando uma caixa
               box(title = 'Filtros', width = '100%',
                   column(width = 12, # criando uma coluna
                          box(
                            width = '100%', # definindo ocupacao proporcional
                            # criando um grupo de selecao de caixa
                            awesomeCheckboxGroup(inline = TRUE, # dispoe as opcoes em formado de linha
                                                 inputId = 'select_UF', # identificacao para o output 
                                                 label = 'Estados:', # rotulo que aparececar para o usuario
                                                 choices = c('Todos', unique(dados$UF)), # opcoes que aparecerao para o usuario a partir de uma coluna da base de dados
                                                 selected = 'Todos') # definindo opcao que aparecera selecioada por padra para o usuario
                          ) ## final box mais interno
                   ), ## final coluna mais interna
                   column(width = 6, # criando uma coluna
                          box(width = '100%', # definindo ocupacao proporcional
                              # criando um range de datas para selecao do usuario
                              dateRangeInput(inputId = 'data_abertura', # determina o id que sera usado pela app para identificacao
                                             label = 'Data Abertura:', # rotulo de identificacao que aparecera para o usuario
                                             format = 'dd-mm-yyyy', # altera o formato de apresentacao padrao para o formato brasileiro
                                             start = min(as.Date(dados$DataAbertura)), # data inicial que aparecera para o usuario
                                             end = max(as.Date(dados$DataAbertura)))
                          ) ## final box mais interno
                   ), ## final coluna mais interna
                   column(width = 6, # criando uma coluna
                          box(width = '100%', # definindo ocupacao proporcional
                              selectizeInput(inputId = 'assunto', # definindo a identificacao que sera usada pela aplicacao
                                             label = 'Descricao Assunto', # definindo o rotulo que aparecera para o usuario
                                             choices = c('Todos', unique(dados$DescricaoAssunto)), # definindo opcoes que aparecerao para o usuario
                                             multiple = T, # pemite ao usuario adicionar mais de uma opcao no seletor 
                                             options = list(maxItems = 5), # definindo o total de opcoes que o usuario podera selecionar
                                             selected = 'Todos') # definindo a opcao que aparecera selecionada por padrao
                          ) ## final box mais interno
                   ) ## final coluna mais interna
               ) ## final box
        ) ## final coluna mais externa
      ), ## final linha1
      fluidRow( # inserindo uma linha fluida na app
        column(width = 12, # criando uma coluna
               box(width = '100%', # definindo ocupacao proporcional
                   plotlyOutput(width = '100%', outputId = 'data'), # grafico com interatividade
                   verbatimTextOutput(outputId = 'descData') # texto para renderizacao
               )
        )
      ), ## final linha2
      fluidRow( # inserindo uma linha fluida na app
        column(width = 6, # criando uma coluna
               box(width = '100%', # definindo ocupacao proporcional
                   plotlyOutput(outputId = 'atendida') # grafico com interatividade
               )),
        column(width = 6, # criando uma coluna
               box(width = '100%', # definindo ocupacao proporcional
                   plotlyOutput(outputId = 'atendidaAno') # grafico com interatividade
               ))
      ),  ## final linha3
      fluidRow( # inserindo uma linha fluida na app
        column(width = 12, # criando uma coluna
               box(width = '100%', # definindo ocupacao proporcional
                   title = 'Reclamacoes por UF', # definindo titulo do grafico
                   plotlyOutput(outputId = 'uf'), # grafico com interatividade
                   textOutput(outputId = 'descUf') # texto para renderizacao
               ))
      ) ## final linha4
    ) ## final dashboardBody
      
    )
  ) ## fim tabitems

# criando uma estrutura organizada para apresentacao ao usuario
ui <- dashboardPage(header = cabecalho, # chamando as informacoes armazenadas em variavel
                    sidebar = barra_lateral, # chamando as informacoes armazenadas em variavel
                    body = painel_principal) # chamando as informacoes armazenadas em variavel


# Define UI for application that draws a histogram
ui2 <- fluidPage(

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
                           start = min(as.Date(dados$DataAbertura)), # data inicial que aparecera para o usuario
                           end = max(as.Date(dados$DataAbertura))), # data final que aparecera para o usuario
            
            # criando uma selecao de entrada 
            selectizeInput(inputId = 'assunto', # definindo a identificacao que sera usada pela aplicacao
                           label = 'Descricao Assunto', # definindo o rotulo que aparecera para o usuario
                           choices = c('Todos', unique(dados$DescricaoAssunto)), # definindo opcoes que aparecerao para o usuario
                           multiple = T, # pemite ao usuario adicionar mais de uma opcao no seletor 
                           options = list(maxItems = 5), # definindo o total de opcoes que o usuario podera selecionar
                           selected = 'Todos') # definindo a opcao que aparecera selecionada por padrao
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
                          # aplicando condicao para que o grafico retorne todos os elementos e/ou elementos selecionados pelo usuario
                          if (! 'Todos' %in% input$select_UF) {
                            # selecionando dados do data frame
                            dados <- dados %>% 
                                       filter(UF %in% input$select_UF)
                          }
    
                          # aplicando condicao para que o grafico retorne todos os elementos e/ou elementos selecionados pelo usuario
                          if (! 'Todos' %in% input$assunto) {
                            dados <- dados %>% 
                                      filter(DescricaoAssunto %in% input$assunto)
                          }
                          
                          dados <- dados %>%
                                            # associando a data inicial selecionada pelo usuario ao retorno que ocorrera no grafico na posicao 1 do array
                                     filter(as.Date(DataAbertura) >= input$data_abertura[1] & 
                                            # associando a data final selecionada pelo usuario ao retorno que ocorrera no grafico na posicao 2 do array  
                                            as.Date(DataAbertura) <= input$data_abertura[2]) 
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
        ggtitle('Quantidade de Reclamacoes por Ano-Mes') + # define o titulo do grafico
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
  
  # criando um resumo de valores para apresentacao ao usuario
  output$qtduf <- renderValueBox({
    valueBox(value = length(unique(dados_selecionados()$UF)), # definindo funcao de apresentacao, tamanho, unicidade e planilha com coluna da qual sera extraido as informacoes de resumo
             subtitle = 'UFs Selecionadas', # subtitulo que aparecera abaixo do valor
             icon = icon('map-marker')) # figura do proprio R, que aparecera para o usuario 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
