# instalando bibliotecas necessarias
install.packages('data.table')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('plotly')
install.packages('shiny')
install.packages('shinyWidgets')
install.packages('shinydashboard')

# usando blibliotecas
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)

# gerando a base de dados
dados <- fread('reclamacao.csv', encoding = 'UTF-8')

View(dados)

# visualizando resumo da tabela
summary(dados)

# selecionando colunas da base de dados
dados %>% 
  select(X.1, V1)

# excluindo colunas da base de dados
reclamacao <- dados %>% 
                select(-X.1, -V1)

# retornando valores unicos de uma coluna
unique(reclamacao$regiao)

# filtrando determinadas informacoes na base de dados
reclamacao %>% 
  filter(regiao != 'N/D')

# salvando base de dados a partir de uma condicao
reclamacao <- reclamacao %>% 
                filter(regiao != 'N/D')

unique(reclamacao$regiao)

# retornando valores unicos de uma coluna
unique(reclamacao$Atendida)

# substituindo valores
reclamacao$Atendida <- gsub(pattern = 'S|Siim', 
                            replacement = 'sim', 
                            x = reclamacao$Atendida)

reclamacao$Atendida <- gsub(pattern = 'N|nAoo|nao', 
                            replacement = 'não' ,
                            x = reclamacao$Atendida)

unique(reclamacao$Atendida)

# eliminando linhas com valores em branco
reclamacao <- reclamacao %>% 
                filter(Atendida != '')

unique(reclamacao$Atendida)

# substituindo valores
unique(reclamacao$SexoConsumidor)

reclamacao$SexoConsumidor <- gsub(pattern = 'N|NULL' ,
                                  replacement = 'N/I' ,
                                  x = reclamacao$SexoConsumidor)

unique(reclamacao$SexoConsumidor)

# criando uma nova base de dados
fwrite(reclamacao, 'dados_limpos.csv', row.names = F)

dados_limpos <- fread('dados_limpos.csv', encoding = 'UTF-8')

View(dados_limpos)

# gerando grafico de barras
grafico_atendida <- ggplot(reclamacao) + 
                      geom_bar(aes(Atendida), # definindo grafico e eixo x e y 
                               fill = c('red', 'green'), # definindo cores
                               stat = 'count') + # a propria funcao conta a quantidade de registro
                      ylab('Quantidade') + # personalizando o rótulo do eixo y
                      theme_bw() + # alterando o tema para branco
                      ggtitle('Quantidade de Chamados Atendidas') # inserindo um titulo para  grafico

grafico_atendida

# adicionando interatividade ao grafico
grafico_atendida <- ggplotly(grafico_atendida) 

grafico_atendida

# criando uma base dados para uma grafico horizontal 
grafico_uf <- data.frame(table(reclamacao$UF)) %>% # gerando um data frame a partir de uma tabela, baseada no data frame ja utilizado
                      rename(UF = Var1, Qtd = Freq) %>% # renomeando colunas geradas na etapa anterior 
                      ggplot(aes(x = reorder(UF, Qtd), y = Qtd, text = paste('UF:', UF, '<br>', 'QTD:', Qtd))) + # define o mapeamento dos eixos x e y, ordena o grafico e apresenta texto que aparecera na interacao
                      geom_bar(fill = 'blue',  stat = 'identity') + # configura o grafico, definindo a cor e ja que estamos passando o valor da coluna y determina o stat = 'identity'
                      coord_flip() + # rotaciona as barras de vertical para horizontal
                      xlab('UF') +
                      theme_bw() + # tema definico como branco
                      ggtitle('Quantidade de Reclamcoes Por UF') # define o titulo do grafico

grafico_uf

# define a interacao quando o usuario passa o cursor por cima da barra
grafico_uf <- ggplotly(grafico_uf, tooltip = 'text') 

grafico_uf

# criando novo data frame, alterando forma de apresentacao de data
grafico_data <- data.frame(table(as.Date(reclamacao$DataArquivamento))) %>% # gerando um data frame a partir de uma tabela, convertendo data original em formato mais legivel
                  rename(Data = Var1, Qtd = Freq) %>% # renomeando as colunas geradas no data frame
                  ggplot(aes(as.Date(Data), Qtd)) + # definindo eixos x e y
                  geom_line(group = 1) + # definindo modelo do grafico
                  theme_bw() + # tema definico como branco
                  ggtitle('Quantidade de Reclamacoes por Ano-mes') + # define o titulo do grafico
                  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +# define a angulacao do rotulo de datas
                  scale_x_date(date_labels = '%b-%Y', breaks = '6 month') +# escala do eixo x como data, com rotulos das datas e quebra de valores
                  xlab('Data') # altera o rotulo do eixo x

# define a interacao quando o usuario passa o cursor por cima da barra
grafico_data <- ggplotly(grafico_data)

grafico_data

# criando uma base de dados comparando numero de reclamacoes atendidas com nao atendidas
grafico_atendida_ano <- data.frame(table(reclamacao$anocalendario, reclamacao$Atendida)) %>% # criando um data frame com duas colunas a partir de uma tabela 
                          rename(Ano = Var1, Atendida = Var2, Qtd = Freq)  %>%  # renomeando as colunas resultado da etapa anterior
                          ggplot() + # 
                          geom_bar(aes(x = Ano, # define eixo x
                                       y = Qtd, # define eixo y
                                       fill = Atendida), # define a legenda
                                       stat = 'identity', 
                                       position = position_dodge2()) + # configura automaticamente a posicao das colunas
                          theme_bw() + # tema definico como branco
                          ggtitle('Quantidade de Reclamacoes Atendidas (nao) Por Ano') # define o titulo do grafico

# define a interacao quando o usuario passa o cursor por cima da barra
grafico_atendida_ano <- ggplotly(grafico_atendida_ano)

grafico_atendida_ano

