#library(shinydashboardPlus)
  library(shiny)
  library(shinyjs)
  library(devtools)
  #library(shinymanager)
  library(keyring)
  #library(shinythemes)
  library(fresh)
  library(bs4Dash)
  library(waiter)
  library(fontawesome)
  library(ggplot2)
  library(plotly)
  library(RColorBrewer)
  library(viridis)
  library(DT)
  library(tidyquant)
  library(hrbrthemes)
  library(tidyverse)
  library(data.table)  # usa o comando shift()
  library(readxl)
  library(formattable)
  library(kableExtra)
  library(data.table)
  library(knitr)
  library(keyring)
  library(scales)
  library(dplyr)
  library(echarts4r)
  library(lubridate)

  #######################
df <- read_excel('C:/Users/rapha/OneDrive/Documentos/Finanças pessoal/Orçamento mensal simples1 (1).xlsx',3) %>% 
  select(1:5)

listas <- read_excel('C:/Users/rapha/OneDrive/Documentos/Finanças pessoal/Orçamento mensal simples1 (1).xlsx','Planilha2')
###
listas1 <- listas %>% 
  distinct(tag, .keep_all = TRUE) %>% 
  select(tipo, tag)
################################################################################
list_month <- df %>% 
  dplyr::select(Data) %>% 
  mutate(mes = lubridate::month(lubridate::ymd(Data), label = TRUE, abbr = FALSE)) %>% 
  dplyr::select(mes) %>% 
  distinct() 
list_month <- as.list(list_month)
########################################################################
df1 <- read_excel('C:/Users/rapha/OneDrive/Documentos/Finanças pessoal/Orçamento mensal simples1 (1).xlsx',3) %>% 
  select(1:5) %>% 
  mutate(key = paste(mes,Tag)) %>% 
  dplyr::group_by(key) %>% 
  summarise(Valor = sum(Valor))

mes1 <- c(1:12) %>% 
  as.data.frame(col.names = c('mes')) %>% 
  dplyr::rename('mes' = '.') %>% 
  mutate(mes = as.Date(paste('2023',mes,'01', sep = '-'))) %>% 
  mutate(mes = lubridate::month(mes,abbr = F, label = TRUE))

tab_fatasm <- listas %>%
  select(tagnum, descricao) %>% 
  crossing(mes1)%>% 
  mutate(tag = substr(tagnum,1,1)) %>% 
  mutate(key = paste(mes,tagnum)) %>% 
  merge(df1, by.x = 'key', by.y = 'key',all.x = T)



######################################################################

# Define UI for application that draws a histogram
ui <- bs4DashPage(

    header = dashboardHeader(
    title = "FINANÇAS",
    sidebarIcon = shiny::icon("home"),
      compact = T,
      lefttUi = shinydashboardPlus::dropdownBlock(
       id = "mydropdown",
       title = NULL,
       icon = tags$span(style="color:#fffff",icon("calendar-days")),
       selectizeInput(
         inputId = 'mes',
         label = NULL,
         choices = list_month,
         options = list(
           placeholder = 'Selecione o mês')
       ))
    ),

  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenu",
      
    menuItem("Resumo geral", 
               tabName = "control", 
               icon = icon("pencil"),
               badgeColor = 'green'
      ),
      
    menuItem("Resumo do Mês", 
             tabName = "Resumo", 
             icon = icon("dashboard"),
             badgeColor = 'green'
             ),
    
    menuItem("Composição dos Gastos", 
             icon = icon("th"), 
             tabName = "table_gastos",
             badgeColor = 'green')
              )
  ),
  
  body = bs4DashBody(
    tabItems(
 
  # Primeira pagina ####
    
   tabItem(
      tabName = "Resumo",
 
   # Segundo Fluidrow cards ####
   
   fluidRow(
      bs4ValueBoxOutput("receita",
                     width = 3),
      
      bs4ValueBoxOutput("despesa",
                        width = 3),
      
      bs4ValueBoxOutput("restante",
                        width = 3),
      
      bs4ValueBoxOutput("prec_gastos",
                        width = 3)
      ),
   fluidRow(
        box(title = "Distribuição de gastos mensais",
            # status = "warning",
            solidHeader = F,
            width = 6,
            collapsible = T,
            plotlyOutput("rec_desp")
            ), 
        box(title = "% de gasta no mês",
                   #status = "warning",
                   solidHeader = F,
                   width = 6,
                   collapsible = T,
                   plotlyOutput("plot_pie")
            )
        
   ),
   
   #tabela de itens
   fluidRow(
     
     box(title = "Gastos por dia",
         solidHeader = T,
         width = 6,
         collapsible = T,
         plotlyOutput("plot_dia")
     ),
     
     box(title = "Registros",
         solidHeader = T,
         width = 6,
         collapsible = T,
         div(DT::DTOutput('ing_df')
         )
     )
   )
  ), 

  
  # Segunda pagina ####  
  tabItem(
    tabName = "control",
    fluidRow(     box(title = "Gastos por dia",
                      solidHeader = T,
                      width = 12,
                      collapsible = T,
                      div(DT::DTOutput('tab_receita'))
                      )
                  )
    ),

  tabItem(
    tabName = "th",
    fluidRow(box(title = "Gastos por dia",
                 solidHeader = T,
                 width = 12,
                 collapsible = T,
                 div(DT::DTOutput('tab_rec'))
                 )
             )
    )

  )
  )
  )


# Servidor ####
server <- function(input, output) {

  # Cards ####
  
  # card das receitas 
  observeEvent(input$mes,{
    rec <- df %>% 
      mutate(tag = substr(Tag,1,1)) %>% 
      dplyr::filter(tag == "R") %>% 
      filter(mes == input$mes) %>% 
      dplyr::summarise(sum(Valor)) %>% 
      as.numeric() %>% 
      round(digits = 0) %>% 
      format(big.mark = '.')
    
    output$receita <- renderValueBox({
      valueBox(
        value = tags$h4(div(paste0("R$ ", rec))),
        subtitle = "Receita totais no mês",
        color = "olive",
        icon = icon("cart-shopping")
        )
      })
    
  # card das despesas
    
    desp <- df %>% 
      mutate(tag = substr(Tag,1,1)) %>% 
      dplyr::filter(tag != "R") %>% 
      filter(mes == input$mes) %>% 
      dplyr::summarise(sum(Valor)) %>% 
      as.numeric() %>% 
      round(digits = 0) %>%
      format(big.mark = '.',small.mark = ',')
    
    output$despesa <- renderValueBox({
      valueBox(
        value = tags$h4(div(paste0("R$ ", desp))),
        subtitle = "Despesas totais no mês",
        color = "maroon",
        icon = icon("circle-down")
      )
    })
    

    # card das Saldo restante
    
    rest <- as.numeric(rec) - as.numeric(desp)   
    
    output$restante <- renderValueBox({
      valueBox(
        value = tags$span(style="color:white",tags$h4(div(paste0("R$ ", rest)))),
        subtitle = tags$span(style="color:white", "% receita sobre gastos"),
        color = "gray-dark",
        icon = icon("money-check-dollar")
      )
    })

    # card das % de gastos
    
    perc <- round(as.numeric(desp)/as.numeric(rec)*100,2)   
   
    output$prec_gastos <- renderValueBox({
      valueBox(
        value = tags$span(style="color:white",tags$h4(div(paste0("% ", perc)))),
        subtitle = tags$span(style="color:white", "% receita sobre gastos"),
        color = "orange",
        icon = icon("percent")
      )
    })
     
    
    observeEvent(input$mes, {
      gastos <- df %>% 
        mutate(tag = substr(Tag,1,1)) %>% 
        dplyr::filter(tag != "R") %>% 
        merge(listas) %>% 
        filter(mes == input$mes) %>% 
        group_by(tipo) %>% 
        summarise(valor = sum(Valor))
      
      despesa <- df %>% 
        mutate(tag = substr(Tag,1,1)) %>% 
        dplyr::filter(tag != "R") %>% 
        filter(mes == input$mes) %>% 
        summarise(valor = sum(Valor), tipo = "despesa")
      
      tabela <- df %>% 
        mutate(tag = substr(Tag,1,1)) %>% 
        dplyr::filter(tag == "R") %>% 
        filter(mes == input$mes) %>% 
        summarise(valor = sum(Valor), tipo = "receita") %>% 
        rbind(despesa) %>% 
        arrange(tipo)
      
      gastos_diario <- df %>% 
        mutate(tag = substr(Tag,1,1)) %>% 
        dplyr::filter(tag != "R") %>% 
        merge(listas) %>% 
        filter(mes == input$mes) %>% 
        mutate(Data = as.Date(Data, format = '%d/%m/%Y')) %>% 
        group_by(Data) %>% 
        summarise(Valor = sum(Valor))
      
      df <- df %>% 
        filter(mes == input$mes) %>% 
        select(Data, `Descrição`,Valor) %>% 
        mutate(Data = as.Date(Data, format = '%d/%m/%Y'))
      
      output$plot_pie <-renderPlotly({
        colors <- c('#EA135F', 
                    '#f6993f', 
                    '#ffed4a', 
                    '#33b431', 
                    '#4dc0b5',
                    '#3490dc', 
                    '#6574cd', 
                    '#9561e2',
                    '#f66d9b')
        
        plot_ly(gastos, labels = ~tipo, values = ~valor, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                text = ~paste('R$', valor, ' Reais'),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF',
                                          width = 1)),
                showlegend = FALSE) %>% 
          layout(autosize = F, width = 600, height = 410)
        
        
        
        
      })
      
      output$rec_desp <-renderPlotly({
        
        plot_ly(tabela, x = ~tipo, y = ~valor, text = ~valor, type = 'bar',
                marker = list(color = c('#EA135F','#33b431'))) 
        
      })
      
      output$plot_dia <-renderPlotly({
        
        plot_ly(gastos_diario, type = 'scatter', mode = 'lines')%>%
          add_trace(x = ~Data, y = ~Valor)%>%
          layout(showlegend = F, title=NULL,
                 xaxis = list(rangeslider = list(visible = T)))
        
      })
      
      output$ing_df <- renderDT({
        datatable(df) %>% 
          formatCurrency(columns = 'Valor',
                         currency = "R$  ",
                         mark = ".", 
                         dec.mark =',')
        })
      
      
      
      
      
    })
    tabela_rec <- tab_fatasm %>% 
        #dplyr::filter(tag =='L') %>% 
        dplyr::select(mes, tagnum, descricao, Valor) %>% 
        pivot_wider(names_from = mes, values_from = Valor, values_fn = sum) %>% 
        select("tagnum", "descricao", "janeiro", "fevereiro", "março", 
               "abril", "maio", "junho", "julho", "agosto",  "setembro",
               "outubro", "novembro", "dezembro")
    output$tab_rec <- renderDT({
        datatable(tabela_rec, 
                  options = list(dom = 't')) %>% 
          
          formatCurrency(columns = c("janeiro", "fevereiro", "março", 
                                     "abril", "maio", "junho", 
                                     "julho", "agosto",  "setembro",
                                     "outubro", "novembro", "dezembro"),
                         currency = " ",
                         mark = ".", 
                         dec.mark =',')
      })
    })
  


  
  
  
}

shinyApp(ui, server)

