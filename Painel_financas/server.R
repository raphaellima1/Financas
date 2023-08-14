#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#library(shinydashboardPlus)
#library(shiny)
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
