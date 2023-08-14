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