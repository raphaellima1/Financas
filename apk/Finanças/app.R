if (interactive()) {
  library(shiny)
  library(shinyWidgets)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(dplyr)
  library(DT)
  library(ggplot2)
  library(plotly)
  library(readxl)
  library(lubridate)
  }


################################################################################
df <- read_excel('C:/Users/rapha/OneDrive/Documentos/Finanças pessoal/Orçamento mensal simples1 (1).xlsx',3) %>% 
  select(1:5)

listas <- read_excel('C:/Users/rapha/OneDrive/Documentos/Finanças pessoal/Orçamento mensal simples1 (1).xlsx',12)

listas <- listas %>% 
  distinct(tag, .keep_all = TRUE) %>% 
  select(tipo, tag)
################################################################################
list_month <- df %>% 
  dplyr::select(Data) %>% 
  mutate(mes = month(ymd(Data), label = TRUE, abbr = FALSE)) %>% 
  dplyr::select(mes) %>% 
  distinct() 
list_month <- as.list(list_month)




lista_receita <- c("Receita", 'Despesa')


ui <- dashboardPage(skin = "black",
                    tags$head(
                      # Note the wrapping of the string in HTML()
                      tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: black;
        color: white;
      }
      h2 {
        font-family: 'Yusei Magic', sans-serif;
      }
      .shiny-input-container {
        color: #474747;
      }"))),

    dashboardHeader(title = "Finanças Pessoais",
                    leftUi = tagList()
                    ),
                    
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Resumo do Mês", 
                 tabName = "Resumo", 
                 icon = icon("dashboard")),
        menuItem("Widgets", 
                 icon = icon("th"), 
                 tabName = "widgets")
      ),
      
      selectizeInput(
      'mes', 
      'Selecione o Mês:', 
      choices = list_month,
      options = list(
        placeholder = 'Mês',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )), 
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "Resumo",
                h3("RESUMO DOS GASTOS"),
        fluidRow(
        infoBoxOutput("receita"),
        infoBoxOutput("despesa"),
        infoBoxOutput("saldo")
      ),
     fluidRow(
        box(title = "Distribuição de gastos mensais",
           # status = "warning",
            solidHeader = F,
            width = 6,
            collapsible = T,
            plotlyOutput("rec_desp")),
      fluidRow(
        box(title = "% de gasta no mês",
            #status = "warning",
            solidHeader = F,
            height = 7,
            width = 6,
            collapsible = T,
            plotlyOutput("plot_pie")
            )
        )
        ),
      
      #tabela de itens
      fluidRow(
        
        box(title = "Gastos por dia",
            solidHeader = T,
            width = 6,
            collapsible = T,
            height = 10,
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
      )
      ),
      
      ## Segunda pagina
      tabItems(
        tabItem(tabName = "widgets",
                h2("widgets")
                )
      )
    )
    )


################################################################################
# Define server logic required to draw a histogram
server <- function(input, output) {

########## Cards ######################
    observeEvent(input$mes,{
    valor <- df %>% 
    mutate(tag = substr(Tag,1,1)) %>% 
    dplyr::filter(tag == "R") %>% 
    filter(mes == input$mes) %>% 
    dplyr::summarise(sum(Valor)) %>% 
    as.numeric() %>% 
    round(digits = 0) %>% 
    format(big.mark = '.')
    
    
  output$receita <- renderValueBox({
    valueBox(paste0("R$ ", valor), 
             "Receita no Mês", icon = icon("brazilian-real-sign"), color = "green")
    })
  })
  
  observeEvent(input$mes,{
    valor <- df %>% 
      mutate(tag = substr(Tag,1,1)) %>% 
      dplyr::filter(tag != "R") %>% 
      filter(mes == input$mes) %>% 
      dplyr::summarise(sum(Valor)) %>% 
      as.numeric() %>% 
      round(digits = 0) %>%
      format(big.mark = '.',small.mark = ',')

    output$despesa <- renderValueBox({
      valueBox(paste0("R$ ", valor), 
               "Despesa no Mês", icon = icon("circle-down"), color = "red")
    })
  })
  
  observeEvent(input$mes,{
    Rec <- df %>% 
      mutate(tag = substr(Tag,1,1)) %>% 
      dplyr::filter(tag == "R") %>% 
      filter(mes == input$mes) %>% 
      dplyr::summarise(sum(Valor)) %>% 
      as.numeric() %>% 
      round(digits = 0)
    
    Des <- df %>% 
      mutate(tag = substr(Tag,1,1)) %>% 
      dplyr::filter(tag != "R") %>% 
      filter(mes == input$mes) %>% 
      dplyr::summarise(sum(Valor)) %>% 
      as.numeric() %>% 
      round(digits = 0) 
    
    valor <-  Rec - Des
    valor <- valor %>% 
      format(big.mark = '.')
    output$saldo <- renderValueBox({
      valueBox(paste0("R$ ", valor), 
               "Receita no Mês", icon = icon("wallet"), color = "blue")
    })
  })
  
########## Plot pie #####################  
  
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
      
      plot_ly(gastos_diario, x = ~Data, y = ~Valor,  type = 'scatter', mode = 'lines+markers')
      
    })
    
    })
    

  
  
  
  
  observeEvent(input$mes, {
    df <- df %>% 
      filter(mes == input$mes) %>% 
      select(Data, `Descrição`,Valor) %>% 
      mutate(Data = as.Date(Data, format = '%d/%m/%Y'))
             
    output$ing_df <- renderDT({
      datatable(df) %>% 
        formatCurrency(columns = 'Valor',
                       currency = "R$  ",
                       mark = ".", 
                       dec.mark =',')
    })
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)
