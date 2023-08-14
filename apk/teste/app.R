df <- read_excel('C:/Users/rapha/OneDrive/Documentos/Finanças pessoal/Gastos_mensais.xlsx',3,range = 'A1:e200' )

receita <- df %>% 
  mutate(tag = substr(Tag,1,1)) %>% 
  dplyr::filter(tag == 'R')
