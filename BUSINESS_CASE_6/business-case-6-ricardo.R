# Aponta diretório de trabalho
setwd('/home/ricardoevers/Downloads')

# Mostra diretório de trabalho
getwd()

# Instala pacotes
install.packages('stringr')

# Chama pacotes instalados
library('stringr')



# Lê arquivo csv
df <- read.csv("Worksheet.csv", sep=";")

# Função para processar DataFrame e retorna-lo transformado 
table_reading_and_processing <- function(df){

  # Remove coluna Id
  df$Id <- NULL

  # Transforma valores das colunas IsApproved e NotMonetize em binario
  transform_to_binary <- c("N" = 0, "Y" = 1)
  df$NotMonetize <- transform_to_binary[df$NotMonetize]
  df$IsApproved <- transform_to_binary[df$IsApproved]
  df$IsApproved <- replace(df$IsApproved, is.na(df$IsApproved), 0)
  df$NotMonetize <- replace(df$NotMonetize, is.na(df$NotMonetize), 0)
  
  # Conta e transforma os dias da coluna InformedDate
  initial_date <- '01/01/2001'
  initial_date <- as.Date(initial_date, format='%d/%m/%Y')
  final_date <- as.Date(df$InformedDate, format='%d/%m/%Y')
  day_count <- final_date - initial_date
  df$InformedDate <- day_count

  #Retira artigos e caracteres especiais
  df$Description <- str_replace_all(df$Description, " o ", " ")
  df$Description <- str_replace_all(df$Description, " os ", " ")
  df$Description <- str_replace_all(df$Description, " a ", " ")
  df$Description <- str_replace_all(df$Description, " as ", " ")
  df$Description <- str_replace_all(df$Description, " um ", " ")
  df$Description <- str_replace_all(df$Description, " uns ", " ")
  df$Description <- str_replace_all(df$Description, " uma ", " ")
  df$Description <- str_replace_all(df$Description, " umas ", " ")
  df$Description <- str_replace_all(df$Description, "[[:punct:]]", ' ')

  #Transforma horas em fração do dia
  df$Worked.hours <- as.POSIXlt(df$Worked.hours, format='%H: %M')
  df$Worked.hours = with(df, (Worked.hours$min*60 + Worked.hours$hour*3600) / (24*3600))
  df$StartTime <- as.POSIXlt(df$StartTime, format='%H: %M')
  df$StartTime = with(df, (StartTime$min*60 + StartTime$hour*3600) / (24*3600))
  df$EndTime <- as.POSIXlt(df$EndTime, format='%H: %M')
  df$EndTime = with(df, (EndTime$min*60 + EndTime$hour*3600) / (24*3600)) 
  df$Worked.hours <- replace(df$Worked.hours, is.na(df$Worked.hours), 0)
  
  # Cria coluna com diferença de tempo entre StarTime e EndTime
  df$TotalTime <- df$EndTime - df$StartTime
  
  # Retorna DataFrame transformado
  return(df)
}

table_reading_and_processing(df)



