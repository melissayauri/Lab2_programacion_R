#lab 2
library(httr)
library(XML)
library(stringr)
library(dplyr) 
library(purrr)
library(ggplot2)
library(gridExtra)
# Pregunta  1
# Incorporando la petición GET para realizar la petición a la URL
web_direction <- GET("https://www.mediawiki.org/wiki/MediaWiki")
# Extrayendo la información de la petición mencionada
information <- content(web_direction, as = "text")
# Conversión de HTML a XML
file_xml <- htmlParse(information, asText = TRUE)
# Exportar el archivo
saveXML(file_xml, file = "D:/MAESTRIA-CIBERSEGURIDAD/SEMINARIO_DATA_SCIENCE/LABS/Lab2_programacion_R/file_xml")

# Pregunta 2
# Extrayendo la etiqueta title 
title_label <- xpathSApply(file_xml, "//title", xmlValue)
title_label

# Pregunta 3

# Extrayendo los nombres de la etiqueta <a></a>
name_hyperlink <- xpathSApply(file_xml, "//a", xmlValue)
#name_hyperlink 
# Extrayendo los valores del atributo href de la etiqueta <a></>
url_hyperlink <- xpathSApply(file_xml, "//a", xmlGetAttr, "href")
#url_hyperlink
# Validando valores null

# Validando si los nombres de la etiqueta y los valores del atributo href tienen los valores null
null_name <- sapply(name_hyperlink, is.null)
null_href <- sapply(url_hyperlink, is.null)
# Reemplazando los valores null por el valor NA
name_hyperlink[null_name] <- NA
url_hyperlink[null_href] <- NA

name_hyperlink <- unlist(name_hyperlink)
url_hyperlink  <- unlist(url_hyperlink)
url_hyperlink




# PREGUNTA 4
# Creando una tabla con el texto y su respectivo url del enlace
links_tables <- data.frame(Text = name_hyperlink, Url = url_hyperlink)
# Convirtiendo a data.frame
concurrences <- as.data.frame(table(links_tables))

#
 # arrange(Text)
# Filtrando solo los enlaces existentes, es decir todos lo que tenga >0 en la columna freqq
links_data <- filter(concurrences, Freq > 0)
View(links_data)
#links_data<- concurrences[concurrences$Freq > 0, -4] %>% arrange(Text)
# Ordenando de mayor a menor
#links_data <- arrange(links_data, Text)
#View(links_data)
# Pregunta 5

#links_data <- links_data %>%
 # mutate(Enlace_final = case_when(
  #  grepl("^/wiki/", Url) ~ paste0("https://www.mediawiki.org", Url),
   # grepl("^/w/", Url) ~ paste0("https://www.mediawiki.org", Url),
  #  grepl("^//", Url) ~ paste0("https://www.mediawiki.org", Url),
  #  grepl("^https", Url) ~ Url,
  #  TRUE ~ NA_character_
  #))

# Agregando la URL completa 
base_url <- "https://www.mediawiki.org"

links_data$Final_Url <- case_when(
  # validando los carácteres al inicio de la URL
  grepl("^/wiki/", links_data$Url) ~ paste0(base_url, links_data$Url),
  grepl("^/w/", links_data$Url) ~ paste0(base_url, links_data$Url),
  grepl("^//", links_data$Url) ~ paste0(base_url, links_data$Url),
  grepl("^https", links_data$Url) ~ links_data$Url,
  grepl("^#", links_data$Url) ~ paste0(base_url,"/wiki/MediaWiki", links_data$Url),
  TRUE ~ NA_character_
)
# Incorporando el valor status_code 
# Demora 1 minuto
# Recorriendo los datos para incorporar HEAD y hallar el status_code
status_codes <- map(links_data$Final_Url, HEAD)
# Agregando una columna con el status_code respectivo
links_data$Status_Code <- map(status_codes, status_code)
# Convirtiendo de lista a character
links_data$Status_Code <- as.character(links_data$Status_Code)
# Demora 4 minutos
#get_status_code <- function(url) {
 # Sys.sleep(1)
  #status_code(HEAD(url))
#}

#status_codes <- map(links_data$Final_Url, get_status_code)
#links_data$Status_Code <- status_codes



#Pregunta 2.1


# Validando si la URL es absoluta o relativa
links_data$Url_type <- ifelse(grepl("^http", links_data$Url), "Absoluta", "Relativa")
View(links_data)

# Creando el gráfico histograma de el n° de enlaces versus la frecuencia
histogram <- ggplot(links_data, aes(x=Freq)) + 
  geom_histogram(aes(fill=Url_type), 
                 binwidth = 1, 
                 position = "dodge") +
  scale_fill_manual(values=c("#FF5733", "#6B33FF")) +
  labs(x = "Frecuencia de apariciones", y = "N° de enlaces") +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 10), 
                     expand = c(0, 0)) +
  theme_light()


# Pregunta 2.2

# Añadiendo si el link es interno o externo
links_data$Hyperlink_type <- ifelse(grepl("^https://www.mediawiki.org", links_data$Final_Url), "Interno", "Externo")
# Hallando la frecuencia
freq_link <- table(links_data$Hyperlink_type)
# Creando el gráfico de barras
bar_graphic <- ggplot(data.frame(Hyperlink_type = names(freq_link), count = as.numeric(freq_link)), aes(x=Hyperlink_type, y=count, fill = Hyperlink_type)) +  
  geom_bar(stat="identity") +
  labs(title="Enlaces internos vs externos", x="Tipo de enlace", y="Cantidad") +
  theme_light() +
  scale_fill_manual(values = c("#6833FF", "#D433FF")) +
scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 20))

# Pregunta 3.3
# Creando el gráfico de tarta

# Hallando el n° de repeticiones del status_code del link
code_freq <- table(links_data$Status_Code)
# Hallando el porcentaje en % ( o.9 a 90%)
percentage_value <- round(prop.table(code_freq) * 100, 2)
percentage_value <- as.numeric(percentage_value)
# data 
code_data <- data.frame(Status_Code = names(code_freq),
                        Frequency = as.numeric(code_freq),
                        Percentage = percentage_value)
chart_graphic <- ggplot(code_data, aes(x="", y=Percentage, fill=Status_Code)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5), size = 3)


View(links_data)

grid.arrange(histogram, bar_graphic, chart_graphic, ncol=3)