rm(list=ls())
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinythemes)
library(tidyverse)
library(shinycssloaders)
library(scales)
library(plotly)

xlsFile <- paste0("psd_alldata.csv")
psd <- read.csv(xlsFile)

mes <- psd %>% filter(Calendar_Year == max(psd$Calendar_Year)) %>% 
  summarise(max(Month)) %>% pull()

ano <- max(psd$Calendar_Year)

DatosAl <- paste0("al ",mes,"-",ano)

psd <- psd %>% filter(Commodity_Description %in% c("Barley","Corn",'Meal, Soybean', 'Meal, Soybean (Local)', 'Meal, Sunflowerseed','Oil, Soybean', 'Oil, Soybean (Local)',
                                                   'Oil, Sunflowerseed','Oilseed, Soybean', 'Oilseed, Soybean (Local)',
                                                   'Oilseed, Sunflowerseed','Sorghum','Wheat')) %>%
            select(Commodity_Description,Country_Name,Attribute_Description,Market_Year,Value)


#armar pais "word"
#no "European Union","EU-15" (AL FINAL SOLO NO EU-15)
#diferenciar yield y exc rate (mean)
`%notin%` <- Negate(`%in%`)
world <- psd %>%
  group_by(Commodity_Description, Attribute_Description, Market_Year) %>%
  summarise(Value = case_when(
    Attribute_Description == "Yield" ~ mean(Value),
    Attribute_Description == "Extr. Rate, 999.9999" ~ mean(Value),
    TRUE ~ sum(Value)
        )) %>% 
  mutate(Country_Name = "World") %>% 
  select(Commodity_Description,Country_Name,Attribute_Description,Market_Year,Value)

world <- world %>% group_by(Commodity_Description,Country_Name,Attribute_Description,Market_Year) %>% 
  summarise(Value = mean(Value))

worldSinChina <- psd %>% filter(Country_Name %notin% c("China")) %>% 
  group_by(Commodity_Description, Attribute_Description, Market_Year) %>%
  summarise(Value = case_when(
    Attribute_Description == "Yield" ~ mean(Value),
    Attribute_Description == "Extr. Rate, 999.9999" ~ mean(Value),
    TRUE ~ sum(Value)
  )) %>% 
  mutate(Country_Name = "World (sin China)") %>% 
  select(Commodity_Description,Country_Name,Attribute_Description,Market_Year,Value)

worldSinChina <- worldSinChina %>% group_by(Commodity_Description,Country_Name,Attribute_Description,Market_Year) %>% 
  summarise(Value = mean(Value))


psd <- rbind(psd,world,worldSinChina)
rm(world,worldSinChina)

#girar y crear stock/consumo
psdwide <- psd %>% pivot_wider(names_from = Attribute_Description, values_from = Value)
psdwide <- psdwide %>% mutate("Stock/Consumo" = round(`Ending Stocks`/(`Domestic Consumption`+Exports)*100, digits = 1))

#PASO A Mtn, Mha y rendimiento a tn/ha
psdwide$Yield = round(psdwide$Yield, digits = 2)
psdwide$`Extr. Rate, 999.9999` = round(psdwide$`Extr. Rate, 999.9999`, digits = 2)
psdwide$`Area Harvested` = psdwide$`Area Harvested`/1000
psdwide$`Beginning Stocks`= psdwide$`Beginning Stocks`/1000
psdwide$`Domestic Consumption` = psdwide$`Domestic Consumption`/1000
psdwide$`Ending Stocks` = psdwide$`Ending Stocks`/1000
psdwide$Exports = psdwide$Exports/1000
psdwide$`Feed Dom. Consumption` = psdwide$`Feed Dom. Consumption`/1000
psdwide$`FSI Consumption` = psdwide$`FSI Consumption`/1000
psdwide$Imports = psdwide$Imports/1000
psdwide$Production = psdwide$Production/1000
psdwide$`Total Distribution` = psdwide$`Total Distribution`/1000
psdwide$`Total Supply` = psdwide$`Total Supply`/1000
psdwide$`TY Exports` = psdwide$`TY Exports`/1000
psdwide$`TY Imp. from U.S.` = psdwide$`TY Imp. from U.S.`/1000
psdwide$`TY Imports` = psdwide$`TY Imports`/1000
psdwide$Crush = psdwide$Crush/1000
psdwide$`Feed Waste Dom. Cons.` = psdwide$`Feed Waste Dom. Cons.`/1000
psdwide$`Food Use Dom. Cons.` = psdwide$`Food Use Dom. Cons.`/1000
psdwide$`Industrial Dom. Cons.` = psdwide$`Industrial Dom. Cons.`/1000
psdwide$SME = psdwide$SME/1000

psd <- psdwide %>% gather(., # el . llama a lo que esta atras del %>% 
       key   = Attribute_Description,
       value = Value,
       4:25) %>% 
  select(Commodity_Description,Country_Name,Attribute_Description,Market_Year,Value)

rm(psdwide)

#agrego Unit_Description
psd <- psd %>% mutate("Unit_Description" = case_when(
  Attribute_Description == "Yield" ~ "tn/ha",
  Attribute_Description == "Area Harvested" ~ "Mha",
  Attribute_Description == "Extr. Rate, 999.9999" ~ "Porcentaje (%)",
  Attribute_Description == "Stock/Consumo" ~ "Porcentaje (%)",
  TRUE ~ "Mtn"
)
)


#genero las listas
paises <- c("All",sort(unique(as.character(psd$Country_Name))))

atributos <- c("All",sort(unique(as.character(psd$Attribute_Description))))

commodity <- c("All", sort(unique(as.character(psd$Commodity_Description))))

#genero psd como % world (saco los worlds y s/c, yield, exc rate)
psdRelativo <- psd %>% filter(Country_Name %notin% c("World","World (sin China)") &
                                Attribute_Description %notin% c("Stock/Consumo","Extr. Rate, 999.9999","Yield")) %>%
  group_by(Commodity_Description, Attribute_Description, Market_Year) %>% 
  summarise("Value" = Value / sum(Value) * 100,
            "Country_Name" = Country_Name)

psdRelativo <- psdRelativo %>% mutate("Unit_Description" = "Porcentaje (%)") %>% 
  select(Commodity_Description,Country_Name,Attribute_Description,Market_Year,Value,Unit_Description)

#agrego mundo
world <- psdRelativo %>%
  group_by(Commodity_Description, Attribute_Description, Market_Year, Unit_Description) %>%
  mutate(Country_Name = "World",
         Value = 100) %>% 
  select(Commodity_Description,Country_Name,Attribute_Description,Market_Year,Value,Unit_Description)

world <- world %>% group_by(Commodity_Description,Country_Name,Attribute_Description,Market_Year,Unit_Description) %>% 
  summarise(Value = mean(Value))

#junto y limpio memoria
psdRelativo <- rbind(psdRelativo,world)
rm(world)

#redondeo de valores porcentuales
psdRelativo$Value = round(psdRelativo$Value, digits = 2)


ui <- fluidPage(theme = shinytheme("cerulean"),
  titlePanel(
             h2("Visor PSD-USDA")),
             h3(paste0("Datos actualizados ",DatosAl)),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(3,
           selectInput("com",
                       "Commodity:",
                       commodity, selected = "Barley")
    ),
    column(3,
           selectInput("atr",
                       "Atributo:",
                       atributos, selected = c("Production","Imports","Exports","Domestic Consumption","Ending Stocks","Stock/Consumo"),
                       multiple=TRUE, selectize=TRUE)
    ),
    column(3,
           selectInput("pais",
                       "País:",
                       paises,
                       selected = as.character("World"),
                        multiple=TRUE, selectize=TRUE)
    ),
  column(3,
         
         # Copy the line below to make a slider range 
         sliderInput("mkty", label = h3("Año Comercial"), min = min(as.numeric(psd$Market_Year)), 
                     max = max(as.numeric(psd$Market_Year)), value = c(min(as.numeric(psd$Market_Year)),max(as.numeric(psd$Market_Year)))
  )
)),

tabsetPanel(type = "tabs",
            tabPanel("Tabla", DT::dataTableOutput("table")),
            tabPanel("Gráficos", shinycssloaders::withSpinner(plotlyOutput("Plot"), type = 5) ),
            tabPanel("Configuración",
                     br(),
                     br(),
                    radioButtons(inputId = "seleccion_tipo_dato",label = "Vista de Datos",
                                      choiceValues = c("valores_absolutos","valores_relativos"),
                                      choiceNames = c("Valores absolutos", "Valores relativos al total mundial"),
                                      selected ="valores_absolutos",inline = T),
                         br(),
                    br()),
            tabPanel("Descarga",br(),br(),br(),downloadButton("download", "Descarga")),br(),br(),br()
            ),

h5("(*) Unidades de referencia:"),
h6("- Yield: tn/ha"),
h6("- Area Harvested: Mha"),
h6("- Extr. Rate, 999.9999: Porcentaje (%)"),
h6("- Stock/Consumo: Porcentaje (%)"),
h6("- Otras Variables: Mtn")


)

server <- function(input, output) {
  
  datos_salida <- reactive({
    if (input$seleccion_tipo_dato == "valores_absolutos") {
    if (input$com != "All") {
      psd <- psd[psd$Commodity_Description == input$com,]
    }
    if (input$atr != "All") {
      psd <- psd[psd$Attribute_Description %in% c(input$atr),]
    }
    if (input$pais != "All") {
      psd <- psd[psd$Country_Name %in% c(input$pais),]
    }
    if (input$mkty != "All") {
      psd <- subset(psd, Market_Year >= input$mkty[1] & Market_Year <= input$mkty[2])
    }
    } 
    else {
        if (input$com != "All") {
          psdRelativo <- psdRelativo[psdRelativo$Commodity_Description == input$com,]
        }
        if (input$atr != "All") {
          psdRelativo <- psdRelativo[psdRelativo$Attribute_Description %in% c(input$atr),]
        }
        if (input$pais != "All") {
          psdRelativo <- psdRelativo[psdRelativo$Country_Name %in% c(input$pais),]
        }
        if (input$mkty != "All") {
          psdRelativo <- subset(psdRelativo, Market_Year >= input$mkty[1] & Market_Year <= input$mkty[2])
        }
      }
  })
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    datos_salida()
  }))  
  
  
  
output$Plot <- renderPlotly({
  p <- ggplot(datos_salida(), aes(x=Market_Year,y=Value,color=Country_Name))+
    geom_line()+
    facet_wrap(~Attribute_Description, scales = "free_y") +
    theme_bw()
  p <- ggplotly(p)
  p
})

  
  output$download <- downloadHandler(
    
    filename = function() {
      paste("PSD-USDA ", input$com ," ",DatosAl, ".csv", sep="")
    },
    content = function(file) {
      df <- datos_salida()
      write.csv(x = df, file = file,fileEncoding = "UTF-8")
    }
    
    
  )
}


# Run the application 
shinyApp(ui = ui, server = server)



