# -- Laboratorio 1

#Remover todos los objetos del "Environment"
rm(list=ls())

#los 0s aceptados antes de expresar una cifra en notación científica
options("scipen"=100, "digits"=4)

### Cargas librerias a utilizar
suppressMessages(library(plotly)) # Graficas interactivas
suppressMessages(library(Quandl)) # Descargar Precios
suppressMessages(library(PortfolioAnalytics)) # Teoría Moderna de Portafolios
suppressMessages(library(ROI)) # Optimizacion para portafolio
suppressMessages(library(kableExtra)) # Tablas en HTML

options(knitr.table.format = "html") 



# Cargar el token de QUANDL
Quandl.api_key("5XjwpZy_ymVwPuxzzGbY")


# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  
  # Funcion para descargar N cantidad de activos desde QUANDL
  # -- Dependencias: QUANDL
  # -- Columns : columnas a incluir : character : c("date", "adj_close", ... )
  # -- Tickers : Tickers o claves de pizarra de los activos : character : "TSLA"
  # -- Fecha_In : Fecha Inicial : character : "2017-01-02"
  # -- Fecha_Fn : Fecha Final : character : "2017-08-02"
  
  # Peticion para descargar precios
  Datos <- Quandl.datatable(code = "WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}


#Tickers de acciones y datos a solicitar a QUANDL
tk <- c("TSLA","BBY","HD")
cs <- c("date","adj_close")

#Fecha inicial y Fecha Final
fs <- c("2015-08-01","2016-08-01")

#Descargar Precios y Calcular rendimientos
Datos <- list()

for (i in 1:length(tk)){
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tk[i], Fecha_In=fs[1], Fecha_Fn=fs[2])
}



Hols  






