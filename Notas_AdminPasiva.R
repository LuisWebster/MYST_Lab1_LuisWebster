# Remover todos los objetos del "Environment"
rm(list = ls())

# los 0s aceptados antes de expresas una cifra en notaci?n cient?fica
options("scipen"=100, "digits"=4)

### Cargas librer?as a utilizar
suppressMessages(library(plotly)) # Graficas interactivas
suppressMessages(library(Quandl)) # Descargar Precios
suppressMessages(library(PortfolioAnalytics)) # Teor?a Moderna de Portafolios
suppressMessages(library(ROI)) # Optimizacion para portafolio

suppressMessages(library(knitr))  # Opciones de documentaci?n + c?digo
suppressMessages(library(kableExtra)) # Tablas en HTML
options(knitr.table.format = "html") 
Capital_Inicial <- 10000

# Cargar el token de QUANDL
Quandl.api_key("5XjwpZy_ymVwPuxzzGbY")

# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  Columns <- cs
  Tickers <- tk[2]
  Fecha_In <- fs[1]
  Fecha_Fn <- fs[2]
  
  # Funcion para descargar N cantidad de activos desde QUANDL
  # -- Dependencias: QUANDL
  # -- Columns : columnas a incluir : character : c("date", "adj_close", ... )
  # -- Tickers : Tickers o claves de pizarra de los activos : character : "TSLA"
  # -- Fecha_In : Fecha Inicial : character : "2017-01-02"
  # -- Fecha_Fn : Fecha Final : character : "2017-08-02"
  
  # Peticion para descargar precios
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}

# Tickers de accciones y datos a solicitar a QUANDL
tk <- c("TSLA", "BBY", "HD")
cs <- c("date", "adj_close")

# Fecha inicial y fecha final
fs <- c("2015-08-01", "2017-08-01")

# Descargar Precios y Calcular rendimientos
Datos <- list()

for(i in 1:length(tk))
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tk[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

names(Datos) <- tk

for(i in 1:length(tk))
  Datos[[i]]$adj_close_r <- c(0, diff(log(Datos[[i]]$adj_close)))

Rends <- xts(x = cbind(Datos[[1]]$adj_close_r, Datos[[2]]$adj_close_r, Datos[[3]]$adj_close_r),
             order.by = Datos[[1]]$date)[-1]
names(Rends) <- tk


Port1<- portfolio.spec(assets=tk)
Port1<- add.constraint(portfolio = Port1, type="full_investment")

#Restricción 2: Limites superior e inferior para el valor de los pesos individuales
Port1<- add.constraint(portfolio=Port1, type="box", min=c(0.01,0.01,0.01),max=c(0.7,0.7,0.7))

Port1<- add.objective(portfolio = Port1, type="return", name="mean")

Port1<- optimize.portfolio(R=Rends, portfolio = Port1, optimize_method="random", trace=TRUE, search_size = 5000)

Portafolios<-vector("list", length=length(Port1$random_portfolio_objective_results))

for(i in 1:length(Port1$random_portfolio_objective_results)) {
  
  Portafolios[[i]]$Pesos <- Port1$random_portfolio_objective_results[[i]]$weights
  Portafolios[[i]]$Medias <- Port1$random_portfolio_objective_results[[i]]$objective_measures$mean
  
  Portafolios[[i]]$Vars <- var.portfolio(R= Port1$R, weights= Portafolios[[i]]$Pesos)
  names(Portafolios[[i]]$Medias)<- NULL

}


df_Portafolios <- data.frame(matrix(nrow=length(Port1$random_portfolio_objective_results),
                                    ncol=3,
                                    data=0))

colnames(df_Portafolios) <- c("Rend","Var","Clase")


for(i in 1:length(Port1$random_portfolio_objective_results)){
  i<-2
  df_Portafolios$Rend[i] <- round(Portafolios[[i]]$Medias*252,4)
  df_Portafolios$Var[i] <- round(sqrt(Portafolios[[i]]$Vars)*sqrt(252),4)
  df_Portafolios$Clase[i] <- "No-Frontera"
  
  for(k in 1:length(tk)){
    
    k<-2
    df_Portafolios[i,paste("Peso_", tk[k], sep="")] <- Portafolios[[i]]$Peso[k]
    
    df_Portafolios[i,paste("Titulos_ini_",tk[k],sep="")] <- 
      (Capital_Inicial*Portafolios[[i]]$Pesos[k])%/%Datos[[k]]$adj_close[1]
  }
}



 


