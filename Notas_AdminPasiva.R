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

#Capital Inicial del Portafolio
Capital_Inicial <- 10000

# Cargar el token de QUANDL
Quandl.api_key("5XjwpZy_ymVwPuxzzGbY")

# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {

  # Peticion para descargar precios
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers, date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}

# Tickers de accciones y datos a solicitar a QUANDL
tk <- c("AXP", "V", "JPM")
cs <- c("date", "adj_close")

# Fecha inicial y fecha final
fs <- c("2015-08-01", "2017-08-01")

# Descargar Precios y Calcular rendimientos
Datos <- list()

for(i in 1:length(tk))
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tk[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

names(Datos) <- tk


#Reordenar precios con base a fechas del más antiguo al más reciente

for(i in 1:length(tk)){
  Datos[[i]]<-Datos[[i]][order(Datos[[i]][,1]),]
}


for(i in 1:length(tk))
  Datos[[i]]$adj_close_r <- c(0, diff(log(Datos[[i]]$adj_close)))

Rends <- xts(x = cbind(Datos[[1]]$adj_close_r, Datos[[2]]$adj_close_r, Datos[[3]]$adj_close_r),
             order.by = Datos[[1]]$date)[-1]
names(Rends) <- tk


Port1<- portfolio.spec(assets=tk)


#Restricciones del portafolio
Port1<- add.constraint(portfolio = Port1, type="full_investment")
Port1<- add.constraint(portfolio=Port1, type="box", min=c(0.01,0.01,0.01),max=c(0.7,0.7,0.7))
Port1<- add.objective(portfolio = Port1, type="return", name="mean")
Port1<- optimize.portfolio(R=Rends, portfolio = Port1, optimize_method="random", trace=TRUE, search_size = 5000)
Portafolios<-vector("list", length=length(Port1$random_portfolio_objective_results))


#los corchetes dobles [[]] se utilizan para indexar listas. Dicho elemento se irá creando con el ciclo for
for(i in 1:length(Port1$random_portfolio_objective_results)) 
{
  Portafolios[[i]]$Pesos <- Port1$random_portfolio_objective_results[[i]]$weights
  Portafolios[[i]]$Medias <- Port1$random_portfolio_objective_results[[i]]$objective_measures$mean
  Portafolios[[i]]$Vars <- var.portfolio(R= Port1$R, weights= Portafolios[[i]]$Pesos)
  names(Portafolios[[i]]$Medias)<- NULL
}


#Creación de un df para almacenar todos los valores que se obtendrán de la lista "Portafolios"
df_Portafolios <- data.frame(matrix(nrow=length(Port1$random_portfolio_objective_results),ncol=3,data=0))

colnames(df_Portafolios) <- c("Rend","Var","Clase")


#ciclos for para llenar de datos el dataframe df_Portafolios

for(i in 1:length(Port1$random_portfolio_objective_results))
{
  df_Portafolios$Rend[i] <- round(Portafolios[[i]]$Medias*252,4)
  df_Portafolios$Var[i] <- round(sqrt(Portafolios[[i]]$Vars)*sqrt(252),4)
  df_Portafolios$Clase[i] <- "No-Frontera"
  
  for(k in 1:length(tk))
  {
    df_Portafolios[i,paste("Peso_", tk[k], sep="")] <- Portafolios[[i]]$Peso[k]
    
    df_Portafolios[i,paste("Titulos_ini_",tk[k],sep="")] <- 
      (Capital_Inicial*Portafolios[[i]]$Pesos[k])%/%Datos[[k]]$adj_close[1]
  }
}





#GRAFICAR
### Nuevas líneas de código
Plot_portafolios <- plot_ly(x=df_Portafolios$Var, y=df_Portafolios$Rend, type='scatter', mode='markers',
                            name = "Portafolios", marker = list(color="grey", size=7), hoverinfo='text', 
                            text = ~paste('Rendimiento: ', paste(df_Portafolios$Rend*100, "%") ,
                                          '<br> Riesgo: ', paste(df_Portafolios$Var*100, "%") )) %>%
  
  
  layout(title = "Portafolios (Markowitz)",
         xaxis = list(title = "Riesgo (Desviaci?n Est?ndar Anualizada)",
                      showgrid = F),
         yaxis = list(title = "Valor Esperado (Rendimiento Anualizado)"),
         legend = list(orientation = 'h', y = -0.25))


Plot_portafolios
Port_1 <- df_Portafolios[which.max(df_Portafolios$Rend),]
# Portafolio con m?nima varianza
Port_2 <- df_Portafolios[which.min(df_Portafolios$Var),]


# Tasa libre de riesgo
rf <- 0.0025          
# Rendimiento de portafolio
rp <- df_Portafolios$Rend
# Varianza de portafolio
sp <- df_Portafolios$Var
# Indice de sharpe
sharpe <- (rp-rf)/sp

# Portafolio con m?ximo Sharpe ratio 
Port_3 <- df_Portafolios[which.max(sharpe),]
Ports <- cbind(rbind(Port_1, Port_2, Port_3),
               "Portafolio" = c("M?ximo Rendimiento","M?nima Varianza","M?ximo Sharpe Ratio"))


Plot_portafolios <- plot_ly(x=df_Portafolios$Var, y=df_Portafolios$Rend, type='scatter', mode='markers',
                            name = "Portafolios", marker = list(color="grey", size=7), hoverinfo='text', 
                            text = ~paste('Rendimiento: ', paste(df_Portafolios$Rend*100, "%") ,
                                          '<br> Riesgo: ', paste(df_Portafolios$Var*100, "%") )) %>% 
  
  layout(title = "Portafolios (Markowitz)",
         xaxis = list(title = "Riesgo (Desviaci?n Est?ndar Anualizada)",
                      showgrid = F),
         yaxis = list(title = "Valor Esperado (Rendimiento Anualizado)"),
         legend = list(orientation = 'h', y = -0.25)) %>%
  add_trace(x = ~Ports$Var[1], y = ~Ports$Rend[1], name = Ports$Portafolio[1],
            mode = 'marker', marker = list(color="red", size=10)) %>%
  add_trace(x = ~Ports$Var[2], y = ~Ports$Rend[2], name = Ports$Portafolio[2],
            mode = 'marker', marker = list(color="blue", size=10)) %>%
  add_trace(x = ~Ports$Var[3], y = ~Ports$Rend[3], name = Ports$Portafolio[3],
            mode = 'marker', marker = list(color="orange", size=10))

Plot_portafolios
# Pesos y titulos iniciales, de todos los activos, para los 3 portafolios
Pesos_Titulos <- Ports[,-c(1,2,3)]
# Encontrar las columnas cuyo nombre contenga "Titulos_ini", con esas encontraremos m?s f?cil los t?tulos
# por portafolio por activo
Ind <- grep(pattern = "Titulos_ini",x = colnames(Pesos_Titulos))
Historicos_Ports <- data.frame("Date" = Datos[[1]]$date)
# Crear data frame que contendr? los datos finales de cada estrategia
for(i in 1:length(Ports[,1])) {
  Historicos_Ports[[paste("Portafolio_",i,sep="")]] <- 
    (Datos[[1]]$adj_close*Pesos_Titulos[i,Ind[1]]  + 
       Datos[[2]]$adj_close*Pesos_Titulos[i,Ind[2]] +
       Datos[[3]]$adj_close*Pesos_Titulos[i,Ind[3]])
}
plot_ly(Historicos_Ports) %>%
  add_trace(x = ~Date, y = ~round(Portafolio_1,2), type = 'scatter', mode = 'lines', name = 'M?ximo Rendimiento',
            line = list(color = 'red'), hoverinfo = "text", text = ~paste('Port_1',round(Portafolio_1,2))) %>%
  add_trace(x = ~Date, y = ~round(Portafolio_2,2), type = 'scatter', mode = 'lines', name = 'M?nima Varianza',
            line = list(color = 'blue'), hoverinfo = "text", text = ~paste('Port_2',round(Portafolio_2,2)))  %>%
  add_trace(x = ~Date, y = ~round(Portafolio_3,2), type = 'scatter', mode = 'lines', name = 'M?ximo Sharpe Ratio',
            line = list(color = 'orange'), hoverinfo = "text", text = ~paste('Port_3',round(Portafolio_3,2)))%>% 
  layout(title = "3 Portafolios distintos objetivos",
         xaxis = list(title = "Fechas", showgrid = T),
         yaxis = list(title = "Balance"), 
         legend = list(orientation = 'h', y = -0.25, x = 0.5))


### Comentarios/Conclusiones financieras

# La combinación de American Express, Visa y JPM nos da un portafolio interesante para los inversionistas que tienen
# preferencia por los portafolios con un "alto riesgo" ya que podemos encontrar un portafolio con un rendimiento aproximado
# de 16.98%, pero con una volatilidad de 19.92%. Por otro lado tenemos el portafolio con el mayor Sharpe Ratio (recordemos
# que el sharpe ratio nos dice el exceso de rendimiento que recibiremos por cada unidad adicional de riesgo asumida) cuyo
# rendimiento es de 13.45% con una volatilidad de 18.46%. Finalmente también existe un portafolio que podría adaptarse
# a cliente con un perfil más conservador, pues tiene una varianza de 17.37% pero su rendimiento disminuye, colocándose
# en 12.71%. Recordemos que para crear esta frontera eficiente de Markowitz, se estableció un rendimiento fijo con una
# tasa de apenas 0.25%. Se pensará en realizar más simulaciones a futuro considerando una tasa más elevada (similar a la
# tasa que actualmente tienen los Bonos del Tesoro o los Cetes. 

# Finalmente, sabemos que el portafolio con el mayor rendimiento está compuesto de la siguiente manera:
# - 1% AXP (American Express)
# - 30.4% V (Visa)
# - 68.6% JPM (JP Morgan)
