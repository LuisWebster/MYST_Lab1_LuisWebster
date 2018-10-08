#Librerias
library(plyr)
library(openxlsx)

# Remover todos los objetos del "Environment"
rm(list = ls())

# los 0s aceptados antes de expresas una cifra en notaci?n cient?fica
options("scipen"=100, "digits"=4)

### Cargas librer?as a utilizar
suppressMessages(library(plotly)) # Graficas interactivas
suppressMessages(library(Quandl)) # Descargar Precios
suppressMessages(library(PortfolioAnalytics)) # Teor?a Moderna de Portafolios
suppressMessages(library(ROI)) # Optimizacion para portafolio
suppressMessages(library(xlsx))
suppressMessages(library(knitr))  # Opciones de documentaci?n + c?digo
suppressMessages(library(kableExtra)) # Tablas en HTML
options(knitr.table.format = "html") 
suppressMessages(library(openxlsx))

#Require
require(DEoptim)
require(TTR)
require(xts)

# Cargar el token de QUANDL
Quandl.api_key("KAxj_3rAYHS5kZnBoSf2")

Capital_Inicial <- 100000

# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  
  # Funcion para descargar N cantidad de activos desde QUANDL
  # -- Columns : columnas a incluir : character : c("date", "adj_close", ... )
  # -- Tickers : Tickers o claves de pizarra de los activos : character : ""
  # -- Fecha_In : Fecha Inicial : character : "2017-01-02"
  # -- Fecha_Fn : Fecha Final : character : "2017-08-02"
  
  # Peticion para descargar precios
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}


DatosETF <- read.xlsx("C:\\Users\\LuisW\\Desktop\\ITESO\\XI Semestre\\Trading\\Códigos\\Práctica1\\MYST_Lab1_LuisWebster\\IVV.xlsx", 
                       sheetIndex= 1) #Sheet Index = Pestaña importada

tk <- as.character(na.omit(DatosETF[which(DatosETF[,1]=="Ticker")+1:length(DatosETF[,1]),1]))
cs <- c("date", "adj_close")



# Fecha inicial y fecha final
fs <- c("2016-08-01", "2018-08-01")

# Descargar Precios y Calcular rendimientos
Datos <- list()

for(i in 1:length(tk))
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tk[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

names(Datos) <- tk

#Reordenar precios con base a fechas del más antiguo al más reciente
for(i in 1:length(tk)){
  Datos[[i]]<-Datos[[i]][order(Datos[[i]][,1]),]
}

longitudes <- c()

for (i in 1:length(Datos)){
  longitudes[i] <- length(Datos [[i]]$date)
}




#original, regresar si no corren mejoras
maximo <- max(longitudes)-1

#Obtener le frequencia de las longitudes que más se repiten
longs <- count(longitudes)

#Encontrar la que más se repite
l <- longs[which.max(longs$freq),1]

#Que todos estén parejos
completos <- which(longitudes == maximo)

#Tener la lista de activos que tienen la misma cantidad de precios
DatosN <- Datos[completos]



#vector para almacenar columnas de interes
columnas <- c()
nuevos <- c()

#Funcion para repetir una funcion por cada columna del data.frame
Precios <- do.call(cbind, DatosN)

#Crear vector con nombre de colimnas de interes = "nombredeactivo.adj_close_r"
for (i in 1:length(tk)) {
  nuevos[i] <- paste(tk[i], ".adj_close", sep = "")
  
}

#Extraer 1 reglon para obtener los nombres de las columnas 
nombres <- colnames(Precios[1,(names(Precios)%in% nuevos)])

#Elegir una columna Date  y las demas columnas de rendimientos
Precios <- Precios[,(names(Precios) %in% nuevos)]
row.names(Precios) <- DatosN[[1]]$date

# Reasignar nombres al data.frame
tk_completos <- as.character(tk[completos])
colnames(Precios) <- tk_completos

Historico <- data.frame("Date" = row.names(Precios),
                        "Precio" = Precios[,1],
                        "R_Precio" = 0,
                        "Estatus_Señal" = 0,
                        "R_Activo" = 0,
                        "R_Cuenta" = 0,
                        "Capital" = 0, "Flotante" = 0, "Balance" = 0, "Titulos" = 0,
                        "Titulos_a"=0,
                        "Comisiones" = 0, "Comisiones_a" = 0, "Mensaje" = NA)




# Parte 2 del código extraído de https://github.com/ITESOIF/MyST/blob/master/Notas/Notas_AdminActiva/Notas_AdminActiva.r

# *Date*       : Fecha (Proviene desde los precios que bajaron).
# *Precio*     : Precio individual del activo.
# *R_Precio*   : Rendimiento diario del precio (dia a dia).
# *Estatus_Señal* : El estatus de si el rendimiento cumple con Regla0_R
# *R_Activo*   : Rendimiento acumulado del precio (Cada dia respecto al precio inicial).
# *Flotante*   : El valor de la posición (Precio diario x Títulos)
# *Capital*    : El dinero no invertido (Equivalente a Efectivo).
# *Balance*    : El valor del portafolio (Precio diario X Titulos).
# *R_Cuenta*   : Balance + Capital (Cada dia respecto al capital inicial).
# *Titulos*    : Acciones que se tienen.
# *Titulos_a*  : Titulos acumulados.
# *Operacion*  : Indicativo de Compra (1), Mantener (0), Venta (-1).
# *Comisiones* : 0.0025 o 0.25% por el valor de la transaccion.
# *Comisiones acumuladas* : Comisiones acumuladas del portafolio
# *Mensaje*    : Un texto que indique alguna decision o indicativo de que ocurrio algo.


Regla0_R <- -0.005  # Considerar una oportunidad de compra en un rendimiento de -3% o menor.
Regla1_I <- 0.20   # Porcentaje de capital para comprar titulos para posicion Inicial.
Regla2_P <- 0.25   # Se utiliza el P% del L capital restante en cada compra.
Regla3_W <- tk_completos # Se realiza la misma estrategia para todos los activos en el portafolio.
Regla4_C <- 0.0025 # Comisiones pagadas por compra.
Regla5_K <- 100000 # Capital Inicial.

# -- ----------------------------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #

#CALCULAR VALORES INICIALES DEL PORTAFOLIO

# -- Calcular los Titulos de posicion inicial
Historico$Titulos[1] <- (Regla5_K*Regla1_I)%/%Historico$Precio[1]

# -- Valor inicial de los títulos acumulados Historico$Titulos_a
Historico$Titulos_a[1] <- Historico$Titulos[1] 

# -- Se calculan comisiones iniciales
Historico$Comisiones[1] <- Historico$Titulos[1]*Historico$Precio[1]*Regla4_C

# -- Calcular las comisiones acumuladas
Historico$Comisiones_a[1] <- Historico$Comisiones[1]

# -- Calcular el valor flotante
Historico$Flotante[1] <- Historico$Titulos_a[1]*Historico$Precio[1]

# -- Todo remanente se dejará registrado en la cuenta de efectivo.
Historico$Capital[1] <- Regla5_K-Historico$Balance[1]-Historico$Comisiones[1]

# -- Calcular el Balance
Historico$Balance[1] <- Historico$Capital[1]*Historico$Flotante[1]

# -- Iniciamos con una postura de mantener.
Historico$Operacion[1] <- "Posicion Inicial"

# -- El rendimiento de capital en el tiempo 1 es 0
Historico$R_Cuenta[1] <- 0

# -- Mensaje inicial
Historico$Mensaje[1] <- "Inicializacion de cartera"

#Rendimiento diario del precio del activo
# -- Calcular R_Precio
Historico$R_Precio <- round(c(0, diff(log(Historico$Precio))),4)



# -- Revisar Estatus de R_Precio
for (i in 1:length(Historico$Date))
{
  Historico$Estatus_Señal[i] <- Historico$R_Precio[i]<= Regla0_R
  
  if(Historico$Estatus_Señal[i] == TRUE){ 
    Historico$Estatus_Señal[i] <- "SEÑAL"
      }
    else
    {
      Historico$Estatus_Señal[i] <- "NO SEÑAL"
    }
}



# -- Calcular R_Activo
PosturaInicial <- Regla5_K %% Historico$Precio[1]

for(i in 1:length(Historico$Date)){
  Historico$R_Activo[i] <- (PosturaInicial*Historico$Precio[i])/(PosturaInicial*Historico$Precio[1])-1
}


# -- ------------------------------------ -- #
# -- ------------------------------------ -- #
# -- ------------------------------------ -- #





#LABORATORIO 2 PARTE 2  
# for para poder hacer cálculos del portafolio

for(i in 2:length(Historico$Date)){
  
  if(Historico$R_Precio[i] <= Regla0_R)
  { # Generar Señal
    # Establecer capital actual, inicialmente, igual al capital anterior
    Historico$Capital[i] <- Historico$Capital[i-1]
    
    if(Historico$Capital[i] > 0){ # Si hay capital
      
      if(Historico$Capital[i]*Regla2_P > Historico$Precio[i]){ # Si Capital minimo
        Historico$Operacion[i] <- "Compra"
        Historico$Titulos[i]   <- (Historico$Capital[i-1]*Regla2_P)%/%Historico$Precio[i]
        compra <- Historico$Precio[i]*Historico$Titulos[i]  
        Historico$Comisiones[i] <- compra*Regla4_C
        Historico$Comisiones_a[i] <- Historico$Comisiones_a[i-1]+Historico$Comisiones[i]
        Historico$Capital[i] <- Historico$Capital[i-1]-Historico$Comisiones[i]-(Historico$Precio[i]*Historico$Titulos[i])
        Historico$Titulos_a[i] <- Historico$Titulos_a[i-1]+Historico$Titulos[i]
        Historico$Flotante[i] <- Historico$Precio[i]*Historico$Titulos_a[i]
        Historico$Mensaje[i] <- "Compra Exitosa 1"
        Historico$Balance[i] <- Historico$Capital[i]+Historico$Flotante[i]
        Historico$R_Cuenta[i] <- ((Historico$Balance[i])/Regla5_K)-1
        }
    }
      else { # No hubo capital mínimo para 1 operación
        Historico$Operacion[i]<- "No hay capital mínimo"
        Historico$Titulos[i] <- 0
        Historico$Comisiones[i] <- 0
        Historico$Comisiones_a[i] <- Historico$Comisiones_a[i-1]+Historico$Comisiones[i]
        Historico$Titulos_a[i] <- Historico$Titulos_a[i-1]+Historico$Titulos[i]
        Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precio[i]
        Historico$Capital[i] <- Historico$Capital[i-1]
        Historico$Balance[i] <- Historico$Capital[i]+Historico$Flotante[i] 
        Historico$R_Cuenta[i] <- (Historico$Balance[i]/Regla5_K)-1
        Historico$Mensaje[i] <- "Hubo señal pero no hubo capital minimo"
      }
  }
  else { # Sin señal
    Historico$Operacion[i] <- 0
    Historico$Titulos[i] <- 0
    Historico$Comisiones[i] <- 0
    Historico$Comisiones_a[i] <- Historico$Comisiones_a[i-1]
    Historico$Titulos_a[i] <- Historico$Titulos_a[i-1]
    Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precio[i]
    Historico$Capital[i] <- Historico$Capital[i-1]
    Historico$Balance[i] <- Historico$Capital[i]+Historico$Flotante[i]
    Historico$R_Cuenta[i] <- ((Historico$Balance[i])/Regla5_K)-1
    Historico$Mensaje[i] <- "Mantener posición - no hay señal"
  } 
}


#Gráfica de Rendimiento Activo vs Rendimiento Cuenta
plot_ly(Historico) %>%
  add_trace(x = ~Date, y = ~round(R_Activo,4), type = 'scatter', mode = 'lines', name = 'Activo',
            line = list(color = 'red')) %>%
  add_trace(x = ~Date, y = ~round(R_Cuenta,4), type = 'scatter', mode = 'lines', name = 'Cuenta',
            line = list(color = 'blue')) %>% 
  layout(title = "Rend del activo VS Rend de la cuenta",
         xaxis = list(title = "Fechas", showgrid = T),
         yaxis = list(title = "Rendimiento"), 
         legend = list(orientation = 'h', y = -0.25, x = 0.5))




#Hay que revisar el if, ya que cuando se genera una señal pero no hay capital suficiente para invertir, el código no
#se está corriendo

#Calcular el Sharpe Ratioy Sortino Ratio para R_Activo y R_Cuenta para set de Reglas 1
Risk_free_rate = 0.07
SharpeRatio(Historico[,5,drop=FALSE], Risk_free_rate, FUN="StdDev")
SortinoRatio(Historico[,5,drop=FALSE],Risk_free_rate)


#Error in checkData(R) : 
#The data cannot be converted into a time series.  If you are trying to pass in names from a data object with one column, 
#you should use the form 'data[rows, columns, drop = FALSE]'.  Rownames should have standard date formats, 
#such as '1985-03-15'. 



