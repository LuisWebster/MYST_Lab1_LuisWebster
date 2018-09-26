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
                        "Capital" = 0, "Balance" = 0, "Titulos" = 0,
                        "Titulos_a"=0,
                        "Comisiones" = 0, "Mensaje" = NA, "R_Cartera")




# Parte 2 del código extraído de https://github.com/ITESOIF/MyST/blob/master/Notas/Notas_AdminActiva/Notas_AdminActiva.r

# *Date*       : Fecha (Proviene desde los precios que bajaron).
# *Precio*     : Precio individual del activo.
# *R_Precio*   : Rendimiento diario del precio (dia a dia).
# *Estatus_Señal* : El estatus de si el rendimiento cumple con Regla0_R
# *R_Activo*   : Rendimiento acumulado del precio (Cada dia respecto al precio inicial).
# *Capital*    : El dinero no invertido (Equivalente a Efectivo).
# *Balance*    : El valor del portafolio (Precio diario X Titulos).
# *R_Cuenta*   : Balance + Capital (Cada dia respecto al capital inicial).
# *Titulos*    : Acciones que se tienen.
# *Titulos_a*  : Titulos acumulados.
# *Operacion*  : Indicativo de Compra (1), Mantener (0), Venta (-1).
# *Comisiones* : 0.0025 o 0.25% por el valor de la transaccion.
# *Mensaje*    : Un texto que indique alguna decision o indicativo de que ocurrio algo.
# *R_Cartera*  : Rendimiento diario de la cartera

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

# -- Calcular el Balance
Historico$Balance[1] <- Historico$Titulos[1]*Historico$Precio[1]

# -- Todo remanente se dejará registrado en la cuenta de efectivo.
Historico$Capital[1] <- Regla5_K-Historico$Balance[1]-Historico$Comisiones[1]

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
for(i in 1:length(Historico$Date)){
  Historico$R_Activo[i] <- round((Historico$Precio[i]/Historico$Precio[1])-1,2)
}

# -- Calcular R_Cartera para i=1
Historico$R_Cartera[1] <- (Historico$Balance[1]+Historico$Capital[1])%/% Regla5_K


# -- Calcular R_Cartera
#for (i in 1:length(Historico$Date)){
#  Historico$R_Cartera[i] <- (Historico$Balance[i]+Historico$Capital[i])%/% Regla5_K
#}


# -- ------------------------------------ -- #
# -- ------------------------------------ -- #
# -- ------------------------------------ -- #





#LABORATORIO 2 PARTE 2  
# for para poder hacer cálculos del portafolio

for(i in 2:length(Historico$Date)){
  
  if(Historico$R_Precio[i] <= Regla0_R){ # Generar Señal
    
    # Establecer capital actual, inicialmente, igual al capital anterior
    Historico$Capital[i] <- Historico$Capital[i-1]
    
    if(Historico$Capital[i] > 0)
      { # Si hay capital
      
      if(Historico$Capital[i]*Regla2_P > Historico$Precio[i])
        { # Si Capital minimo
        
        Historico$Balance[i] <- Historico$Precio[i]*Historico$Titulos[i]
        Historico$R_Cuenta[i] <- Capital_Inicial + Historico$Balance[i] 
        
        Historico$Operacion[i] <- "Compra"
        Historico$Titulos[i]   <- (Historico$Capital[i]*Regla2_P)%/%Historico$Precio[i]
        
        compra <- Historico$Precio[i]*Historico$Titulos[i]  
        Historico$Comisiones[i] <- compra*Regla4_C
        
        Historico$Titulos_a[i] <- Historico$Titulos_a[i-1]+Historico$Titulos[i]
        Historico$Mensaje[i] <- "Compra Exitosa"
        
        #Revisar si es la operación correcta para el cálculo del capital
        Historico$Capital[i] <- Historico$Capital[i-1]-(Historico$Precio[i]*Historico$Titulos[i])-Historico$Comisiones[i]
        
        #Revisar si es correcto
        #Rendimiento de la cartera para el tiempo i
        Historico$R_Cartera[i] <- (Historico$Balance[i]+Historico$Capital[i])%/% Regla5_K
        }
      }
    else { # No hubo capital
    }
  }
  else { # Sin señal
    # Establecer capital actual, inicialmente, igual al capital anterior
    Historico$Capital[i] <- Historico$Capital[i-1]
    Historico$Balance[i] <- Historico$Precio[i]*Historico$Titulos_a[i]
    Historico$Titulos_a[i] <- Historico$Titulos_a[i-1]+Historico$Titulos[i]
    Historico$R_Cuenta[i] <- Capital_Inicial + Historico$Balance[i] 
    Historico$Operacion[i] <- "Mantener" #se mantiene ya que no hay señal de compra 
    Historico$Comisiones[i] <- 0 #la comisión es cero ya que no se realizó alguna compra
    Historico$Mensaje[i] <- "Mantener posición"
    
    #Revisar si es correcto
    Historico$R_Cartera[i] <- (Historico$Balance[i]+Historico$Capital[i])%/% Regla5_K #rendimiento de la cartera en el tiempo
  }
}