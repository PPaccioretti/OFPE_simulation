library(tmap)
library(RandomFields)
library(ggplot2)
library(ggpubr)
library(stars)
source("src/simular.R")
RFoptions(seed = 11)
mediaCampo <- 10


# rsv <- function(sill, nugget) {
#   (sill / (sill + nugget)) * 100
#   
# }
# 
# ALTO RSV
# rsv(7.85, 0.94)
# MEDIO RSV
# rsv(4.81, 2.54)


# Rango	Psill	Nugget
# 38.78	9.69	0.06
### Alto
# rango = 47.11,
# sill = 7.85,
# nugget = 0.94,
# media = 80 
### Medio 
# rango = 56.82,
# sill = 4.81,
# nugget = 2.54,
# media = 80 
###
# Rango	Psill	Nugget
# 45.48	1.26	1.51
# Escenarios simulacion ----
#Estima el modelo y Simula los datos
## Simulacion ----
### Minima espacialidad ----
# simuMinEsp <- simular(rango = 38.78,
#                        sill = 9.69,
#                        nugget = 0.06,
#                        media = mediaCampo )
### Alto RSV ----
simuAltoEsp <- simular(rango = 47.11,
                sill = 7.85,
                nugget = 0.94,
                media = mediaCampo )

### Media RSV ----
simuMedioEsp <- simular(rango = 56.82,
                        sill = 4.81,
                        nugget = 2.54,
                        media = mediaCampo )

### Maxima espacialidad ----
# simuMaxEsp <- simular(rango = 45.48,
#                         sill = 1.26,
#                         nugget = 1.51,
#                         media = mediaCampo )

## Grafico Simulacion ----
### Minima espacialidad ----
# ggplot_minimo <- ggplot_simulacion(simuMinEsp)
### Baja espacialidad ----
ggplot_bajo <- ggplot_simulacion(simuAltoEsp)
### Media espacialidad ----
ggplot_medio <- ggplot_simulacion(simuMedioEsp)
### Maxima espacialidad ----
# ggplot_maximo <- ggplot_simulacion(simuMaxEsp)
 

figura <-
  ggarrange(
    # ggplot_minimo,
    ggplot_bajo,
    ggplot_medio,
    # ggplot_maximo,
    ncol = 2,
    nrow = 1,
    labels = "auto",
    common.legend = TRUE,
    legend = 'bottom'
  )

ggsave('images/SpatialCorrExample.png', figura,
       width = 15,
       height = 16/2,
       units = "cm")


#Crea un data.frame con la simulacion
Simulacion <-
  data.frame(expand.grid(list("x" = x, "y" = y)), "Error" = simu)
#   Cantidad de parcelas en la simulacion: nrow(Simulacion)

#Categoriza las coordenadas del eje X en Nzonas (Sumar 5 y 10 cte???)
EfZona <-
  findInterval(Simulacion$x, seq(0, xcampo, length = Nzonas + 1)[-c(1, Nzonas +
                                                                      1)],
               left.open = TRUE) * (Heterog * (sill + nugget))
#   Cantidad de parcelas por zona: table(as.factor(EfZona))

#Junta la simulacion con la zona
Simulacion <-
  data.frame(Simulacion,
             "Zona" = EfZona,
             "ErrorConEfecto" = Simulacion$variable1 + EfZona)
# plot(Simulacion$x,Simulacion$y,col=as.factor(Simulacion$Zona))
# abline(v=seq(0,xcampo, length=Nzonas+1)[-c(1,Nzonas+1)])

#Divide al eje y en Ndis=3 zonas para repartir los Diseños
NDis <- 3
NombresDis <- c("DBCA", "DCA", "DFR")
AleatFranja <- TRUE
Dis <-
  as.factor(findInterval(Simulacion$y, seq(0, ycampo, length = NDis + 1)[-c(1, NDis +
                                                                              1)],  left.open = TRUE))
#   Cantidad de parcelas por Disnios:  table(Dis)
#                                      table(Dis, EfZona)
#Asigna **ALEATORIAMENTE** a cada intervalo un diseño
levels(Dis) <-
  NombresDis[sample(1:NDis)] #[c(2, 3, 1)] 

#Junta la simulación con el Diseño
Simulacion <- data.frame(Simulacion, "Disenio" = Dis)

#   Cantidad de parcelas por Disnios:  table(Simulacion$Disenio)
#                                      table(Simulacion$Disenio, Simulacion$Zona)
# plot(Simulacion$x,Simulacion$y,col=as.factor(paste(Simulacion$Disenio,Simulacion$Zona, sep="_")))
# plot(Simulacion$x,Simulacion$y,col=as.factor(Simulacion$Disenio))
# abline(v=seq(0,xcampo, length=Nzonas+1)[-c(1,Nzonas+1)])

# write.table(Simulacion, "C:/Users/Pablo/Desktop/Tabla.txt", quote=FALSE, sep="\t")

################################    ################################    ################################
Formula <- "variable1~1"
MyFile <- Simulacion[, 1:3]
coordinates(MyFile) <- c(1, 2)
Variograma <-
  autofitVariogram(as.formula(Formula), MyFile, model = "Exp")

ModeloVariograma <-
  cbind(Variograma$var_model[2, 1:3],
        Variograma$var_model[1, 2],
        Variograma$sserr)
names(ModeloVariograma) <- c("Model", "Parcial Sill", "Range", "Nugget", "SCE")

##########################################################################################################
######################## Asignación de tratamientos dentro de cada diseño ################################
##########################################################################################################
######Diseño en FRANJA
#Solo se queda con las parcelas que tienen el diseño en franja
MisParcelasFR <- Simulacion[Simulacion$Disenio == "DFR", ]
# Asigna un tratamiento a cada coordenada y.
# Aleatoriza los tratamientos y todas las franjas tienen el mismo orden de Tratamiento
# o aleatoriza todas las franjas sin restricción
AsigTrat <- if (AleatFranja) {
  AleatTrat <-
    do.call(rbind,
            sapply(1:ceiling(length(unique(MisParcelasFR$y)) / length(Trat)),
                   function(x)
                     data.frame(
                       "AsigTrat" = sample(1:length(Trat))
                       ,
                       "FranjaRep" = paste0("FranjaRep_", x)
                     )
                   , simplify = FALSE))
  TratAsigFr <-
    Trat[AleatTrat$AsigTrat[1:length(unique(MisParcelasFR$y))]]
  AleatTrat <- AleatTrat[1:length(TratAsigFr), ]
  AleatTrat$AsigTrat <- TratAsigFr
  AleatTrat
} else {
  rep(Trat[sample(1:length(Trat))], length =
        length(unique(MisParcelasFR$y)))
}
#Junta las coordenadas y con el orden de los tratamientos
MisTratAsigFR <- data.frame(
  "y" = unique(MisParcelasFR$y),
  "Franja" = paste("Franja", trunc(unique(MisParcelasFR$y)), sep =
                     "_"),
  AsigTrat
)
#Junta toda la información de las parcelas con el diseño en Franja sumado 
#al tratamiento que le fue asignado
MisParcelasFRTrat <- merge(MisParcelasFR, MisTratAsigFR, sort = FALSE)

#Plotea las franjas
#plot(MisParcelasFRTrat$x, MisParcelasFRTrat$y, col=MisParcelasFRTrat$AsigTrat)


######Diseño en BLOQUE Completamente Aleatorizado
#Selecciona las parcelas a las que le tocó DBCA
MisParcelasDBCA <- Simulacion[Simulacion$Disenio == "DBCA", ]
#Genera la división de bloques en el eje X y el Eje Y
FranX <-
  findInterval(
    MisParcelasDBCA$x,
    seq(
      min(MisParcelasDBCA$x),
      max(MisParcelasDBCA$x),
      length = length(unique(MisParcelasDBCA$x))
    ),
    left.open = FALSE,
    rightmost.closed = FALSE
  )
# FranY<-findInterval(MisParcelasDBCA$y, seq(min(MisParcelasDBCA$y),max(MisParcelasDBCA$y),
#                                            length=ceiling(length(unique(MisParcelasDBCA$y))/length(Trat))),  left.open = TRUE, rightmost.closed=FALSE)
FranY <-
  findInterval(
    MisParcelasDBCA$y,
    seq(
      min(MisParcelasDBCA$y),
      max(MisParcelasDBCA$y),
      by = min(diff(unique(MisParcelasDBCA$y))) * length(Trat)
    ),
    left.open = FALSE,
    rightmost.closed = TRUE
  )
#Genera los bloques individuales que es la combinación de la división anterior
MisParcelasBloq <-
  data.frame(
    MisParcelasDBCA,
    "FranjaX" = FranX,
    "FranjaY" = FranY,
    "Bloque" = paste(FranX, FranY, sep = "_")
  )


#Separa MisParcelasBloq en Bloques incompletos y en bloques completos
BloquesIncompletos <-
  names(table(MisParcelasBloq$Bloque))[table(MisParcelasBloq$Bloque) < length(Trat)]
MisParcelasBloqComp <-
  MisParcelasBloq[!MisParcelasBloq$Bloque %in% BloquesIncompletos,]
MisParcelasBloqIncomp <-
  MisParcelasBloq[MisParcelasBloq$Bloque %in% BloquesIncompletos,]
#Aleatoriza los tratamientos dentro de cada bloque
MisTratBloque <- with(
  MisParcelasBloq,
  by(
    MisParcelasBloqComp,
    MisParcelasBloqComp$Bloque,
    FUN = function(x) {
      data.frame(x, "AsigTrat" = Trat[sample(1:length(Trat))])
    },
    simplify = FALSE
  )
)

MisTratBloqueCompleto <- do.call(rbind, MisTratBloque)
#Saca las columnas de FranjaX y FranjaY generadas para formar bloques
MisTratBloqueCompleto <-
  MisTratBloqueCompleto[, !colnames(MisTratBloqueCompleto) %in% c("FranjaX", "FranjaY")]
MisParcelasBloqIncomp <-
  MisParcelasBloqIncomp[, !colnames(MisParcelasBloqIncomp) %in% c("FranjaX", "FranjaY")]
#Los bloques incompletos no tienen tratamientos
try(MisParcelasBloqIncomp$AsigTrat <- NA)
#Combina los bloques incompletos y completos en una sola tabla
MisParcelasDBCATrat <-
  rbind(MisTratBloqueCompleto, MisParcelasBloqIncomp)


# plot(MisParcelasDBCA$x,MisParcelasDBCA$y, pch=23, cex=2)
# plot(MisParcelasBloq$x, MisParcelasBloq$y, col=as.factor(MisParcelasBloq$Bloque))
# abline(h=seq(min(MisParcelasDBCA$y),max(MisParcelasDBCA$y),
#              by=min(diff(unique(MisParcelasDBCA$y)))*length(Trat)))
# text(MisParcelasBloq$x, MisParcelasBloq$y, labels=MisParcelasBloq$Bloque, srt = 90)
#

#######Diseño Completamente Aleatorizado DCA
MisParcelasDCA <- Simulacion[Simulacion$Disenio == "DCA", ]
#Asigna tratamientos aleatoriamente###### ?????
MisParcelasDCA$AsigTrat <-
  sample(rep(Trat, length = nrow(MisParcelasDCA)))
MisParcelasDCATrat <- MisParcelasDCA

###########################################################################################
########              Asignación de RENDIMIENTO              ##############################
###########################################################################################
#Aleatoriza un beta1 y beta2 SIN CORRELACION!!!!!!!!!!                                                                #######################################
misBetas <-
  mvrnorm(
    n = 1,
    mu = c(mediaBeta1, mediaBeta2),
    Sigma = matrix(c(varBeta1, rep(covBetas, 2), varBeta2), 2),
    tol = 1e-6,
    empirical = FALSE,
    EISPACK = FALSE
  )
miBeta1 <- misBetas[1]
miBeta2 <- misBetas[2]
# miBeta1<-rnorm(1, mediaBeta1,varBeta1)
# miBeta2<-rnorm(1, mediaBeta2,varBeta2)
# mediaBeta2<- -0.0004
# varBeta2<- 0.00000001
# misBetas<-mvrnorm(n = 100000, mu=c(mediaBeta1,mediaBeta2), Sigma=matrix(c(varBeta1,rep(covBetas,2),varBeta2),2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
# plot(misBetas)
# abline(h=0)
TerInterZonError <- 0.0034
EstRto <-
  function(df1,
           miBeta11 = miBeta1,
           miBeta22 = miBeta2,
           TerInterZonError = 0.0034) {
    MiRto <- df1["AsigTrat"]
    names(MiRto) <- "MiRto"
    MiRto <-
      miBeta11 * MiRto + miBeta22 * MiRto ^ 2 + TerInterZonError * MiRto * df1["Zona"] + df1["ErrorConEfecto"]                  ##############################
    data.frame(df1, MiRto)
  }
# write.table(MisParcelasFRTrat, paste0("clipboard-", 2^10), quote = FALSE, sep="\t", row.names = FALSE)
MisParcelasFRTrat <- EstRto(MisParcelasFRTrat)
MisParcelasDCATrat <- EstRto(MisParcelasDCATrat)
MisParcelasDBCATrat <- EstRto(MisParcelasDBCATrat)

# plot(MisParcelasFRTrat$AsigTrat+1, t(MisParcelasFRTrat$MiRto), col='blue')
# points(MisParcelasDCATrat$AsigTrat+2, t(MisParcelasDCATrat$MiRto), col='red')
# points(MisParcelasDBCATrat$AsigTrat+3, t(MisParcelasDBCATrat$MiRto), col='darkgreen')
# Cantidad de parcelas por tratamiento asignado:
# table(MisParcelasFRTrat$AsigTrat)
# table(MisParcelasDCATrat$AsigTrat)
# table(MisParcelasDBCATrat$AsigTrat)
