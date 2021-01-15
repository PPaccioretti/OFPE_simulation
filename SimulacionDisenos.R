setwd("/home/ppaccioretti/SimulacionEnsayos/")
library(RandomFields)
library(MASS)
library(nlme)
library(automap)
library(parallel)
RFoptions(seed = NA)
args <- commandArgs(TRUE)

semilla <- 1990
if (length(args) >= 1) {
  semilla <- as.numeric(args[[1]])
}

nsim <- 250
if (length(args) >= 2) {
  nsim <- as.numeric(args[[2]])
}


cat('Corriendo con semilla', semilla, '\n')
cat('Corriendo', nsim, 'simulaciones\n')

RNGkind("L'Ecuyer-CMRG")
set.seed(semilla)



# set.seed(1)
# RFoptions(seed = seed, always_open_device=T, always_close_device=F)

# #Dimensiones Campo
# xcampo<- 1000 #metros
# ycampo<- 1000 #metros
# 
# #Dimensiones Parcela
# xlargoparcela<- 70 #metros
# yanchoparcela<- 30 #metros
# 
# #Parámetros error correlacionado Exponencial
# #Recopilación de 247 mapas de rendimiento de maíz: Rango 49metros, sill=var 7, nugget 1.2
# rango <- 49 #Scale = Rango ~Gamma (41.3, 17.5)
# sill <- 7 #Sill = Var de Exp
# nugget <- 1.2 #varianza nugget ~ Weibull ()
# MediaCultivo<- 58
# #Número de zonas (Divide al eje X)
# Nzonas <- 2
# 
# #HeterogeneidadEntreZonas (Heterog *(Sill+Nugget) )
# Heterog <- 3
# 
# #Tratamientos
# Trat <- c(1,40,70,100,140)
# 
# #Betas de regresion
# mediaBeta1 <- 0.12
# varBeta1<- 0.0001
# mediaBeta2<- -0.0004
# varBeta2<- 0.00000001
# covBetas<- 0

GenerarCampoAjustarModelos <- function(#Dimensiones Campo
  xcampo = 1000, #metros
  ycampo = 1000, #metros
  
  #Dimensiones Parcela
  xlargoparcela = 70, #metros
  yanchoparcela = 33, #metros
  
  # Parámetros error correlacionado Exponencial
  # Recopilación de 247 mapas de rendimiento de maíz: 
  # Rango 49metros, sill=var 7, nugget 1.2
  rango = 49, #Scale = Rango ~Gamma(41.3, 17.5)
  sill = 7, #Sill = Var de Exp
  nugget = 1.2, #varianza nugget ~Weibull()
  
  MediaCultivo = 80,#qq#8,     
  
  #########
  #Número de zonas (Divide al eje X)
  Nzonas = 2,
  
  #HeterogeneidadEntreZonas (Heterog *(Sill+Nugget) )
  Heterog = 2,
  
  #Tratamientos
  Trat = c(1, 40, 70, 100, 140),
  
  #Betas de regresion
  mediaBeta1 = 0.244,#qq #12.76/100,
  varBeta1 = 0,#0.000001,#0.001,
  mediaBeta2 = -0.0015,#qq #-0.001,
  varBeta2 = 0,#0.0000001,
  covBetas = 0,
  TerInterZonError = 0.0034) {
  if (Sys.info()['sysname'] == "Windows") {
    browser()
  }
# 
  #Futuras coordenadas del campo
  # x <- seq(0, xcampo, xcampo / xlargoparcela)
  # y <- seq(0, ycampo, ycampo / yanchoparcela)
  x <- seq(0, xcampo, xlargoparcela)
  y <- seq(0, ycampo, yanchoparcela)
  #Fija una semilla para la simulación
  MiSemilla = round(runif(1, min = -99999, max = 99999), 0)

  #Estima el modelo y Simula los datos
  modelo <-
    RMexp(var = sill, scale = rango) + 
    RMnugget(var = nugget) + 
    RMtrend(mean = MediaCultivo)
  simu <- RFsimulate(modelo, x, y,  spConform = TRUE)#, x=x, y=y)
  #Crea un data.frame con la simulacion
  Simulacion <-
    data.frame(expand.grid(list("x" = x, "y" = y)), "Error" = simu)
  #   Cantidad de parcelas en la simulacion: nrow(Simulacion)
  
  #Categoriza las coordenadas del eje X en Nzonas (Sumar 5 y 10 cte???)
  EfZona <-
    findInterval(Simulacion$x, seq(0, xcampo, length = Nzonas + 1)[-c(1, Nzonas +
                                                                        1)],
                 left.open = TRUE) * (Heterog * sqrt(sill + nugget))
  #   Cantidad de parcelas por zona: table(as.factor(EfZona))
  
  #Junta la simulacion con la zona
  Simulacion <-
    data.frame(Simulacion,
               "Zona" = EfZona,
               "ErrorConEfecto" = Simulacion$variable1 + EfZona)
  # plot(Simulacion$x,Simulacion$y,col=as.factor(Simulacion$Zona))
  # abline(v=seq(0,xcampo, length=Nzonas+1)[-c(1,Nzonas+1)])
  
  #Divide al eje y en Ndis=3 zonas para repartir los Diseños
  NDis <- 3                                 ################################
  NombresDis <- c("DBCA", "DCA", "DFR")     ################################
  AleatFranja <- TRUE                       ################################
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
  if (AleatFranja) {
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
    AsigTrat <- AleatTrat
  } else {
    AsigTrat <- 
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
      rightmost.closed = FALSE
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
  try(MisParcelasBloqIncomp$AsigTrat <- NA, silent = TRUE)
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

  EstRto <-
    function(df1,
             miBeta11 = miBeta1,
             miBeta22 = miBeta2,
             InterZonError = TerInterZonError) {
      MiRto <- df1["AsigTrat"]
      names(MiRto) <- "MiRto"
      MiRto <-
        miBeta11 * MiRto + miBeta22 * MiRto ^ 2 + InterZonError * MiRto * df1["Zona"] + df1["ErrorConEfecto"]                  ##############################
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
  ########################################################################################
  #####                  AJUSTE DE MODELOS                  ############################## 
  ########################################################################################
  #El modelo de ANAVA nulo se ajusta con ErrorConEfecto DE ZONA, habria que 
  #ajustarlo con los valores sin el efecto zona??????????  #######################################        ##############################
  Formula.Nulo <- "ErrorConEfecto~1+AsigTrat"
  Formula.Nulo.Zo <-
    "ErrorConEfecto~1+AsigTrat+as.factor(Zona)+AsigTrat:as.factor(Zona)"
  
  Formula.Rto <- "MiRto~1+AsigTrat+I(AsigTrat^2)"
  Formula.Rto.Zo <-
    "MiRto~1+AsigTrat+as.factor(Zona)+AsigTrat:as.factor(Zona)+I(AsigTrat^2)"
  
  pb = txtProgressBar(
    min = 0,
    max = 26,
    initial = 0,
    style = 3,
    title = "Ajustando Modelos"
  )
  
  ####### Diseño en FRANJA  // MisParcelasFRTrat
  ### ANAVA NULO CON ERRORES
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.FR.Nulo.FrA <- lme(as.formula(Formula.Nulo)
                       ,random = list(FranjaRep = pdIdent(~1))                  ##############################                   ##############################                   ############################## 
                       ,method = "REML"
                       ,control = lmeControl(niterEM = 150
                                             ,msMaxIter = 200)
                       ,na.action = na.omit
                       ,data = MisParcelasFRTrat
                       ,keep.data = FALSE)
  setTxtProgressBar(pb, 1)
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.FR.Nulo.Zo.FrA <-lme(as.formula(Formula.Nulo.Zo)
                         ,random=list(FranjaRep=pdIdent(~1))
                         ,method="REML"
                         ,control=lmeControl(niterEM=150
                                             ,msMaxIter=200)
                         ,na.action=na.omit
                         ,data=MisParcelasFRTrat
                         ,keep.data=FALSE)
  setTxtProgressBar(pb, 2)
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.FR.Nulo.FrA.Co<-lme(as.formula(Formula.Nulo)
                        ,random=list(FranjaRep=pdIdent(~1))
                        ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                            ,metric="euclidean"
                                            ,nugget=FALSE)
                        ,method="REML"
                        ,control=lmeControl(niterEM=150
                                            ,msMaxIter=200)
                        ,na.action=na.omit
                        ,data=MisParcelasFRTrat
                        ,keep.data=FALSE)
  setTxtProgressBar(pb, 3)
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.FR.Nulo.Zo.FrA.Co <-lme(as.formula(Formula.Nulo.Zo)
                            ,random=list(FranjaRep=pdIdent(~1))
                            ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                                ,metric="euclidean"
                                                ,nugget=FALSE)
                            ,method="REML"
                            ,control=lmeControl(niterEM=150
                                                ,msMaxIter=200)
                            ,na.action=na.omit
                            ,data=MisParcelasFRTrat
                            ,keep.data=FALSE)
  setTxtProgressBar(pb, 4)
  ##################CON RENDIMIENTO  
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.FR.FrA<-lme(as.formula(Formula.Rto)
                ,random=list(FranjaRep=pdIdent(~1))
                ,method="REML"
                ,control=lmeControl(niterEM=150
                                    ,msMaxIter=200)
                ,na.action=na.omit
                ,data=MisParcelasFRTrat
                ,keep.data=FALSE)
  setTxtProgressBar(pb, 5)
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.FR.Zo.FrA <-lme(as.formula(Formula.Rto.Zo)
                    ,random=list(FranjaRep=pdIdent(~1))
                    ,method="REML"
                    ,control=lmeControl(niterEM=150
                                        ,msMaxIter=200)
                    ,na.action=na.omit
                    ,data=MisParcelasFRTrat
                    ,keep.data=FALSE)
  setTxtProgressBar(pb, 6)
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.FR.FrA.Co<-lme(as.formula(Formula.Rto)
                   ,random=list(FranjaRep=pdIdent(~1))
                   ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                       ,metric="euclidean"
                                       ,nugget=FALSE)
                   ,method="REML"
                   ,control=lmeControl(niterEM=150
                                       ,msMaxIter=200)
                   ,na.action=na.omit
                   ,data=MisParcelasFRTrat
                   ,keep.data=FALSE)
  setTxtProgressBar(pb, 7)
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.FR.Zo.FrA.Co <-lme(as.formula(Formula.Rto.Zo)
                       ,random=list(FranjaRep=pdIdent(~1))
                       ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                           ,metric="euclidean"
                                           ,nugget=FALSE)
                       ,method="REML"
                       ,control=lmeControl(niterEM=150
                                           ,msMaxIter=200)
                       ,na.action=na.omit
                       ,data=MisParcelasFRTrat
                       ,keep.data=FALSE)
  setTxtProgressBar(pb, 8)
  ####################################################################################   
  ####################################################################################          
  #################################################################################### 
  ######Diseño en BLOQUE Completamente Aleatorizado  \\  MisParcelasDBCATrat
  ### ANAVA NULO
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.DBCA.Nulo.BlA<-lme(as.formula(Formula.Nulo)
                       ,random=list(Bloque=pdIdent(~1))
                       ,method="REML"
                       ,control=lmeControl(niterEM=150
                                           ,msMaxIter=200)
                       ,na.action=na.omit
                       ,data=MisParcelasDBCATrat
                       ,keep.data=FALSE)
  setTxtProgressBar(pb, 9)
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.DBCA.Nulo.Zo.BlA <-lme(as.formula(Formula.Nulo.Zo)
                           ,random=list(Bloque=pdIdent(~1))
                           ,method="REML"
                           ,control=lmeControl(niterEM=150
                                               ,msMaxIter=200)
                           ,na.action=na.omit
                           ,data=MisParcelasDBCATrat
                           ,keep.data=FALSE)
  setTxtProgressBar(pb, 10)
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.DBCA.Nulo.BlA.Co<-lme(as.formula(Formula.Nulo)
                          ,random=list(Bloque=pdIdent(~1))
                          ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                              ,metric="euclidean"
                                              ,nugget=FALSE)
                          ,method="REML"
                          ,control=lmeControl(niterEM=150
                                              ,msMaxIter=200)
                          ,na.action=na.omit
                          ,data=MisParcelasDBCATrat
                          ,keep.data=FALSE)
  setTxtProgressBar(pb, 11)
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.DBCA.Nulo.Zo.BlA.Co <-lme(as.formula(Formula.Nulo.Zo)
                              ,random=list(Bloque=pdIdent(~1))
                              ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                                  ,metric="euclidean"
                                                  ,nugget=FALSE)
                              ,method="REML"
                              ,control=lmeControl(niterEM=150
                                                  ,msMaxIter=200)
                              ,na.action=na.omit
                              ,data=MisParcelasDBCATrat
                              ,keep.data=FALSE)
  setTxtProgressBar(pb, 12)
  ##################CON RENDIMIENTO  
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.DBCA.BlA<-lme(as.formula(Formula.Rto)
                  ,random=list(Bloque=pdIdent(~1))
                  ,method="REML"
                  ,control=lmeControl(niterEM=150
                                      ,msMaxIter=200)
                  ,na.action=na.omit
                  ,data=MisParcelasDBCATrat
                  ,keep.data=FALSE)
  setTxtProgressBar(pb, 13)
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.DBCA.Zo.BlA <-lme(as.formula(Formula.Rto.Zo)
                      ,random=list(Bloque=pdIdent(~1))
                      ,method="REML"
                      ,control=lmeControl(niterEM=150
                                          ,msMaxIter=200)
                      ,na.action=na.omit
                      ,data=MisParcelasDBCATrat
                      ,keep.data=FALSE)
  setTxtProgressBar(pb, 14)
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.DBCA.BlA.Co<-lme(as.formula(Formula.Rto)
                     ,random=list(Bloque=pdIdent(~1))
                     ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                         ,metric="euclidean"
                                         ,nugget=FALSE)
                     ,method="REML"
                     ,control=lmeControl(niterEM=150
                                         ,msMaxIter=200)
                     ,na.action=na.omit
                     ,data=MisParcelasDBCATrat
                     ,keep.data=FALSE)
  setTxtProgressBar(pb, 15)
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.DBCA.Zo.BlA.Co <-lme(as.formula(Formula.Rto.Zo)
                         ,random=list(Bloque=pdIdent(~1))
                         ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                             ,metric="euclidean"
                                             ,nugget=FALSE)
                         ,method="REML"
                         ,control=lmeControl(niterEM=150
                                             ,msMaxIter=200)
                         ,na.action=na.omit
                         ,data=MisParcelasDBCATrat
                         ,keep.data=FALSE)
  setTxtProgressBar(pb, 16)
  ####################################################################################   
  ####################################################################################   
  ####################################################################################   
  
  ####################################################################################   
  #######Diseño Completamente Aleatorizado DCA  \\  MisParcelasDCATrat
  ### ANAVA NULO
  ### ANAVA NULO
  #ANOVA = ErrorConEfecto~1+AsigTrat
  M.DCA.Nulo<-lm(as.formula(Formula.Nulo)
                 ,na.action=na.omit
                 ,data=MisParcelasDCATrat)
  setTxtProgressBar(pb, 17)
  #ANOVA CON CORRELACION ESPACIAL 
  M.DCA.Nulo.Co<-gls(as.formula(Formula.Nulo)
                     ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                         ,metric="euclidean"
                                         ,nugget=FALSE)
                     ,method="REML"
                     ,na.action=na.omit
                     ,data=MisParcelasDCATrat)
  setTxtProgressBar(pb, 18)
  #ANOVA = ErrorConEfecto~1+AsigTrat
  M.DCA.Nulo.Zo<-lm(as.formula(Formula.Nulo.Zo)
                    ,na.action=na.omit
                    ,data=MisParcelasDCATrat)
  setTxtProgressBar(pb, 19)
  #ANOVA CON CORRELACION ESPACIAL 
  M.DCA.Nulo.Zo.Co<-gls(as.formula(Formula.Nulo.Zo)
                        ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                            ,metric="euclidean"
                                            ,nugget=FALSE)
                        ,method="REML"
                        ,na.action=na.omit
                        ,data=MisParcelasDCATrat)
  setTxtProgressBar(pb, 20)
  ##### CON RENDIMIENTO      
  #ANOVA = MiRto~1+AsigTrat
  M.DCA<-lm(as.formula(Formula.Rto)
            ,na.action=na.omit
            ,data=MisParcelasDCATrat)
  setTxtProgressBar(pb, 21)
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.DCA.Zo<-lm(as.formula(Formula.Rto.Zo)
               ,na.action=na.omit
               ,data=MisParcelasDCATrat)
  setTxtProgressBar(pb, 22)
  #ANOVA CON CORRELACION ESPACIAL 
  M.DCA.Co<-gls(as.formula(Formula.Rto)
                ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                    ,metric="euclidean"
                                    ,nugget=FALSE)
                ,method="REML"
                ,na.action=na.omit
                ,data=MisParcelasDCATrat)
  setTxtProgressBar(pb, 23)
  #ANOVA CON CORRELACION ESPACIAL 
  M.DCA.Zo.Co<-gls(as.formula(Formula.Rto.Zo)
                   ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                       ,metric="euclidean"
                                       ,nugget=FALSE)
                   ,method="REML"
                   ,na.action=na.omit
                   ,data=MisParcelasDCATrat)
  setTxtProgressBar(pb, 24)
  ####################################################################################   
  #################################################################################### 
  # Modelo <- M.DCA
  # Modelo <- M.DCA.Co
  # Modelo <- M.DCA.Zo.Co
  # Modelo <- M.FR.Zo.FrA.Co
  # Modelo <- M.FR.Zo.FrA
  ResumirModelo <- function(Modelo) {
    ModelResumen <- data.frame(
      "Modelo" = deparse(substitute(Modelo)),
      "rango" = as.numeric(as.character(
        capture.output(Modelo$modelStruct$corStruct)[3]
      )) * 3,
      ###################### SE MULTIPLICA POR TRES PARA PODER COMPARARLO 
      ###################### CON EL RANGO REAL
      "Akaike" = AIC(Modelo),
      "SigmaError" = sigma(Modelo)
    )
    if (class(Modelo) == "gls") {
      TablaAnava <- summary(Modelo)$tTable
      colnames(TablaAnava) <-
        c("Value", "Std.Error", "t-value", "p-value")

      intervalos <- confint(Modelo)
      colnames(intervalos) <- c('lower', 'upper')
      
      Betas <- data.frame(
        "Modelo" = deparse(substitute(Modelo)),
        "Coeficientes" = rownames(summary(Modelo)$tTable),
        TablaAnava,
        "DF" = NA,
        row.names = NULL
      )[, c(1:4, 7, 5:6)]
      Betas <- data.frame(Betas, intervalos)
    }
    
    if (class(Modelo) == "lme") {
      TablaAnava <- summary(Modelo)$tTable
      colnames(TablaAnava) <-
        c("Value", "Std.Error", "DF", "t-value", "p-value")
      intervalos <- intervals(Modelo, which = "fixed")
      intervalos <- intervalos$fixed[, c(1,3)]
      Betas <- data.frame(
        "Modelo" = deparse(substitute(Modelo)),
        "Coeficientes" = rownames(summary(Modelo)$tTable),
        TablaAnava,
        intervalos,
        row.names = NULL
      )
    }
    if (class(Modelo) == "lm") {
      TablaAnava <- summary(Modelo)$coefficients
      colnames(TablaAnava) <-
        c("Value", "Std.Error", "t-value", "p-value")
      
      intervalos <- confint(Modelo)
      colnames(intervalos) <- c('lower', 'upper')
      
      Betas <- data.frame(
        "Modelo" = deparse(substitute(Modelo)),
        "Coeficientes" = rownames(summary(Modelo)$coefficients),
        TablaAnava,
        "DF" = NA,
        row.names = NULL
      )[, c(1:4, 7, 5:6)]
      Betas <- data.frame(Betas, intervalos)
      
    }
    return(list("Resumen" = ModelResumen, "Betas" = Betas))
  }
  
  media_disenio <- function(misParcelas) {
    mean(misParcelas$MiRto, na.rm = T)
  }

  ResumenModelos <-
    sapply(ls(pattern = "M[.]"), function(Es) {
      eval(parse(text = paste0("ResumirModelo(", Es, ")")))
    },
    USE.NAMES = TRUE, simplify = FALSE)
  
  ResumenResult <-
    do.call(rbind, lapply(ResumenModelos, function(x) {
      x$Resumen
    }))
  ResumenResult$RangoReal <- NA
  ResumenResult[!is.na(ResumenResult$rango), "RangoReal"] <-
    ModeloVariograma$Range
  
  # Calculo el CV teniendo en cuenta el Sigma del modelo y la media dela
  # base de datos de cada disenio
  media_disenio <- function(misParcelas) {
    mean(misParcelas$MiRto, na.rm = T)
  }
  
  sigma_cv <- function(diesnio, misParcelas) {
    misDis <- grepl(diesnio, ResumenResult$Modelo)
    misSigmas <- ResumenResult$SigmaError[misDis]
    misModelos <- ResumenResult$Modelo[misDis]
    misSigmas_cv <- misSigmas / media_disenio(misParcelas) * 100
    data.frame('Modelo' = misModelos,
               'Sigma_CV' = misSigmas_cv)
  }
  misSigma_cv <- rbind(
    sigma_cv('M[.]DBCA[.]*', MisParcelasDBCATrat),
    sigma_cv('M[.]DCA[.]*', MisParcelasDCATrat),
    sigma_cv('M[.]FR[.]*', MisParcelasFRTrat)
  )

  ResumenResult <- merge(ResumenResult, misSigma_cv, all.x = T)
  
  setTxtProgressBar(pb, 25)
  
  #Betas Estimados
  BetasEst <-
    do.call(rbind, lapply(ResumenModelos, function(x) {
      x$Betas
    }))  ### INTENTAR SACARLO DE LA LISTA
  BetasEst$Orden <- 1:nrow(BetasEst)
  #Betas Reales
  misNombresCoef <- unique(BetasEst$Coeficientes)
  
  efectZona <- 
    misNombresCoef[grep("^as.factor(Zona)*", misNombresCoef)]
  
  interaccTratZona <-
    misNombresCoef[grep("AsigTrat:as.factor(Zona)*", misNombresCoef)]
  
  BetasReales <- data.frame(
    "Coeficientes" = c("AsigTrat", "I(AsigTrat^2)", efectZona, interaccTratZona),
    "ValoresReales" = c(miBeta1, miBeta2, max(EfZona), TerInterZonError * unique(Simulacion$Zona)[2])
  )
  BetasEstReal <- merge(BetasReales, BetasEst, all.y = T, sort = FALSE)
  # Es -1 para que saque la columna Orden
  BetasEstReal <-
    BetasEstReal[order(BetasEstReal$Orden), c(3, 1, 4, 2, 5:(ncol(BetasEstReal) -
                                                               1))] 
  setTxtProgressBar(pb, 26)
  close(pb)
  cat('\n')

  return(list(
    "Resumen" = ResumenResult,
    "BetasEstReal" = BetasEstReal,
    "Semilla" = MiSemilla
  ))
  
}


# ## "<0.25"
# Rango	47.11
# Psill	7.85
# Nugget	0.94
# 
# ## "0.25-0.75"
# Rango	56.82
# Psill	4.81
# Nugget	2.54


# set.seed(1)

simulaciones <- mclapply(1:nsim, function(i) {
  RFoptions(storing = FALSE)
  # Con 0 en los trat
  ## Alta variacion Tratamientos
  ### baja correlacion espacial "<0.25"
  Test_AltaVarT_BajaCorr <-
    GenerarCampoAjustarModelos(Trat = c(1, 70, 140, 210, 280),
                               rango = 47.11,
                               sill = 7.85,
                               nugget = 0.94)
  ### media correlacion espacial "0.25-0.75"
  Test_AltaVarT_MediaCorr <-
    GenerarCampoAjustarModelos(Trat = c(1, 70, 140, 210, 280),
                               rango = 56.82,
                               sill = 4.81,
                               nugget = 2.54)
  
  ## Baja variacion Tratamientos
  ### baja correlacion espacial "<0.25"
  Test_BajaVarT_BajaCorr <- 
    GenerarCampoAjustarModelos(Trat = c(1, 40, 70, 100, 140),
                               rango = 47.11,
                               sill = 7.85,
                               nugget = 0.94)
  ### media correlacion espacial "0.25-0.75"
  Test_BajaVarT_MediaCorr <- 
    GenerarCampoAjustarModelos(Trat = c(1, 40, 70, 100, 140),
                               rango = 56.82,
                               sill = 4.81,
                               nugget = 2.54)
  
  # Sin 0 en los trat
  ## Alta variacion Tratamientos
  ### baja correlacion espacial "<0.25"
  SinTest_AltaVarT_BajaCorr <- 
    GenerarCampoAjustarModelos(Trat = c(70, 140, 210, 280),
                               rango = 47.11,
                               sill = 7.85,
                               nugget = 0.94)
  ### media correlacion espacial "0.25-0.75"
  SinTest_AltaVarT_MediaCorr <- 
    GenerarCampoAjustarModelos(Trat = c(70, 140, 210, 280),
                               rango = 56.82,
                               sill = 4.81,
                               nugget = 2.54)
  
  ## Baja variacion Tratamientos
  ### baja correlacion espacial "<0.25"
  SinTest_BajaVarT_BajaCorr <- 
    GenerarCampoAjustarModelos(Trat = c(40, 70, 100, 140),
                               rango = 47.11,
                               sill = 7.85,
                               nugget = 0.94)
  ### media correlacion espacial "0.25-0.75"
  SinTest_BajaVarT_MediaCorr <-
    GenerarCampoAjustarModelos(Trat = c(40, 70, 100, 140),
                               rango = 56.82,
                               sill = 4.81,
                               nugget = 2.54)

  
  info_Test_AltaVarT_BajaCorr <- data.frame(
    'Simulacion' = i,
    'Dosis0' = 'Si',
    'TratMax' = 280,
    'CorrEsp' = 'baja')
  
  Test_AltaVarT_BajaCorr$Resumen <- 
    data.frame(info_Test_AltaVarT_BajaCorr,
               Test_AltaVarT_BajaCorr$Resumen)
  
  Test_AltaVarT_BajaCorr$BetasEstReal <- 
    data.frame(info_Test_AltaVarT_BajaCorr,
               Test_AltaVarT_BajaCorr$BetasEstReal)
  
  
  info_Test_AltaVarT_MediaCorr <- data.frame(
    'Simulacion' = i,
    'Dosis0' = 'Si',
    'TratMax' = 280,
    'CorrEsp' = 'media')
  
  Test_AltaVarT_MediaCorr$Resumen <- 
    data.frame(info_Test_AltaVarT_MediaCorr,
               Test_AltaVarT_MediaCorr$Resumen)
  
  Test_AltaVarT_MediaCorr$BetasEstReal <- 
    data.frame(info_Test_AltaVarT_MediaCorr,
               Test_AltaVarT_MediaCorr$BetasEstReal)
  
  
  info_Test_BajaVarT_BajaCorr <- data.frame(
    'Simulacion' = i,
    'Dosis0' = 'Si',
    'TratMax' = 140,
    'CorrEsp' = 'baja')
  
  Test_BajaVarT_BajaCorr$Resumen <- 
    data.frame(info_Test_BajaVarT_BajaCorr,
               Test_BajaVarT_BajaCorr$Resumen)
  
  Test_BajaVarT_BajaCorr$BetasEstReal <- 
    data.frame(info_Test_BajaVarT_BajaCorr,
               Test_BajaVarT_BajaCorr$BetasEstReal)
  
  
  info_Test_BajaVarT_MediaCorr <- data.frame(
    'Simulacion' = i,
    'Dosis0' = 'Si',
    'TratMax' = 140,
    'CorrEsp' = 'media')
  
  Test_BajaVarT_MediaCorr$Resumen <- 
    data.frame(info_Test_BajaVarT_MediaCorr,
               Test_BajaVarT_MediaCorr$Resumen)
  
  Test_BajaVarT_MediaCorr$BetasEstReal <- 
    data.frame(info_Test_BajaVarT_MediaCorr,
               Test_BajaVarT_MediaCorr$BetasEstReal)
  
  
  
  info_SinTest_AltaVarT_BajaCorr <- data.frame(
    'Simulacion' = i,
    'Dosis0' = 'No',
    'TratMax' = 280,
    'CorrEsp' = 'baja')
  
  SinTest_AltaVarT_BajaCorr$Resumen <- 
    data.frame(info_SinTest_AltaVarT_BajaCorr,
               SinTest_AltaVarT_BajaCorr$Resumen)
  
  SinTest_AltaVarT_BajaCorr$BetasEstReal <- 
    data.frame(info_SinTest_AltaVarT_BajaCorr,
               SinTest_AltaVarT_BajaCorr$BetasEstReal)
  
  
  info_SinTest_AltaVarT_MediaCorr <- data.frame(
    'Simulacion' = i,
    'Dosis0' = 'No',
    'TratMax' = 280,
    'CorrEsp' = 'media')
  
  SinTest_AltaVarT_MediaCorr$Resumen <- 
    data.frame(info_SinTest_AltaVarT_MediaCorr,
               SinTest_AltaVarT_MediaCorr$Resumen)
  
  SinTest_AltaVarT_MediaCorr$BetasEstReal <- 
    data.frame(info_SinTest_AltaVarT_MediaCorr,
               SinTest_AltaVarT_MediaCorr$BetasEstReal)
  
  
  info_SinTest_BajaVarT_BajaCorr <- data.frame(
    'Simulacion' = i,
    'Dosis0' = 'No',
    'TratMax' = 140,
    'CorrEsp' = 'baja')
  
  SinTest_BajaVarT_BajaCorr$Resumen <- 
    data.frame(info_SinTest_BajaVarT_BajaCorr,
               SinTest_BajaVarT_BajaCorr$Resumen)
  
  SinTest_BajaVarT_BajaCorr$BetasEstReal <- 
    data.frame(info_SinTest_BajaVarT_BajaCorr,
               SinTest_BajaVarT_BajaCorr$BetasEstReal)
  
  
  info_SinTest_BajaVarT_MediaCorr <- data.frame(
    'Simulacion' = i,
    'Dosis0' = 'No',
    'TratMax' = 140,
    'CorrEsp' = 'media')
  
  SinTest_BajaVarT_MediaCorr$Resumen <- 
    data.frame(info_SinTest_BajaVarT_MediaCorr,
               SinTest_BajaVarT_MediaCorr$Resumen)
  
  SinTest_BajaVarT_MediaCorr$BetasEstReal <- 
    data.frame(info_SinTest_BajaVarT_MediaCorr,
               SinTest_BajaVarT_MediaCorr$BetasEstReal)
  
  
  
  resumen <- do.call(
    rbind,
    list(
      Test_AltaVarT_BajaCorr$Resumen,
      Test_AltaVarT_MediaCorr$Resumen,
      Test_BajaVarT_BajaCorr$Resumen,
      Test_BajaVarT_MediaCorr$Resumen,
      SinTest_AltaVarT_BajaCorr$Resumen,
      SinTest_AltaVarT_MediaCorr$Resumen,
      SinTest_BajaVarT_BajaCorr$Resumen,
      SinTest_BajaVarT_MediaCorr$Resumen
    )
  )
  
  betas <- do.call(
    rbind,
    list(
      Test_AltaVarT_BajaCorr$BetasEstReal,
      Test_AltaVarT_MediaCorr$BetasEstReal,
      Test_BajaVarT_BajaCorr$BetasEstReal,
      Test_BajaVarT_MediaCorr$BetasEstReal,
      SinTest_AltaVarT_BajaCorr$BetasEstReal,
      SinTest_AltaVarT_MediaCorr$BetasEstReal,
      SinTest_BajaVarT_BajaCorr$BetasEstReal,
      SinTest_BajaVarT_MediaCorr$BetasEstReal
    )
  )
  
  semilla <- do.call(
    rbind,
    list(
      Test_AltaVarT_BajaCorr$Semilla,
      Test_AltaVarT_MediaCorr$Semilla,
      Test_BajaVarT_BajaCorr$Semilla,
      Test_BajaVarT_MediaCorr$Semilla,
      SinTest_AltaVarT_BajaCorr$Semilla,
      SinTest_AltaVarT_MediaCorr$Semilla,
      SinTest_BajaVarT_BajaCorr$Semilla,
      SinTest_BajaVarT_MediaCorr$Semilla
    )
  )
  semilla <- data.frame('Simulacion' = i, 'Semilla' = semilla)
  
  list(
    'Resumen' = resumen,
    'BetasEstReal' = betas,
    'Semilla' = semilla)
  
}, mc.cores = detectCores() - 1)

#saveRDS(simulaciones, "Simulaciones_500_Nov_2020.rds")
saveRDS(simulaciones, 
        paste0("Simulaciones_",
               "Sem_", semilla,
               "_",
               "nsim_",
               nsim,
               "_",
               format(Sys.time(), "%Y_%d_%m_%H-%M-%S"), ".rds"))
