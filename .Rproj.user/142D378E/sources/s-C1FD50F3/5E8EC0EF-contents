library(RandomFields)
library(MASS)
library(nlme)
library(automap)

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
  yanchoparcela = 30, #metros
  
  # Parámetros error correlacionado Exponencial
  # Recopilación de 247 mapas de rendimiento de maíz: 
  # Rango 49metros, sill=var 7, nugget 1.2
  rango = 49, #Scale = Rango ~Gamma(41.3, 17.5)
  sill = 7, #Sill = Var de Exp
  nugget = 1.2, #varianza nugget ~Weibull()
  
  MediaCultivo = 80,     

  #########
  #Número de zonas (Divide al eje X)
  Nzonas = 2,
  
  #HeterogeneidadEntreZonas (Heterog *(Sill+Nugget) )
  Heterog = 3,
  
  #Tratamientos
  Trat = c(0, 40, 70, 100, 140),
  
  #Betas de regresion
  mediaBeta1 = 0.12,
  varBeta1 = 0.0001,
  mediaBeta2 = -0.0004,
  varBeta2 = 0.00000001,
  covBetas = 0) {

  #Futuras coordenadas del campo
  x <- seq(0, xcampo, xcampo / xlargoparcela)
  y <- seq(0, ycampo, ycampo / yanchoparcela)
  #Fija una semilla para la simulación
  MiSemilla = round(runif(1, min = -99999, max = 99999), 0)
  RFoptions(seed = MiSemilla)
  #Estima el modelo y Simula los datos
  modelo<-RMexp(var=sill, scale = rango)+ RMnugget(var=nugget)+ RMtrend(mean=MediaCultivo)
  simu <- RFsimulate(modelo,x,y,  spConform=TRUE)#, x=x, y=y)
  #Crea un data.frame con la simulacion
  Simulacion<-data.frame(expand.grid(list("x"=x,"y"=y)), "Error"=simu)
  #   Cantidad de parcelas en la simulacion: nrow(Simulacion)
  
  #Categoriza las coordenadas del eje X en Nzonas (Sumar 5 y 10 cte???)
  EfZona<- findInterval(Simulacion$x, seq(0,xcampo, length=Nzonas+1)[-c(1,Nzonas+1)],  
                        left.open = TRUE) * (Heterog * (sill+nugget))
  #   Cantidad de parcelas por zona: table(as.factor(EfZona))
  
  #Junta la simulacion con la zona
  Simulacion<- data.frame(Simulacion, "Zona"=EfZona, "ErrorConEfecto"=Simulacion$variable1+EfZona)
  # plot(Simulacion$x,Simulacion$y,col=as.factor(Simulacion$Zona))
  # abline(v=seq(0,xcampo, length=Nzonas+1)[-c(1,Nzonas+1)])
  
  #Divide al eje y en Ndis=3 zonas para repartir los Diseños
  NDis<- 3                                ################################
  NombresDis<-c("DBCA", "DCA", "DFR")     ################################
  AleatFranja <- TRUE                     ################################
  Dis<- as.factor(findInterval(Simulacion$y, seq(0,ycampo, length=NDis+1)[-c(1,NDis+1)],  left.open = TRUE))
  #   Cantidad de parcelas por Disnios:  table(Dis)
  #                                      table(Dis, EfZona)
  #Asigna **ALEATORIAMENTE** a cada intervalo un diseño
  levels(Dis) <- NombresDis[c(2,3,1)]  #[sample(1:NDis, NDis, replace = F)] 
  
  #Junta la simulación con el Diseño
  Simulacion <- data.frame(Simulacion, "Disenio"=Dis)
  
  #   Cantidad de parcelas por Disnios:  table(Simulacion$Disenio)
  #                                      table(Simulacion$Disenio, Simulacion$Zona)
  # plot(Simulacion$x,Simulacion$y,col=as.factor(paste(Simulacion$Disenio,Simulacion$Zona, sep="_")))
  # plot(Simulacion$x,Simulacion$y,col=as.factor(Simulacion$Disenio))
  # abline(v=seq(0,xcampo, length=Nzonas+1)[-c(1,Nzonas+1)])
  
  # write.table(Simulacion, "C:/Users/Pablo/Desktop/Tabla.txt", quote=FALSE, sep="\t")
  
  ################################    ################################    ################################
  Formula <- "variable1~1"
  MyFile <- Simulacion[,1:3]
  coordinates(MyFile) <- c(1,2)
  Variograma<-autofitVariogram(as.formula(Formula), MyFile, model = "Exp")
  
  ModeloVariograma <- cbind(Variograma$var_model[2,1:3],Variograma$var_model[1,2],Variograma$sserr)
  names(ModeloVariograma)=c("Model", "Parcial Sill","Range","Nugget","SCE")
  
  ##################################################################################################################
  ################################ Asignación de tratamientos dentro de cada diseño ################################
  ##################################################################################################################
  ######Diseño en FRANJA
  #Solo se queda con las parcelas que tienen el diseño en franja
  MisParcelasFR<-Simulacion[Simulacion$Disenio=="DFR",]
  # Asigna un tratamiento a cada coordenada y.
  # Aleatoriza los tratamientos y todas las franjas tienen el mismo orden de Tratamiento
  # o aleatoriza todas las franjas sin restricción
  AsigTrat<-if(AleatFranja){
    AleatTrat<-do.call(rbind,sapply(1:ceiling(length(unique(MisParcelasFR$y))/length(Trat)), 
                                    function(x) data.frame("AsigTrat"=sample(1:length(Trat),length(Trat),replace=FALSE)
                                                           ,"FranjaRep"=paste0("FranjaRep_",x))
                                    , simplify = FALSE))
    TratAsigFr<-Trat[AleatTrat$AsigTrat[1:length(unique(MisParcelasFR$y))]]
    AleatTrat<-AleatTrat[1:length(TratAsigFr),]
    AleatTrat$AsigTrat<-TratAsigFr
    AleatTrat  
  } else { 
    rep(Trat[sample(1:length(Trat),length(Trat),replace=FALSE)],length=length(unique(MisParcelasFR$y)))
  }      
  #Junta las coordenadas y con el orden de los tratamientos
  MisTratAsigFR<-data.frame("y"=unique(MisParcelasFR$y),
                            "Franja"=paste("Franja", trunc(unique(MisParcelasFR$y)), sep="_"),
                            AsigTrat)
  #Junta toda la información de las parcelas con el diseño en Franja sumado al tratamiento que le fue asignado
  MisParcelasFRTrat<-merge(MisParcelasFR, MisTratAsigFR,sort=FALSE)
  
  #Plotea las franjas
  #plot(MisParcelasFRTrat$x, MisParcelasFRTrat$y, col=MisParcelasFRTrat$AsigTrat)
  
  
  ######Diseño en BLOQUE Completamente Aleatorizado
  #Selecciona las parcelas a las que le tocó DBCA
  MisParcelasDBCA<-Simulacion[Simulacion$Disenio=="DBCA",]
  #Genera la división de bloques en el eje X y el Eje Y
  FranX<-findInterval(MisParcelasDBCA$x, seq(min(MisParcelasDBCA$x),max(MisParcelasDBCA$x), 
                                             length=length(unique(MisParcelasDBCA$x))),  left.open = FALSE, rightmost.closed=FALSE)
  # FranY<-findInterval(MisParcelasDBCA$y, seq(min(MisParcelasDBCA$y),max(MisParcelasDBCA$y), 
  #                                            length=ceiling(length(unique(MisParcelasDBCA$y))/length(Trat))),  left.open = TRUE, rightmost.closed=FALSE)
  FranY<-findInterval(MisParcelasDBCA$y, seq(min(MisParcelasDBCA$y),max(MisParcelasDBCA$y), 
                                             by=min(diff(unique(MisParcelasDBCA$y)))*length(Trat)),  left.open = FALSE, rightmost.closed=TRUE)
  #Genera los bloques individuales que es la combinación de la división anterior
  MisParcelasBloq <- data.frame(MisParcelasDBCA, "FranjaX"=FranX,"FranjaY"=FranY, "Bloque"=paste(FranX,FranY, sep="_"))
  #Separa MisParcelasBloq en Bloques incompletos y en bloques completos
  BloquesIncompletos<-names(table(MisParcelasBloq$Bloque))[table(MisParcelasBloq$Bloque)<length(Trat)]
  MisParcelasBloqComp <- MisParcelasBloq[!MisParcelasBloq$Bloque %in% BloquesIncompletos, ]
  MisParcelasBloqIncomp <- MisParcelasBloq[MisParcelasBloq$Bloque %in% BloquesIncompletos, ]
  #Aleatoriza los tratamientos dentro de cada bloque
  MisTratBloque<-with(MisParcelasBloq,
                      by(MisParcelasBloqComp, MisParcelasBloqComp$Bloque,FUN= function (x) {
                        data.frame(x,"AsigTrat"=Trat[sample(1:length(Trat),length(Trat),replace=FALSE)])}, simplify = FALSE
                      ))
  
  MisTratBloqueCompleto<-do.call(rbind, MisTratBloque)
  #Saca las columnas de FranjaX y FranjaY generadas para formar bloques
  MisTratBloqueCompleto<-MisTratBloqueCompleto[,!colnames(MisTratBloqueCompleto) %in% c("FranjaX", "FranjaY")]
  MisParcelasBloqIncomp<-MisParcelasBloqIncomp[,!colnames(MisParcelasBloqIncomp) %in% c("FranjaX", "FranjaY")]
  #Los bloques incompletos no tienen tratamientos
  MisParcelasBloqIncomp$AsigTrat <- NA
  #Combina los bloques incompletos y completos en una sola tabla
  MisParcelasDBCATrat<-rbind(MisTratBloqueCompleto, MisParcelasBloqIncomp)
  
  
  # plot(MisParcelasDBCA$x,MisParcelasDBCA$y, pch=23, cex=2)
  # plot(MisParcelasBloq$x, MisParcelasBloq$y, col=MisParcelasBloq$Bloque)
  # abline(h=seq(min(MisParcelasDBCA$y),max(MisParcelasDBCA$y), 
  #              by=min(diff(unique(MisParcelasDBCA$y)))*length(Trat)))
  #text(MisParcelasBloq$x, MisParcelasBloq$y, labels=MisParcelasBloq$Bloque, srt = 90)
  #
  
  #######Diseño Completamente Aleatorizado DCA
  MisParcelasDCA<-Simulacion[Simulacion$Disenio=="DCA",]
  #Asigna tratamientos aleatoriamente###### ?????
  MisParcelasDCA$AsigTrat <- sample(rep(Trat,length=nrow(MisParcelasDCA)), replace=FALSE)
  MisParcelasDCATrat<-MisParcelasDCA
  
  ##################################################################################################################
  ###############################              Asignación de RENDIMIENTO              ############################## 
  ##################################################################################################################
  #Aleatoriza un beta1 y beta2 SIN CORRELACION!!!!!!!!!!                                                                #######################################
  misBetas<-mvrnorm(n = 1, mu=c(mediaBeta1,mediaBeta2), Sigma=matrix(c(varBeta1,rep(covBetas,2),varBeta2),2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
  miBeta1<- misBetas[1]
  miBeta2<- misBetas[2]  
  # miBeta1<-rnorm(1, mediaBeta1,varBeta1)
  # miBeta2<-rnorm(1, mediaBeta2,varBeta2)
  # mediaBeta2<- -0.0004
  # varBeta2<- 0.00000001
  # misBetas<-mvrnorm(n = 100000, mu=c(mediaBeta1,mediaBeta2), Sigma=matrix(c(varBeta1,rep(covBetas,2),varBeta2),2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
  # plot(misBetas)
  # abline(h=0)
  TerInterZonError<- 0.0034
  EstRto <- function (df1,miBeta11=miBeta1, miBeta22=miBeta2, TerInterZonError=0.0034){
    MiRto<-df1["AsigTrat"]
    names(MiRto)<-"MiRto"
    MiRto<- miBeta11 * MiRto + miBeta22 * MiRto^2 + TerInterZonError*MiRto*df1["Zona"]+ df1["ErrorConEfecto"]                  ############################## 
    data.frame(df1, MiRto)     
  }
  # write.table(MisParcelasFRTrat, paste0("clipboard-", 2^10), quote = FALSE, sep="\t", row.names = FALSE)
  MisParcelasFRTrat<-EstRto(MisParcelasFRTrat)
  MisParcelasDCATrat <-EstRto(MisParcelasDCATrat)
  MisParcelasDBCATrat <-EstRto(MisParcelasDBCATrat)
  
  # plot(MisParcelasFRTrat$AsigTrat+1, t(MisParcelasFRTrat$MiRto), col='blue')
  # points(MisParcelasDCATrat$AsigTrat+2, t(MisParcelasDCATrat$MiRto), col='red')
  # points(MisParcelasDBCATrat$AsigTrat+3, t(MisParcelasDBCATrat$MiRto), col='darkgreen')
  # Cantidad de parcelas por tratamiento asignado:
  # table(MisParcelasFRTrat$AsigTrat)
  # table(MisParcelasDCATrat$AsigTrat)
  # table(MisParcelasDBCATrat$AsigTrat)
  ##################################################################################################################
  ###############################                  AJUSTE DE MODELOS                  ############################## 
  ##################################################################################################################
  #El modelo de ANAVA nulo se ajusta con ErrorConEfecto DE ZONA, habria que ajustarlo con los valores sin el efecto zona??????????  #######################################        ##############################
  Formula.Nulo <- "ErrorConEfecto~1+AsigTrat"
  Formula.Nulo.Zo <- "ErrorConEfecto~1+AsigTrat+as.factor(Zona)+AsigTrat:as.factor(Zona)"
  
  Formula.Rto <- "MiRto~1+AsigTrat+I(AsigTrat^2)"
  Formula.Rto.Zo <- "MiRto~1+AsigTrat+as.factor(Zona)+AsigTrat:as.factor(Zona)+I(AsigTrat^2)"
  
  pb = txtProgressBar(min = 0, max = 26, initial = 0, style = 3, title="Ajustando Modelos")  

  ####### Diseño en FRANJA  // MisParcelasFRTrat
  ### ANAVA NULO CON ERRORES
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.FR.Nulo.FrA<-lme(as.formula(Formula.Nulo)
                     ,random=list(FranjaRep=pdIdent(~1))                  ##############################                   ##############################                   ############################## 
                     ,method="REML"
                     ,control=lmeControl(niterEM=150
                                         ,msMaxIter=200)
                     ,na.action=na.omit
                     ,data=MisParcelasFRTrat
                     ,keep.data=FALSE)
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
  # Modelo<-M.DCA
  # Modelo<-M.DCA.Co
  # Modelo<-M.DCA.Zo.Co
  # Modelo <- M.FR.Zo.FrA.Co
  # Modelo <- M.FR.Zo.FrA
  ResumirModelo<-function (Modelo) {
    ModelResumen<-data.frame(
      "Modelo"=deparse(substitute(Modelo)),
      "rango"=as.numeric(as.character(capture.output(Modelo$modelStruct$corStruct)[3]))*3,  ###################### SE MULTIPLICA POR TRES PARA PODER COMPARARLO CON EL RANGO REAL 
      "Akaike"=AIC(Modelo),
      "SigmaError"=sigma(Modelo))
    if(class(Modelo) == "gls") {
      TablaAnava<-summary(Modelo)$tTable
      colnames(TablaAnava)<-c("Value","Std.Error","t-value","p-value")
      Betas<-data.frame("Modelo"=deparse(substitute(Modelo)),
                        "Coeficientes"=rownames(summary(Modelo)$tTable),
                        TablaAnava,"DF"=NA,row.names = NULL)[,c(1:4,7,5:6)] }
    
    if(class(Modelo) == "lme") {
      TablaAnava<-summary(Modelo)$tTable
      colnames(TablaAnava)<-c("Value","Std.Error","DF","t-value","p-value")
      Betas<-data.frame("Modelo"=deparse(substitute(Modelo)),
                        "Coeficientes"=rownames(summary(Modelo)$tTable),
                        TablaAnava,row.names = NULL) }
    if(class(Modelo) == "lm") {
      TablaAnava<- summary(Modelo)$coefficients
      colnames(TablaAnava)<-c("Value","Std.Error","t-value","p-value")
      Betas<-data.frame("Modelo"=deparse(substitute(Modelo)),
                        "Coeficientes"=rownames(summary(Modelo)$coefficients),
                        TablaAnava, "DF"=NA, row.names = NULL)[,c(1:4,7,5:6)]  }
    return(list("Resumen"=ModelResumen, "Betas"=Betas))
  }
  
  ResumenModelos<-sapply(ls(pattern = "M[.]"), function(Es) {eval(parse(text=paste0("ResumirModelo(", Es,")")))},
                         USE.NAMES = TRUE,simplify = FALSE)
  
  ResumenResult<-do.call(rbind,lapply(ResumenModelos, function (x) {x$Resumen}))
  ResumenResult$RangoReal <- NA
  ResumenResult[!is.na(ResumenResult$rango),"RangoReal"] <- ModeloVariograma$Range
  setTxtProgressBar(pb, 25)
  

  
  #Betas Estimados
  BetasEst<-do.call(rbind,lapply(ResumenModelos, function (x) {x$Betas}) )  ### INTENTAR SACARLO DE LA LISTA
  BetasEst$Orden<-1:nrow(BetasEst)
  #Betas Reales
  BetasReales<-data.frame(
    "Coeficientes"=c("AsigTrat", "I(AsigTrat^2)","AsigTrat:as.factor(Zona)24.6"),
    "ValoresReales"=c(miBeta1,miBeta2,TerInterZonError*unique(Simulacion$Zona)[2]))
  BetasEstReal<-merge(BetasReales,BetasEst, all.y=T, sort=FALSE)
  BetasEstReal<-BetasEstReal[order(BetasEstReal$Orden),c(3,1,4,2,5:(ncol(BetasEstReal)-1))] # Es -1 para que saque la columna Orden
  setTxtProgressBar(pb, 26)
  # close(pb)
  return(list("Resumen"=ResumenResult,"BetasEstReal"=BetasEstReal, "Semilla"=MiSemilla))
  
}


set.seed(1)
Ad<-sapply(1:1, function (i) {
  print(paste("Simulacion", i ,"de", 1000))
  GenerarCampoAjustarModelos() })
save.image("DatosSimulados.RData")


# plot(A$Modelo,A$SigmaError, las=3)
# abline(h=0)
# plot(as.numeric(as.character(A$rango)),A$SigmaError)
# 
# plot(as.numeric(as.character(Result$rango)), Result$SigmaError)
# 
# 
# 
# 
# 









# 
# max(Dis)
# 
# var(Simulacion$variable1)
# RFcalc(modelo)
# 
# 
# plot(Simulacion$x, Simulacion$y, col= findInterval(Simulacion$x, seq(0,xcampo, length=Nzonas+1)[-c(1,Nzonas+1)],  left.open = TRUE)+1)
# # abline(v=DivideZona, col='red')
# # abline(v=c(1000/3, 1000/3 + 1000/3), col='blue')
# # abline(v=c(1000/4, 1000/4 + 1000/4, 1000/4 + 1000/4+ 1000/4), col='darkgreen')
# 
# dev.new()
# abline(a=0, b=1)
# image(Simulacion)
# 
# 
# 
# aggregate(ErrorConEfecto ~ Zona, data=Simulacion, mean)
# 
# 
# 
# 
# 
# 
# 
# color.gradient <- function(x, colors=c("red","yellow","darkgreen"), colsteps=100) {
#   return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
# }
# dev.new()
# plot(Simulacion[,1:2], col= color.gradient(Simulacion$ErrorConEfecto), pch=20, cex=2)
# dev.new()
# plot(Simulacion[,1:2], col= color.gradient(Simulacion$variable1), pch=20, cex=2)
# 
# 
# 
# 
# 
# z <- RFsimulate(RMexp(), x, spConform=FALSE)
# simu2 <- conventional2RFspDataFrame(simu, coord=expand.grid(x,y))
# Print(simu,simu2)
# 
# 
# 
# 
# valorError<-as.data.frame(simu)
# class(valorError[,1])
# color.gradient(as.data.frame(simu)[,1])
# # dev.new()
# # heat.colors()
# # topo.colors(data.frame(simu))
# 
# plot(simu)
# Simulacion <- data.frame(simu)
# as.data.frame(simu)
# 
# str(simu)
# 
