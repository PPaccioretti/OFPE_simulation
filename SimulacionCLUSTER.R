# setwd("C:/Users/Pablo/OneDrive/Documentos/Estadística/Análisis de datos/Estadística Espacial - Mariano/Simulacion Ensayos")
# 
# require(RandomFields)
# require(MASS)
# require(nlme)
# require(automap)
# 
# # set.seed(1)
# # RFoptions(seed = seed, always_open_device=T, always_close_device=F)
# 
# # #Dimensiones Campo
# # xcampo<- 1000 #metros
# # ycampo<- 1000 #metros
# #
# # #Dimensiones Parcela
# # xlargoparcela<- 70 #metros
# # yanchoparcela<- 30 #metros
# #
# # #Parámetros error correlacionado Exponencial
# # #Recopilación de 247 mapas de rendimiento de maíz: Rango 49metros, sill=var 7, nugget 1.2
# # rango <- 49 #Scale = Rango ~Gamma (41.3, 17.5)
# # sill <- 7 #Sill = Var de Exp
# # nugget <- 1.2 #varianza nugget ~ Weibull ()
# # MediaCultivo<- 58
# # #Número de zonas (Divide al eje X)
# # Nzonas <- 2
# #
# # #HeterogeneidadEntreZonas (Heterog *(Sill+Nugget) )
# # Heterog <- 3
# #
# # #Tratamientos
# # # Trat <- c(1,40,70,100,140)
# # Trat <- c(1,70,140,210,280)
# # #Betas de regresion
# # mediaBeta1 <- 0.12
# # varBeta1<- 0.0001
# # mediaBeta2<- -0.0004
# # varBeta2<- 0.00000001
# # covBetas<- 0
# 
# GenerarCampo <- function (
#   i,
#   #Dimensiones Campo
#   xcampo= 1000 #metros
#   ,ycampo= 1000 #metros
# 
#   #Dimensiones Parcela
#   ,xlargoparcela= 70 #metros
#   ,yanchoparcela= 30 #metros
# 
#   #Parámetros error correlacionado Exponencial
#   #Recopilación de 247 mapas de rendimiento de maíz: Rango 49metros, sill=var 7, nugget 1.2
#   ,rango = 49 #Scale = Rango ~Gamma (41.3, 17.5)
#   ,sill = 7 #Sill = Var de Exp
#   ,nugget = 1.2 #varianza nugget ~ Weibull ()
#   ,MediaCultivo= 80     ################################################################################################################################
#   #Número de zonas (Divide al eje X)
#   ,Nzonas = 2
# 
#   #HeterogeneidadEntreZonas (Heterog *(Sill+Nugget) )
#   ,Heterog = 3
# 
#   #Tratamientos
#   ,Trat = c(0,70,140,210,280)
# 
#   #Betas de regresion
#   ,mediaBeta1= 0.12
#   ,varBeta1= 0.0001
#   ,mediaBeta2= -0.0004
#   ,varBeta2= 0.00000001
#   ,covBetas=0
# 
# ){
#   ##################################################################################################################
# 
#   #Futuras coordenadas del campo
#   x <- seq(0, xcampo, xcampo/xlargoparcela)
#   y <- seq(0, ycampo, ycampo/yanchoparcela)
#   #Fija una semilla para la simulación
#   MiSemilla=round(runif(1,min=-99999999999,max=99999999999 ),0)
#   RFoptions(seed = MiSemilla )
#   #Estima el modelo y Simula los datos
#   modelo<-RMexp(var=sill, scale = rango)+ RMnugget(var=nugget)+ RMtrend(mean=MediaCultivo)
#   simu <- RFsimulate(modelo,x,y,  spConform=TRUE)#, x=x, y=y)
#   #Crea un data.frame con la simulacion
#   Simulacion<-data.frame(expand.grid(list("x"=x,"y"=y)), "Error"=simu)
#   #   Cantidad de parcelas en la simulacion: nrow(Simulacion)
# 
#   #Categoriza las coordenadas del eje X en Nzonas (Sumar 5 y 10 cte???)
#   EfZona<- findInterval(Simulacion$x, seq(0,xcampo, length=Nzonas+1)[-c(1,Nzonas+1)],
#                         left.open = TRUE) * (Heterog * (sill+nugget))
#   #   Cantidad de parcelas por zona: table(as.factor(EfZona))
# 
#   #Junta la simulacion con la zona
#   Simulacion<- data.frame(Simulacion, "Zona"=EfZona, "ErrorConEfecto"=Simulacion$variable1+EfZona)
#   # plot(Simulacion$x,Simulacion$y,col=as.factor(Simulacion$Zona))
#   # abline(v=seq(0,xcampo, length=Nzonas+1)[-c(1,Nzonas+1)])
# 
#   #Divide al eje y en Ndis=3 zonas para repartir los Diseños
#   NDis<- 3                                ################################
#   NombresDis<-c("DBCA", "DCA", "DFR")     ################################
#   AleatFranja <- TRUE                     ################################
#   Dis<- as.factor(findInterval(Simulacion$y, seq(0,ycampo, length=NDis+1)[-c(1,NDis+1)],  left.open = TRUE))
#   #   Cantidad de parcelas por Disnios:  table(Dis)
#   #                                      table(Dis, EfZona)
#   #Asigna **ALEATORIAMENTE** a cada intervalo un diseño
#   levels(Dis) <- NombresDis[c(2,3,1)]  #[sample(1:NDis, NDis, replace = F)]
# 
#   #Junta la simulación con el Diseño
#   Simulacion <- data.frame(Simulacion, "Disenio"=Dis)
# 
#   #   Cantidad de parcelas por Disnios:  table(Simulacion$Disenio)
#   #                                      table(Simulacion$Disenio, Simulacion$Zona)
#   # plot(Simulacion$x,Simulacion$y,col=as.factor(paste(Simulacion$Disenio,Simulacion$Zona, sep="_")))
#   # plot(Simulacion$x,Simulacion$y,col=as.factor(Simulacion$Disenio))
#   # abline(v=seq(0,xcampo, length=Nzonas+1)[-c(1,Nzonas+1)])
# 
#   # write.table(Simulacion, "C:/Users/Pablo/Desktop/Tabla.txt", quote=FALSE, sep="\t")
# 
#   ################################    ################################    ################################
#   Formula <- "variable1~1"
#   MyFile <- Simulacion[,1:3]
#   coordinates(MyFile) <- c(1,2)
#   Variograma<-autofitVariogram(as.formula(Formula), MyFile, model = "Exp")
# 
#   ModeloVariograma <- cbind(Variograma$var_model[2,1:3],Variograma$var_model[1,2],Variograma$sserr)
#   names(ModeloVariograma)=c("Model", "Parcial Sill","Range","Nugget","SCE")
# 
#   ##################################################################################################################
#   ################################ Asignación de tratamientos dentro de cada diseño ################################
#   ##################################################################################################################
#   ######Diseño en FRANJA
#   #Solo se queda con las parcelas que tienen el diseño en franja
#   MisParcelasFR<-Simulacion[Simulacion$Disenio=="DFR",]
#   # Asigna un tratamiento a cada coordenada y.
#   # Aleatoriza los tratamientos y todas las franjas tienen el mismo orden de Tratamiento
#   # o aleatoriza todas las franjas sin restricción
#   AsigTrat<-if(AleatFranja){
#     AleatTrat<-do.call(rbind,sapply(1:ceiling(length(unique(MisParcelasFR$y))/length(Trat)),
#                                     function(x) data.frame("AsigTrat"=sample(1:length(Trat),length(Trat),replace=FALSE)
#                                                            ,"FranjaRep"=paste0("FranjaRep_",x))
#                                     , simplify = FALSE))
#     TratAsigFr<-Trat[AleatTrat$AsigTrat[1:length(unique(MisParcelasFR$y))]]
#     AleatTrat<-AleatTrat[1:length(TratAsigFr),]
#     AleatTrat$AsigTrat<-TratAsigFr
#     AleatTrat
#   } else {
#     rep(Trat[sample(1:length(Trat),length(Trat),replace=FALSE)],length=length(unique(MisParcelasFR$y)))
#   }
#   #Junta las coordenadas y con el orden de los tratamientos
#   MisTratAsigFR<-data.frame("y"=unique(MisParcelasFR$y),
#                             "Franja"=paste("Franja", trunc(unique(MisParcelasFR$y)), sep="_"),
#                             AsigTrat)
#   #Junta toda la información de las parcelas con el diseño en Franja sumado al tratamiento que le fue asignado
#   MisParcelasFRTrat<-merge(MisParcelasFR, MisTratAsigFR,sort=FALSE)
# 
#   #Plotea las franjas
#   #plot(MisParcelasFRTrat$x, MisParcelasFRTrat$y, col=MisParcelasFRTrat$AsigTrat)
# 
# 
#   ######Diseño en BLOQUE Completamente Aleatorizado
#   #Selecciona las parcelas a las que le tocó DBCA
#   MisParcelasDBCA<-Simulacion[Simulacion$Disenio=="DBCA",]
#   #Genera la división de bloques en el eje X y el Eje Y
#   FranX<-findInterval(MisParcelasDBCA$x, seq(min(MisParcelasDBCA$x),max(MisParcelasDBCA$x),
#                                              length=length(unique(MisParcelasDBCA$x))),  left.open = FALSE, rightmost.closed=FALSE)
#   # FranY<-findInterval(MisParcelasDBCA$y, seq(min(MisParcelasDBCA$y),max(MisParcelasDBCA$y),
#   #                                            length=ceiling(length(unique(MisParcelasDBCA$y))/length(Trat))),  left.open = TRUE, rightmost.closed=FALSE)
#   FranY<-findInterval(MisParcelasDBCA$y, seq(min(MisParcelasDBCA$y),max(MisParcelasDBCA$y),
#                                              by=min(diff(unique(MisParcelasDBCA$y)))*length(Trat)),  left.open = FALSE, rightmost.closed=TRUE)
#   #Genera los bloques individuales que es la combinación de la división anterior
#   MisParcelasBloq <- data.frame(MisParcelasDBCA, "FranjaX"=FranX,"FranjaY"=FranY, "Bloque"=paste(FranX,FranY, sep="_"))
#   #Separa MisParcelasBloq en Bloques incompletos y en bloques completos
#   BloquesIncompletos<-names(table(MisParcelasBloq$Bloque))[table(MisParcelasBloq$Bloque)<length(Trat)]
#   MisParcelasBloqComp <- MisParcelasBloq[!MisParcelasBloq$Bloque %in% BloquesIncompletos, ]
#   MisParcelasBloqIncomp <- MisParcelasBloq[MisParcelasBloq$Bloque %in% BloquesIncompletos, ]
#   #Aleatoriza los tratamientos dentro de cada bloque
#   MisTratBloque<-with(MisParcelasBloq,
#                       by(MisParcelasBloqComp, MisParcelasBloqComp$Bloque,FUN= function (x) {
#                         data.frame(x,"AsigTrat"=Trat[sample(1:length(Trat),length(Trat),replace=FALSE)])}, simplify = FALSE
#                       ))
# 
#   MisTratBloqueCompleto<-do.call(rbind, MisTratBloque)
#   #Saca las columnas de FranjaX y FranjaY generadas para formar bloques
#   MisTratBloqueCompleto<-MisTratBloqueCompleto[,!colnames(MisTratBloqueCompleto) %in% c("FranjaX", "FranjaY")]
#   MisParcelasBloqIncomp<-MisParcelasBloqIncomp[,!colnames(MisParcelasBloqIncomp) %in% c("FranjaX", "FranjaY")]
#   #Los bloques incompletos no tienen tratamientos
#   MisParcelasBloqIncomp$AsigTrat <- NA
#   #Combina los bloques incompletos y completos en una sola tabla
#   MisParcelasDBCATrat<-rbind(MisTratBloqueCompleto, MisParcelasBloqIncomp)
# 
# 
#   # plot(MisParcelasDBCA$x,MisParcelasDBCA$y, pch=23, cex=2)
#   # plot(MisParcelasBloq$x, MisParcelasBloq$y, col=MisParcelasBloq$Bloque)
#   # abline(h=seq(min(MisParcelasDBCA$y),max(MisParcelasDBCA$y),
#   #              by=min(diff(unique(MisParcelasDBCA$y)))*length(Trat)))
#   #text(MisParcelasBloq$x, MisParcelasBloq$y, labels=MisParcelasBloq$Bloque, srt = 90)
#   #
# 
#   #######Diseño Completamente Aleatorizado DCA
#   MisParcelasDCA<-Simulacion[Simulacion$Disenio=="DCA",]
#   #Asigna tratamientos aleatoriamente###### ?????
#   MisParcelasDCA$AsigTrat <- sample(rep(Trat,length=nrow(MisParcelasDCA)), replace=FALSE)
#   MisParcelasDCATrat<-MisParcelasDCA
# 
#   ##################################################################################################################
#   ###############################              Asignación de RENDIMIENTO              ##############################
#   ##################################################################################################################
#   #Aleatoriza un beta1 y beta2 SIN CORRELACION!!!!!!!!!!                                                                #######################################
#   misBetas<-mvrnorm(n = 1, mu=c(mediaBeta1,mediaBeta2), Sigma=matrix(c(varBeta1,rep(covBetas,2),varBeta2),2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
#   miBeta1<- misBetas[1]
#   miBeta2<- misBetas[2]
#   # miBeta1<-rnorm(1, mediaBeta1,varBeta1)
#   # miBeta2<-rnorm(1, mediaBeta2,varBeta2)
#   # mediaBeta2<- -0.0004
#   # varBeta2<- 0.00000001
#   # misBetas<-mvrnorm(n = 100000, mu=c(mediaBeta1,mediaBeta2), Sigma=matrix(c(varBeta1,rep(covBetas,2),varBeta2),2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
#   # plot(misBetas)
#   # abline(h=0)
#   TerInterZonError<- 0.0034
#   EstRto <- function (df1,miBeta11=miBeta1, miBeta22=miBeta2, TerInterZonError=0.0034){
#     MiRto<-df1["AsigTrat"]
#     names(MiRto)<-"MiRto"
#     MiRto<- miBeta11 * MiRto + miBeta22 * MiRto^2 + TerInterZonError*MiRto*df1["Zona"]+ df1["ErrorConEfecto"]                  ##############################
#     data.frame(df1, MiRto)
#   }
#   # write.table(MisParcelasFRTrat, paste0("clipboard-", 2^10), quote = FALSE, sep="\t", row.names = FALSE)
#   MisParcelasFRTrat<-EstRto(MisParcelasFRTrat)
#   MisParcelasDCATrat <-EstRto(MisParcelasDCATrat)
#   MisParcelasDBCATrat <-EstRto(MisParcelasDBCATrat)
# 
#   Param<-paste(i,MiSemilla,ModeloVariograma$Range, miBeta1, miBeta2, TerInterZonError*unique(Simulacion$Zona)[2], sep="_")
#   write.table(MisParcelasFRTrat, paste0("./TablasSimuladas/Franja/",Param,"_Fr.txt"), quote=FALSE,row.names=FALSE, sep="\t")
#   write.table(MisParcelasDCATrat, paste0("./TablasSimuladas/DCA/",Param,"_DCA.txt"), quote=FALSE,row.names=FALSE, sep="\t")
#   write.table(MisParcelasDBCATrat, paste0("./TablasSimuladas/DBCA/",Param,"_DBCA.txt"), quote=FALSE,row.names=FALSE, sep="\t")
# 
# }
# set.seed(2)
# sapply(1:1000, function(i){GenerarCampo(i)})
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


AjustarModelo<- function (i,MisArchDBCA,MisArchDCA,MisArchFranja) {
MiRep<-strsplit(MisArchDBCA[i],"_")[[1]][1]
MiSemilla<-strsplit(MisArchDBCA[i],"_")[[1]][2]
ModeloVariograma<-strsplit(MisArchDBCA[i],"_")[[1]][3]
miBeta1<-strsplit(MisArchDBCA[i],"_")[[1]][4]
miBeta2<-strsplit(MisArchDBCA[i],"_")[[1]][5]
Interaccion<-strsplit(MisArchDBCA[i],"_")[[1]][6]
# i<-as.numeric(as.character(i))

MisParcelasFRTrat<- read.table(paste0("/home/elmer/monicalab/TablasSimuladas/Franja/", MisArchFranja[i]), header=T, sep="\t")
MisParcelasDCATrat<- read.table(paste0("/home/elmer/monicalab/TablasSimuladas/DCA/", MisArchDCA[i]), header=T, sep="\t")
MisParcelasDBCATrat<- read.table(paste0("/home/elmer/monicalab/TablasSimuladas/DBCA/", MisArchDBCA[i]), header=T, sep="\t")

  
  # AjustarModelo<- function (i,MisParcelasFRTrat,MisParcelasDCATrat,MisParcelasDBCATrat, MiRep,
  #                           MiSemilla,ModeloVariograma,miBeta1,miBeta2,Interaccion ) {  
  # 
  
  
  
  #El modelo de ANAVA nulo se ajusta con ErrorConEfecto DE ZONA, habria que ajustarlo con los valores sin el efecto zona??????????  #######################################        ##############################
  Formula.Nulo <- "ErrorConEfecto~1+AsigTrat"
  Formula.Nulo.Zo <- "ErrorConEfecto~1+AsigTrat+as.factor(Zona)+AsigTrat:as.factor(Zona)"
  
  Formula.Rto <- "MiRto~1+AsigTrat+I(AsigTrat^2)"
  Formula.Rto.Zo <- "MiRto~1+AsigTrat+as.factor(Zona)+AsigTrat:as.factor(Zona)+I(AsigTrat^2)"
  
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

  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.FR.Nulo.Zo.FrA <-lme(as.formula(Formula.Nulo.Zo)
                         ,random=list(FranjaRep=pdIdent(~1))
                         ,method="REML"
                         ,control=lmeControl(niterEM=150
                                             ,msMaxIter=200)
                         ,na.action=na.omit
                         ,data=MisParcelasFRTrat
                         ,keep.data=FALSE)

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

  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.FR.Zo.FrA <-lme(as.formula(Formula.Rto.Zo)
                    ,random=list(FranjaRep=pdIdent(~1))
                    ,method="REML"
                    ,control=lmeControl(niterEM=150
                                        ,msMaxIter=200)
                    ,na.action=na.omit
                    ,data=MisParcelasFRTrat
                    ,keep.data=FALSE)

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

  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.DBCA.Nulo.Zo.BlA <-lme(as.formula(Formula.Nulo.Zo)
                           ,random=list(Bloque=pdIdent(~1))
                           ,method="REML"
                           ,control=lmeControl(niterEM=150
                                               ,msMaxIter=200)
                           ,na.action=na.omit
                           ,data=MisParcelasDBCATrat
                           ,keep.data=FALSE)

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
 
  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.DBCA.Zo.BlA <-lme(as.formula(Formula.Rto.Zo)
                      ,random=list(Bloque=pdIdent(~1))
                      ,method="REML"
                      ,control=lmeControl(niterEM=150
                                          ,msMaxIter=200)
                      ,na.action=na.omit
                      ,data=MisParcelasDBCATrat
                      ,keep.data=FALSE)

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

  #ANOVA CON CORRELACION ESPACIAL 
  M.DCA.Nulo.Co<-gls(as.formula(Formula.Nulo)
                     ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                         ,metric="euclidean"
                                         ,nugget=FALSE)
                     ,method="REML"
                     ,na.action=na.omit
                     ,data=MisParcelasDCATrat)

  #ANOVA = ErrorConEfecto~1+AsigTrat
  M.DCA.Nulo.Zo<-lm(as.formula(Formula.Nulo.Zo)
                    ,na.action=na.omit
                    ,data=MisParcelasDCATrat)

  #ANOVA CON CORRELACION ESPACIAL 
  M.DCA.Nulo.Zo.Co<-gls(as.formula(Formula.Nulo.Zo)
                        ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                            ,metric="euclidean"
                                            ,nugget=FALSE)
                        ,method="REML"
                        ,na.action=na.omit
                        ,data=MisParcelasDCATrat)

  ##### CON RENDIMIENTO      
  #ANOVA = MiRto~1+AsigTrat
  M.DCA<-lm(as.formula(Formula.Rto)
            ,na.action=na.omit
            ,data=MisParcelasDCATrat)

  #ANOVA CON EFECTO ALEATORIO FRANJA
  M.DCA.Zo<-lm(as.formula(Formula.Rto.Zo)
               ,na.action=na.omit
               ,data=MisParcelasDCATrat)

  #ANOVA CON CORRELACION ESPACIAL 
  M.DCA.Co<-gls(as.formula(Formula.Rto)
                ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                    ,metric="euclidean"
                                    ,nugget=FALSE)
                ,method="REML"
                ,na.action=na.omit
                ,data=MisParcelasDCATrat)

  #ANOVA CON CORRELACION ESPACIAL 
  M.DCA.Zo.Co<-gls(as.formula(Formula.Rto.Zo)
                   ,correlation=corExp(form=~as.numeric(as.character(x))+as.numeric(as.character(y))
                                       ,metric="euclidean"
                                       ,nugget=FALSE)
                   ,method="REML"
                   ,na.action=na.omit
                   ,data=MisParcelasDCATrat)

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
      "SigmaError"=tryCatch({sigma(Modelo)}, error=function (e) {sigma(summary(Modelo))}, error=function(e){NA} ))
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
  ResumenResult[!is.na(ResumenResult$rango),"RangoReal"] <- ModeloVariograma

  

  
  #Betas Estimados
  BetasEst<-do.call(rbind,lapply(ResumenModelos, function (x) {x$Betas}) )  ### INTENTAR SACARLO DE LA LISTA
  BetasEst$Orden<-1:nrow(BetasEst)
  #Betas Reales
  BetasReales<-data.frame(
    "Coeficientes"=c("AsigTrat", "I(AsigTrat^2)","AsigTrat:as.factor(Zona)24.6"),
    "ValoresReales"=c(miBeta1,miBeta2,Interaccion))
  BetasEstReal<-merge(BetasReales,BetasEst, all.y=T, sort=FALSE)
  BetasEstReal<-BetasEstReal[order(BetasEstReal$Orden),c(3,1,4,2,5:(ncol(BetasEstReal)-1))] # Es -1 para que saque la columna Orden

  return(list("Resumen"=ResumenResult,"BetasEstReal"=BetasEstReal, "Semilla"=MiSemilla))
  
}




require(parallel)
require(MASS)
require(nlme)
# require(automap)

set.seed(1)
# sapply(1:1000, function (i) {GenerarCampo(i) })
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,{library(MASS);library(nlme); library(base)})#; source("/home/elmer/monicalab/TablasSimuladas/FuncionAjustar.R")})

MisArchDBCA=list.files("/home/elmer/monicalab/TablasSimuladas/DBCA")
MisArchDCA=list.files("/home/elmer/monicalab/TablasSimuladas/DCA")
MisArchFranja=list.files("/home/elmer/monicalab/TablasSimuladas/Franja")
#put objects in place that might be needed for the code
clusterExport(cl,c("MisArchDBCA","MisArchDCA","MisArchFranja", "AjustarModelo"))

# =list.files("/home/elmer/monicalab/TablasSimuladas/DBCA"),
                   # MisArchDCA=list.files("/home/elmer/monicalab/TablasSimuladas/DCA"),
#                    MisArchFranja=list.files("/home/elmer/monicalab/TablasSimuladas/Franja")
#                    ))
#... then parallel replicate...
# MisResultados<-parSapply(cl, 1:2,FUN = function(i, MisArchDBCA,MisArchDCA, MisArchFranja) {A<-tryCatch({AjustarModelo(i,MisArchDBCA=MisArchDBCA ,MisArchDCA= MisArchDCA,MisArchFranja= MisArchFranja) }, error=function(e){}); return(A)} )
MisResultados<-parSapply(cl, X=1:1000,FUN = function (X) {AjustarModelo(i=as.numeric(X),MisArchDBCA=MisArchDBCA ,MisArchDCA=MisArchDCA,MisArchFranja=MisArchFranja)})

# MisResultados<-parSapply(cl, X=1:5,FUN = function (i=X,MisArchFranja,MisArchDCA,MisArchDBCA) {
#   MiRep<-strsplit(MisArchDBCA,"_")[[1]][1]
#   MiSemilla<-strsplit(MisArchDBCA,"_")[[1]][2]
#   ModeloVariograma<-strsplit(MisArchDBCA,"_")[[1]][3]
#   miBeta1<-strsplit(MisArchDBCA,"_")[[1]][4]
#   miBeta2<-strsplit(MisArchDBCA,"_")[[1]][5]
#   Interaccion<-strsplit(MisArchDBCA,"_")[[1]][6]
#   
#   print(i)
#   MisParcelasFRTrat<- read.table(paste0("/home/elmer/monicalab/TablasSimuladas/Franja/", MisArchFranja[i]), header=T, sep="\t")
#   MisParcelasDCATrat<- read.table(paste0("/home/elmer/monicalab/TablasSimuladas/DCA/", MisArchDCA[i]), header=T, sep="\t")
#   MisParcelasDBCATrat<- read.table(paste0("/home/elmer/monicalab/TablasSimuladas/DBCA/", MisArchDBCA[i]), header=T, sep="\t")
#   i<<-i+1
#   AjustarModelo(i=as.numeric(X),MisParcelasFRTrat,MisParcelasDCATrat,MisParcelasDBCATrat,MiRep,MiSemilla,ModeloVariograma,
#                 miBeta1,miBeta2,Interaccion)}, MisArchFranja,MisArchDCA,MisArchDBCA)


# MisResultados<-parSapply(cl, X=1:5,FUN = function (i=X,MisArchFranja,MisArchDCA,MisArchDBCA) {
#   paste0(i,"/home/elmer/monicalab/TablasSimuladas/Franja/", MisArchFranja[i])
#   
#   
#   })



# AjustarModelo<- function (i,MisParcelasFRTrat,MisParcelasDCATrat,MisParcelasDBCATrat, MiRep,
#                           MiSemilla,ModeloVariograma,miBeta1,miBeta2,Interaccion ) { 
#   
  
# MisResultados<-sapply(1,FUN = function(i) {A<-tryCatch({AjustarModelo(i,MisArchDBCA=MisArchDBCA ,MisArchDCA= MisArchDCA,MisArchFranja= MisArchFranja) }, error=function(e){}); return(A)} )

# save.image(file="/home/elmer/monicalab/TablasSimuladas/Resultados/DatosSimulados.RData")
MisResultados
#stop the cluster
stopCluster(cl)
save.image(file="/home/elmer/monicalab/TablasSimuladas/Resultados/DatosSimulados1.RData")
MisResultados


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
