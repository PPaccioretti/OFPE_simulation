setwd("C:/Users/Pablo/OneDrive/Documentos/Estadística/Análisis de datos/Estadística Espacial - Mariano/Simulacion Ensayos")
load("./DatosSimulados/DatosSimuladosFINAL.RData" ,verbose = FALSE)
Simulaciones<-MisResultados
rownames(Simulaciones)
MedResumen<-do.call(rbind,Simulaciones["Resumen",])
BetasReales<-do.call(rbind,Simulaciones["BetasEstReal",])
Semilla<-do.call(rbind,Simulaciones["Semilla",])

rownames(Simulaciones)
BetasEst<-Simulaciones["BetasEst",]


MisBetas<-rapply(BetasEst, function(x){tryCatch(x[,c("Value","Std.Error")], error=function(e){x[,c("Estimate","Std. Error")]},
                                                error= function (e) {browser()})},
                 how = c("list"))

MisBetas<-lapply(MisBetas,function(x){
  suppressWarnings(
    data.frame("Modelo"=rep(names(x),lapply(x,nrow)),
               "Parametro"=rownames(do.call(rbind,x)),
               do.call(rbind,x))
  )
})
MisBetas<-do.call(rbind,MisBetas)
# nrow(MisBetas)
MisCoef<-BetasReales[BetasReales$Parametro=="AsigTrat" | BetasReales$Parametro=="I(AsigTrat^2)",]
cbind(MisCoef,rep(BetasReales,each=nlevels(MisBetas$Modelo)))
colnames(BetasReales)
B1<-cbind(BetasReales[BetasReales$Coeficientes=="AsigTrat",],"ValueReal"=rep(BetasReales[,1],each=nlevels(BetasReales$Modelo)))
B2<-cbind(BetasReales[BetasReales$Coeficientes=="I(AsigTrat^2)",],"ValueReal"=rep(BetasReales[,2],each=nlevels(BetasReales$Modelo)))

dev.new()
plot(B1$ValueReal,B1$Value, col=B1$Modelo)
legend("bottomleft",legend = unique(B1$Modelo), col=B1$Modelo, lty=4, cex=0.6)
abline(0,1)

plot(B2$ValueReal,B2$Value, col=B2$Modelo)
legend("topleft",legend = unique(B2$Modelo), col=B2$Modelo, lty=4, cex=0.6)
abline(0,1)

par(mar=c(10,2,0.5,0.5))
plot(MedResumen$Modelo,MedResumen$SigmaError, las=2)

nrow(MisCoef)/2
nrow

dim(MisBetas)
nrow(BetasReales)*22
plot(BetasReales)
