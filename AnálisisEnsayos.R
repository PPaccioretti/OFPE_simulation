setwd("C:/Users/Pablo/OneDrive/Documentos/Estadística/Análisis de datos/Estadística Espacial - Mariano/Simulacion Ensayos")
load("./DatosSimulados/DatosSimuladosFINAL.RData" ,verbose = FALSE)
Simulaciones<-MisResultados
rownames(Simulaciones)
MedResumen<-do.call(rbind,Simulaciones["Resumen",])
BetasReales<-do.call(rbind,Simulaciones["BetasEstReal",])
Semilla<-do.call(rbind,Simulaciones["Semilla",])




require(ggplot2)

x11(width=842,height=900)
options(OutDec = ",")
ggplot(MedResumen[MedResumen$Modelo %in% c("M.DBCA.Zo.BlA.Co", "M.DCA.Zo.Co","M.FR.Zo.FrA"), ],aes(x = SigmaError)) +
  stat_ecdf(aes(colour = Modelo), pad=FALSE, show.legend = FALSE, lwd=1.1) +
  scale_x_continuous(limits=c(2.15,3.5),breaks=seq(2,3.5,0.5), name="Desvío estándar residual") +
  scale_y_continuous(name="Distribución empírica") +
  scale_colour_manual(name="Modelo",
                        values=c("#228B22", "#FF4500", "#0000CD"),
                      breaks=c("M.DBCA.Zo.BlA.Co", "M.DCA.Zo.Co", "M.FR.Zo.FrA"),
                      labels=c("DBCA", "DCA", "Franjas")) +
  theme(text = element_text(size=30),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) + ggtitle(label= "Contemplando efecto Zona")




ggplot(MedResumen[MedResumen$Modelo %in% c("M.DBCA.BlA.Co", "M.DCA.Co","M.FR.FrA.Co"), ],aes(x = SigmaError)) +
  stat_ecdf(aes(colour = Modelo), pad=FALSE, show.legend = NA, lwd=1.1) +
  scale_x_continuous(breaks=seq(5,35,5), name="Desvío estándar residual") + theme(axis.ticks.y = element_blank()) +
  scale_y_continuous(name=NULL, labels=NULL)  +
  scale_colour_manual(name="Modelo",
                     values=c("#228B22", "#FF4500", "#0000CD"),
                        breaks=c( "M.DCA.Co","M.DBCA.BlA.Co", "M.FR.FrA.Co"),
                        labels=c( "DCA","DBCA", "Franjas")) +
  theme(text = element_text(size=30),legend.justification=c(1.1,-0.1),legend.position=c(1,0),
        legend.key.size = unit(2.5, 'lines'),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  ggtitle(label= "Sin contemplar efecto Zona")
        # ,rect = element_rect(fill = "black")) # or theme_blank())


# scale_color_manual(values=c("#228B22", "#FF4500", "#0000CD"))



range(MedResumen[MedResumen$Modelo %in% c("M.DBCA.BlA.Co", "M.DCA.Co","M.FR.FrA.Co"), "SigmaError"])





  DBCAZo<-MedResumen$SigmaError[MedResumen$Modelo==DBCA]
  DCAZo<-MedResumen$SigmaError[MedResumen$Modelo==DCA]
  FRZo<-MedResumen$SigmaError[MedResumen$Modelo==FR]
  plot(ecdf(DBCAZo),xlim=range(c(FRZo, DCAZo,DBCAZo)), col='red', lwd=ancholinea, ...)
  plot(ecdf(DCAZo), add=T, col='blue', lwd=ancholinea)
  plot(ecdf(FRZo), add=T, col='darkgreen', lwd=ancholinea) 
  legend("bottomright",legend = c("DBCA", "DCA", "Franja"), col=c("red","blue","green"),lty=1)}

Plotea(DBCA="M.DBCA.Zo.BlA.Co",DCA="M.DCA.Zo.Co",FR="M.FR.Zo.FrA", main="Con Efecto Zona",bty="L", xlab="Desvio Estandar Residual")
Plotea(DBCA="M.DBCA.BlA.Co",DCA="M.DCA.Co",FR="M.FR.FrA.Co", main="Sin Efecto Zona", bty="L",  xlab="Desvio Estandar Residual")
BetasRealesSubset<-BetasReales[BetasReales$Modelo %in% c("M.DBCA.Zo.BlA.Co","M.DCA.Zo.Co","M.FR.Zo.FrA","M.DBCA.BlA.Co","M.DCA.Co","M.FR.FrA.Co"),]

colnames(BetasReales)
Particionada<-sapply(levels(BetasReales$Coeficientes), function (x) {BetasReales[BetasReales$Coeficientes==x,]}, simplify = FALSE)
Particionada$`(Intercept)`
BetasReales$Value
BetasReales$ValoresReales


plot(ecdf(MedResumen$SigmaError[MedResumen$Modelo=="M.DBCA.BlA.Co"]))
plot(ecdf(MedResumen$SigmaError[MedResumen$Modelo=="M.DCA.Co"]))
plot(ecdf(MedResumen$SigmaError[MedResumen$Modelo=="M.FR.FrA.Co"]))




plot(ecdf(BetasReales$Value[BetasReales$Coeficientes=="AsigTrat" & BetasReales$Modelo=="M.DBCA.Zo.BlA.Co"]))




# AsigTrat AsigTrat:as.factor(Zona)24.6 I(AsigTrat^2) (Intercept) as.factor(Zona)24.6
levels(BetasReales$Modelo)
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
MisCoef<-MisBetas[MisBetas$Parametro=="AsigTrat" | MisBetas$Parametro=="I(AsigTrat^2)",]
cbind(MisCoef,rep(BetasReales,each=nlevels(MisBetas$Modelo)))

B1<-cbind(MisBetas[MisBetas$Parametro=="AsigTrat",],"ValueReal"=rep(BetasReales[,1],each=nlevels(MisBetas$Modelo)))
B2<-cbind(MisBetas[MisBetas$Parametro=="I(AsigTrat^2)",],"ValueReal"=rep(BetasReales[,2],each=nlevels(MisBetas$Modelo)))

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















####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################


require(ggplot2)
options(OutDec = ".")
x11(width=842,height=900)
ggplot(MedResumen[MedResumen$Modelo %in% c("M.DBCA.Zo.BlA.Co", "M.DCA.Zo.Co","M.FR.Zo.FrA"), ],aes(x = SigmaError)) +
  stat_ecdf(aes(colour = Modelo), pad=FALSE, show.legend = FALSE, lwd=1.1) +
  scale_x_continuous(limits=c(2.15,3.5),breaks=seq(2,3.5,0.5), name="Residual Standar Deviation") +
  scale_y_continuous(name="Cumulative relative frecuency") +
  scale_colour_manual(name="Model",
                      values=c("#228B22", "#FF4500", "#0000CD"),
                      breaks=c("M.DBCA.Zo.BlA.Co", "M.DCA.Zo.Co", "M.FR.Zo.FrA"),
                      labels=c("RCBD", "RCD", "Strip")) +
  theme(text = element_text(size=30),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) + ggtitle(label= "Accounting for site variables")



x11(width=842,height=900)
ggplot(MedResumen[MedResumen$Modelo %in% c("M.DBCA.BlA.Co", "M.DCA.Co","M.FR.FrA.Co"), ],aes(x = SigmaError)) +
  stat_ecdf(aes(colour = Modelo), pad=FALSE, show.legend = NA, lwd=1.1) +
  scale_x_continuous(breaks=seq(5,35,5), name="Residual Standar Deviation") + theme(axis.ticks.y = element_blank()) +
  scale_y_continuous(name=NULL, labels=NULL)  +
  scale_colour_manual(name="Model",
                      values=c("#228B22", "#FF4500", "#0000CD"),
                      breaks=c( "M.DCA.Co","M.DBCA.BlA.Co", "M.FR.FrA.Co"),
                      labels=c( "RCD","RCBD", "Strip")) +
  theme(text = element_text(size=30),legend.justification=c(1.1,-0.1),legend.position=c(1,0),
        legend.key.size = unit(2.5, 'lines'),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  ggtitle(label= "Without site variables")
# ,rect = element_rect(fill = "black")) # or theme_blank())


# scale_color_manual(values=c("#228B22", "#FF4500", "#0000CD"))

