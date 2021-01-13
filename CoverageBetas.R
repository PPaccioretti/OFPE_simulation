setwd("C:/Users/Pablo/OneDrive/Documentos/Estadística/Análisis de datos/Estadística Espacial - Mariano/Simulacion Ensayos")
load("./DatosSimulados/DatosSimuladosFINAL.RData" ,verbose = FALSE)
Simulaciones<-MisResultados
rownames(Simulaciones)
MedResumen<-do.call(rbind,Simulaciones["Resumen",])
BetasReales<-do.call(rbind,Simulaciones["BetasEstReal",])
Semilla<-do.call(rbind,Simulaciones["Semilla",])


head(BetasReales)
class(BetasReales)
colnames(BetasReales)
dim(BetasReales)

table(BetasReales$Modelo,BetasReales$Coeficientes)
BetasReales[BetasReales$Modelo=='M.DCA.Zo.Co',]

MiCoverage<-apply(BetasReales,1,function(Fila,nombres,alfa=0.05){
  # browser()
  
  Est<-as.numeric(as.character(Fila["Value"])) 
  EE<-as.numeric(as.character(Fila["Std.Error"]))
  Real<-as.numeric(as.character(Fila["ValoresReales"]))
  GL<-as.numeric(as.character(Fila["DF"]))
  if(is.na(GL)) {GL<-706}
  pvalorSig<-as.numeric(as.character(Fila["p.value"]))<alfa
  
  valorT<-qt(alfa/2,df=GL, lower.tail = F)
  
  LI<-Est-EE*valorT
  LS<-Est+EE*valorT
  
  Incl<- LI<= Real & Real<=LS
  
  data.frame(
    "Modelo"=Fila["Modelo"],
    "Coeficientes"=Fila["Coeficientes"],
    "LI"=LI,
    "Estimado"=Est,
    "LS"=LS,
    "Real"=Real,
    "Incluye"=Incl,
    "Sig05"=pvalorSig)
  
},nombres=colnames(BetasReales))


MiCoverage<-do.call(rbind,MiCoverage)
head(MiCoverage)
TablaCoverage<-do.call(rbind,list(by(MiCoverage, list(MiCoverage$Modelo, MiCoverage$Coeficientes),function(x) {
  # browser()
  mean(x$Incluye,na.rm=T)})))

write.table(TablaCoverage ,paste0("clipboard-",123), sep="\t",quote=F,row.names = TRUE)

write.table(
data.frame(prop.table(table(paste(MiCoverage$Modelo,MiCoverage$Coeficientes,sep="_"), MiCoverage$Incluye),1))
,paste0("clipboard-",123), sep="\t",quote=F,row.names = FALSE)


write.table(data.frame(table(paste(MiCoverage$Modelo,MiCoverage$Coeficientes,sep="_"), MiCoverage$Incluye)),
            paste0("clipboard-",123), sep="\t",quote=F,row.names = FALSE)


nrow(cbind(MiCoverage$Modelo,MiCoverage$Coeficientes))
length( MiCoverage$Incluye)
dim(MiCoverage)


library(nlme)


fm1 <- lme(distance ~ age, data = Orthodont) # random is ~ age
fm2 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
summary(fm1)
summary(fm2)

intervals(fm1)

tTabla<-summary(fm1)$tTable
colnames(tTabla)


(15.2183222-tTabla[,"Value"])/tTabla[,"Std.Error"]
16.761111+qnorm(0.05/2)*0.7752461


qnorm(0.05/2)
qnorm(0.01)

