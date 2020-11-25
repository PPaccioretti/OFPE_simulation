require(ggplot2)
BetasReales
DBCAZo="M.DBCA.Zo.BlA.Co"
DCAZo="M.DCA.Zo.Co"
FRZo="M.FR.Zo.FrA"

DBCA="M.DBCA.BlA.Co"
DCA="M.DCA.Co"
FR="M.FR.FrA.Co"

SubSetBetasReales<-BetasReales[BetasReales$Modelo %in% c(DBCAZo, DCAZo, FRZo) ,]
SubSetBetasReales<-BetasReales[BetasReales$Modelo %in% c(DBCAZo, DCAZo, FRZo, DBCA,DCA, FR) , ,drop=TRUE]

# 
# levels(SubSetBetasReales$Modelo)[levels(SubSetBetasReales$Modelo) %in% c(DBCAZo, DCAZo, FRZo, DBCA,DCA, FR)] <-
#   c("DBCA","DBCA con Ef. Zona" ,"DCA","DCA con Ef. Zona","Franja","Franja con Ef. Zona")
# c("M.DBCA.BlA.Co","M.DBCA.Zo.BlA.Co", "M.DCA.Co","M.DCA.Zo.Co","M.FR.FrA.Co","M.FR.Zo.FrA")
#   
########################################################################
options(OutDec = ",", scipen = 3)
tamanio <- 18
SubSetBetasRealesCoeficiente<-SubSetBetasReales[SubSetBetasReales$Coeficientes=="(Intercept)", ]
SubSetBetasRealesSesgo<-data.frame("Modelo"=SubSetBetasRealesCoeficiente[, "Modelo"],  "Sesgo"=SubSetBetasRealesCoeficiente[, "Value"] - 80)

plotMu <-ggplot(SubSetBetasRealesSesgo,aes(x = Sesgo)) +
  geom_density(aes(colour = Modelo), show.legend = NA, lwd=1.1, trim=TRUE,adjust = 2) +
      scale_colour_manual(name="Modelo",
                    values=c("#D3FF00", "#FF8500", "#137AFF","#05D8FF", "#FFF200", "#FF00FF")[c(1,3,5,2,4,6)],
                    breaks=c("M.DBCA.BlA.Co","M.DBCA.Zo.BlA.Co", "M.DCA.Co","M.DCA.Zo.Co","M.FR.FrA.Co","M.FR.Zo.FrA")[c(1,3,5,2,4,6)],
                    labels=c("DBCA","DBCA con Ef Zona", "DCA","DCA con Ef Zona","Franja","Franja con Ef Zona")[c(1,3,5,2,4,6)])+
  geom_vline(xintercept=0, colour = "darkgreen") + scale_x_continuous(name="Sesgo")+ theme(axis.ticks.y = element_blank()) +
  scale_y_continuous(name=NULL, labels=NULL) +
  ggtitle(label= expression(mu))+ theme(text = element_text(size=tamanio),
                                             axis.text.x = element_text(colour = "black"),
                                             axis.text.y = element_text(colour = "black"))
# plotMu
########################################################################
########################################################################
SubSetBetasRealesCoeficiente<-SubSetBetasReales[SubSetBetasReales$Coeficientes=="AsigTrat", ]
SubSetBetasRealesSesgo<-data.frame("Modelo"=SubSetBetasRealesCoeficiente[, "Modelo"],  "Sesgo"=SubSetBetasRealesCoeficiente[, "Value"] - 
                                     as.numeric(as.character(SubSetBetasRealesCoeficiente[, "ValoresReales"])))

plotN<-ggplot(SubSetBetasRealesSesgo,aes(x = Sesgo)) +
  geom_density(aes(colour = Modelo), show.legend = NA, lwd=1.1, trim=TRUE,adjust = 2) +
  scale_colour_manual(name="Modelo",
                      values=c("#D3FF00", "#FF8500", "#137AFF","#05D8FF", "#FFF200", "#FF00FF")[c(1,3,5,2,4,6)],
                      breaks=c("M.DBCA.BlA.Co","M.DBCA.Zo.BlA.Co", "M.DCA.Co","M.DCA.Zo.Co","M.FR.FrA.Co","M.FR.Zo.FrA")[c(1,3,5,2,4,6)],
                      labels=c("DBCA","DBCA con Ef Zona", "DCA","DCA con Ef Zona","Franja","Franja con Ef Zona")[c(1,3,5,2,4,6)])+
  geom_vline(xintercept=0, colour = "darkgreen") + scale_x_continuous(name="Sesgo")+ theme(axis.ticks.y = element_blank()) +
  scale_y_continuous(name=NULL, labels=NULL) +
  ggtitle(label= expression(beta[1]*N[i]))+ theme(text = element_text(size=tamanio),
                                                  axis.text.x = element_text(colour = "black"),
                                                  axis.text.y = element_text(colour = "black"))


########################################################################
########################################################################
SubSetBetasRealesCoeficiente<-SubSetBetasReales[SubSetBetasReales$Coeficientes=="I(AsigTrat^2)", ]
SubSetBetasRealesSesgo<-data.frame("Modelo"=SubSetBetasRealesCoeficiente[, "Modelo"],  "Sesgo"=SubSetBetasRealesCoeficiente[, "Value"] - 
                                     as.numeric(as.character(SubSetBetasRealesCoeficiente[, "ValoresReales"])))
# table(SubSetBetasRealesSesgo[,1])
plotNN<-ggplot(SubSetBetasRealesSesgo,aes(x = Sesgo)) +
  geom_density(aes(colour = Modelo), show.legend = NA, lwd=1.1, trim=TRUE,adjust = 2) +
  scale_colour_manual(name="Modelo",
                      values=c("#D3FF00", "#FF8500", "#137AFF","#05D8FF", "#FFF200", "#FF00FF")[c(1,3,5,2,4,6)],
                      breaks=c("M.DBCA.BlA.Co","M.DBCA.Zo.BlA.Co", "M.DCA.Co","M.DCA.Zo.Co","M.FR.FrA.Co","M.FR.Zo.FrA")[c(1,3,5,2,4,6)],
                      labels=c("DBCA","DBCA con Ef Zona", "DCA","DCA con Ef Zona","Franja","Franja con Ef Zona")[c(1,3,5,2,4,6)])+
  geom_vline(xintercept=0, colour = "darkgreen") + scale_x_continuous(name="Sesgo", breaks = c(-0.00006,0,0.00006))+ theme(axis.ticks.y = element_blank()) +
  scale_y_continuous(name=NULL, labels=NULL) +
  ggtitle(label= expression(beta[2]*N[i]^2))+ theme(text = element_text(size=tamanio),
                                                    axis.text.x = element_text(colour = "black"),
                                                    axis.text.y = element_text(colour = "black"))

# , breaks = c(-0.00005,0,0.00005)
########################################################################
########################################################################
SubSetBetasRealesCoeficiente<-SubSetBetasReales[SubSetBetasReales$Coeficientes=="as.factor(Zona)24.6", ]
SubSetBetasRealesSesgo<-data.frame("Modelo"=SubSetBetasRealesCoeficiente[, "Modelo"],  "Sesgo"=SubSetBetasRealesCoeficiente[, "Value"] - 24.6)

plotZ<-ggplot(SubSetBetasRealesSesgo,aes(x = Sesgo)) +
  geom_density(aes(colour = Modelo), show.legend = NA, lwd=1.1, trim=TRUE,adjust = 2) +
  scale_colour_manual(name="Modelo",
                      values=c("#D3FF00", "#FF8500", "#137AFF","#05D8FF", "#FFF200", "#FF00FF")[c(1,3,5,2,4,6)],
                      breaks=c("M.DBCA.BlA.Co","M.DBCA.Zo.BlA.Co", "M.DCA.Co","M.DCA.Zo.Co","M.FR.FrA.Co","M.FR.Zo.FrA")[c(1,3,5,2,4,6)],
                      labels=c("DBCA","DBCA con Ef Zona", "DCA","DCA con Ef Zona","Franja","Franja con Ef Zona")[c(1,3,5,2,4,6)])+
  geom_vline(xintercept=0, colour = "darkgreen") + scale_x_continuous(name="Sesgo")+ theme(axis.ticks.y = element_blank()) +
  scale_y_continuous(name=NULL, labels=NULL) +
  ggtitle(label= expression(beta[3]*Z[k]))+ theme(text = element_text(size=tamanio),
                                                  axis.text.x = element_text(colour = "black"),
                                                  axis.text.y = element_text(colour = "black"))


########################################################################
########################################################################
SubSetBetasRealesCoeficiente<-SubSetBetasReales[SubSetBetasReales$Coeficientes=="AsigTrat:as.factor(Zona)24.6", ]
SubSetBetasRealesSesgo<-data.frame("Modelo"=SubSetBetasRealesCoeficiente[, "Modelo"],  "Sesgo"=SubSetBetasRealesCoeficiente[, "Value"] - 
                                     as.numeric(as.character(SubSetBetasRealesCoeficiente[, "ValoresReales"])))

plotNZ<-ggplot(SubSetBetasRealesSesgo,aes(x = Sesgo)) +
  geom_density(aes(colour = Modelo), show.legend = NA, lwd=1.1, trim=TRUE,adjust = 2) +
  scale_colour_manual(name="Modelo",
                      values=c("#D3FF00", "#FF8500", "#137AFF","#05D8FF", "#FFF200", "#FF00FF")[c(1,3,5,2,4,6)],
                      breaks=c("M.DBCA.BlA.Co","M.DBCA.Zo.BlA.Co", "M.DCA.Co","M.DCA.Zo.Co","M.FR.FrA.Co","M.FR.Zo.FrA")[c(1,3,5,2,4,6)],
                      labels=c("DBCA","DBCA con Ef Zona", "DCA","DCA con Ef Zona","Franja","Franja con Ef Zona")[c(1,3,5,2,4,6)])+
  geom_vline(xintercept=0, colour = "darkgreen") + theme(axis.ticks.y = element_blank()) +
  scale_y_continuous(name=NULL, labels=NULL) +
  ggtitle(label= expression(beta[4]*(N[i]%*%Z[k]))) + theme(text = element_text(size=tamanio),
                                                            axis.text.x = element_text(colour = "black"),
                                                            axis.text.y = element_text(colour = "black"))


########################################################################
########################################################################
# Arranging the plot
require("ggpubr")
# jpeg("C:/Users/Pablo/Desktop/Grafico.jpg",width= 20, height = 31, units = 'cm' , res = 80)
ggarrange(plotMu,plotN,plotNN, plotZ, plotNZ,
          ncol =2, nrow = 3,  #align = "hv", 
          # widths = c(2, 1), heights = c(1, 2),
          legend='none' )#,common.legend = TRUE)
# dev.off()
getwd()
########################################################################
########################################################################


########################################################################
########################################################################


########################################################################
########################################################################





levels(SubSetBetasReales$Coeficientes)
