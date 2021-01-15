graficarEnsayo <- function(sill,
                           rango,
                           nugget,
                           media,
                           xcampo = 980,
                           ycampo = 900,
                           xlargoparcela = 70,
                           yanchoparcela = 30,
                           Nzonas = 2,
                           Heterog = 2,
                           Trat = c(1, 40, 70, 100, 140),
                           mediaBeta1 = 12.76,
                           varBeta1 = 0.001,
                           mediaBeta2 = -0.001,
                           varBeta2 = 0.0000001,
                           covBetas = 0) {
  
  library(RandomFields)
  # x <- seq(0, xcampo, xcampo / xlargoparcela)
  # y <- seq(0, ycampo, ycampo / yanchoparcela)

  x <- seq(0, xcampo, xlargoparcela)
  y <- seq(0, ycampo, yanchoparcela)
  
  modelo <-
    RMexp(var = sill, scale = rango) +
    RMnugget(var = nugget) +
    RMtrend(mean = media)
  simu <- RFsimulate(modelo, x, y,  spConform = TRUE)
  
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
  
  
  #Divide al eje y en Ndis=3 zonas para repartir los Diseños
  NDis <- 3
  NombresDis <- c("DBCA", "DCA", "DFR")
  AleatFranja <- TRUE
  Dis <-
    as.factor(findInterval(Simulacion$y, seq(0, ycampo, length = NDis + 1)[-c(1, NDis +
                                                                        1)],  left.open = TRUE))
  #   Cantidad de parcelas por Disnios:  table(Dis)
  #                                      table(Dis, EfZona)
  cat('Hay', table(Dis), 'parcelas para cada disenio\n')     
  
  
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
  
  
  
  # Asignación de tratamientos dentro de cada diseño ----
  
  ## Diseño en FRANJA ----
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
                         "AsigTrat" = sample(1:length(Trat)),
                         "FranjaRep" = paste0("FranjaRep_", x)
                       ), simplify = FALSE))
    
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
  # browser()
  
  FranX <-
    findInterval(
      MisParcelasDBCA$x,
      seq(
        min(MisParcelasDBCA$x),
        max(MisParcelasDBCA$x),
        length = length(unique(MisParcelasDBCA$x))
      ),
      left.open = TRUE,
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
      left.open = TRUE,
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
    MASS::mvrnorm(
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
  
  
  # simu_sf <- sf::st_as_sf(simu)
  # simu <- stars::st_rasterize(sf::st_as_sf(simu),
  #                             nx = length(x),
  #                             ny = length(y))
  
  miEnsayo <- dplyr::bind_rows(MisParcelasFRTrat,
                               MisParcelasDCATrat,
                               MisParcelasDBCATrat)
  
  miEnsayo_sf <- sf::st_as_sf(miEnsayo, coords = c('x', 'y'))
  
  # plot(miEnsayo_sf)
  
  miEnsayo_polygon <- sf::st_make_grid(miEnsayo_sf,
                                       n = c(length(x),
                                             length(y)))
  miEnsayo_polygon <- sf::st_as_sf(miEnsayo_polygon)
  
  miEnsayo_polygon <- sf::st_join(miEnsayo_polygon, miEnsayo_sf)
  
  
  plot(miEnsayo_polygon)
  
}

a <- graficarEnsayo(
  rango = 38.78,
  sill = 9.69,
  nugget = 0.06,
  media = 80
)
b <- graficarEnsayo(
  rango = 38.78,
  sill = 9.69,
  nugget = 0.06,
  media = 80
)
image(a)
identical(a, b)
# plot(MisParcelasFRTrat$AsigTrat+1, t(MisParcelasFRTrat$MiRto), col='blue')
# points(MisParcelasDCATrat$AsigTrat+2, t(MisParcelasDCATrat$MiRto), col='red')
# points(MisParcelasDBCATrat$AsigTrat+3, t(MisParcelasDBCATrat$MiRto), col='darkgreen')
# Cantidad de parcelas por tratamiento asignado:
# table(MisParcelasFRTrat$AsigTrat)
# table(MisParcelasDCATrat$AsigTrat)
# table(MisParcelasDBCATrat$AsigTrat)
