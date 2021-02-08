
no_null_or_error <- function(x) {
  !(is.null(x) | inherits(x, 'try-error'))
}

selecciona <- function(datos, nombre) {
  misListas <- lapply(datos, function(y) {
    try(y[[nombre]], silent = TRUE)
  })
  do.call(rbind, misListas)
}

specialround <-
  function(x, digits, nsmall = NULL) {
    if (is.null(nsmall)) {
      nsmall <- as.numeric(digits)
    }
    # if (!is.numeric(x))
    #   return(x)
    format(round(x, digits = digits), nsmall = nsmall)
  }


mediaSinNaN <- function(x) {
  isNan <- is.nan(x)
  as.numeric(mean(x[!isNan]))
}


leerRDS <- function(x) {
  miRDS <- tryCatch(readRDS(x), error = function(e) {return(NULL)})
  if (is.null(miRDS)) {
    cat(x, 'es NULL')
    return(NULL)
  }
  
  miSplit <- strsplit(x, "_")[[1]]
  miCorrida <- miSplit[which(miSplit == "Sem") + 1]
  
  nullError <- unlist(lapply(miRDS, no_null_or_error))
  cat('Se Eliminaron',
      formatC(sum(!nullError), 2),
      'simulaciones por ser errores o null.\n')
  miRDS <- miRDS[nullError]
  
  resumen <- selecciona(miRDS, 'Resumen')
  betas <- selecciona(miRDS, 'BetasEstReal')
  semillas <- selecciona(miRDS, 'Semilla')
  
  betas$Coeficientes <- as.factor(as.character(betas$Coeficientes))
  
  resumen$Corrida <- as.numeric(miCorrida)
  betas$Corrida <- as.numeric(miCorrida)
  semillas$Corrida <- as.numeric(miCorrida)
  
  list(Resumen = resumen,
       BetasEstReal = betas,
       semillas = semillas,
       Simulaciones = length(miRDS))
  
}



modelos_list <- list(
  levels = c(
    'M.DBCA.BlA.Co',
    'M.DBCA.Zo.BlA.Co',
    'M.DCA.Co',
    'M.DCA.Zo.Co',
    'M.FR.FrA.Co',
    'M.FR.Zo.FrA.Co',
    'M.DBCA.BlA',
    'M.DBCA.Zo.BlA',
    'M.DCA',
    'M.DCA.Zo',
    'M.FR.FrA',
    'M.FR.Zo.FrA'
  ),
  labels = c(
    'DBCA',
    'DBCA + Zona',
    'DCA',
    'DCA + Zona',
    'Franjas',
    'Franjas + Zona',
    'DBCA sin Correlacion',
    'DBCA + Zona sin Correlacion',
    'DCA sin Correlacion',
    'DCA + Zona sin Correlacion',
    'Franjas sin Correlacion',
    'Franjas + Zona sin Correlacion'
  )
)

coeficientes_list <- list(
  levels = c(
    "(Intercept)",
    "AsigTrat",
    "I(AsigTrat^2)",
    "as.factor(Zona)5.92958683214944",
    "as.factor(Zona)5.42217668469038",
    "AsigTrat:as.factor(Zona)5.92958683214944",
    "AsigTrat:as.factor(Zona)5.42217668469038"
  ),
  labels = c(
    "beta[0]",
    "beta[1]%*%N[i]",
    "beta[2]%*%N[i]^2",
    "beta[3]%*%Z[k]",
    "beta[3]%*%Z[k]",
    "beta[4]%*%Z[k]%*%N[i]",
    "beta[4]%*%Z[k]%*%N[i]"
  )
)