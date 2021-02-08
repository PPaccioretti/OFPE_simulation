# Paquetes ----
library(dplyr)
library(ggplot2)
# Funciones ----
source('src/ggplot_ecdf.R', encoding = "UTF-8")
source('src/funcionesResultados.R', encoding = "UTF-8")

# Objetos con modelos de interes ----
modelos_sin_zona <-
  c("M.DBCA.BlA.Co", "M.DCA.Co", "M.FR.FrA.Co")

modelos_zona <-
  c("M.DBCA.Zo.BlA.Co", "M.DCA.Zo.Co", "M.FR.Zo.FrA.Co")

modelos_zona_Corr_NoCorr <-
  c("M.DBCA.Zo.BlA.Co", "M.DCA.Zo.Co", "M.FR.Zo.FrA.Co",
    "M.DBCA.Zo.BlA", "M.DCA.Zo", "M.FR.Zo.FrA")

modelos_sin_zona_Corr_NoCorr <-
  c("M.DBCA.BlA.Co", "M.DCA.Co", "M.FR.FrA.Co",
    "M.DBCA.BlA", "M.DCA", "M.FR.FrA")

# Lectura datos ----
patronResultados <- '(_2021_(15)_01_)'
misRDS <-
  list.files('resultados/', pattern = patronResultados, full.names = TRUE)
# misRDS

misObjetos  <- lapply(misRDS, leerRDS)

# Cantidad de simulaciones total
sum(selecciona(misObjetos, 'Simulaciones'))

resumen <- selecciona(misObjetos, 'Resumen')
betas <- selecciona(misObjetos, 'BetasEstReal')
semillas <- selecciona(misObjetos, 'semillas')



sigmaalto <- resumen[resumen$Sigma_CV > 15, c('Corrida', 'Simulacion')]
cat("Hay", nrow(sigmaalto), "datos raros\n")
if (nrow(sigmaalto) > 0) {
  warning('Se estan eliminando corridas por datos raros')
  resumen <- resumen[ !(resumen$Corrida %in% sigmaalto$Corrida & 
                          resumen$Simulacion %in% sigmaalto$Simulacion), ]
  
  betas <- betas[ !(betas$Corrida %in% sigmaalto$Corrida & 
                      betas$Simulacion %in% sigmaalto$Simulacion), ]
  
  semillas <- semillas[ !(semillas$Corrida %in% sigmaalto$Corrida & 
                            semillas$Simulacion %in% sigmaalto$Simulacion), ]
}

misZonas <- gsub('(as\\.factor)|([aA-zZ]+)|(\\(.*\\)|:|)',
                 '',
                 unique(betas$Coeficientes),
                 perl = TRUE)

betas$ValoresReales[betas$Coeficientes %in% '(Intercept)'] <- 80
betas$ValoresReales[betas$Coeficientes %in% 'as.factor(Zona)5.92958683214944'] <-
  5.92958683214944
betas$ValoresReales[betas$Coeficientes %in% 'as.factor(Zona)5.42217668469038'] <-
  5.42217668469038
betas$ValoresReales[betas$Coeficientes %in% 'AsigTrat:as.factor(Zona)5.92958683214944'] <-
  5.92958683214944 * 0.0034
betas$ValoresReales[betas$Coeficientes %in% 'AsigTrat:as.factor(Zona)5.42217668469038'] <-
  5.42217668469038 * 0.0034


resumen$ModeloLab <-
  factor(
    resumen$Modelo,
    modelos_list$levels,
    modelos_list$labels
  )


betas$ModeloLab <-
  factor(
    betas$Modelo,
    modelos_list$levels,
    modelos_list$labels
  )

betas$CoeficientesParse <-
  factor(
    betas$Coeficientes,
    coeficientes_list$levels,
    coeficientes_list$labels
  )


misModelosConCorr <- grepl('[.]Co$', resumen$Modelo)
resumen$ConCorr <- misModelosConCorr

resumen$Disenio[grepl('M[.]DBCA[.]*', resumen$Modelo)] <- 'DBCA'
resumen$Disenio[grepl('M[.]DCA[.]*', resumen$Modelo)] <- 'DCA'
resumen$Disenio[grepl('M[.]FR[.]*', resumen$Modelo)] <- 'Franjas'

# Error estandar residual ----
## Sin contemplar efecto zona ----

resumen_modelos_sin_zona <- resumen %>%
  filter(Modelo %in% modelos_sin_zona)

## Contemplando efecto zona ----

resumen_modelos_zona <- resumen %>%
  filter(Modelo %in% modelos_zona)

# Sigma medio con-sin correlacion espacial ----
# Media del sigma del error para los disenios particionado por 
# Correlacion espacial y por si se tuvo en cuenta la espacialidad de los datos
# o no


resumen_modelos_zona_Corr_NoCorr <- resumen %>%
  filter(Modelo %in% modelos_zona_Corr_NoCorr)

descriptiva_modelos_zona_Corr_NoCorr <-
  resumen_modelos_zona_Corr_NoCorr %>%
  group_by(Disenio, CorrEsp, ConCorr) %>%
  summarise(
    media = mean(Sigma_CV, na.rm = TRUE),
    ee = sd(Sigma_CV, na.rm = TRUE) / sqrt(n()),
    p025 = quantile(Sigma_CV, 0.025),
    p975 = quantile(Sigma_CV, 0.975)
  )

# Sesgo y Cobertura ----
tablaSigma <-
  resumen %>%
  filter(Modelo %in% c(modelos_zona, modelos_sin_zona),
         !is.na(ModeloLab),
         CorrEsp == 'baja') %>%
  group_by(ModeloLab, Dosis0, TratMax, CorrEsp) %>%
  summarise(
    Sigma_CV_media = mean(Sigma_CV),
    Sigma_CV_sd = sd(Sigma_CV),
    Sigma_CV_cv = (Sigma_CV_sd/Sigma_CV_media) * 100,
  )  %>%
  mutate_if(specialround, 1, .predicate = 'is.numeric')

write.table(
  tablaSigma,
  'clipboard',
  sep = '\t',
  quote = FALSE,
  na = '--',
  row.names = FALSE
)



betas_ss <-
  betas %>%
  mutate(
    sesgo = Value - ValoresReales,
    sesgo_prop = (Value - ValoresReales) / ValoresReales,
    cobertura = ValoresReales >= lower & ValoresReales <= upper
  ) %>%
  filter(
    Modelo %in% c(modelos_zona, modelos_sin_zona),
    !is.na(ModeloLab),
    CorrEsp == 'baja'
  ) 

## Cobertura  ----

coberturas <-
  betas_ss %>%
  group_by(ModeloLab, Dosis0, TratMax, CorrEsp, CoeficientesParse) %>%
  summarise(
    sesgo_prom = mean(sesgo_prop),
    sesgo_sd = sd(sesgo_prop),
    sesgo_amplitud = diff(range(sesgo_prop)),
    cobertura_p = mean(cobertura)*100)



tablaSesgo_export <-
  tidyr::pivot_wider(
    coberturas,
    id_cols = c(ModeloLab, Dosis0, TratMax, CorrEsp),
    names_from = CoeficientesParse,
    values_from = c('sesgo_prom', 'sesgo_sd', 'sesgo_amplitud'),
    # values_fn = mean,
    # names_glue = '{sesgo_prom}_{sesgo_sd}'
    values_fn = mean
  ) %>%
    mutate_if(
      formatC,
      # 1,
      .predicate = 'is.numeric',
      # nsmall = 2,
      format = "e",
      digits = 2
      # scientific = -1
      # small.interval = 7
      # width = 4
      # justify = 'left'
      # format = 'fg',
      # flag = ' ',
      # width = 5
      # preserve.width = 'none',
      # replace.zero = TRUE,
      # drop0trailing = TRUE
    )
    

  # mutate_if(
  #   format,
  #   # 1,
  #   .predicate = 'is.numeric',
  #   # nsmall = 2,
  #   digits = 2,
  #   scientific = -1
  #   # small.interval = 7
  #   # width = 4
  #   # justify = 'left'
  #   # format = 'fg',
  #   # flag = ' ',
  #   # width = 5
  #   # preserve.width = 'none',
  #   # replace.zero = TRUE,
  #   # drop0trailing = TRUE
  # )
  # mutate_if(
  #   prettyNum,
  #   1,
  #   .predicate = 'is.numeric',
  #   digits = 3,
  #   format = 'fg',
  #   # flag = ' ',
  #   width = 5
  #   # preserve.width = 'none',
  #   # replace.zero = TRUE,
  #   # drop0trailing = TRUE
  # )

write.table(
  tablaSesgo_export,
  'clipboard',
  sep = '\t',
  quote = FALSE,
  na = '--',
  row.names = FALSE
)


TablaCobertura <-
  coberturas %>%
  reshape2::dcast(ModeloLab + Dosis0 + TratMax + CorrEsp ~ CoeficientesParse,
                  value.var = c('cobertura_p','sesgo_prom'),
                  fun.aggregate = mean) %>%
  rowwise() %>% 
  mutate(media = mediaSinNaN(c_across(starts_with("beta")))) 

TablaCobertura <-
  coberturas %>%
  reshape2::dcast(ModeloLab + Dosis0 + TratMax + CorrEsp ~ CoeficientesParse,
                  value.var = 'cobertura_p',
                  fun.aggregate = mean) %>%
  rowwise() %>% 
  mutate(media = mediaSinNaN(c_across(starts_with("beta")))) 


TablaCobertura_format <- 
  TablaCobertura %>% 
  mutate_if(specialround, 1, .predicate = 'is.numeric')



x <- c(0.00004, 0.000000004)

format(round(x, digits = 90), nsmall = 2)
sprintf("%2.7f ", x)
