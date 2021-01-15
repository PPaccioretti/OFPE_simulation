# Paquetes ----
library(dplyr)
library(ggplot2)
# Funciones ----
source('src/ggplot_ecdf.R', encoding = "UTF-8")

no_null_or_error <- function(x) {
  !(is.null(x) | inherits(x, 'try-error'))
}

selecciona <- function(datos, nombre) {
  misListas <- lapply(datos, function(y) {
    try(y[[nombre]], silent = TRUE)
  })
  do.call(rbind, misListas)
}
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
# patronResultados <- '_2020_(10)_12_*'
# patronResultados <- '_2020_(15)_12_*'
# patronResultados <- '(_2020_(17)_12_(19|20))'
# patronResultados <- '(_2020_(18)_12_)'
patronResultados <- '(_2021_(14)_01_)'
misRDS <-
  list.files('resultados/', pattern = patronResultados, full.names = TRUE)
misRDS

misObjetos  <- lapply(misRDS, function(x) {
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

})

resumen <- selecciona(misObjetos, 'Resumen')
betas <- selecciona(misObjetos, 'BetasEstReal')
semillas <- selecciona(misObjetos, 'semillas')

sigmaalto <- resumen[resumen$Sigma_CV > 15, c('Corrida', 'Simulacion')]
resumen <- resumen[ !(resumen$Corrida %in% sigmaalto$Corrida & 
                        resumen$Simulacion %in% sigmaalto$Simulacion), ]

betas <- betas[ !(betas$Corrida %in% sigmaalto$Corrida & 
                    betas$Simulacion %in% sigmaalto$Simulacion), ]

semillas <- semillas[ !(semillas$Corrida %in% sigmaalto$Corrida & 
                          semillas$Simulacion %in% sigmaalto$Simulacion), ]




# table(sigmaalto$Modelo)
# resumen <- resumen[resumen$Corrida != 50 & resumen$Simulacion != 44, ]
# betas <- betas[betas$Corrida != 50 & betas$Simulacion != 44, ]

# Cantidad de simulaciones total
sum(selecciona(misObjetos, 'Simulaciones'))

misModelosConCorr <- grepl('[.]Co$', resumen$Modelo)
resumen$ConCorr <- misModelosConCorr

resumen$Disenio[grepl('M[.]DBCA[.]*', resumen$Modelo)] <- 'DBCA'
resumen$Disenio[grepl('M[.]DCA[.]*', resumen$Modelo)] <- 'DCA'
resumen$Disenio[grepl('M[.]FR[.]*', resumen$Modelo)] <- 'Franjas'

# Error estandar residual ----
## Sin contemplar efecto zona ----

resumen_modelos_sin_zona <- resumen %>%
  filter(Modelo %in% modelos_sin_zona)

ecdf_error_SinZona <- 
  ggplot_ecdf(resumen_modelos_sin_zona) +
  facet_grid(Dosis0 ~ TratMax,
             labeller = label_dosisTrat) 

ggsave('images/ecdf_error_SinZona.png',
       ecdf_error_SinZona,
       width = 15,
       height = 10,
       units = "cm")

# ### Alta correlación espacial ----
# resumen_modelos_sin_zona_AltoRSV <-
#   resumen_modelos_sin_zona %>%
#   filter(CorrEsp == 'baja')
# 
# ecdf_error_SinZona_AltoRSV <-
#   ggplot_ecdf(resumen_modelos_sin_zona_AltoRSV) +
#   facet_grid(Dosis0 ~ TratMax,
#              labeller = label_dosisTrat)
# 
# ### Media correlación espacial ----
# resumen_modelos_sin_zona_MedioRSV <-
#   resumen_modelos_sin_zona %>%
#   filter(CorrEsp == 'media')
# 
# ecdf_error_SinZona_MedioRSV <-
#   ggplot_ecdf(resumen_modelos_sin_zona_MedioRSV) +
#   facet_grid(Dosis0 ~ TratMax,
#              labeller = label_dosisTrat)
# ecdf_error_SinZona <- 
#   unificar_ejes_ggplot(ecdf_error_SinZona_AltoRSV,
#                        ecdf_error_SinZona_MedioRSV)
# 
# 
# ggsave('images/ecdf_error_SinZona_AltoRSV.png', 
#        ecdf_error_SinZona[[1]],
#        width = 15,
#        height = 10,
#        units = "cm")
# ggsave('images/ecdf_error_SinZona_MedioRSV.png', 
#        ecdf_error_SinZona[[2]],
#        width = 15,
#        height = 10,
#        units = "cm")


## Contemplando efecto zona ----

resumen_modelos_zona <- resumen %>%
  filter(Modelo %in% modelos_zona)

ecdf_error_Zona <-
  ggplot_ecdf(resumen_modelos_zona) +
  facet_grid(Dosis0 ~ TratMax,
             labeller = label_dosisTrat)

ggsave('images/ecdf_error_Zona.png',
       ecdf_error_Zona,
       width = 15,
       height = 10,
       units = "cm")



# ### Alta correlación espacial ----
# resumen_modelos_zona_AltoRSV <-
#   resumen_modelos_zona %>%
#   filter(CorrEsp == 'baja')
# 
# ecdf_error_Zona_AltoRSV <-
#   ggplot_ecdf(resumen_modelos_zona_AltoRSV) +
#   facet_grid(Dosis0 ~ TratMax,
#              labeller = label_dosisTrat)
# 
# ### Media correlación espacial ----
# resumen_modelos_zona_MedioRSV <-
#   resumen_modelos_zona %>%
#   filter(CorrEsp == 'media')
# 
# ecdf_error_Zona_MedioRSV <-
#   ggplot_ecdf(resumen_modelos_zona_MedioRSV) +
#   facet_grid(Dosis0 ~ TratMax,
#              labeller = label_dosisTrat)
# 
# ecdf_error_Zona <- 
#   unificar_ejes_ggplot(ecdf_error_Zona_AltoRSV,
#                        ecdf_error_Zona_MedioRSV)
# 
# 
# 
# ggsave('images/ecdf_error_Zona_AltoRSV.png', 
#        ecdf_error_Zona[[1]],
#        width = 15,
#        height = 10,
#        units = "cm")
# ggsave('images/ecdf_error_Zona_MedioRSV.png', 
#        ecdf_error_Zona[[2]],
#        width = 15,
#        height = 10,
#        units = "cm")
# Sigma medio con-sin correlacion espacial ----
# Media del sigma del error para los disenios particionado por 
# Correlacion espacial y por si se tuvo en cuenta la espacialidad de los datos
# o no


resumen_modelos_zona_Corr_NoCorr <- resumen %>%
  filter(Modelo %in% modelos_zona_Corr_NoCorr)
# datos <- resumen_modelos_zona_Corr_NoCorr

descriptiva_modelos_zona_Corr_NoCorr <-
  resumen_modelos_zona_Corr_NoCorr %>%
  group_by(Disenio, CorrEsp, ConCorr) %>%
  summarise(
    media = mean(Sigma_CV, na.rm = TRUE),
    ee = sd(Sigma_CV, na.rm = TRUE) / sqrt(n()),
    p025 = quantile(Sigma_CV, 0.025),
    p975 = quantile(Sigma_CV, 0.975)
  )

descriptiva_corrEsp <-
  ggplot(descriptiva_modelos_zona_Corr_NoCorr,
         aes(x = Disenio, color = ConCorr)) +
  # geom_errorbar(aes(ymin = media - ee,
  #                   ymax = media + ee),
  #               width = 0.09) +
  geom_errorbar(aes(ymin = p025,
                    ymax = p975),
                width = 0.09) +
  geom_point(aes(y = media)) +
  scale_colour_manual(
    name = "Errores",
    values = c('FALSE' = '#8dd3c7', 'TRUE' = '#fb8072'),
    # values = c('FALSE' = '#4D4D4D', 'TRUE' = '#E6E6E6'),
    labels = c('FALSE' = 'Independientes', 'TRUE' = 'Correlacionados')
  ) +
  labs(x = 'Diseño experimental',
       y = 'Error Estandar residual') +
  theme_minimal() +
  theme(
    # legend.position = "none",
    text = element_text(size = 30 / ggplot2:::.pt),
    legend.justification = c(1, 0),
    legend.position = c(1, 0.4),
    legend.key.size = unit(2 / ggplot2:::.pt, 'lines'),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black")
  ) +
  facet_grid(CorrEsp ~ .,
             labeller =  labeller(CorrEsp = c(baja = 'Alto RSV',
                                              media = 'Medio RSV')))

ggsave(
  'images/descriptiva_corrEsp.png',
  descriptiva_corrEsp,
  width = 15,
  height = 8,
  units = "cm"
)

# Sesgo y Cobertura ----
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
  5.92958683214944 * 0.034
betas$ValoresReales[betas$Coeficientes %in% 'AsigTrat:as.factor(Zona)5.42217668469038'] <-
  5.42217668469038 * 0.034



betas$ModeloLab <-
  factor(
    betas$Modelo,
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

betas$CoeficientesParse =
  factor(
    betas$Coeficientes,
    levels = c(
      "(Intercept)",
      "AsigTrat",
      "I(AsigTrat^2)",
      "as.factor(Zona)5.92958683214944",
      "as.factor(Zona)5.42217668469038",
      # "AsigTrat:as.factor(Zona)24.6",
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
      # "beta[4]%*%Z[k]%*%N[i]",
      "beta[4]%*%Z[k]%*%N[i]"
    )
  )


betas_ss <-
  betas %>%
  mutate(
    sesgo = Value - ValoresReales,
    sesgo_prop = (Value - ValoresReales) / ValoresReales,
    LI = Value + Std.Error * qnorm(0.025),
    LS = Value + Std.Error * qnorm(0.975),
    cobertura = ValoresReales >= LI & ValoresReales <= LS
  ) %>%
   filter(#!is.na(sesgo),
    Modelo %in% c(modelos_zona, modelos_sin_zona),
    # Modelo %in% modelos_zona_Corr_NoCorr
    # Modelo %in% modelos_sin_zona_Corr_NoCorr
     !is.na(ModeloLab)
   ) 
  
## Sesgo ----
sesgo_plot <-
  sapply(levels(betas_ss$CoeficientesParse), function(x) {
    # x <- levels(betas_ss$CoeficientesParse)[1]
    
    subset <- betas_ss[betas_ss$CoeficientesParse == x, ]
    ggplot(subset, aes(x = sesgo)) +
      geom_vline(xintercept = 0, col = 'grey8') +
      geom_density(aes(colour = ModeloLab, fill = ModeloLab),
                   alpha = 0.1,
                   adjust = 3) +
      facet_grid(
        interaction(CorrEsp, Dosis0) ~ CoeficientesParse,
        # labeller = labeller(CorrEsp = c(baja = 'Alto RSV', media = 'Medio RSV')),
        labeller = labeller(CorrEsp = c(baja = 'Alto RSV', media = 'Medio RSV'),
                            CoeficientesParse = label_parsed)#,
        # scales = "free_y",
        # ncol = 1#,
        # dir = 'v'
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        # text = element_text(size = 30/ggplot2:::.pt),
        # legend.justification = c(1, 0),
        # legend.position = c(1, 0.4),
        # legend.key.size = unit(2/ggplot2:::.pt, 'lines'),
        # axis.text.x = element_text(colour = "black"),
        # axis.text.y = element_text(colour = "black")
      ) +
      labs(colour = "Diseño",
           fill = "Diseño",
           x = 'Sesgo',
           y = 'Densidad')
    
    
  }, simplify = FALSE)

sesgo_plot_arrange <-
  ggpubr::ggarrange(sesgo_plot[[1]],
                    sesgo_plot[[2]],
                    sesgo_plot[[3]],
                    sesgo_plot[[4]],
                    sesgo_plot[[5]],
                    # paramZonaInter,
                    common.legend = TRUE, 
                    ncol = 3,
                    legend = 'bottom')
ggsave('images/sesgo_disenios.png',
       sesgo_plot_arrange,
       width = 15,
       height = 8,
       units = "cm")
## Cual disenioo elegir
## La importancia de contemplar la zona dado que existe un efecto de zona
## El impacto de considerar la estructura espacial

## Cobertura  ----
specialround <-
  function(x, digits, nsmall = NULL) {
    if (is.null(nsmall)) {
      nsmall <- as.numeric(digits)
    }
    # if (!is.numeric(x))
    #   return(x)
    format(round(x, digits = digits), nsmall = nsmall)
  }

coberturas <-
  betas_ss %>%
  group_by(ModeloLab, Dosis0, TratMax, CorrEsp, CoeficientesParse) %>%
  summarise(cobertura_p = mean(cobertura)*100)

TablaCobertura <-
  coberturas %>%
  reshape2::dcast(ModeloLab + Dosis0 + TratMax + CorrEsp ~ CoeficientesParse,
                  value.var = 'cobertura_p',
                  fun.aggregate = mean) %>%
  mutate_if(specialround, 1, .predicate = 'is.numeric') 

write.table(
  TablaCobertura,
  'clipboard',
  sep = '\t',
  quote = FALSE,
  na = '--',
  row.names = FALSE
)


ggplot(coberturas, aes(x = ModeloLab, y = cobertura_p)) +
  geom_point() +
  facet_grid(
    interaction(CorrEsp, Dosis0) ~ interaction(CoeficientesParse, TratMax),
    labeller =  labeller(CorrEsp = c(baja = 'Alto RSV',
                                     media = 'Medio RSV')),
    scales = "fixed"
  ) +
  geom_hline(yintercept = 0.95, color = 'red') +
  theme(axis.text.x = element_text(angle = -90))

mean(betas_ss$cobertura)
range(betas_ss$Std.Error)
mean(betas_ss$p.value > 0.05)

