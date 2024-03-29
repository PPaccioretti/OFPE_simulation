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
# patronResultados <- '_2020_(10)_12_*'
# patronResultados <- '_2020_(15)_12_*'
# patronResultados <- '(_2020_(17)_12_(19|20))'
# patronResultados <- '(_2020_(18)_12_)'
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


# table(sigmaalto$Modelo)
# resumen <- resumen[resumen$Corrida != 50 & resumen$Simulacion != 44, ]
# betas <- betas[betas$Corrida != 50 & betas$Simulacion != 44, ]

misModelosConCorr <- grepl('[.]Co$', resumen$Modelo)
resumen$ConCorr <- misModelosConCorr

resumen$Disenio[grepl('M[.]DBCA[.]*', resumen$Modelo)] <- 'DBCA'
resumen$Disenio[grepl('M[.]DCA[.]*', resumen$Modelo)] <- 'DCA'
resumen$Disenio[grepl('M[.]FR[.]*', resumen$Modelo)] <- 'Franjas'

# Error estandar residual ----
## Sin contemplar efecto zona ----

resumen_modelos_sin_zona <- resumen %>%
  filter(Modelo %in% modelos_sin_zona,
         CorrEsp == 'baja')

ecdf_error_SinZona <- 
  ggplot_ecdf(resumen_modelos_sin_zona) +
  facet_grid(Dosis0 ~ TratMax,
             labeller = label_dosisTrat) 

ggsave('images/ecdf_error_SinZona.png',
       ecdf_error_SinZona,
       width = 15,
       height = 10,
       units = "cm")


## Contemplando efecto zona ----

resumen_modelos_zona <- resumen %>%
  filter(Modelo %in% modelos_zona,
         CorrEsp == 'baja')

ecdf_error_Zona <-
  ggplot_ecdf(resumen_modelos_zona) +
  facet_grid(Dosis0 ~ TratMax,
             labeller = label_dosisTrat)

ggsave('images/ecdf_error_Zona.png',
       ecdf_error_Zona,
       width = 15,
       height = 10,
       units = "cm")



# Sigma medio con-sin correlacion espacial ----
# Media del sigma del error para los disenios particionado por 
# Correlacion espacial y por si se tuvo en cuenta la espacialidad de los datos
# o no


resumen_modelos_zona_Corr_NoCorr <- resumen %>%
  filter(Modelo %in% modelos_zona_Corr_NoCorr,
         CorrEsp == 'baja')
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


betas_ss <-
  betas %>%
  mutate(
    sesgo = Value - ValoresReales,
    sesgo_prop = (Value - ValoresReales) / ValoresReales,
    LI = Value + Std.Error * qnorm(0.025),
    LS = Value + Std.Error * qnorm(0.975),
    cobertura = ValoresReales >= lower & ValoresReales <= upper,
    zona = grepl('Zona', ModeloLab)
  ) %>%
   filter(#!is.na(sesgo),
    Modelo %in% c(modelos_zona, modelos_sin_zona),
    # Modelo %in% modelos_zona_Corr_NoCorr
    # Modelo %in% modelos_sin_zona_Corr_NoCorr
     !is.na(ModeloLab),
    CorrEsp == 'baja'
   ) 
  
## Sesgo ----
# Graficos densidad betas
guardarPlots <- TRUE
# source('02_a_plotDensidadBetas.R', encoding = 'utf8')

## Cual disenioo elegir
## La importancia de contemplar la zona dado que existe un efecto de zona
## El impacto de considerar la estructura espacial

## Cobertura  ----

coberturas <-
  betas_ss %>%
  group_by(ModeloLab, Dosis0, TratMax, CorrEsp, CoeficientesParse) %>%
  summarise(cobertura_p = mean(cobertura)*100)

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

write.table(
  TablaCobertura_format,
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
  geom_hline(yintercept = 95, color = 'red') +
  theme(axis.text.x = element_text(angle = -90))

mean(betas_ss$cobertura)
range(betas_ss$Std.Error)
mean(betas_ss$p.value > 0.05)

