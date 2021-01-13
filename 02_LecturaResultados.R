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

# Lectura datos ----
# patronResultados <- '_2020_(10)_12_*'
patronResultados <- '_2020_(15)_12_*'
misRDS <-
  list.files('resultados/', pattern = patronResultados, full.names = TRUE)


misObjetos  <- lapply(misRDS, function(x) {
  miRDS <- tryCatch(readRDS(x), error = function(e) {return(NULL)})
  if (is.null(miRDS)) 
    return(NULL)
  miRDS <- miRDS[unlist(lapply(miRDS, no_null_or_error))]
  
  resumen <- selecciona(miRDS, 'Resumen')
  betas <- selecciona(miRDS, 'BetasEstReal')
  semillas <- selecciona(miRDS, 'semillas')
  
  list(Resumen = resumen,
       BetasEstReal = betas,
       semillas = semillas,
       Simulaciones = length(miRDS))

})

resumen <- selecciona(misObjetos, 'Resumen')
betas <- selecciona(misObjetos, 'BetasEstReal')
semillas <- selecciona(misObjetos, 'semillas')

# Cantidad de simulaciones total
sum(selecciona(misObjetos, 'Simulaciones'))

misModelosConCorr <- grepl('[.]Co$', resumen$Modelo)
resumen$ConCorr <- misModelosConCorr

resumen$Disenio[grepl('M[.]DBCA[.]*', resumen$Modelo)] <- 'DBCA'
resumen$Disenio[grepl('M[.]DCA[.]*', resumen$Modelo)] <- 'DCA'
resumen$Disenio[grepl('M[.]FR[.]*', resumen$Modelo)] <- 'Franjas'

# Error estandar residual ----
## Sin contemplar efecto zona ----
modelos_sin_zona <-
  c("M.DBCA.BlA.Co", "M.DCA.Co", "M.FR.FrA.Co")

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

modelos_zona <-
  c("M.DBCA.Zo.BlA.Co", "M.DCA.Zo.Co", "M.FR.Zo.FrA.Co")

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
modelos_interes <-
  c("M.DBCA.Zo.BlA.Co", "M.DCA.Zo.Co", "M.FR.Zo.FrA.Co",
    "M.DBCA.Zo.BlA", "M.DCA.Zo", "M.FR.Zo.FrA")

resumen_modelos_interes <- resumen %>%
  filter(Modelo %in% modelos_interes)
# datos <- resumen_modelos_interes

descriptiva_modelos_interes <-
  resumen_modelos_interes %>%
  group_by(Disenio, CorrEsp, ConCorr) %>%
  summarise(
    media = mean(Sigma_CV, na.rm = TRUE),
    ee = sd(Sigma_CV, na.rm = TRUE) / sqrt(n()),
    p025 = quantile(Sigma_CV, 0.025),
    p975 = quantile(Sigma_CV, 0.975)
  )

descriptiva_corrEsp <-
  ggplot(descriptiva_modelos_interes,
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
betas$ValoresReales[betas$Coeficientes %in% '(Intercept)'] <- 8
betas$ValoresReales[betas$Coeficientes %in% 'as.factor(Zona)26.37'] <-
  26.37
betas$ValoresReales[betas$Coeficientes %in% 'as.factor(Zona)22.05'] <-
  22.05
betas$ValoresReales[betas$Coeficientes %in% 'AsigTrat:as.factor(Zona)26.37'] <-
  26.37 * 0.034
betas$ValoresReales[betas$Coeficientes %in% 'AsigTrat:as.factor(Zona)22.05'] <-
  22.05 * 0.034



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
    Modelo %in% c(modelos_zona, modelos_sin_zona)) %>%
  mutate(
    ModeloLab =
      factor(
        betas_ss$Modelo,
        levels = c(
          'M.DBCA.BlA.Co',
          'M.DBCA.Zo.BlA.Co',
          'M.DCA.Co',
          'M.DCA.Zo.Co',
          'M.FR.FrA.Co',
          'M.FR.Zo.FrA.Co'
        ),
        labels = c(
          'DBCA',
          'DBCA + Zona',
          'DCA',
          'DCA + Zona',
          'Franjas',
          'Franjas + Zona'
        )
      ),
    CoeficientesParse =
      factor(
        betas_ss$Coeficientes,
        levels = c(
          "(Intercept)",
          "AsigTrat",
          "I(AsigTrat^2)",
          "as.factor(Zona)26.37",
          "as.factor(Zona)22.05",
          "AsigTrat:as.factor(Zona)24.6",
          "AsigTrat:as.factor(Zona)26.37",
          "AsigTrat:as.factor(Zona)22.05"
        ),
        labels = c(
          "beta[0]",
          "beta[1]%*%N[i]",
          "beta[2]%*%N[i]^2",
          "beta[3]%*%Z[k]",
          "beta[3]%*%Z[k]",
          "beta[4]%*%Z[k]%*%N[i]",
          "beta[4]%*%Z[k]%*%N[i]",
          "beta[4]%*%Z[k]%*%N[i]"
        )
      )
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
        CorrEsp ~ CoeficientesParse,
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
                    ncol = 5,
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
  reshape2::dcast(ModeloLab + Dosis0 + TratMax ~ CoeficientesParse,
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


ggplot(coberturas, aes(x = ModeloLab, y = cobert)) +
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

