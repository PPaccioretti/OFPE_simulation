if (!exists('guardarPlots')) {
  guardarPlots  <- FALSE
}

if (guardarPlots) {
  cat('Se guardaran los graficos\n')
}

# guardarPlots  <- TRUE
sesgo_plot <-
  sapply(levels(betas_ss$CoeficientesParse), function(x) {
    # x <- levels(betas_ss$CoeficientesParse)[2]
    label_dosisTratCorr <-
      labeller(
        'Dosis0' = as_labeller(c('Si' = 'Con testigo',
                               'No' = 'Sin testigo'),
                             label_value),
        'TratMax' = as_labeller(
          c('140' = 'Dosis~máxima~140~Kg~N~ha^-1',
            '280' = 'Dosis~máxima~280~Kg~N~ha^-1'),
          label_parsed
        ),
        'CorrEsp' = as_labeller(c(baja = 'Alto RSV', media = 'Medio RSV'),
                              label_value),
        'CoeficientesParse' = label_parsed,
        'interaction(CorrEsp, Dosis0)' = as_labeller(
          c(
            'baja.Si' = 'Alto RSV\nCon Testigo',
            'media.Si' = 'Medio RSV\nCon Testigo',
            'baja.No' = 'Alto RSV\nSin Testigo',
            'media.No' = 'Medio RSV\nSin Testigo'
          ),
          label_value
        ),
        'zona' = as_labeller(c('TRUE' = 'Con efecto zona',
                               'FALSE' = 'Sin efecto zona'),
                             label_value),
        'interaction(zona, Dosis0)' = as_labeller(
          c(
            'TRUE.Si' = 'Con Testigo\nCon efecto',
            'FALSE.Si' = 'Con Testigo\nSin efecto',
            'TRUE.No' = 'Sin Testigo\nCon efecto',
            'FALSE.No' = 'Sin Testigo\nSin efecto'
          ),
          label_value
        )
      )
    
    subset <- betas_ss[betas_ss$CoeficientesParse == x, , drop = T]
    plot_dens <- ggplot(subset, aes(x = sesgo)) +
      geom_vline(xintercept = 0, col = 'grey8') +
      geom_density(aes(colour = ModeloLab, fill = ModeloLab),
                   alpha = 0.15,
                   adjust = 3) +
      facet_grid(
        interaction(zona, Dosis0) ~ TratMax,
        # interaction(CorrEsp, Dosis0) ~ TratMax,
        labeller = label_dosisTratCorr
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
    x <- gsub('(\\[|\\*|\\]|\\*|\\%)', '',x)
    if (guardarPlots) {
      ggsave(paste0('images/sesgo_disenios_',x,'.png'),
             plot_dens,
             width = 15,
             height = 16,
             units = "cm")
    }
    plot_dens
  }, simplify = FALSE)

sesgo_plot[[1]]
sesgo_plot[[2]]
sesgo_plot[[3]]
sesgo_plot[[4]]
sesgo_plot[[5]]

descrSesgo <- 
  betas_ss %>% 
  group_by(CoeficientesParse, CorrEsp, TratMax, Dosis0) %>% 
  summarise(media = round(mean(sesgo_prop), 3),
            iqr = round(IQR(sesgo_prop), 3))

write.table(descrSesgo,
            'clipboard',
            quote = F,
            row.names = F, 
            sep = '\t')

betasfiltra <-
  betas_ss %>% 
  filter(CoeficientesParse == 'beta[0]',
         CorrEsp == 'baja',
         TratMax == '280',
         Dosis0 == 'No') 

ks.test(betasfiltra %>% filter(ModeloLab == 'DBCA + Zona') %>% pull(sesgo_prop),
        betasfiltra %>% filter(ModeloLab == 'Franjas + Zona') %>% pull(sesgo_prop))

