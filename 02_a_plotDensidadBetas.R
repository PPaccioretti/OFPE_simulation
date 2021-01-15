guardarPlots  <- FALSE

sesgo_plot <-
  sapply(levels(betas_ss$CoeficientesParse), function(x) {
    # x <- levels(betas_ss$CoeficientesParse)[2]
    label_dosisTratCorr <-
      labeller(
        Dosis0 = as_labeller(c('Si' = 'Con testigo',
                               'No' = 'Sin testigo'),
                             label_value),
        TratMax = as_labeller(
          c('140' = 'Dosis~máxima~140~Kg~N~ha^-1',
            '280' = 'Dosis~máxima~280~Kg~N~ha^-1'),
          label_parsed
        ),
        CorrEsp = as_labeller(c(baja = 'Alto RSV', media = 'Medio RSV'),
                              label_value),
        CoeficientesParse = label_parsed,
        'interaction(CorrEsp, Dosis0)' = as_labeller(
          c(
            'baja.Si' = 'Alto RSV\nConTestigo',
            'media.Si' = 'Medio RSV\nConTestigo',
            'baja.No' = 'Alto RSV\nSinTestigo',
            'media.No' = 'Medio RSV\nSinTestigo'
          ),
          label_value
        )
      )
    
    subset <- betas_ss[betas_ss$CoeficientesParse == x, ]
    plot_dens <- ggplot(subset, aes(x = sesgo)) +
      geom_vline(xintercept = 0, col = 'grey8') +
      geom_density(aes(colour = ModeloLab, fill = ModeloLab),
                   alpha = 0.1,
                   adjust = 3) +
      facet_grid(
        interaction(CorrEsp, Dosis0) ~ TratMax,
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