library(ggplot2)

label_dosisTrat <-
  labeller(Dosis0 = as_labeller(c('Si' = 'Con testigo',
                                   'No' = 'Sin testigo'),
                                 label_value),
           TratMax = as_labeller(
             c('140' = 'Dosis~máxima~140~kg~N~ha^-1',
               '280' = 'Dosis~máxima~280~kg~N~ha^-1'),
             label_parsed
           ))

ggplot_ecdf <- function(datos) {
  etiquetasDisenios <- function(patron, datos) {
    misModelos <- datos$Modelo
    mislabels <- grep(patron, misModelos)
    mislabels <- unique(misModelos[mislabels])
    as.character(mislabels)
  }
  
  misDBCA <- etiquetasDisenios('M[.]DBCA[.]*', datos)
  misDCA <- etiquetasDisenios('M[.]DCA[.]*', datos)
  misFR <- etiquetasDisenios('M[.]FR[.]*', datos)
  
  stopifnot(length(misDBCA) == 1, length(misDCA) == 1, length(misFR) == 1)
  
  misNombres <- c(misDCA, misDBCA, misFR)
  # misColours <- c("#228B22", "#FF4500", "#0000CD")
  misColours <- c('#1b9e77', '#d95f02', '#7570b3')
  names(misColours) <- misNombres
  
  misLabels <- c("DCA", "DBCA", "Franjas")
  names(misLabels) <- misNombres
  
  ggplot(datos, aes(x = Sigma_CV)) +
    stat_ecdf(
      aes(colour = Modelo),#, linetype = CorrEsp),
      pad = FALSE,
      show.legend = NA,
      lwd = 1.1
    ) +
    scale_linetype_manual(
      name = 'RSV',
      breaks = c('baja', 'media'),
      values = c(1, 3),
      labels = c('baja' = 'Alto', 'media' = 'Medio')
    ) +
    scale_colour_manual(name = "Diseño",
                        values = misColours,
                        labels = misLabels) +
    theme_minimal() +
    theme(
      text = element_text(size = 35 / ggplot2:::.pt),
      legend.justification = c(1, 0),
      legend.position = c(1, 0),
      legend.key.size = unit(2 / ggplot2:::.pt, 'lines'),
      axis.text.x = element_text(colour = "black"),
      axis.text.y = element_text(colour = "black")
    ) +
    labs(x = "Desvío estándar residual (%)",
         y = "Distribución empírica") #+
  # facet_grid(interaction(CorrEsp,Dosis0) ~ TratMax)
  
}



unificar_ejes_ggplot = function(...) {
  p = list(...)
  
  yr = purrr::map(p, ~ layer_scales(.x)$y$get_limits()) %>%
    unlist %>% range
  
  xr = purrr::map(p, ~ layer_scales(.x)$x$get_limits()) %>%
    unlist %>% range
  
  p %>% purrr::map( ~ .x + xlim(xr) + ylim(yr))
}
