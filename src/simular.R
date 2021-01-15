# Simular Campos ----
simular <- function(sill,
                    rango,
                    nugget,
                    media,
                    xcampo = 1000,
                    ycampo = 1000,
                    xlargoparcela = 33,
                    yanchoparcela = 70) {
  # x <- seq(0, xcampo, xcampo / xlargoparcela)
  # y <- seq(0, ycampo, ycampo / yanchoparcela)
  
  x <- seq(0, xcampo, xlargoparcela)
  y <- seq(0, ycampo, yanchoparcela)
  
  modelo <-
    RMexp(var = sill, scale = rango) +
    RMnugget(var = nugget) +
    RMtrend(mean = media)
  simu <- RFsimulate(modelo, x, y,  spConform = TRUE)
  
  simu <- stars::st_rasterize(sf::st_as_sf(simu), 
                              nx = length(x),
                              ny = length(y))
  
  simu
}


# Graficar ----
ggplot_simulacion <- function(datos) {
  ggplot_sim <- 
    ggplot() +
    geom_stars(data = datos) +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_fill_gradient2(
      low = "#245668",# '#a81419',
      mid = "#39AB7E",# '#c9c600',
      high = "#EDEF5D",# '#047a00',
      midpoint = mediaCampo
    ) +
    labs(x = 'Coordenada x (m)',
         y = 'Coordenada y (m)',
         fill = 'Rendimiento')
  
  return(ggplot_sim)
  
}
  


# library(rcartocolor)
# rcartocolor::display_carto_all(colorblind_friendly = TRUE)
# rcartocolor::carto_pal(n = 3, "ag_Sunset")
# rcartocolor::carto_pal(n = 3, "ag_GrnYl")
