library(ggplot2)
valormax <- function(a,b) {
  -a/(2*b)
}
# saveRDS(MisParcelasFRTrat, 
#         'extra/EnsayoCurvaFertilizacion.RDS')
 misParcelas <- readRDS('extra/EnsayoCurvaFertilizacion.RDS')

ggplot(misParcelas,
       aes(
         x = AsigTrat,
         y = MiRto,
         group = as.factor(Zona),
         color = as.factor(Zona)
       )) +
  # geom_point() +
  geom_smooth(formula = y ~ x + I(x ^ 2), method = 'lm') +
  labs(y = expression('Rendimiento simulado [' ~ qq ~ ha^-1 ~ ']'),
       x = expression('Dosis fertilizante [' ~ kg ~ N ~ ha^-1 ~ ']'),
       color = 'Zona') +
  scale_color_grey(start = 0.1, end = 0.7) +
  scale_x_continuous(breaks = seq(0, 250, 50)) +
  guides(color = FALSE) +
  # scale_colour_discrete(labels = c('A', 'B')) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())


ggsave('images/Curvas_Restpuesta.tiff', width = 12, height = 8, units = 'cm')




# ggplot(MisParcelasFRTrat, 
#        aes(x = AsigTrat, y = MiRto, group = as.factor(Zona))) +
#   geom_point() +
#   geom_smooth(formula = y ~ x + I(x^2), method = 'lm')
# 
# ggplot(MisParcelasDCATrat, 
#        aes(x = AsigTrat, y = MiRto, group = as.factor(Zona))) +
#   geom_point() +
#   geom_smooth(formula = y ~ x + I(x^2), method = 'lm')
# 
# ggplot(MisParcelasDBCATrat, 
#        aes(x = AsigTrat, y = MiRto, group = as.factor(Zona))) +
#   geom_point() +
#   geom_smooth(formula = y ~ x + I(x^2), method = 'lm')
# 
# 
# 
# library(sf)
# 
# MisParcelasFRTrat_sf <- st_as_sf(MisParcelasFRTrat, coords = c('x', 'y'))
# ggplot(MisParcelasFRTrat_sf) +
#   geom_sf(aes(color = MiRto), size = 4)
# 
# MisParcelasDCATrat_sf <- st_as_sf(MisParcelasDCATrat, coords = c('x', 'y'))
# ggplot(MisParcelasDCATrat_sf) +
#   geom_sf(aes(color = MiRto), size = 4)
# 
# MisParcelasDBCATrat_sf <- st_as_sf(MisParcelasDBCATrat, coords = c('x', 'y'))
# ggplot(MisParcelasDBCATrat_sf) +
#   geom_sf(aes(color = Bloque), size = 4)
# 
# miBeta1 * MiRto + 
#   miBeta2 * MiRto ^ 2 + 
#   TerInterZonError * MisParcelasFRTrat["Zona"] * MiRto + 
#   MisParcelasFRTrat["ErrorConEfecto"]
# 
# 
# 0.00324 * MisParcelasFRTrat["Zona"]
# 
# valormax(miBeta1, miBeta2)

