setwd("/home/ppaccioretti/AjustesSemivar/")
library(sp)
library(automap)
library(dplyr)
library(parallel)


rsv <- function(sillparc, nugget) {
  sillparc / (sillparc + nugget) * 100
}
catrsv <- function(rsv) {
  ifelse(rsv <= 25, 'bajo',
         ifelse(rsv > 25 & rsv < 75, 'medio', 'alto'))
}


misArchivos <-
  list.files('Depurados/',
             full.names = TRUE)

# misArchivos <-
#   list.files(
#     'C:/Users/Pablo/OneDrive/Documentos/Estadística/Doctorado/MonitoresRendimiento/Depurados',
#     full.names = TRUE
#   )


misAjustes <-
  mclapply(misArchivos, function(arch) {
    mibase <- read.table(arch, header = TRUE)

    if (!('PRODUCTO' %in% names(mibase)) |
        !('REND' %in% names(mibase)) |
        !('x' %in% names(mibase)) |
        !('y' %in% names(mibase)) |
        !(length(unique(mibase[['PRODUCTO']])) == 1)) {
      return(NULL)
    }

    if (unique(mibase[['PRODUCTO']]) != 'MAIZ')
    {
      return(NULL)
    }
    
    cat(arch, '\n\n')
    if (unique(mibase[['PRODUCTO']]) == 'MAIZ')
    {
      browser()
      mibase$REND <- mibase$REND  *  10

      archivo <- tail(strsplit(arch, '/')[[1]], 1)
      media <- mean(mibase$REND, na.rm = TRUE)

      suppressWarnings({
        coordinates(mibase) <- c('x', 'y')
        
        VariogramaSph <-
          autofitVariogram(REND  ~  1, mibase, model = c("Sph"))  
        
        Variograma <-
          autofitVariogram(REND  ~  1, mibase, model = c("Exp", "Sph", "Gau"),
                           start_vals = c(VariogramaSph$var_model[1,2],
                                          VariogramaSph$var_model[2,3],
                                          VariogramaSph$var_model[2,2]))

      })
      ModeloVariograma <-
        cbind(archivo,
              media,
              Variograma$var_model[2, 1:3],
              Variograma$var_model[1, 2],
              Variograma$sserr)
      names(ModeloVariograma) <-
        c("Archivo",
          "Media",
          "Model",
          "Parcial Sill",
          "Range",
          "Nugget",
          "SCE")


      return(ModeloVariograma)
    }
  }, mc.cores = 30)


misAjustesMaiz <-
  do.call(rbind, misAjustes[!unlist(lapply(misAjustes, is.null))])

rownames(misAjustesMaiz) <- NULL

misAjustesMaiz$rsv <-
  rsv(misAjustesMaiz$`Parcial Sill`, misAjustesMaiz$Nugget)
misAjustesMaiz$catrsv <- catrsv(misAjustesMaiz$rsv)


# saveRDS(misAjustesMaiz, 'AjustesSemivariogramas.rds')


misAjustesMaiz <- readRDS('AjustesSemivariogramas_startVal.rds')

misAjustesMaiz %>% 
  group_by(catrsv, Model) %>% 
  summarise(
    media = mean(Media, na.rm = T),
    mediana = median(Media, na.rm = T),
    ParcialSillmed = median(`Parcial Sill`, na.rm = T),
    Rangemed = median(Range, na.rm = T),
    Nuggetmed = median(Nugget, na.rm = T),
    rsvmed = median(rsv, na.rm = T),
    n = n())






