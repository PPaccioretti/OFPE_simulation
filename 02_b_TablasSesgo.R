descriptiva_sesgo <- function(TablaInteres) {
  
  diferencias <- diff(TablaCob_Zona$media)
  
  Tabla_ss <- TablaCob_Zona[seq(1, length(diferencias), 2) , 1:3]
  Tabla_ss$diferencias <- diferencias[seq(1, length(diferencias), 2)]
  
  miscat <- TablaCob_Zona[1:2 , 4, drop = TRUE]
  miscat <- paste('Diferencia:', miscat[2], '-', miscat[1])
  
  mediaporGrpo <-
    Tabla_ss %>% 
    group_by(ModeloLab) %>% 
    summarise(media = mean(diferencias),
              mediana = median(diferencias))
  
  list(miscat = miscat,
       Tabla = Tabla_ss,
       mediaporGrupo = mediaporGrpo,
       mediaGral = mean(diferencias[seq(1, length(diferencias), 2)]),
       medianaGral = median(diferencias[seq(1, length(diferencias), 2)]))
  
}

# CorrEsp ----
TablaCobertura <-
  coberturas %>%
  reshape2::dcast(ModeloLab + Dosis0 + TratMax + CorrEsp ~ CoeficientesParse,
                  value.var = 'cobertura_p',
                  fun.aggregate = mean) %>%
  rowwise() %>% 
  mutate(media = mediaSinNaN(c_across(starts_with("beta")))) 

TablaCob_Zona <- 
  TablaCobertura %>% 
  filter(agrepl('Zona', ModeloLab))

descriptiva_sesgo(TablaCob_Zona)

## TratMaximo ---- 

TablaCobertura <-
  coberturas %>%
  reshape2::dcast(ModeloLab + Dosis0 + CorrEsp + TratMax ~ CoeficientesParse,
                  value.var = 'cobertura_p',
                  fun.aggregate = mean) %>%
  rowwise() %>% 
  mutate(media = mediaSinNaN(c_across(starts_with("beta")))) 



TablaCob_Zona <- 
  TablaCobertura %>% 
  filter(agrepl('Zona', ModeloLab))

descriptiva_sesgo(TablaCob_Zona)


## Dosis0 ----

TablaCobertura <-
  coberturas %>%
  reshape2::dcast(ModeloLab + CorrEsp + TratMax + Dosis0 ~ CoeficientesParse,
                  value.var = 'cobertura_p',
                  fun.aggregate = mean) %>%
  rowwise() %>% 
  mutate(media = mediaSinNaN(c_across(starts_with("beta")))) 



TablaCob_Zona <- 
  TablaCobertura %>% 
  filter(agrepl('Zona', ModeloLab))

descriptiva_sesgo(TablaCob_Zona)


# Sin Zona ----
## CorrEsp ----
TablaCobertura <-
  coberturas %>%
  reshape2::dcast(ModeloLab + Dosis0 + TratMax + CorrEsp ~ CoeficientesParse,
                  value.var = 'cobertura_p',
                  fun.aggregate = mean) %>%
  rowwise() %>% 
  mutate(media = mediaSinNaN(c_across(starts_with("beta")))) 

TablaCob_Zona <- 
  TablaCobertura %>% 
  filter(!agrepl('Zona', ModeloLab))

descriptiva_sesgo(TablaCob_Zona)

## TratMaximo ---- 

TablaCobertura <-
  coberturas %>%
  reshape2::dcast(ModeloLab + Dosis0 + CorrEsp + TratMax ~ CoeficientesParse,
                  value.var = 'cobertura_p',
                  fun.aggregate = mean) %>%
  rowwise() %>% 
  mutate(media = mediaSinNaN(c_across(starts_with("beta")))) 



TablaCob_Zona <- 
  TablaCobertura %>% 
  filter(!agrepl('Zona', ModeloLab))

descriptiva_sesgo(TablaCob_Zona)


## Dosis0 ----

TablaCobertura <-
  coberturas %>%
  reshape2::dcast(ModeloLab + CorrEsp + TratMax + Dosis0 ~ CoeficientesParse,
                  value.var = 'cobertura_p',
                  fun.aggregate = mean) %>%
  rowwise() %>% 
  mutate(media = mediaSinNaN(c_across(starts_with("beta")))) 



TablaCob_Zona <- 
  TablaCobertura %>% 
  filter(!agrepl('Zona', ModeloLab))

descriptiva_sesgo(TablaCob_Zona)




