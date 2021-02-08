betas %>% 
  group_by(Modelo, Dosis0, TratMax, CorrEsp, Coeficientes) %>% 
  summarise(media = mean(Value),
            n = n())

betas_ss$disenio <-
  unlist(lapply(strsplit(as.character(betas_ss$ModeloLab), '[ + ]'), '[[', 1))

betas_ss$ModcZona <-
  ifelse(agrepl('Zona?', as.character(betas_ss$ModeloLab)), "ConZona", "SinZona")
       
       
descript <-
  betas_ss %>% 
  group_by(ModeloLab, disenio, ModcZona, Dosis0, TratMax, CorrEsp, CoeficientesParse) %>% 
  summarise(media = mean(Value),
            cobert = mean(cobertura) * 100,
            ampl = mean(upper - lower),
            lower = mean(lower),
            upper = mean(upper),
            n = n()) #%>% 
  # filter(CoeficientesParse == '(Intercept)')


ggplot(descript) +
  geom_point(aes(cobert, ampl, color = Dosis0)) +
  facet_wrap(CoeficientesParse ~ ., scales = 'free')

ggplot(descript) +
  geom_point(aes(cobert, ampl, color = interaction(ModcZona, Dosis0)))



ggplot(descript,
       aes(
         ModeloLab,
         media,
         color = interaction(Dosis0, TratMax),
         group = interaction(Dosis0, TratMax)
       )) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.3)) +
  facet_grid(CoeficientesParse ~ CorrEsp, scales = 'free',
             labeller = labeller(CoeficientesParse = label_parsed))

