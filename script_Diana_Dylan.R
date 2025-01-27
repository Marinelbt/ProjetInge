library(effects)
library(tidyverse)
library(latex2exp)


data_glm  <- readr::read_rds("data_glm.rds")


mod.logistic_growth_by_sp <-glm(growth_bin ~ Espèce + Temp_num + AW_num   +
                         Espèce:Temp_num+  Espèce:AW_num
                         , family = "binomial", data = data_glm)


data_effects <- allEffects(mod.logistic_growth_by_sp)
# les effets d'interaction dominent (car les variable seule ne sont pas indépendante), la fonction va 
# montrer les effets de chaque combinaison de cette interaction sur la varibale réponse en tenant les 
# autres variables comme constante (moy ou med selon le cas)

# sans interaction, la fonction nous montre les effets de chaque modalité des variable sur chaque modalité
# de la variable réponse
# chaque valeur correspond à la probabilité estimé de la varibale réponse pour chaque modalité de la variable
# toutes choses égales par ailleurs

vec_color_species = c(`F. graminearum` = "indianred4", `F. avenaceum` = "deepskyblue", 
                      `F. poae` = "orchid1", `F. tricinctum` = "gold2", `F. langsethiae` = "lightgreen"
)
# names(vec_color_species) = data_strains_color %>% distinct(Espèce) %>% pull(Espèce)



data_all_effects <- allEffects(mod.logistic_growth_by_sp)$`Espèce:Temp_num` %>% 
  as_tibble() %>% 
  bind_rows(., allEffects(mod.logistic_growth_by_sp)$`Espèce:AW_num` %>% as_tibble())



appender <- function(string) { TeX(string)  }

vec_color_species <- c(`F. graminearum` = "indianred4", `F. avenaceum` = "deepskyblue", 
                       `F. poae` = "orchid1", `F. tricinctum` = "gold2", `F. langsethiae` = "lightgreen"
)

data_all_effects_aw <-allEffects(mod.logistic_growth_by_sp,
                                 given.values=c(AW_num = 94))$`Espèce:Temp_num` %>%
  as_tibble %>% 
  mutate(aw ="$a_w$ = 0.94")%>%
  bind_rows(., 
            allEffects(mod.logistic_growth_by_sp, given.values=c(AW_num = 95))$`Espèce:Temp_num`%>%
              as_tibble%>% mutate(aw ="$a_w$  = 0.95"), 
            allEffects(mod.logistic_growth_by_sp, given.values=c(AW_num = 96))$`Espèce:Temp_num`%>%
              as_tibble%>% mutate(aw ="$a_w$  = 0.96"), 
            allEffects(mod.logistic_growth_by_sp, given.values=c(AW_num = 97))$`Espèce:Temp_num`%>%
              as_tibble%>% mutate(aw ="$a_w$  = 0.97"),
            allEffects(mod.logistic_growth_by_sp, given.values=c(AW_num = 98))$`Espèce:Temp_num`%>%
              as_tibble%>% mutate(aw ="$a_w$  = 0.98"),
            allEffects(mod.logistic_growth_by_sp, given.values=c(AW_num = 99))$`Espèce:Temp_num`%>%
              as_tibble %>% mutate(aw ="$a_w$  = 0.99") ) %>% 
  mutate(aw = fct_relevel(aw, "$a_w$ = 0.94"))

plot_prob_growth_against_Temp <- data_all_effects_aw %>% 
  ggplot(aes(x = Temp_num, y = fit, color = Espèce, fill = Espèce)) +
  geom_line(aes(group = Espèce), linewidth = 0.3) + 
  geom_point(size=  0.7) +
  geom_ribbon(aes(ymin = lower, 
                  ymax = upper), alpha = 0.3, linewidth = 0.2) +
  labs(x = "Temperature (°C)", y= TeX("$\\hat{P}(growth)$"), fill  = "Species",  color = "Species") +
  facet_wrap(aw~. , 
             labeller = as_labeller(appender, 
                                    default = label_parsed)) +
  theme(text = element_text(size = 7), title = element_text(size = 7))+ 
  scale_color_manual(values = vec_color_species) +
  scale_fill_manual(values = vec_color_species)


data_all_effects_Temp <-allEffects(mod.logistic_growth_by_sp, given.values=c(Temp_num = 15))$`Espèce:AW_num` %>% as_tibble %>% mutate(Temp ="15 °C")%>% 
  bind_rows(., allEffects(mod.logistic_growth_by_sp, given.values=c(Temp_num = 20))$`Espèce:AW_num`%>% as_tibble%>% mutate(Temp ="20 °C"), allEffects(mod.logistic_growth_by_sp, given.values=c(Temp_num = 25))$`Espèce:AW_num`%>% as_tibble %>% mutate(Temp ="25 °C"), allEffects(mod.logistic_growth_by_sp, given.values=c(Temp_num = 30))$`Espèce:AW_num`%>% as_tibble %>% mutate(Temp ="30 °C")  )


plot_prob_growth_against_aw <- data_all_effects_Temp %>% 
  mutate(AW_num = AW_num/100) %>% 
  ggplot(aes(x = AW_num, y = fit, color = Espèce, fill = Espèce)) +
  geom_line(aes(group = Espèce), linewidth = 0.3) + 
  geom_point(size=  0.7) +
  geom_ribbon(aes(ymin = lower, 
                  ymax = upper), alpha = 0.3, linewidth = 0.2) +
  labs(x = TeX("$a_w$"), y= TeX("$\\hat{P}(growth)$"), fill  = "Species",  color = "Species") +
  facet_wrap(Temp~. )


