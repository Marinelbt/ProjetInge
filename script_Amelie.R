library(readr) 
library(dplyr)
library(tidyverse)
library(GGally)
library(RcmdrMisc)

data <- read.csv("data_final/df_pdfs_final.csv", header = TRUE, sep = ",")

data <- data %>%
  separate(score_final, into = c("score_final_r", "score_final_v"), sep = "-") %>%
  mutate(
    score_final_r = as.integer(score_final_r),
    score_final_v = as.integer(score_final_v)
  )

diff_norme <- read.csv("data_final/data_diff_norme_final.csv", row.names = 1)
diff_norme$saison <- factor(diff_norme$saison)

# p1 = 59:00 - 59:20
# p2 = 59:20 - 59:40
# p3 = 59:40 - 60:00

########## Data 59 min période 1 : 59:00 - 59:20 ########## 

data_mod1_p1 <- data %>%
  group_by(code_rencontre, club_recevant, coach_recevant, club_visiteur, coach_visiteur, journee, score_final_r, score_final_v, fichier) %>%
  summarise(temps_proche_59_min = max(Temps_Sec[Temps_Sec < 59 * 60], na.rm = TRUE),
            statut_59_min_r = first(statut_recevant[Temps_Sec == temps_proche_59_min]),
            statut_59_min_v = -statut_59_min_r,
            score_recevant_59_min = first(score_recevant[Temps_Sec == temps_proche_59_min]),
            score_visiteur_59_min = first(score_visiteur[Temps_Sec == temps_proche_59_min]),
            
            temps_proche_p1 = max(Temps_Sec[Temps_Sec <= 59 * 60], na.rm = T),
            statut_p1 = first(statut_recevant[Temps_Sec == temps_proche_p1]),
            
            TM_derniere_min_r = if_else(
              any(Temps_Sec >= 59 * 60 & Temps_Sec <= 59 * 60 + 20 & grepl("tem", action, ignore.case = TRUE) & action_equipe == "r"),
              1, 
              0
            ),
            TM_derniere_min_v = if_else(
              any(Temps_Sec >= 59 * 60 & Temps_Sec <= 59 * 60 + 20 & grepl("tem", action, ignore.case = TRUE) & action_equipe == "v"),
              1, 
              0
            ),
            DM_r = if_else(
              any(Temps_Sec >= 57 * 60 & (grepl("2MN", action, ignore.case = TRUE) | grepl("Dis", action, ignore.case = TRUE)) & action_equipe == "r"),
              1, 
              0
            ),
            DM_v = if_else(
              any(Temps_Sec >= 57 * 60 & (grepl("2MN", action, ignore.case = TRUE) | grepl("Dis", action, ignore.case = TRUE)) & action_equipe == "v"),
              1, 
              0
            ),
            .groups = "drop"
  ) %>%
  filter(abs(score_recevant_59_min - score_visiteur_59_min) < 3) %>% 
  pivot_longer(
    cols = c(statut_59_min_r, statut_59_min_v), 
    names_to = "equipe", 
    values_to = "statut_59_min"
  ) %>%
  mutate(
    equipe = case_when(
      equipe == "statut_59_min_r" ~ "R",
      equipe == "statut_59_min_v" ~ "V"
    ),
    statut_final = case_when(
      equipe == "R" & score_final_r > score_final_v ~ 1,
      equipe == "V" & score_final_v > score_final_r ~ 1,
      score_final_v == score_final_r ~ 0,
      equipe == "R" & score_final_r < score_final_v ~ -1,
      equipe == "V" & score_final_v < score_final_r ~ -1,
    ),
    effet_positif = case_when(
      statut_p1 == -1 & (statut_final == 0 | statut_final == 1) ~ 1,
      statut_p1 == 1 & statut_final == 1 ~ 1,
      statut_p1 == 0 & statut_final == 1 ~ 1,
      TRUE ~ 0
    ),
    TM_derniere_min = case_when(
      equipe == "R" & TM_derniere_min_r == 1 ~ 1,
      equipe == "V" & TM_derniere_min_v == 1 ~ 1,
      TRUE ~ 0
    ),
    TM_derniere_min_adverse = case_when(
      equipe == "V" & TM_derniere_min_r == 1 ~ 1,
      equipe == "R" & TM_derniere_min_v == 1 ~ 1,
      TRUE ~ 0
    ),
    HF = case_when(
      grepl("F", fichier) ~ "F",
      TRUE ~ "H"
    ),
    DM = case_when(
      equipe == "R" & DM_r == 1 ~ 1,
      equipe == "V" & DM_v == 1 ~ 1,
      TRUE ~ 0
    ),
    DM_adverse = case_when(
      equipe == "V" & DM_r == 1 ~ 1,
      equipe == "R" & DM_v == 1 ~ 1,
      TRUE ~ 0
    ),
    division = case_when(
      grepl("D1", fichier, ignore.case = TRUE) ~ "D1",
      grepl("D2", fichier, ignore.case = TRUE) ~ "D2",
      grepl("N1", fichier, ignore.case = TRUE) ~ "N1"
    ),
    saison = case_when(
      grepl("2021", fichier, ignore.case = TRUE) ~ "2021",
      grepl("2122", fichier, ignore.case = TRUE) ~ "2122",
      grepl("2223", fichier, ignore.case = TRUE) ~ "2223",
      grepl("2324", fichier, ignore.case = TRUE) ~ "2324"
    )
  ) %>% 
  select(-c(TM_derniere_min_r, TM_derniere_min_v, DM_r, DM_v, temps_proche_59_min, temps_proche_p1)) %>% 
  ungroup()  %>% 
  filter (saison != 2425) %>%
  filter(equipe == "R")  %>% 
  filter(statut_59_min != 0 | statut_final != 0)

data_mod1_p1$statut_59_min <- factor(data_mod1_p1$statut_59_min)
data_mod1_p1$statut_final <- factor(data_mod1_p1$statut_final)
data_mod1_p1$effet_positif <- factor(data_mod1_p1$effet_positif)
data_mod1_p1$TM_derniere_min <- factor(data_mod1_p1$TM_derniere_min)
data_mod1_p1$TM_derniere_min_adverse <- factor(data_mod1_p1$TM_derniere_min_adverse)
data_mod1_p1$HF <- factor(data_mod1_p1$HF)
data_mod1_p1$DM <- factor(data_mod1_p1$DM)
data_mod1_p1$DM_adverse <- factor(data_mod1_p1$DM_adverse)
data_mod1_p1$club_recevant <- factor(data_mod1_p1$club_recevant)
data_mod1_p1$saison <- factor(data_mod1_p1$saison)
data_mod1_p1$division <- factor(data_mod1_p1$division)

data_mod1_p1 <- data_mod1_p1 %>% 
  select(-c("journee","division","HF", "saison")) %>% 
  left_join(diff_norme, by = join_by(code_rencontre)) %>% 
  drop_na()

quantiles <- quantile(data_mod1_p1$diff_point_norm, probs = c(0, 1/3, 2/3, 1))

print(quantiles)

data_mod1_p1$group_diff_point <- cut(data_mod1_p1$diff_point_norm,
                                     breaks = quantiles,
                                     labels = c("moins fort","meme niveau","plus fort"),
                                     include.lowest = T)
table(data_mod1_p1$group_diff_point)

ggplot(data_mod1_p1, aes(x = group_diff_point, y = diff_point_norm, fill = group_diff_point)) +
  geom_boxplot() +
  labs(title = "Répartition des groupes créés", x = "Groupes", y = "Valeurs de A") +
  theme_minimal()

data_mod1_p1 <- data_mod1_p1 %>% 
  mutate(ecart_classement = case_when(
    group_diff_point == "moins fort" ~ 1,
    group_diff_point == "meme niveau" ~ 2,
    group_diff_point == "plus fort" ~ 3
  ))

data_mod1_p1$ecart_classement <- as.factor(data_mod1_p1$ecart_classement)

data_mod1_p1 <- data_mod1_p1 %>% 
  select(-c(score_recevant_59_min, score_visiteur_59_min, equipe, statut_59_min))


write.csv(data_mod1_p1, "data_59_p1.csv")

########## Data 59 min période 2 : 59:20 - 59:40 ########## 

data_mod1_p2 <- data %>%
  group_by(code_rencontre, club_recevant, coach_recevant, club_visiteur, coach_visiteur, journee, score_final_r, score_final_v, fichier) %>%
  summarise(temps_proche_59_min = max(Temps_Sec[Temps_Sec < 59 * 60], na.rm = TRUE),
            statut_59_min_r = first(statut_recevant[Temps_Sec == temps_proche_59_min]),
            statut_59_min_v = -statut_59_min_r,
            score_recevant_59_min = first(score_recevant[Temps_Sec == temps_proche_59_min]),
            score_visiteur_59_min = first(score_visiteur[Temps_Sec == temps_proche_59_min]),
            
            temps_proche_p2 = max(Temps_Sec[Temps_Sec <= 59 * 60 + 20], na.rm = T),
            statut_p2 = first(statut_recevant[Temps_Sec == temps_proche_p2]),
            
            TM_derniere_min_r = if_else(
              any(Temps_Sec >= 59 * 60 + 20 & Temps_Sec <= 59 * 60 + 40 & grepl("tem", action, ignore.case = TRUE) & action_equipe == "r"),
              1, 
              0
            ),
            TM_derniere_min_v = if_else(
              any(Temps_Sec >= 59 * 60 + 20 & Temps_Sec <= 59 * 60 + 40 & grepl("tem", action, ignore.case = TRUE) & action_equipe == "v"),
              1, 
              0
            ),
            DM_r = if_else(
              any(Temps_Sec >= 57 * 60 & (grepl("2MN", action, ignore.case = TRUE) | grepl("Dis", action, ignore.case = TRUE)) & action_equipe == "r"),
              1, 
              0
            ),
            DM_v = if_else(
              any(Temps_Sec >= 57 * 60 & (grepl("2MN", action, ignore.case = TRUE) | grepl("Dis", action, ignore.case = TRUE)) & action_equipe == "v"),
              1, 
              0
            ),
            .groups = "drop"
  ) %>%
  filter(abs(score_recevant_59_min - score_visiteur_59_min) < 3) %>% 
  pivot_longer(
    cols = c(statut_59_min_r, statut_59_min_v), 
    names_to = "equipe", 
    values_to = "statut_59_min"
  ) %>%
  mutate(
    equipe = case_when(
      equipe == "statut_59_min_r" ~ "R",
      equipe == "statut_59_min_v" ~ "V"
    ),
    statut_final = case_when(
      equipe == "R" & score_final_r > score_final_v ~ 1,
      equipe == "V" & score_final_v > score_final_r ~ 1,
      score_final_v == score_final_r ~ 0,
      equipe == "R" & score_final_r < score_final_v ~ -1,
      equipe == "V" & score_final_v < score_final_r ~ -1,
    ),
    effet_positif = case_when(
      statut_p2 == -1 & (statut_final == 0 | statut_final == 1) ~ 1,
      statut_p2 == 1 & statut_final == 1 ~ 1,
      statut_p2 == 0 & statut_final == 1 ~ 1,
      TRUE ~ 0
    ),
    TM_derniere_min = case_when(
      equipe == "R" & TM_derniere_min_r == 1 ~ 1,
      equipe == "V" & TM_derniere_min_v == 1 ~ 1,
      TRUE ~ 0
    ),
    TM_derniere_min_adverse = case_when(
      equipe == "V" & TM_derniere_min_r == 1 ~ 1,
      equipe == "R" & TM_derniere_min_v == 1 ~ 1,
      TRUE ~ 0
    ),
    HF = case_when(
      grepl("F", fichier) ~ "F",
      TRUE ~ "H"
    ),
    DM = case_when(
      equipe == "R" & DM_r == 1 ~ 1,
      equipe == "V" & DM_v == 1 ~ 1,
      TRUE ~ 0
    ),
    DM_adverse = case_when(
      equipe == "V" & DM_r == 1 ~ 1,
      equipe == "R" & DM_v == 1 ~ 1,
      TRUE ~ 0
    ),
    division = case_when(
      grepl("D1", fichier, ignore.case = TRUE) ~ "D1",
      grepl("D2", fichier, ignore.case = TRUE) ~ "D2",
      grepl("N1", fichier, ignore.case = TRUE) ~ "N1"
    ),
    saison = case_when(
      grepl("2021", fichier, ignore.case = TRUE) ~ "2021",
      grepl("2122", fichier, ignore.case = TRUE) ~ "2122",
      grepl("2223", fichier, ignore.case = TRUE) ~ "2223",
      grepl("2324", fichier, ignore.case = TRUE) ~ "2324"
    )
  ) %>% 
  select(-c(TM_derniere_min_r, TM_derniere_min_v, DM_r, DM_v, temps_proche_59_min, temps_proche_p2)) %>% 
  ungroup()  %>% 
  filter (saison != 2425) %>%
  filter(equipe == "R")  %>% 
  filter(statut_59_min != 0 | statut_final != 0)

data_mod1_p2$statut_59_min <- factor(data_mod1_p2$statut_59_min)
data_mod1_p2$statut_final <- factor(data_mod1_p2$statut_final)
data_mod1_p2$effet_positif <- factor(data_mod1_p2$effet_positif)
data_mod1_p2$TM_derniere_min <- factor(data_mod1_p2$TM_derniere_min)
data_mod1_p2$TM_derniere_min_adverse <- factor(data_mod1_p2$TM_derniere_min_adverse)
data_mod1_p2$HF <- factor(data_mod1_p2$HF)
data_mod1_p2$DM <- factor(data_mod1_p2$DM)
data_mod1_p2$DM_adverse <- factor(data_mod1_p2$DM_adverse)
data_mod1_p2$club_recevant <- factor(data_mod1_p2$club_recevant)
data_mod1_p2$saison <- factor(data_mod1_p2$saison)
data_mod1_p2$division <- factor(data_mod1_p2$division)

data_mod1_p2 <- data_mod1_p2 %>% 
  select(-c("journee","division","HF", "saison")) %>% 
  left_join(diff_norme, by = join_by(code_rencontre)) %>% 
  drop_na()

quantiles <- quantile(data_mod1_p2$diff_point_norm, probs = c(0, 1/3, 2/3, 1))

print(quantiles)

data_mod1_p2$group_diff_point <- cut(data_mod1_p2$diff_point_norm,
                                  breaks = quantiles,
                                  labels = c("moins fort","meme niveau","plus fort"),
                                  include.lowest = T)
table(data_mod1_p2$group_diff_point)

ggplot(data_mod1_p2, aes(x = group_diff_point, y = diff_point_norm, fill = group_diff_point)) +
  geom_boxplot() +
  labs(title = "Répartition des groupes créés", x = "Groupes", y = "Valeurs de A") +
  theme_minimal()

data_mod1_p2 <- data_mod1_p2 %>% 
  mutate(ecart_classement = case_when(
    group_diff_point == "moins fort" ~ 1,
    group_diff_point == "meme niveau" ~ 2,
    group_diff_point == "plus fort" ~ 3
  ))

data_mod1_p2$ecart_classement <- as.factor(data_mod1_p2$ecart_classement)

data_mod1_p2 <- data_mod1_p2 %>% 
  select(-c(score_recevant_59_min, score_visiteur_59_min, equipe, statut_59_min))


write.csv(data_mod1_p2, "data_59_p2.csv")

########## Data 59 min période 3 : 59:40 - 60:00 ##########

data_mod1_p3 <- data %>%
  group_by(code_rencontre, club_recevant, coach_recevant, club_visiteur, coach_visiteur, journee, score_final_r, score_final_v, fichier) %>%
  summarise(temps_proche_59_min = max(Temps_Sec[Temps_Sec < 59 * 60], na.rm = TRUE),
            statut_59_min_r = first(statut_recevant[Temps_Sec == temps_proche_59_min]),
            statut_59_min_v = -statut_59_min_r,
            score_recevant_59_min = first(score_recevant[Temps_Sec == temps_proche_59_min]),
            score_visiteur_59_min = first(score_visiteur[Temps_Sec == temps_proche_59_min]),
            
            temps_proche_p3 = max(Temps_Sec[Temps_Sec <= 59 * 60 + 40], na.rm = T),
            statut_p3 = first(statut_recevant[Temps_Sec == temps_proche_p3]),
            
            TM_derniere_min_r = if_else(
              any(Temps_Sec >= 59 * 60 + 40 & grepl("tem", action, ignore.case = TRUE) & action_equipe == "r"),
              1, 
              0
            ),
            TM_derniere_min_v = if_else(
              any(Temps_Sec >= 59 * 60 + 40 & grepl("tem", action, ignore.case = TRUE) & action_equipe == "v"),
              1, 
              0
            ),
            DM_r = if_else(
              any(Temps_Sec >= 57 * 60 & (grepl("2MN", action, ignore.case = TRUE) | grepl("Dis", action, ignore.case = TRUE)) & action_equipe == "r"),
              1, 
              0
            ),
            DM_v = if_else(
              any(Temps_Sec >= 57 * 60 & (grepl("2MN", action, ignore.case = TRUE) | grepl("Dis", action, ignore.case = TRUE)) & action_equipe == "v"),
              1, 
              0
            ),
            .groups = "drop"
  ) %>%
  filter(abs(score_recevant_59_min - score_visiteur_59_min) < 3) %>% 
  pivot_longer(
    cols = c(statut_59_min_r, statut_59_min_v), 
    names_to = "equipe", 
    values_to = "statut_59_min"
  ) %>%
  mutate(
    equipe = case_when(
      equipe == "statut_59_min_r" ~ "R",
      equipe == "statut_59_min_v" ~ "V"
    ),
    statut_final = case_when(
      equipe == "R" & score_final_r > score_final_v ~ 1,
      equipe == "V" & score_final_v > score_final_r ~ 1,
      score_final_v == score_final_r ~ 0,
      equipe == "R" & score_final_r < score_final_v ~ -1,
      equipe == "V" & score_final_v < score_final_r ~ -1,
    ),
    effet_positif = case_when(
      statut_p3 == -1 & (statut_final == 0 | statut_final == 1) ~ 1,
      statut_p3 == 1 & statut_final == 1 ~ 1,
      statut_p3 == 0 & statut_final == 1 ~ 1,
      TRUE ~ 0
    ),
    TM_derniere_min = case_when(
      equipe == "R" & TM_derniere_min_r == 1 ~ 1,
      equipe == "V" & TM_derniere_min_v == 1 ~ 1,
      TRUE ~ 0
    ),
    TM_derniere_min_adverse = case_when(
      equipe == "V" & TM_derniere_min_r == 1 ~ 1,
      equipe == "R" & TM_derniere_min_v == 1 ~ 1,
      TRUE ~ 0
    ),
    HF = case_when(
      grepl("F", fichier) ~ "F",
      TRUE ~ "H"
    ),
    DM = case_when(
      equipe == "R" & DM_r == 1 ~ 1,
      equipe == "V" & DM_v == 1 ~ 1,
      TRUE ~ 0
    ),
    DM_adverse = case_when(
      equipe == "V" & DM_r == 1 ~ 1,
      equipe == "R" & DM_v == 1 ~ 1,
      TRUE ~ 0
    ),
    division = case_when(
      grepl("D1", fichier, ignore.case = TRUE) ~ "D1",
      grepl("D2", fichier, ignore.case = TRUE) ~ "D2",
      grepl("N1", fichier, ignore.case = TRUE) ~ "N1"
    ),
    saison = case_when(
      grepl("2021", fichier, ignore.case = TRUE) ~ "2021",
      grepl("2122", fichier, ignore.case = TRUE) ~ "2122",
      grepl("2223", fichier, ignore.case = TRUE) ~ "2223",
      grepl("2324", fichier, ignore.case = TRUE) ~ "2324"
    )
  ) %>% 
  select(-c(TM_derniere_min_r, TM_derniere_min_v, DM_r, DM_v, temps_proche_59_min, temps_proche_p3)) %>% 
  ungroup()  %>% 
  filter (saison != 2425) %>%
  filter(equipe == "R")  %>% 
  filter(statut_59_min != 0 | statut_final != 0)

data_mod1_p3$statut_59_min <- factor(data_mod1_p3$statut_59_min)
data_mod1_p3$statut_final <- factor(data_mod1_p3$statut_final)
data_mod1_p3$effet_positif <- factor(data_mod1_p3$effet_positif)
data_mod1_p3$TM_derniere_min <- factor(data_mod1_p3$TM_derniere_min)
data_mod1_p3$TM_derniere_min_adverse <- factor(data_mod1_p3$TM_derniere_min_adverse)
data_mod1_p3$HF <- factor(data_mod1_p3$HF)
data_mod1_p3$DM <- factor(data_mod1_p3$DM)
data_mod1_p3$DM_adverse <- factor(data_mod1_p3$DM_adverse)
data_mod1_p3$club_recevant <- factor(data_mod1_p3$club_recevant)
data_mod1_p3$saison <- factor(data_mod1_p3$saison)
data_mod1_p3$division <- factor(data_mod1_p3$division)

data_mod1_p3 <- data_mod1_p3 %>% 
  select(-c("journee","division","HF", "saison")) %>% 
  left_join(diff_norme, by = join_by(code_rencontre)) %>% 
  drop_na()

quantiles <- quantile(data_mod1_p3$diff_point_norm, probs = c(0, 1/3, 2/3, 1))

print(quantiles)

data_mod1_p3$group_diff_point <- cut(data_mod1_p3$diff_point_norm,
                                     breaks = quantiles,
                                     labels = c("moins fort","meme niveau","plus fort"),
                                     include.lowest = T)
table(data_mod1_p3$group_diff_point)

ggplot(data_mod1_p3, aes(x = group_diff_point, y = diff_point_norm, fill = group_diff_point)) +
  geom_boxplot() +
  labs(title = "Répartition des groupes créés", x = "Groupes", y = "Valeurs de A") +
  theme_minimal()

data_mod1_p3 <- data_mod1_p3 %>% 
  mutate(ecart_classement = case_when(
    group_diff_point == "moins fort" ~ 1,
    group_diff_point == "meme niveau" ~ 2,
    group_diff_point == "plus fort" ~ 3
  ))

data_mod1_p3$ecart_classement <- as.factor(data_mod1_p3$ecart_classement)

data_mod1_p3 <- data_mod1_p3 %>% 
  select(-c(score_recevant_59_min, score_visiteur_59_min, equipe, statut_59_min))


write.csv(data_mod1_p3, "data_59_p3.csv")


########## Analyse ##########

#### p1 gagnant
data_mod1_p1$ecart_classement <- relevel(data_mod1_p1$ecart_classement, ref = 2)
data_mod1_p1$HF <- relevel(data_mod1_p1$HF, ref = 'H')

data_mod1_p1_g <- data_mod1_p1 %>%
  filter(statut_p1 == 1)

mod1_p1 <- glm(effet_positif ~ TM_derniere_min + TM_derniere_min_adverse + DM_adverse + DM + ecart_classement + HF, 
            data = data_mod1_p1_g, family = "binomial")
summary(mod1_p1)

mod_final_p1_g <- stepwise(mod1, direction = "backward/forward", criterion = "AIC")


#### p2 gagnant
data_mod1_p2$ecart_classement <- relevel(data_mod1_p2$ecart_classement, ref = 2)
data_mod1_p2$HF <- relevel(data_mod1_p2$HF, ref = 'H')

data_mod1_p2_g <- data_mod1_p2 %>%
  filter(statut_p2 == 1)

mod1_p2 <- glm(effet_positif ~ TM_derniere_min + TM_derniere_min_adverse + DM_adverse + DM + ecart_classement + HF, 
               data = data_mod1_p2_g, family = "binomial")
summary(mod1_p2)

mod_final_p2_g <- stepwise(mod1_p2, direction = "backward/forward", criterion = "AIC")


#### p3 gagnant
data_mod1_p3$ecart_classement <- relevel(data_mod1_p3$ecart_classement, ref = 2)
data_mod1_p3$HF <- relevel(data_mod1_p3$HF, ref = 'H')

data_mod1_p3_g <- data_mod1_p3 %>%
  filter(statut_p3 == 1)

mod1_p3 <- glm(effet_positif ~ TM_derniere_min + TM_derniere_min_adverse + DM_adverse + DM + ecart_classement + HF, 
               data = data_mod1_p3_g, family = "binomial")
#summary(mod1_p2)

mod_final_p3_g <- stepwise(mod1_p3, direction = "backward/forward", criterion = "AIC")




pdf("Documents/modele_59_periode_gagnant.pdf", width = 12, height = 9)
ggcoef_compare(
  list(
    "Modèle P1" = mod_final_p1_g,
    "Modèle P2" = mod_final_p2_g,
    "Modèle P3" = mod_final_p3_g
  ),
  type = "faceted"
) +
  coord_cartesian(xlim = c(-1.5,1.5)) +  # Limiter l'axe des abscisses
  scale_x_continuous(
    breaks = seq(-1.5,1.5, by = 1),     # Ajouter des ticks tous les 1
    minor_breaks = seq(-1.5,1.5, by = 0.5)  # Sous-ticks tous les 0.5
  )  +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "longdash"),
    panel.grid.minor.x = element_line(color = "grey90", linetype = "longdash") # Ajouter les lignes pointillées pour les sous-ticks
  ) +
  ggtitle("Modèle par période de 20 sec durant la dernière minute avec statut gagnant au debut de la période")
dev.off()



ggplot(data.frame(Var = names(table(data_mod1_p2$TM_derniere_min)), 
                  Freq = as.numeric(table(data_mod1_p2$TM_derniere_min))), aes(x = Var, y = Freq, fill = Var)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Freq), vjust = -0.5) +
  labs(title = "Distribution de TM_derniere_min", x = "Temps mort à la dernière minute", y = "Fréquence") +
  scale_fill_manual(values = c("skyblue", "orange")) +
  theme_minimal() +
  theme(legend.position = "none")


#### p1 egalite
data_mod1_p1_e <- data_mod1_p1 %>%
  filter(statut_p1 == 0)

mod1_p1_e <- glm(effet_positif ~ TM_derniere_min + TM_derniere_min_adverse + DM_adverse + DM + ecart_classement + HF, 
               data = data_mod1_p1_e, family = "binomial")
summary(mod1_p1_e)

mod_final_p1_e <- stepwise(mod1_p1_e, direction = "backward/forward", criterion = "AIC")


#### p2 gagnant
data_mod1_p2_e <- data_mod1_p2 %>%
  filter(statut_p2 == 0)

mod1_p2_e <- glm(effet_positif ~ TM_derniere_min + TM_derniere_min_adverse + DM_adverse + DM + ecart_classement + HF, 
               data = data_mod1_p2_e, family = "binomial")
summary(mod1_p2_e)

mod_final_p2_e <- stepwise(mod1_p2_e, direction = "backward/forward", criterion = "AIC")


#### p3 gagnant
data_mod1_p3_e <- data_mod1_p3 %>%
  filter(statut_p3 == 0)

mod1_p3_e <- glm(effet_positif ~ TM_derniere_min + TM_derniere_min_adverse + DM_adverse + DM + ecart_classement + HF, 
               data = data_mod1_p3_e, family = "binomial")
#summary(mod1_p2)

mod_final_p3_e <- stepwise(mod1_p3_e, direction = "backward/forward", criterion = "AIC")




pdf("Documents/modele_59_periode_egalite.pdf", width = 12, height = 9)
ggcoef_compare(
  list(
    "Modèle P1" = mod_final_p1_e,
    "Modèle P2" = mod_final_p2_e,
    "Modèle P3" = mod_final_p3_e
  ),
  type = "faceted"
) +
  coord_cartesian(xlim = c(-1.5,1.5)) +  # Limiter l'axe des abscisses
  scale_x_continuous(
    breaks = seq(-1.5,1.5, by = 1),     # Ajouter des ticks tous les 1
    minor_breaks = seq(-1.5,1.5, by = 0.5)  # Sous-ticks tous les 0.5
  )  +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "longdash"),
    panel.grid.minor.x = element_line(color = "grey90", linetype = "longdash") # Ajouter les lignes pointillées pour les sous-ticks
  ) +
  ggtitle("Modèle par période de 20 sec durant la dernière minute avec statut egalite au debut de la période")
dev.off()


#### p1 perdant
data_mod1_p1_p <- data_mod1_p1 %>%
  filter(statut_p1 == -1)

mod1_p1_p <- glm(effet_positif ~ TM_derniere_min + TM_derniere_min_adverse + DM_adverse + DM + ecart_classement + HF, 
                 data = data_mod1_p1_p, family = "binomial")
summary(mod1_p1_p)

mod_final_p1_p <- stepwise(mod1_p1_p, direction = "backward/forward", criterion = "AIC")


#### p2 gagnant
data_mod1_p2_p <- data_mod1_p2 %>%
  filter(statut_p2 == -1)

mod1_p2_p <- glm(effet_positif ~ TM_derniere_min + TM_derniere_min_adverse + DM_adverse + DM + ecart_classement + HF, 
                 data = data_mod1_p2_p, family = "binomial")
summary(mod1_p2_p)

mod_final_p2_p <- stepwise(mod1_p2_p, direction = "backward/forward", criterion = "AIC")


#### p3 gagnant
data_mod1_p3_p <- data_mod1_p3 %>%
  filter(statut_p3 == -1)

mod1_p3_p <- glm(effet_positif ~ TM_derniere_min + TM_derniere_min_adverse + DM_adverse + DM + ecart_classement + HF, 
                 data = data_mod1_p3_p, family = "binomial")
#summary(mod1_p2)

mod_final_p3_p <- stepwise(mod1_p3_p, direction = "backward/forward", criterion = "AIC")




pdf("Documents/modele_59_periode_perdant.pdf", width = 12, height = 9)
ggcoef_compare(
  list(
    "Modèle P1" = mod_final_p1_p,
    "Modèle P2" = mod_final_p2_p,
    "Modèle P3" = mod_final_p3_p
  ),
  type = "faceted"
) +
  coord_cartesian(xlim = c(-1.5,1.5)) +  # Limiter l'axe des abscisses
  scale_x_continuous(
    breaks = seq(-1.5,1.5, by = 1),     # Ajouter des ticks tous les 1
    minor_breaks = seq(-1.5,1.5, by = 0.5)  # Sous-ticks tous les 0.5
  )  +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "longdash"),
    panel.grid.minor.x = element_line(color = "grey90", linetype = "longdash") # Ajouter les lignes pointillées pour les sous-ticks
  ) +
  ggtitle("Modèle par période de 20 sec durant la dernière minute avec statut perdant au debut de la période")
dev.off()