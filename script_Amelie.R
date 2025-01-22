library(readr) 
library(dplyr)
library(tidyverse)

data <- read.csv("data_final/df_pdfs_final.csv", header = TRUE, sep = ",", stringsAsFactors = T)

data <- data %>%
  separate(score_final, into = c("score_final_r", "score_final_v"), sep = "-") %>%
  mutate(
    score_final_r = as.integer(score_final_r),
    score_final_v = as.integer(score_final_v)
  )

# p1 = 59:00 - 59:20
# p2 = 59:20 - 59:40
# p3 = 59:40 - 60:00


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
      grepl("D1F", fichier, ignore.case = TRUE) | grepl("D2F", fichier, ignore.case = TRUE) ~ "F",
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

diff_norme <- read.csv("data_final/data_diff_norme_final.csv", row.names = 1)
diff_norme$saison <- factor(diff_norme$saison)

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
