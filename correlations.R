library(Lahman)
library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)

##Relationship between at bats per game and runs per game
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  summarise(cor(AB_per_game,R_per_game))

##Relationship between win rate per game and fielding errors per game
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(E_per_game = E/G, W_per_game = W/G) %>%
  ggplot(aes(E_per_game, W_per_game)) + 
  geom_point(alpha = 0.5)
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(E_per_game = E/G, W_per_game = W/G) %>%
  summarise(cor(E_per_game,W_per_game))

##Relationship between triples per game vs doubles per gameTeams %>% filter(yearID %in% 1961:2001 ) %>%
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G) %>%
  ggplot(aes(X2B_per_game, X3B_per_game)) + 
  geom_point(alpha = 0.5)
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G) %>%
  summarise(cor(X3B_per_game,X2B_per_game))

#Least Square Estimate and Linear Model
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(R_per_game = R/G,HR_per_game = HR/G,BB_per_game = BB/G) %>%
  lm(R_per_game ~ BB_per_game + HR_per_game,data = .) %>% .$coef