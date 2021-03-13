library(Lahman)
library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)

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

#LSE part 2
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
bat_99_01 <- aggregate(bat_99_01[,2:3],list(bat_99_01$playerID), mean)
names(bat_99_01)[1] <- "playerID"
names(bat_99_01)[2] <- "mean_singles"
names(bat_99_01)[3] <- "mean_bb"
  
#Joining tables
bat_99_02 <- inner_join(bat_02,bat_99_01)
bat_99_02 %>% summarise(cor(singles,mean_singles),cor(bb,mean_bb))

#Creating multiple scatter plots on one page
plot_1 <- bat_99_02 %>% ggplot(aes(singles,mean_singles)) + geom_point()
plot_2 <- bat_99_02 %>% ggplot(aes(bb,mean_bb)) + geom_point()
ggarrange(plot_1,plot_2)

#Predicting 2002 results using 1999-2001 data
bat_99_02 %>% lm(singles ~ mean_singles,data = .) %>% .$coef
bat_99_02 %>% lm(bb ~ mean_bb,data = .) %>% .$coef


#Tibble exercise
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

