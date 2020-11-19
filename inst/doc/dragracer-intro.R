## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dragracer)
library(tibble)
library(dplyr)
library(tidyr)

## -----------------------------------------------------------------------------
rpdr_contestants %>%
  group_by(season) %>%
  summarize(mean_age = mean(age))

## -----------------------------------------------------------------------------
rpdr_ep %>%
  group_by(lipsyncartist) %>%
  summarize(n = n()) %>% 
  na.omit %>%
  arrange(-n) %>% head(10)

## -----------------------------------------------------------------------------
rpdr_contep %>%
  filter(contestant == "Jinkx Monsoon") %>%
  select(season, contestant, episode, outcome, finale)

## -----------------------------------------------------------------------------

rpdr_ep %>%
  select(season, minicw1:minicw3) %>%
  group_by(season) %>%
  gather(Category, contestant, minicw1:minicw3) %>%
  na.omit %>%
  group_by(season, contestant) %>%
  summarize(minicwins = n()) %>%
  left_join(rpdr_contestants, .) %>%
  mutate(minicwins = ifelse(is.na(minicwins), 0, minicwins)) -> D


## -----------------------------------------------------------------------------
rpdr_contep %>%
  filter(participant == 1 & finale == 0 & penultimate == 0) %>%
  mutate(high = ifelse(outcome == "HIGH", 1, 0),
         win = ifelse(outcome == "WIN", 1, 0),
         low = ifelse(outcome == "LOW", 1, 0),
         safe = ifelse(outcome == "SAFE", 1, 0),
         highsafe = ifelse(outcome %in% c("HIGH", "SAFE"), 1, 0),
         winhigh = ifelse(outcome %in% c("HIGH", "WIN"), 1, 0),
         btm = ifelse(outcome == "BTM", 1, 0),
         lowbtm = ifelse(outcome %in% c("BTM", "LOW"), 1, 0)) %>%
  group_by(season,contestant,rank) %>%
  mutate(numcontests = n()) %>%
  group_by(season,contestant, numcontests, rank) %>%
  summarize(perc_high = sum(high)/unique(numcontests),
            perc_win = sum(win)/unique(numcontests),
            perc_winhigh = sum(winhigh)/unique(numcontests),
            perc_low = sum(low)/unique(numcontests),
            perc_btm = sum(btm)/unique(numcontests),
            perc_lowbtm = sum(lowbtm)/unique(numcontests),
            num_high = sum(high),
            num_win = sum(win),
            num_winhigh = sum(winhigh),
            num_btm = sum(btm),
            num_low = sum(low),
            num_lowbtm = sum(lowbtm),
            db_score = 2*sum(win, na.rm=T) +
              1*sum(high, na.rm=T) +
              (sum(safe, na.rm=T)*0) +
              (sum(low, na.rm=T)*-1) + (sum(btm, na.rm=T)*-2)) %>%
  ungroup() %>%
  mutate(points = (2*num_win + num_high - num_low + (-2)*num_btm),
            ppe = points/numcontests) %>%
  full_join(D, .) -> D


## -----------------------------------------------------------------------------
D %>%
  arrange(-db_score) %>%
  head(10) %>%
  select(season, contestant, rank, db_score)

## -----------------------------------------------------------------------------
D %>%
  arrange(-ppe) %>%
  head(10) %>%
  select(season, contestant, rank, ppe)

