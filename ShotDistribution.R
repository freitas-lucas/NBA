library(ballr)
library(tidyverse)
library(cowplot)

# Main table with all players data (per 100 poss) from seasons 1980-2018.                                                            
per_100 <- NBAPerGameStatisticsPer100Poss(season = 1980) # Get first table as main table.
per_100 <- per_100 %>% mutate(year = 1980, mpg = mp/g) # Create year and minutes per game columns

# Loop to repeat the same process from season 1980 to all other seasons, adding the year table to the main table.
for (i in 1981:2019){
  table <- NBAPerGameStatisticsPer100Poss(season = i)
  table <- table %>% mutate(year = i, mpg = mp/g)
  per_100 <- full_join(per_100, table)
}                                                                                                                                       

# Keep only TOT when player played for two or more teams in a same season
per_100_noDup <- per_100 %>% arrange(player, year, desc(tm == "TOT")) %>% 
  distinct(player, year, .keep_all = TRUE)

# Select only players that played more than 20 minutes per game, >35 games in a season and 3pt% < 0.55
table20mpg <- per_100_noDup %>% filter(mpg >= 20 & g >= 35 & x3ppercent < 0.55)

# Rename double positions to only one
table20mpg <- table20mpg %>% 
  mutate(pos = recode(pos, "C-PF" = "C", "PF-C" = "PF", "PF-SF" = "PF",
                      "SF-PF" = "SF", "SF-SG" = "SF", "SG-PF" = "SG",
                      "SG-SF" = "SG", "SG-PG" = "SG", "PG-SF" = "PG", "PG-SG" = "PG"))


# Per position t-test
# t-values, p-values, degrees of freedom, confidence intervals, cohen's d and sample estimates)
# Empty tibble
t_test_position <- tibble(Season = character(), Position = character(), Shot_stats = character(),
                          MeanYear1 = double(), MeanYear2 = double(), MeanDifference = double(), 
                          conf_interval = character(), t_value = double(), df = double(),
                          cohen_d = double(), p_value = double(), FDR = double())

# Loop to fill table with statistics comparing consecutive seasons
for (stat_type in c("x3ppercent", "x2ppercent", "x3pa", "x2pa")) {
  for (position in c("PG", "SG", "SF", "PF", "C")) {   
    year1 <- table20mpg %>% filter(year == 1980, pos == position) %>% select(stat_type)
    season_year <- c(1980) # Start vector with year 1.
    for (i in 1981:2019) {
      year2 <- table20mpg %>% filter(year == i, pos == position) %>% select(stat_type)
      full_season <- paste(season_year, i, sep = "-")
      t_test <- t.test(year1, year2)
      t_test_position <- t_test_position %>% 
        add_row(Season = full_season, Position = position, Shot_stats = stat_type, 
                MeanYear1 = t_test$estimate[1], MeanYear2 = t_test$estimate[2], 
                MeanDifference = t_test$estimate[1] - t_test$estimate[2], 
                conf_interval = paste(round(t_test$conf.int[1], 4), round(t_test$conf.int[2], 4), sep = ", "),
                t_value = t_test$statistic, df = t_test$parameter, 
                cohen_d = cohensD(year1[[1]], year2[[1]]), p_value = t_test$p.value)
      year1 <- table20mpg %>% filter(year == i, pos == position) %>% select(stat_type) # Set year 1 again.
      season_year <- c(i) # Start vector again.
    }
  }
}

# Add FDR
p_valueBH <- p.adjust(t_test_position$p_value, method = "BH")
t_test_position <- t_test_position %>% mutate(FDR = p_valueBH) %>% arrange(FDR)

# Overall t-test
# t-values, p-values, degrees of freedom, confidence intervals, cohen's d and sample estimates)
# Empty tibble
t_test_overall <- tibble(Season = character(), Shot_stats = character(), MeanYear1 = double(), 
                 MeanYear2 = double(), MeanDifference = double(), conf_interval = character(),
                 t_value = double(), df = double(), cohen_d = double(),
                 p_value = double(), FDR = double())

# Loop to fill table with statistics comparing consecutive seasons
for (stat_type in c("x3ppercent", "x2ppercent", "x3pa", "x2pa")) {
    year1 <- table20mpg %>% filter(year == 1980) %>% select(stat_type)
    season_year <- c(1980) # Start vector with year 1.
    for (i in 1981:2019) {
      year2 <- table20mpg %>% filter(year == i) %>% select(stat_type)
      full_season <- paste(season_year, i, sep = "-")
      t_test <- t.test(year1, year2)
      t_test_overall <- t_test_overall %>% 
        add_row(Season = full_season, Shot_stats = stat_type, 
                MeanYear1 = t_test$estimate[1], MeanYear2 = t_test$estimate[2], 
                MeanDifference = t_test$estimate[1] - t_test$estimate[2],
                conf_interval = paste(round(t_test$conf.int[1], 4), round(t_test$conf.int[2], 4), sep = ", "),
                t_value = t_test$statistic, df = t_test$parameter, 
                cohen_d = cohensD(year1[[1]], year2[[1]]), p_value = t_test$p.value)
      year1 <- table20mpg %>% filter(year == i) %>% select(stat_type) # Set year 1 again.
      season_year <- c(i) # Start vector again.
    }
  }

# Add FDR
p_valueBH <- round(p.adjust(t_test_overall$p_value, method = "BH"), 4)
t_test_overall <- t_test_overall %>% mutate(FDR = p_valueBH) %>% arrange(FDR)

# Figure 1
fig1a <- table20mpg %>% select("player", "year", "x3pa", "x2pa") %>%
  pivot_longer(cols = c("x3pa", "x2pa"), names_to = "shot_type") %>% dplyr::rename("attempts" = "value") %>%
  mutate(year = paste0(year-1, "-", year)) %>% 
  ggplot(aes(x = as.character(year), y = attempts, fill = as.character(shot_type))) +
  geom_boxplot(notch = TRUE, outlier.shape = NA, na.rm = TRUE) +
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2pa", "x3pa"), labels = c("2-pt shot", "3-pt shot")) +
  labs(x = "", y = "Shots attempts") + theme_bw() +
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") + # Add lines in the years 3-pt range changed.
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.text.x = element_blank())

fig1b <- table20mpg %>% select("player", "year", "x3ppercent", "x2ppercent") %>%
  pivot_longer(cols = c("x3ppercent", "x2ppercent"), names_to = "shot_type") %>% 
  dplyr::rename("percentage" = "value") %>% mutate(year = paste0(year-1, "-", year)) %>% 
  ggplot(aes(x = as.character(year), y = percentage, fill = as.character(shot_type))) +
  geom_boxplot(notch = TRUE, outlier.shape = NA, na.rm = TRUE) +
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2ppercent", "x3ppercent"), labels = c("2-pt shot", "3-pt shot")) +
  labs(x = "Season", y = "Shot percentage") + theme_bw() +
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") + # Add lines in the years 3-pt range changed.
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.text.x = element_text(angle = 90)) + 
  ylim(c(0, 0.7))

fig_legend <- get_legend(
  fig1a + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "top")
)

fig1 <- plot_grid(
  fig1a + theme(legend.position = "none"),
  fig1b + theme(legend.position = "none"),
  align = "v",
  labels = c("A", "B"),
  nrow = 2
)

plot_grid(fig1, fig_legend, ncol = 1, rel_heights = c(1, .1)) +
  ggsave("Fig1.pdf", height = 8, width = 12)
