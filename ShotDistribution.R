library(ballr)
library(reshape2)
library(ggpubr)
library(tidyverse)
library(cowplot)

## Rule changes: http://www.nba.com/analysis/rules_history.html
## Master table
per_100 <- NBAPerGameStatisticsPer100Poss(season = 1980) # Get first table as main table.
per_100 <- per_100 %>% mutate(year = 1980, mpg = mp/g)

for (i in 1981:2019){ # Loop to repeat the same process from season 1980 to all other seasons, adding the year table to the main table.
  table <- NBAPerGameStatisticsPer100Poss(season = i)
  table <- table %>% mutate(year = i, mpg = mp/g)
  per_100 <- full_join(per_100, table)
}

## Keep only TOT when player played for two or more teams in a same season
per_100_noDup <- per_100 %>% arrange(player, year, desc(tm == "TOT")) %>% 
  distinct(player, year, .keep_all = TRUE)

## Select only players that played more than 20 minutes per game, 35 games in a season and only totals if player played for two or more teams in a season.
table20mpg <- per_100_noDup %>% filter(mpg >= 20 & g >= 35)

## Change combo positions to primary position
table20mpg$pos[table20mpg$pos == "C-PF"] <- "C"
table20mpg$pos[table20mpg$pos == "PF-C"] <- "PF"
table20mpg$pos[table20mpg$pos == "PF-SF"] <- "PF"
table20mpg$pos[table20mpg$pos == "SF-PF"] <- "SF"
table20mpg$pos[table20mpg$pos == "SF-SG"] <- "SF"
table20mpg$pos[table20mpg$pos == "SG-PF"] <- "SG"
table20mpg$pos[table20mpg$pos == "SG-SF"] <- "SG"
table20mpg$pos[table20mpg$pos == "SG-PG"] <- "SG"
table20mpg$pos[table20mpg$pos == "PG-SF"] <- "PG"
table20mpg$pos[table20mpg$pos == "PG-SG"] <- "PG"

## Overall and positions T-Tests
## Per position test
t_test_position <- data.frame(matrix(ncol = 4, nrow = 0)) # Start empty p_value data frame.
for (stat_type in c("x3ppercent", "x2ppercent", "x3pa", "x2pa")) {
  for (position in c("PG", "SG", "SF", "PF", "C")) { 
    year1 <- table20mpg[[stat_type]][which(table20mpg$year == 1980 & table20mpg$pos == position)] # Set year 1.
    lista_t <- c(1980) # Start vector with year 1.                                                                                  
    for (i in 1981:2019) {
      year2 <- table20mpg[[stat_type]][which(table20mpg$year == i & table20mpg$pos == position)] # Set year 2.
      lista_t <- c(paste(lista_t, i, sep = "-"), position)
      t_test <- as.numeric(t.test(year1, year2)$p.value)
      lista_t <- c(lista_t, stat_type, t_test) # Fill vector with seasons comparison, stat type, which set and the p_value for normal test.
      t_test_position <- rbind(t_test_position, lista_t, stringsAsFactors = FALSE) # Add vector do data frame.
      year1 <- table20mpg[[stat_type]][which(table20mpg$year == i & table20mpg$pos == position)] # Set year 1 again.
      lista_t <- c(i) # Start vector again.
    }
  }
}

## Change p_values data frame columns names.
colnames(t_test_position) <- c("Season", "Dataset", "Stat Type", "p_value")

## Add Benjamini-Hochberg False Discovery Rate (10% or p_valeBH =< 0.1) to p_value data frame.
p_valueBH <- p.adjust(t_test_position$p_value, method = "BH")
t_test_position <- cbind(t_test_position, FDR_BH = p_valueBH)

## Overall test
t_test_overall <- data.frame(matrix(ncol = 4, nrow = 0)) # Start empty p_value data frame.
for (stat_type in c("x3ppercent", "x2ppercent", "x3pa", "x2pa")) { # Start loop to get p_value for these stats.
  year1 <- table20mpg[[stat_type]][which(table20mpg$year == 1980)] # Set year 1.
  lista_t <- c(1980) # Start vector with year 1.
  for (i in 1981:2019) { # Loop trough years.
    year2 <- table20mpg[[stat_type]][which(table20mpg$year == i)] # Set year 2.
    t_test <- as.numeric(t.test(year1, year2)$p.value) # T test.
    lista_t <- c(paste(lista_t, i, sep = "-"), "Overall", stat_type, t_test) # Fill vector with seasons comparison, stat type, which set and the p_value for KW test.
    t_test_overall <- rbind(t_test_overall, lista_t, stringsAsFactors = FALSE) # Add vector do data frame.
    year1 <- table20mpg[[stat_type]][which(table20mpg$year == i)] # Set year 1 again.
    lista_t <- c(i) # Start vector again.
  }
}

## Change p_values data frame columns names.
colnames(t_test_overall) <- c("Season", "Dataset", "Stat Type", "p_value")

## Add Benjamini-Hochberg False Discovery Rate (10% or p_valeBH =< 0.1) to p_value data frame.
p_valueBH <- p.adjust(t_test_overall$p_value, method = "BH")
t_test_overall <- cbind(t_test_overall, FDR_BH = p_valueBH)


## Start graphics.
## Overall shot attempts distribution.
table20mpg_shots_a <- table20mpg[c("player", "year", "x3pa", "x2pa")] # Select these columns.
shots_a_mel <- as.data.frame(melt(table20mpg_shots_a, id = c("player", "year"))) # Melt table to perform boxplot.
colnames(shots_a_mel) <- c("player", "year", "shot_type", "attempts") # Change column names.
shots_a_mel <- shots_a_mel %>% mutate(year = paste0(year-1, "-", year))

plot_attempt <- ggplot(shots_a_mel, aes(x = as.character(year), y = attempts, fill = as.character(shot_type))) +
  geom_boxplot(notch = TRUE, outlier.shape = NA, na.rm = TRUE) +
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2pa", "x3pa"), labels = c("2-pt shot", "3-pt shot")) +
  labs(x = "", y = "Shots attempts") + theme_bw() +
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") + # Add lines in the years 3-pt range changed.
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.text.x = element_blank())


## Overall shot percentage distribution.
table20mpg_shots_p <- table20mpg[c("player", "year", "x3ppercent", "x2ppercent")] # Select these columns.
shots_p_mel <- as.data.frame(melt(table20mpg_shots_p, id = c("player", "year"))) # Melt table to perform boxplot.
colnames(shots_p_mel) <- c("player", "year", "shot_type", "percentage") # Change column names.
shots_p_mel <- shots_p_mel %>% mutate(year = paste0(year-1, "-", year))

plot_percentage <- ggplot(shots_p_mel, aes(x = as.character(year), y = percentage, fill = as.character(shot_type))) +
  geom_boxplot(notch = TRUE, outlier.shape = NA, na.rm = TRUE) +
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2ppercent", "x3ppercent"), labels = c("2-pt shot", "3-pt shot")) +
  labs(x = "Season", y = "Shot percentage") + theme_bw() +
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") + # Add lines in the years 3-pt range changed.
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.text.x = element_text(angle = 90)) + 
  ylim(c(0, 0.7))

legend <- get_legend(
  plot_attempt + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "top")
)

overall <- plot_grid(
  plot_attempt + theme(legend.position = "none"),
  plot_percentage + theme(legend.position = "none"),
  align = "v",
  labels = c("A", "B"),
  nrow = 2
)
plot_grid(overall, legend, ncol = 1, rel_heights = c(1, .1)) %>%
  ggexport(filename = "/path/to/Fig1.png",
           width = 4500, height = 2000, pointsize = 100, res = 300) 
