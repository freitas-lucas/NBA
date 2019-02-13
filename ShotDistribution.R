library(ballr)
library(dplyr)
library(ggplot2)
library(ggpubr)

## Rule changes: http://www.nba.com/analysis/rules_history.html
##########################################################################################################################################
## Master table with all players data (per 100 poss) from seasons 1980-2018.                                                            ##
##########################################################################################################################################
per_100 <- NBAPerGameStatisticsPer100Poss(season = 1980) # Get first table as main table.                                               ##
per_100$year <- 1980 # New column: year.                                                                                                ##
per_100$mpg <- per_100$mp/per_100$g # New column: minutes played per game.                                                              ##
for (i in 1981:2018){ # Loop to repeat the same process from season 1980 to all other seasons, adding the year table to the main table. ##
  table <- NBAPerGameStatisticsPer100Poss(season = i)                                                                                   ##
  table$year <- i                                                                                                                       ##
  table$mpg <- table$mp/table$g                                                                                                         ##
  per_100 <- full_join(per_100, table)                                                                                                  ##
}                                                                                                                                       ##
##########################################################################################################################################

####################################################################################################
## Select only players that played more than 20 minutes per game and 35 games in a season.        ##
####################################################################################################
table20mpg <- per_100[which(per_100$mpg >= 20 & per_100$g >= 35), ]                               ##
write.table(table20mpg, "/path/to/NBA20mpg.tab", sep = "\t") # Save to Python script.             ##
####################################################################################################

#############################################################################################
## Python script to select only the "TOT" rows when players played for more than one team  ##
## in the same year. Then load table, change positions and create tables by position.      ##
#############################################################################################
table20mpg <- read.table(file = "/path/to/NBA20mpg_OK.tab", header = TRUE)                 ##
#table(table20mpg$pos) # To find all positions I need to change                            ##
table20mpg$pos[table20mpg$pos == "C-PF"] <- "C"                                            ##
table20mpg$pos[table20mpg$pos == "PF-C"] <- "PF"                                           ##
table20mpg$pos[table20mpg$pos == "PF-SF"] <- "PF"                                          ##
table20mpg$pos[table20mpg$pos == "SF-PF"] <- "SF"                                          ##
table20mpg$pos[table20mpg$pos == "SF-SG"] <- "SF"                                          ##
table20mpg$pos[table20mpg$pos == "SG-PF"] <- "SG"                                          ##
table20mpg$pos[table20mpg$pos == "SG-SF"] <- "SG"                                          ##
table20mpg$pos[table20mpg$pos == "SG-PG"] <- "SG"                                          ##
table20mpg$pos[table20mpg$pos == "PG-SF"] <- "PG"                                          ##
table20mpg$pos[table20mpg$pos == "PG-SG"] <- "PG"                                          ##
#############################################################################################

#############################################################################################
# Overall and positions T-Tests                                                            ##
#############################################################################################

##############################################################################################################################################
### Per position test                                                                                                                       ##
##############################################################################################################################################
t_test_position <- data.frame(matrix(ncol = 4, nrow = 0)) # Start empty p_value data frame.                                                 ##
for (stat_type in c("x3ppercent", "x2ppercent", "x3pa", "x2pa")) {                                                                          ##
  for (position in c("PG", "SG", "SF", "PF", "C")) {                                                                                        ## 
    year1 <- table20mpg[[stat_type]][which(table20mpg$year == 1980 & table20mpg$pos == position)] # Set year 1.                             ##                                                                                           ##
    lista_t <- c(1980) # Start vector with year 1.                                                                                          ##                                                                                  
    for (i in 1981:2018) {                                                                                                                  ##
      year2 <- table20mpg[[stat_type]][which(table20mpg$year == i & table20mpg$pos == position)] # Set year 2.                              ##
      lista_t <- c(paste(lista_t, i, sep = "-"), position)                                                                                  ##
      t_test <- as.numeric(t.test(year1, year2)$p.value)                                                                                    ##
      lista_t <- c(lista_t, stat_type, t_test) # Fill vector with seasons comparison, stat type, which set and the p_value for normal test. ##
      t_test_position <- rbind(t_test_position, lista_t, stringsAsFactors = FALSE) # Add vector do data frame.                              ##
      year1 <- table20mpg[[stat_type]][which(table20mpg$year == i & table20mpg$pos == position)] # Set year 1 again.                        ##                                                                    ##
      lista_t <- c(i) # Start vector again.                                                                                                 ##
    }                                                                                                                                       ##
  }                                                                                                                                         ##
}                                                                                                                                           ##
##############################################################################################################################################

###############################################################################
## Change p_values data frame columns names.                                 ##
colnames(t_test_position) <- c("Season", "Dataset", "Stat Type", "p_value")  ##
###############################################################################

#################################################################################################
## Add Benjamini-Hochberg False Discovery Rate (10% or p_valeBH =< 0.1) to p_value data frame. ##
p_valueBH <- p.adjust(t_test_position$p_value, method = "BH")                                  ##
t_test_position <- cbind(t_test_position, FDR_BH = p_valueBH)                                  ##
#################################################################################################


########################################################################################################################################################################
### Overall test                                                                                                                                                      ##
########################################################################################################################################################################
                                                                                                                                                                      ##
t_test_overall <- data.frame(matrix(ncol = 4, nrow = 0)) # Start empty p_value data frame.                                                                            ##
for (stat_type in c("x3ppercent", "x2ppercent", "x3pa", "x2pa")) { # Start loop to get p_value for these stats.                                                       ##
  year1 <- table20mpg[[stat_type]][which(table20mpg$year == 1980)] # Set year 1.                                                                                      ##
  lista_t <- c(1980) # Start vector with year 1.                                                                                                                      ##                                                                                                              ##
  for (i in 1981:2018) { # Loop trough years.                                                                                                                         ##
    year2 <- table20mpg[[stat_type]][which(table20mpg$year == i)] # Set year 2.                                                                                       ##
    t_test <- as.numeric(t.test(year1, year2)$p.value) # T test.                                                                                                      ##
    lista_t <- c(paste(lista_t, i, sep = "-"), "Overall", stat_type, t_test) # Fill vector with seasons comparison, stat type, which set and the p_value for KW test. ##
    t_test_overall <- rbind(t_test_overall, lista_t, stringsAsFactors = FALSE) # Add vector do data frame.                                                            ##
    year1 <- table20mpg[[stat_type]][which(table20mpg$year == i)] # Set year 1 again.                                                                                 ##
    lista_t <- c(i) # Start vector again.                                                                                                                             ##
  }                                                                                                                                                                   ##
}                                                                                                                                                                     ##
########################################################################################################################################################################

#############################################################################
## Change p_values data frame columns names.                               ##
colnames(t_test_overall) <- c("Season", "Dataset", "Stat Type", "p_value") ##
#############################################################################

#################################################################################################
## Add Benjamini-Hochberg False Discovery Rate (10% or p_valeBH =< 0.1) to p_value data frame. ##
p_valueBH <- p.adjust(t_test_overall$p_value, method = "BH")                                   ##
t_test_overall <- cbind(t_test_overall, FDR_BH = p_valueBH)                                    ##
#################################################################################################



#######################################################################################################################################
## Start graphics.                                                                                                                   ##
#######################################################################################################################################
## Overall shot attempts distribution. Will not include position                                                                     ##
#######################################################################################################################################
table20mpg_shots_a <- table20mpg[c("player", "year", "x3pa", "x2pa")] # Select these columns.                                        ##
shots_a_mel <- as.data.frame(melt(table20mpg_shots_a, id = c("player", "year"))) # Melt table to perform boxplot.                    ##
colnames(shots_a_mel) <- c("player", "year", "shot_type", "attempts") # Change column names.                                         ##
plot_attempt <- ggplot(shots_a_mel, aes(x = as.character(year), y = attempts, fill = as.character(shot_type))) +                     ##
  geom_boxplot(notch = TRUE, outlier.shape = NA, na.rm = TRUE) +                                                                     ##
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2pa", "x3pa"), labels = c("2-pt shot", "3-pt shot")) + ##
  labs(x = "Year", y = "Shots attempts") + #ylim(0,30) +                                                                             ##
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") # Add lines in the years 3-pt range changed.       ##
#######################################################################################################################################

###################################################################################################################################################
## Overall shot percentage distribution. Will not include position.                                                                              ##
###################################################################################################################################################
table20mpg_shots_p <- table20mpg[c("player", "year", "x3ppercent", "x2ppercent")] # Select these columns.                                        ##
shots_p_mel <- as.data.frame(melt(table20mpg_shots_p, id = c("player", "year"))) # Melt table to perform boxplot.                                ##
colnames(shots_p_mel) <- c("player", "year", "shot_type", "percentage") # Change column names.                                                   ##
plot_percentage <- ggplot(shots_p_mel, aes(x = as.character(year), y = percentage, fill = as.character(shot_type))) +                            ##
  geom_boxplot(notch = TRUE, outlier.shape = NA, na.rm = TRUE) +                                                                                 ##
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2ppercent", "x3ppercent"), labels = c("2-pt shot", "3-pt shot")) + ##
  labs(x = "Year", y = "Shot percentage") + ylim(0, 0.75) +                                                                                      ##
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") # Add lines in the years 3-pt range changed                    ##
###################################################################################################################################################

#######################################################################################################################################
## Overall shot attempts distribution for PG.                                                                                        ##
#######################################################################################################################################
table20mpg_shots_a <- table20mpg_PG[c("player", "year", "x3pa", "x2pa")] # Select these columns.                                     ##
shots_a_mel <- as.data.frame(melt(table20mpg_shots_a, id = c("player", "year"))) # Melt table to perform boxplot.                    ##
colnames(shots_a_mel) <- c("player", "year", "shot_type", "attempts") # Change column names.                                         ##
plot_attempt_PG <- ggplot(shots_a_mel, aes(x = as.character(year), y = attempts, fill = as.character(shot_type))) +                  ##
  geom_boxplot(outlier.shape = NA, na.rm = TRUE) +                                                                                   ##
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2pa", "x3pa"), labels = c("2-pt shot", "3-pt shot")) + ##
  labs(x = "Year", y = "Shots attempts") + #ylim(0,30) +                                                                             ##
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") # Add lines in the years 3-pt range changed.       ##
#######################################################################################################################################

###################################################################################################################################################
## Overall shot percentage distribution for PG.                                                                                                  ##
###################################################################################################################################################
table20mpg_shots_p <- table20mpg_PG[c("player", "year", "x3ppercent", "x2ppercent")] # Select these columns.                                     ##
shots_p_mel <- as.data.frame(melt(table20mpg_shots_p, id = c("player", "year"))) # Melt table to perform boxplot.                                ##
colnames(shots_p_mel) <- c("player", "year", "shot_type", "percentage") # Change column names.                                                   ##
plot_percentage_PG <- ggplot(shots_p_mel, aes(x = as.character(year), y = percentage, fill = as.character(shot_type))) +                         ##
  geom_boxplot(outlier.shape = NA, na.rm = TRUE) +                                                                                               ##
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2ppercent", "x3ppercent"), labels = c("2-pt shot", "3-pt shot")) + ##
  labs(x = "Year", y = "Shot percentage") + ylim(0, 0.75) +                                                                                      ##
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") # Add lines in the years 3-pt range changed                    ##
###################################################################################################################################################

#######################################################################################################################################
## Overall shot attempts distribution for SG.                                                                                        ##
#######################################################################################################################################
table20mpg_shots_a <- table20mpg_SG[c("player", "year", "x3pa", "x2pa")] # Select these columns.                                     ##
shots_a_mel <- as.data.frame(melt(table20mpg_shots_a, id = c("player", "year"))) # Melt table to perform boxplot.                    ##
colnames(shots_a_mel) <- c("player", "year", "shot_type", "attempts") # Change column names.                                         ##
plot_attempt_SG <- ggplot(shots_a_mel, aes(x = as.character(year), y = attempts, fill = as.character(shot_type))) +                  ##
  geom_boxplot(outlier.shape = NA, na.rm = TRUE) +                                                                                   ##
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2pa", "x3pa"), labels = c("2-pt shot", "3-pt shot")) + ##
  labs(x = "Year", y = "Shots attempts") + #ylim(0,30) +                                                                             ##
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") # Add lines in the years 3-pt range changed.       ##
#######################################################################################################################################

###################################################################################################################################################
## Overall shot percentage distribution for SG.                                                                                                  ##
###################################################################################################################################################
table20mpg_shots_p <- table20mpg_SG[c("player", "year", "x3ppercent", "x2ppercent")] # Select these columns.                                     ##
shots_p_mel <- as.data.frame(melt(table20mpg_shots_p, id = c("player", "year"))) # Melt table to perform boxplot.                                ##
colnames(shots_p_mel) <- c("player", "year", "shot_type", "percentage") # Change column names.                                                   ##
plot_percentage_SG <- ggplot(shots_p_mel, aes(x = as.character(year), y = percentage, fill = as.character(shot_type))) +                         ##
  geom_boxplot(outlier.shape = NA, na.rm = TRUE) +                                                                                               ##
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2ppercent", "x3ppercent"), labels = c("2-pt shot", "3-pt shot")) + ##
  labs(x = "Year", y = "Shot percentage") + ylim(0, 0.75) +                                                                                      ##
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") # Add lines in the years 3-pt range changed                    ##
###################################################################################################################################################

#######################################################################################################################################
## Overall shot attempts distribution for SF.                                                                                        ##
#######################################################################################################################################
table20mpg_shots_a <- table20mpg_SF[c("player", "year", "x3pa", "x2pa")] # Select these columns.                                     ##
shots_a_mel <- as.data.frame(melt(table20mpg_shots_a, id = c("player", "year"))) # Melt table to perform boxplot.                    ##
colnames(shots_a_mel) <- c("player", "year", "shot_type", "attempts") # Change column names.                                         ##
plot_attempt_SF <- ggplot(shots_a_mel, aes(x = as.character(year), y = attempts, fill = as.character(shot_type))) +                  ##
  geom_boxplot(outlier.shape = NA, na.rm = TRUE) +                                                                                   ##
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2pa", "x3pa"), labels = c("2-pt shot", "3-pt shot")) + ##
  labs(x = "Year", y = "Shots attempts") + #ylim(0,30) +                                                                             ##
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") # Add lines in the years 3-pt range changed.       ##
#######################################################################################################################################

###################################################################################################################################################
## Overall shot percentage distribution for SF.                                                                                                  ##
###################################################################################################################################################
table20mpg_shots_p <- table20mpg_SF[c("player", "year", "x3ppercent", "x2ppercent")] # Select these columns.                                     ##
shots_p_mel <- as.data.frame(melt(table20mpg_shots_p, id = c("player", "year"))) # Melt table to perform boxplot.                                ##
colnames(shots_p_mel) <- c("player", "year", "shot_type", "percentage") # Change column names.                                                   ##
plot_percentage_SF <- ggplot(shots_p_mel, aes(x = as.character(year), y = percentage, fill = as.character(shot_type))) +                         ##
  geom_boxplot(outlier.shape = NA, na.rm = TRUE) +                                                                                               ##
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2ppercent", "x3ppercent"), labels = c("2-pt shot", "3-pt shot")) + ##
  labs(x = "Year", y = "Shot percentage") + ylim(0, 0.75) +                                                                                      ##
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") # Add lines in the years 3-pt range changed                    ##
###################################################################################################################################################

#######################################################################################################################################
## Overall shot attempts distribution for PF.                                                                                        ##
#######################################################################################################################################
table20mpg_shots_a <- table20mpg_PF[c("player", "year", "x3pa", "x2pa")] # Select these columns.                                     ##
shots_a_mel <- as.data.frame(melt(table20mpg_shots_a, id = c("player", "year"))) # Melt table to perform boxplot.                    ##
colnames(shots_a_mel) <- c("player", "year", "shot_type", "attempts") # Change column names.                                         ##
plot_attempt_PF <- ggplot(shots_a_mel, aes(x = as.character(year), y = attempts, fill = as.character(shot_type))) +                  ##
  geom_boxplot(outlier.shape = NA, na.rm = TRUE) +                                                                                   ##
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2pa", "x3pa"), labels = c("2-pt shot", "3-pt shot")) + ##
  labs(x = "Year", y = "Shots attempts") + #ylim(0,30) +                                                                             ##
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") # Add lines in the years 3-pt range changed.       ##
#######################################################################################################################################

###################################################################################################################################################
## Overall shot percentage distribution for PF.                                                                                                  ##
###################################################################################################################################################
table20mpg_shots_p <- table20mpg_PF[c("player", "year", "x3ppercent", "x2ppercent")] # Select these columns.                                     ##
shots_p_mel <- as.data.frame(melt(table20mpg_shots_p, id = c("player", "year"))) # Melt table to perform boxplot.                                ##
colnames(shots_p_mel) <- c("player", "year", "shot_type", "percentage") # Change column names.                                                   ##
plot_percentage_PF <- ggplot(shots_p_mel, aes(x = as.character(year), y = percentage, fill = as.character(shot_type))) +                         ##
  geom_boxplot(outlier.shape = NA, na.rm = TRUE) +                                                                                               ##
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2ppercent", "x3ppercent"), labels = c("2-pt shot", "3-pt shot")) + ##
  labs(x = "Year", y = "Shot percentage") + ylim(0, 0.75) +                                                                                      ##
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") # Add lines in the years 3-pt range changed                    ##
###################################################################################################################################################

#######################################################################################################################################
## Overall shot attempts distribution for C.                                                                                         ##
#######################################################################################################################################
table20mpg_shots_a <- table20mpg_C[c("player", "year", "x3pa", "x2pa")] # Select these columns.                                      ##
shots_a_mel <- as.data.frame(melt(table20mpg_shots_a, id = c("player", "year"))) # Melt table to perform boxplot.                    ##
colnames(shots_a_mel) <- c("player", "year", "shot_type", "attempts") # Change column names.                                         ##
plot_attempt_C <- ggplot(shots_a_mel, aes(x = as.character(year), y = attempts, fill = as.character(shot_type))) +                   ##
  geom_boxplot(outlier.shape = NA, na.rm = TRUE) +                                                                                   ##
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2pa", "x3pa"), labels = c("2-pt shot", "3-pt shot")) + ##
  labs(x = "Year", y = "Shots attempts") + #ylim(0,30) +                                                                             ##
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") # Add lines in the years 3-pt range changed.       ##
#######################################################################################################################################

###################################################################################################################################################
## Overall shot percentage distribution for C.                                                                                                   ##
###################################################################################################################################################
table20mpg_shots_p <- table20mpg_C[c("player", "year", "x3ppercent", "x2ppercent")] # Select these columns.                                      ##
shots_p_mel <- as.data.frame(melt(table20mpg_shots_p, id = c("player", "year"))) # Melt table to perform boxplot.                                ##
colnames(shots_p_mel) <- c("player", "year", "shot_type", "percentage") # Change column names.                                                   ##
plot_percentage_C <- ggplot(shots_p_mel, aes(x = as.character(year), y = percentage, fill = as.character(shot_type))) +                          ##
  geom_boxplot(outlier.shape = NA, na.rm = TRUE) +                                                                                               ##
  scale_fill_manual(values = c("#BFBFBF", "#FFFFFF"), name = "", breaks = c("x2ppercent", "x3ppercent"), labels = c("2-pt shot", "3-pt shot")) + ##
  labs(x = "Year", y = "Shot percentage") + ylim(0, 0.75) +                                                                                      ##
  geom_vline (xintercept = c(15.5, 18.5), linetype = "longdash", color = "black") # Add lines in the years 3-pt range changed                    ##
###################################################################################################################################################


####################################################################################################################
## PNGs to journal.                                                                                               ##
####################################################################################################################
## Overall.                                                                                                       ##
####################################################################################################################
ggarrange(plot_attempt, plot_percentage, common.legend = TRUE, legend = "top",                                    ##
          labels = c("A", "B"),                                                                                   ##
          ncol = 1, nrow = 2) %>%                                                                                 ##
  ggexport(filename = "/path/to/Overall.png", ##
           width = 4000, height = 2000, pointsize = 100, res = 300)                                               ##
####################################################################################################################

####################################################################################################################
## PG.                                                                                                            ##
####################################################################################################################
ggarrange(plot_attempt_PG, plot_percentage_PG, common.legend = TRUE, legend = "top",                              ##
          labels = c("A", "B"),                                                                                   ##
          ncol = 1, nrow = 2) %>%                                                                                 ##
  ggexport(filename = "/path/to/PG.png",      ##
           width = 4000, height = 2000, pointsize = 100, res = 300)                                               ##
####################################################################################################################

####################################################################################################################
## SG.                                                                                                            ##
####################################################################################################################
ggarrange(plot_attempt_SG, plot_percentage_SG, common.legend = TRUE, legend = "top",                              ##
          labels = c("A", "B"),                                                                                   ##
          ncol = 1, nrow = 2) %>%                                                                                 ##
  ggexport(filename = "/path/to/SG.png",      ##
           width = 4000, height = 2000, pointsize = 100, res = 300)                                               ##
####################################################################################################################

####################################################################################################################
## SF.                                                                                                            ##
####################################################################################################################
ggarrange(plot_attempt_SF, plot_percentage_SF, common.legend = TRUE, legend = "top",                              ##
          labels = c("A", "B"),                                                                                   ##
          ncol = 1, nrow = 2) %>%                                                                                 ##
  ggexport(filename = "/path/to/SF.png",      ##
           width = 4000, height = 2000, pointsize = 100, res = 300)                                               ##
####################################################################################################################

####################################################################################################################
## PF.                                                                                                            ##
####################################################################################################################
ggarrange(plot_attempt_PF, plot_percentage_PF, common.legend = TRUE, legend = "top",                              ##
          labels = c("A", "B"),                                                                                   ##
          ncol = 1, nrow = 2) %>%                                                                                 ##
  ggexport(filename = "/path/to/PF.png",      ##
           width = 4000, height = 2000, pointsize = 100, res = 300)                                               ##
####################################################################################################################

####################################################################################################################
## C.                                                                                                             ##
####################################################################################################################
ggarrange(plot_attempt_C, plot_percentage_C, common.legend = TRUE, legend = "top",                                ##
          labels = c("A", "B"),                                                                                   ##
          ncol = 1, nrow = 2) %>%                                                                                 ##
  ggexport(filename = "/path/to/C.png",       ##
           width = 4000, height = 2000, pointsize = 100, res = 300)                                               ##
####################################################################################################################

