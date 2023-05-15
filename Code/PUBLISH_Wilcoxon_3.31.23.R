library(tidyverse)
library(rstatix)
library(ggpubr)
library(coin)


ALL_Syll_Loc_Data_XCandML <- read.csv("C:/Users/xxime/Box/Ximena_Nicole/TOWHEE_ANALYSES_2022-2023/1_PUBLISH_MATERIALS/DATA/PUBLISH_All_XCandML_raw_songdata_3.31.23.csv")
##In order to do analysis on breeding season samples only, read in the following file:
#ALL_Syll_Loc_Data_XCandML <- read.csv("C:/Users/xxime/Box/Ximena_Nicole/TOWHEE_ANALYSES_2022-2023/1_PUBLISH_MATERIALS/DATA/PUBLISH_breedingseasonONLY_XCandML_raw_songdata_4.1.23.csv")


################################################################################
ALL_Syll_Loc_Data_XCandML[,c(4:19)] <- log(ALL_Syll_Loc_Data_XCandML[,c(4:19)]) #log transform song data


ALL_Syll_Loc_Data_XCandML_level <- ALL_Syll_Loc_Data_XCandML                                   #duplicate dataframe
ALL_Syll_Loc_Data_XCandML_level$Species <- factor(ALL_Syll_Loc_Data_XCandML_level$Species,     #reorder factor levels
                                c("Pipilo maculatus", "Pipilo erythrophthalmus"))
################################################################################
use_song_data_cols <- c(4:19)


wilcoxon_stat_table = data.frame(matrix(ncol = 2, nrow = 16))
colnames_df_vector <- colnames(ALL_Syll_Loc_Data_XCandML_level[c(4:19)])
data_names <- c("W", "p-val")
rownames(wilcoxon_stat_table) <- colnames_df_vector
colnames(wilcoxon_stat_table) <- data_names

for (ii in 1:16) {
  i = use_song_data_cols[ii]
  stat.test <-  
    wilcox.test(ALL_Syll_Loc_Data_XCandML_level[,i] ~ ALL_Syll_Loc_Data_XCandML_level$Species)
  wilcoxon_stat_table[ii,1] = stat.test$statistic
  wilcoxon_stat_table[ii,2] = stat.test$p.value
}


################################################################################
################################# boxplot ######################################

#stat.test <- ALL_Syll_Loc_Data_XCandML_level %>%                                #same results as in loop but different format bc otherwise can't add the significance to the plot
#  rstatix::wilcox_test(num_syllables ~ Species) %>%                          #replace song feature for each plot
#  add_significance()


#stat.test <- stat.test %>% add_xy_position(x = "Species")

################################################################################

bout_duration_ms_wilcoxon_boxplot <- ggboxplot(
  ALL_Syll_Loc_Data_XCandML_level, x = "Species", y = "bout_duration_ms", 
  ylab = "Log (Bout Duration (ms))", xlab = "Species", add = "jitter", color = "Species", palette =c("#5C6B9C","#960019")) +
  theme_grey() +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Bout Duration") +
  theme_classic()

num_syllables_wilcoxon_boxplot <- ggboxplot(
  ALL_Syll_Loc_Data_XCandML_level, x = "Species", y = "num_syllables", 
  ylab = "Log (# of Sylls.)", xlab = "Species", add = "jitter", color = "Species", palette =c("#5C6B9C","#960019")) +
  theme_grey() +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Number of Syllables") +
  theme_classic()

avg_syllable_duration_ms_wilcoxon_boxplot <- ggboxplot(
  ALL_Syll_Loc_Data_XCandML_level, x = "Species", y = "avg_syllable_duration_ms", 
  ylab = "Log (Avg. Syll. Dur. (ms))", xlab = "Species", add = "jitter", color = "Species", palette =c("#5C6B9C","#960019")) +
  theme_grey() +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Average Syllable Duration") +
  theme_classic()

num_unique_syllables_wilcoxon_boxplot <- ggboxplot(
  ALL_Syll_Loc_Data_XCandML_level, x = "Species", y = "num_unique_syllables", 
  ylab = "Log (# of Unique Sylls.)", xlab = "Species", add = "jitter", color = "Species", palette =c("#5C6B9C","#960019")) +
  theme_grey() +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Number of Unique Syllables") +
  theme_classic()

avg_sylls_upper_freq_Hz_wilcoxon_boxplot <- ggboxplot(
  ALL_Syll_Loc_Data_XCandML_level, x = "Species", y = "avg_sylls_upper_freq_Hz", 
  ylab = "Log (Avg. Sylls. Upper Freq. (Hz))", xlab = "Species", add = "jitter", color = "Species", palette =c("#5C6B9C","#960019")) +
  theme_grey() +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Average Syllables Upper Frequency") +
  theme_classic()
