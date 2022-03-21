#load packages

library(tidyverse)

# filter out samples where phage is not present

C8_A_0.3_present <- filter(df, df$C6N2_1062_C8_A_0.3 > 0)
C8_B_0.3_present <- filter(df, df$C6N2_1062_C8_B_0.3 > 0)
HTVC010P_present <- filter(df, df$HTVC010P > 0)
HTVC028P_present <- filter(df, df$HTVC028P > 0)

# simplify and combine

phage <- rep(c("Hödr"), each = length(C8_A_0.3_present$C6N2_1062_C8_A_0.3))
data_set <- rep(c("Samples where phage present"), each = length(C8_A_0.3_present$C6N2_1062_C8_A_0.3))
reads_mapped <- c(C8_A_0.3_present$C6N2_1062_C8_A_0.3)
C8_A_0.3_present <- data.frame(reads_mapped, phage, data_set) 

phage <- rep(c("Baldr"), each = length(C8_B_0.3_present$C6N2_1062_C8_B_0.3))
data_set <- rep(c("Samples where phage present"), each = length(C8_B_0.3_present$C6N2_1062_C8_B_0.3))
reads_mapped <- c(C8_B_0.3_present$C6N2_1062_C8_B_0.3)
C8_B_0.3_present <- data.frame(reads_mapped, phage, data_set)

phage <- rep(c("HTVC010P"), each = length(HTVC010P_present$HTVC010P))
data_set <- rep(c("Samples where phage present"), each = length(HTVC010P_present$HTVC010P))
reads_mapped <- c(HTVC010P_present$HTVC010P)
HTVC010P_present <- data.frame(reads_mapped, phage, data_set)

phage <- rep(c("HTVC028P"), each = length(HTVC028P_present$HTVC028P))
data_set <- rep(c("Samples where phage present"), each = length(HTVC028P_present$HTVC028P))
reads_mapped <- c(HTVC028P_present$HTVC028P)
HTVC028P_present <- data.frame(reads_mapped, phage, data_set)

phage_present <- rbind(C8_A_0.3_present, C8_B_0.3_present, HTVC010P_present, HTVC028P_present)
phage_present <- na.omit(phage_present)

view(phage_present)


#mean from all samples
# simplify and combine

phage <- rep(c("Hödr"), each = length(df$C6N2_1062_C8_A_0.3))
data_set <- rep(c("All samples"), each = length(df$C6N2_1062_C8_A_0.3))
reads_mapped <- c(df$C6N2_1062_C8_A_0.3)
C8_A_0.3 <- data.frame(reads_mapped, phage, data_set) 

phage <- rep(c("Baldr"), each = length(df$C6N2_1062_C8_B_0.3))
data_set <- rep(c("All samples"), each = length(df$C6N2_1062_C8_B_0.3))
reads_mapped <- c(df$C6N2_1062_C8_B_0.3)
C8_B_0.3 <- data.frame(reads_mapped, phage, data_set)

phage <- rep(c("HTVC010P"), each = length(df$HTVC010P))
data_set <- rep(c("All samples"), each = length(df$HTVC010P))
reads_mapped <- c(df$HTVC010P)
HTVC010P <- data.frame(reads_mapped, phage, data_set)

phage <- rep(c("HTVC028P"), each = length(df$HTVC028P))
data_set <- rep(c("All samples"), each = length(df$HTVC028P))
reads_mapped <- c(df$HTVC028P)
HTVC028P <- data.frame(reads_mapped, phage, data_set)

phage_abund <- rbind(C8_A_0.3, C8_B_0.3, HTVC010P, HTVC028P)
phage_abund <- na.omit(phage_abund)

view(phage_present)

#combine data sets

phage_combined <- rbind(phage_present, phage_abund)
view(phage_combined)

#plot data

ggplot(phage_combined, aes(x=phage, y=reads_mapped)) + 
  geom_boxplot() +labs(y = "Reads mapped", x = "Phage") +
  theme(legend.position = "none", axis.title = element_text(size = 18), 
        strip.text.x = element_text(size = 15), plot.title = element_text(size = 15), 
        axis.text = element_text(size = 15), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~data_set)

#stats

t.test(HTVC010P_present$reads_mapped, C8_B_0.3_present$reads_mapped, var.equal = TRUE)

