#load packages

library(tidyverse)

#load data

df <- read.csv("GOV2_metadata.csv")

#filter out mediteranean samples

med_data <- filter(df, df$ocean_region == '[MS] Mediterranean Sea (MRGID:1905)')

#format data for ggplot

abundance <- c(med_data$C6N2_1062_C8_B_0.3)
phage <- rep(c("Baldr"), each = length(med_data$ocean_region))
Baldr <- data.frame(abundance, phage)

abundance <- c(med_data$HTVC010P)
phage <- rep(c("HTVC010P"), each = length(med_data$ocean_region))
HTVC010P <- data.frame(abundance, phage)

abundance <- c(med_data$HTVC021P)
phage <- rep(c("HTVC021P"), each = length(med_data$ocean_region))
HTVC021P <- data.frame(abundance, phage)

abundance <- c(med_data$HTVC027P)
phage <- rep(c("HTVC027P"), each = length(med_data$ocean_region))
HTVC027P <- data.frame(abundance, phage)

abundance <- c(med_data$HTVC109P)
phage <- rep(c("HTVC109P"), each = length(med_data$ocean_region))
HTVC109P <- data.frame(abundance, phage)

abundance <- c(med_data$HTVC111P)
phage <- rep(c("HTVC111P"), each = length(med_data$ocean_region))
HTVC111P <- data.frame(abundance, phage)

abundance <- c(med_data$HTVC202P)
phage <- rep(c("HTVC202P"), each = length(med_data$ocean_region))
HTVC202P <- data.frame(abundance, phage)

abundance <- c(med_data$HTVC203P)
phage <- rep(c("HTVC203P"), each = length(med_data$ocean_region))
HTVC203P <- data.frame(abundance, phage)

abundance <- c(med_data$Kolga)
phage <- rep(c("Kólga"), each = length(med_data$ocean_region))
Kólga <- data.frame(abundance, phage)

#combine data frames

med_phages <- rbind(Baldr, HTVC010P, HTVC021P, HTVC027P, HTVC109P, HTVC111P, HTVC202P, HTVC203P, Kólga)
med_phages <- na.omit(med_phages)

#plot

ggplot(med_phages, aes(x=phage, y=abundance)) + 
  geom_boxplot() +labs(y = "Relative Abundance", x = "Phage") +
  theme(legend.position = "none", axis.title = element_text(size = 18), 
        strip.text.x = element_text(size = 15), plot.title = element_text(size = 15), 
        axis.text = element_text(size = 12), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))