#load packages
library(tidyverse)
library(ggrepel)

#load data
df <- read.csv("GOV2_metadata.csv")

#generate dataframe populated with rows where C8_A_0.3 or C8_B_0.3 are present

dist_df <- filter(df, df$C6N2_1062_C8_A_0.3 > 0 | df$C6N2_1062_C8_B_0.3 > 0)

#ensure phages do not occur in the same sample, test_frame should be empty

test_frame <- filter(dist_df, dist_df$C6N2_1062_C8_A_0.3 > 0 & dist_df$C6N2_1062_C8_B_0.3 > 0)

view(test_frame)

#label samples by phage of intrest present

dist_df$phages <- ifelse(dist_df$C6N2_1062_C8_A_0.3 > 0 , "Hödr", "Baldr")

#combined abundance of both phages

#dist_df$abund <- c(6.018057, 3.449055, 4.521903, 2.038494, 1.780686, 4.010430, 24.710360, 5.388527, 6.117178, 4.758540, 1.641428, 4.147557, 1.482746, 11.242913, 45.538452, 23.330837, 7.626487, 4.826442, 4.561864, 2.739276, 2.702771, 4.150815, 4.434126, 2.356943, 3.235952, 5.354465, 2.952803, 1.904172, 2.507953, 1.947523, 1.965776, 2.115482, 2.461832, 2.108214, 1.348044, 1.395828)

#plot of abundance

ggplot() + 
  geom_polygon(data = worldmap, aes(x = long, y = lat, group = group), 
               colour = "gray90", fill = "gray90") + 
  geom_point(data = dist_df, 
             aes(x = long, 
                 y = lat, colour = phages), size = 3, shape = 1) + 
    labs(size = "Percentage of metagenomic sample", colour = "Isolate", 
         y = "Lattitude", x = "Longitude", 
         title = "Distribution of Hödr & Baldr") +
  geom_text_repel(aes(label=dist_df$ï..sample_label, x = dist_df$long, y = dist_df$lat),size = 3, colour = "black") +
  theme_dark() +
  theme(axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20), 
        legend.title = element_text(size = 15), legend.text = element_text(size = 10))

