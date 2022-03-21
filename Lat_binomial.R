library(tidyverse)
df = read.csv('GOV2_metadata.csv', header=TRUE)

#HTVC010P
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_1 = tibble(presence=df$HTVC010P>0, abs_lat=abs(df$lat))


model1 <- glm(presence ~ abs_lat, family = binomial(link = "logit"),
              data = temp_df_1)
summary(model1)
# significance <0.05

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred1 <- tibble(abs_lat = seq(from = min(temp_df_1$abs_lat, na.rm=TRUE),
                              to = max(temp_df_1$abs_lat, na.rm=TRUE),
                              length.out = 100))

Pred1 <- predict(model1, newdata = pred1, type = "response")

plot(x = temp_df_1$abs_lat, y = temp_df_1$presence, main = 'HTVC010P', xlab = 'absolute lattitude', ylab = 'phage presence')
lines(pred1$abs_lat, Pred)

#Hödr
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_2 = tibble(presence=df$C6N2_1062_C8_A_0.3>0, abs_lat=abs(df$lat))


model2 <- glm(presence ~ abs_lat, family = binomial(link = "logit"),
              data = temp_df_2)
summary(model2)
#significance <0.05

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred2 <- tibble(abs_lat = seq(from = min(temp_df_2$abs_lat, na.rm=TRUE),
                              to = max(temp_df_2$abs_lat, na.rm=TRUE),
                              length.out = 100))

Pred2 <- predict(model2, newdata = pred2, type = "response")

plot(x = temp_df_2$abs_lat, y = temp_df_2$presence, main = 'Hödr', xlab = 'absolute lattitude', ylab = 'phage presence')
lines(pred2$abs_lat, Pred2)

#Baldr
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_3 = tibble(presence=df$C6N2_1062_C8_B_0.3>0, abs_lat=abs(df$lat))


model3 <- glm(presence ~ abs_lat, family = binomial(link = "logit"),
              data = temp_df_3)
summary(model3)
#significance <0.05

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred3 <- tibble(abs_lat = seq(from = min(temp_df_3$abs_lat, na.rm=TRUE),
                              to = max(temp_df_3$abs_lat, na.rm=TRUE),
                              length.out = 100))

Pred3 <- predict(model3, newdata = pred3, type = "response")

plot(x = temp_df_3$abs_lat, y = temp_df_3$presence, main = 'Baldr', xlab = 'absolute lattitude', ylab = 'phage presence')
lines(pred3$abs_lat, Pred)

#HTVC028P
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_4 = tibble(presence=df$HTVC028P>0, abs_lat=abs(df$lat))


model4 <- glm(presence ~ abs_lat, family = binomial(link = "logit"),
              data = temp_df_4)
summary(model4)
# significance <0.05

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred4 <- tibble(abs_lat = seq(from = min(temp_df_4$abs_lat, na.rm=TRUE),
                              to = max(temp_df_4$abs_lat, na.rm=TRUE),
                              length.out = 100))

Pred4 <- predict(model4, newdata = pred4, type = "response")

plot(x = temp_df_4$abs_lat, y = temp_df_4$presence, main = 'HTVC028P', xlab = 'absolute lattitude', ylab = 'phage presence')
lines(pred4$abs_lat, Pred4)


#combined plot

plot(x = temp_df_1$abs_lat, y = temp_df_1$presence, type = 'n', main = 'Absolute lattitude', xlab = 'Absolute lattitude', ylab = 'Phage presence', cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.2)
lines(pred1$abs_lat, Pred1, col = "steelblue4")
lines(pred2$abs_lat, Pred2, col = "aquamarine2")
lines(pred3$abs_lat, Pred3, col = "red")
lines(pred4$abs_lat, Pred4, col = "yellow3")
legend("topright", legend=c("HTVC010P", "HTVC28P", "Hödr", "Baldr"),
       col=c("steelblue4", "yellow3", "aquamarine2", "red"), lty=1, cex=1.2,
       title="Phage", text.font=4, bg='gray60')

#model coef & std error storage
model1_coef_lat <- summary(model1)$coef[2,1]
model2_coef_lat <- summary(model2)$coef[2,1]
model3_coef_lat <- summary(model3)$coef[2,1]
model4_coef_lat <- summary(model4)$coef[2,1]

model1_stderr_lat <- summary(model1)$coef[2,2]
model2_stderr_lat <- summary(model2)$coef[2,2]
model3_stderr_lat <- summary(model3)$coef[2,2]
model4_stderr_lat <- summary(model4)$coef[2,2]

model1_lower_lat <- model1_coef_lat - 1.96*model1_stderr_lat
model1_higher_lat <- model1_coef_lat + 1.96*model1_stderr_lat

model2_lower_lat <- model2_coef_lat - 1.96*model2_stderr_lat
model2_higher_lat <- model2_coef_lat + 1.96*model2_stderr_lat

model3_lower_lat <- model3_coef_lat - 1.96*model3_stderr_lat
model3_higher_lat <- model3_coef_lat + 1.96*model3_stderr_lat

model4_lower_lat <- model4_coef_lat - 1.96*model4_stderr_lat
model4_higher_lat <- model4_coef_lat + 1.96*model4_stderr_lat

model_lat <- c("HTVC010P", "Hödr", "Baldr", "HTVC028P")
cat_lat <- c("Latitude","Latitude","Latitude","Latitude")
effect_lat <- c(model1_coef_lat, model2_coef_lat, model3_coef_lat, model4_coef_lat)
lower_lat <- c(model1_lower_lat, model2_lower_lat, model3_lower_lat, model4_lower_lat)
upper_lat <- c(model1_higher_lat, model2_higher_lat, model3_higher_lat, model4_higher_lat)
error_data_lat <- data.frame(model_lat, effect_lat, lower_lat, upper_lat)

#plot effect error

ggplot(data = error_data_lat, aes(y = model_lat, x = effect_lat, xmin = lower_lat, xmax = upper_lat)) +
  geom_point() +
  labs(y = "Phage model", x = "Latitude model error") +
  theme(axis.title = element_text(size = 19), axis.text = element_text(size = 14)) +
  geom_errorbarh(height = 0.1) 
  

