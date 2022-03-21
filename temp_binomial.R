library(tidyverse)


#HTVC010P
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_1 = tibble(presence=df$HTVC010P>0, temp=df$temp_median)


model1 <- glm(presence ~ temp, family = binomial(link = "logit"),
              data = temp_df_1)
summary(model1)
#significance <0.05

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred1 <- tibble(temp = seq(from = min(temp_df_1$temp, na.rm=TRUE),
                               to = max(temp_df_1$temp, na.rm=TRUE),
                               length.out = 100))

Pred1 <- predict(model1, newdata = pred1, type = "response")

plot(x = temp_df_1$temp, y = temp_df_1$presence, main = 'HTVC010P', xlab = 'median temperature', ylab = 'phage presence')
lines(pred1$temp, Pred1)

#Hodr
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_2 = tibble(presence=df$C6N2_1062_C8_A_0.3>0, temp=df$temp_median)


model2 <- glm(presence ~ temp, family = binomial(link = "logit"),
              data = temp_df_2)
summary(model2)
#significance <0.05

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred2 <- tibble(temp = seq(from = min(temp_df_2$temp, na.rm=TRUE),
                           to = max(temp_df_2$temp, na.rm=TRUE),
                           length.out = 100))

Pred2 <- predict(model2, newdata = pred2, type = "response")

plot(x = temp_df_2$temp, y = temp_df_2$presence, main = 'Hödr', xlab = 'median temperature', ylab = 'phage presence')
lines(pred2$temp, Pred2)

#Baldr
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_3 = tibble(presence=df$C6N2_1062_C8_B_0.3>0, temp=df$temp_median)


model3 <- glm(presence ~ temp, family = binomial(link = "logit"),
              data = temp_df_3)
summary(model3)
#significance >0.1

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred3 <- tibble(temp = seq(from = min(temp_df_3$temp, na.rm=TRUE),
                           to = max(temp_df_3$temp, na.rm=TRUE),
                           length.out = 100))

Pred3 <- predict(model3, newdata = pred3, type = "response")

plot(x = temp_df_3$temp, y = temp_df_3$presence, main = 'Baldr', xlab = 'median temperature', ylab = 'phage presence')
lines(pred3$temp, Pred3)

#HTVC028P
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_4 = tibble(presence=df$HTVC028P>0, temp=df$temp_median)


model4 <- glm(presence ~ temp, family = binomial(link = "logit"),
              data = temp_df_4)
summary(model4)
#significance <0.05

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred4 <- tibble(temp = seq(from = min(temp_df_4$temp, na.rm=TRUE),
                           to = max(temp_df_4$temp, na.rm=TRUE),
                           length.out = 100))

Pred4 <- predict(model4, newdata = pred4, type = "response")

plot(x = temp_df_4$temp, y = temp_df_4$presence, main = 'HTVC028P', xlab = 'median temperature', ylab = 'phage presence')
lines(pred4$temp, Pred4)

#combined plot

plot(x = temp_df_1$temp, y = temp_df_1$presence, type = "n", main = 'Temperature of sample', xlab = 'Median temperature (°C)', ylab = 'Phage presence', cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.2)
lines(pred1$temp, Pred1, col = "steelblue4")
lines(pred2$temp, Pred2, col = "aquamarine2")
lines(pred3$temp, Pred3, col = "red")
lines(pred4$temp, Pred4, col = "yellow3")
legend("topleft", legend=c("HTVC010P", "HTVC28P", "Hödr", "Baldr*"),
       col=c("steelblue4", "yellow3", "aquamarine2", "red"), lty=1, cex=1.2,
       title="Phage", text.font=4, bg='gray60')

#model coef & std error storage

model1_coef_temp <- summary(model1)$coef[2,1]
model2_coef_temp <- summary(model2)$coef[2,1]
model3_coef_temp <- summary(model3)$coef[2,1]
model4_coef_temp <- summary(model4)$coef[2,1]

model1_stderr_temp <- summary(model1)$coef[2,2]
model2_stderr_temp <- summary(model2)$coef[2,2]
model3_stderr_temp <- summary(model3)$coef[2,2]
model4_stderr_temp <- summary(model4)$coef[2,2]

model1_lower_temp <- model1_coef_temp - 1.96*model1_stderr_temp
model1_higher_temp <- model1_coef_temp + 1.96*model1_stderr_temp

model2_lower_temp <- model2_coef_temp - 1.96*model2_stderr_temp
model2_higher_temp <- model2_coef_temp + 1.96*model2_stderr_temp

model3_lower_temp <- model3_coef_temp - 1.96*model3_stderr_temp
model3_higher_temp <- model3_coef_temp + 1.96*model3_stderr_temp

model4_lower_temp <- model4_coef_temp - 1.96*model4_stderr_temp
model4_higher_temp <- model4_coef_temp + 1.96*model4_stderr_temp

model_temp <- c("HTVC010P", "Hödr", "Baldr", "HTVC028P")
cat_temp <- c("Temperature", "Temperature", "Temperature", "Temperature")
effect_temp <- c(model1_coef_temp, model2_coef_temp, model3_coef_temp, model4_coef_temp)
lower_temp <- c(model1_lower_temp, model2_lower_temp, model3_lower_temp, model4_lower_temp)
upper_temp <- c(model1_higher_temp, model2_higher_temp, model3_higher_temp, model4_higher_temp)
error_data_temp <- data.frame(model_temp, cat_temp, effect_temp, lower_temp, upper_temp)

#plot effect error

ggplot(data = error_data_temp, aes(y = model_temp, x = effect_temp, xmin = lower_temp, xmax = upper_temp)) +
  geom_point() +
  labs(y = "Phage model", x = "Temperature model error") +
  theme(axis.title = element_text(size = 19), axis.text = element_text(size = 14)) +
  geom_errorbarh(height = 0.1)
