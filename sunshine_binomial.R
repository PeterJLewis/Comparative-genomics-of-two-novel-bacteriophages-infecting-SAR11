library(tidyverse)

#HTVC010P
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_1 = tibble(presence=df$HTVC010P>0, sunshine=df$sunshine_duration)


model1 <- glm(presence ~ sunshine, family = binomial(link = "logit"),
              data = temp_df)
summary(model1)
#significance <0.05

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred1 <- tibble(sunshine = seq(from = min(temp_df_1$sunshine, na.rm=TRUE),
                              to = max(temp_df_1$sunshine, na.rm=TRUE),
                              length.out = 100))

Pred1 <- predict(model1, newdata = pred1, type = "response")

plot(x = temp_df_1$sunshine, y = temp_df_1$presence, main = 'HTVC010P', xlab='sunshine duration (min/day)', ylab='phage presence')
lines(pred1$sunshine, Pred1)

#Hödr
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_2 = tibble(presence=df$C6N2_1062_C8_A_0.3>0, sunshine=df$sunshine_duration)


model2 <- glm(presence ~ sunshine, family = binomial(link = "logit"),
              data = temp_df_2)
summary(model2)
#significance <0.05

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred2 <- tibble(sunshine = seq(from = min(temp_df_2$sunshine, na.rm=TRUE),
                               to = max(temp_df_2$sunshine, na.rm=TRUE),
                               length.out = 100))

Pred2 <- predict(model2, newdata = pred2, type = "response")

plot(x = temp_df_2$sunshine, y = temp_df_2$presence, main = 'Hödr', xlab='sunshine duration (min/day)', ylab='phage presence')
lines(pred2$sunshine, Pred2)

#Baldr
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_3 = tibble(presence=df$C6N2_1062_C8_B_0.3>0, sunshine=df$sunshine_duration)


model3 <- glm(presence ~ sunshine, family = binomial(link = "logit"),
              data = temp_df_3)
summary(model1)
#significance <0.05

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred3 <- tibble(sunshine = seq(from = min(temp_df_3$sunshine, na.rm=TRUE),
                               to = max(temp_df_3$sunshine, na.rm=TRUE),
                               length.out = 100))

Pred3 <- predict(model3, newdata = pred3, type = "response")

plot(x = temp_df_3$sunshine, y = temp_df_3$presence, main = 'Baldr', xlab='sunshine duration (min/day)', ylab='phage presence')
lines(pred3$sunshine, Pred3)

#HTVC028P
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_4 = tibble(presence=df$HTVC028P>0, sunshine=df$sunshine_duration)


model4 <- glm(presence ~ sunshine, family = binomial(link = "logit"),
              data = temp_df_4)
summary(model4)
#significance >0.1

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred4 <- tibble(sunshine = seq(from = min(temp_df_4$sunshine, na.rm=TRUE),
                               to = max(temp_df_4$sunshine, na.rm=TRUE),
                               length.out = 100))

Pred4 <- predict(model4, newdata = pred4, type = "response")

plot(x = temp_df_4$sunshine, y = temp_df_4$presence, main = 'HTVC028P', xlab='sunshine duration (min/day)', ylab='phage presence')
lines(pred4$sunshine, Pred4)


#combined plot

plot(x = temp_df_1$sunshine, y = temp_df_1$presence, type = 'n', main = 'Sunshine duration', xlab = 'Sunshine duration (min/day)', ylab = 'Phage presence', cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.2)
lines(pred1$sunshine, Pred1, col = "steelblue4")
lines(pred2$sunshine, Pred2, col = "aquamarine2")
lines(pred3$sunshine, Pred3, col = "red")
lines(pred4$sunshine, Pred4, col = "yellow3")
legend("topright", legend=c("HTVC010P", "HTVC28P*", "Hödr", "Baldr"),
       col=c("steelblue4", "yellow3", "aquamarine2", "red"), lty=1, cex=1.2,
       title="Phage", text.font=4, bg='gray60')

#model coef & std error storage
model1_coef_sun <- summary(model1)$coef[2,1]
model2_coef_sun <- summary(model2)$coef[2,1]
model3_coef_sun <- summary(model3)$coef[2,1]
model4_coef_sun <- summary(model4)$coef[2,1]

model1_stderr_sun <- summary(model1)$coef[2,2]
model2_stderr_sun <- summary(model2)$coef[2,2]
model3_stderr_sun <- summary(model3)$coef[2,2]
model4_stderr_sun <- summary(model4)$coef[2,2]

model1_lower_sun <- model1_coef_sun - 1.96*model1_stderr_sun
model1_higher_sun <- model1_coef_sun + 1.96*model1_stderr_sun

model2_lower_sun <- model2_coef_sun - 1.96*model2_stderr_sun
model2_higher_sun <- model2_coef_sun + 1.96*model2_stderr_sun

model3_lower_sun <- model3_coef_sun - 1.96*model3_stderr_sun
model3_higher_sun <- model3_coef_sun + 1.96*model3_stderr_sun

model4_lower_sun <- model4_coef_sun - 1.96*model4_stderr_sun
model4_higher_sun <- model4_coef_sun + 1.96*model4_stderr_sun

model_sun <- c("HTVC010P", "Hödr", "Baldr", "HTVC028P")
cat_sun <- c("Sunshine", "Sunshine", "Sunshine", "Sunshine")
effect_sun <- c(model1_coef_sun, model2_coef_sun, model3_coef_sun, model4_coef_sun)
lower_sun <- c(model1_lower_sun, model2_lower_sun, model3_lower_sun, model4_lower_sun)
upper_sun <- c(model1_higher_sun, model2_higher_sun, model3_higher_sun, model4_higher_sun)
error_data_sun <- data.frame(model_sun, cat_sun, effect_sun, lower_sun, upper_sun)

#plot effect error

ggplot(data = error_data_sun, aes(y = model_sun, x = effect_sun, xmin = lower_sun, xmax = upper_sun)) +
  geom_point() +
  labs(y = "Phage model", x = "Sunshine model error") +
  theme(axis.title = element_text(size = 19), axis.text = element_text(size = 14)) +
  geom_errorbarh(height = 0.1) 
