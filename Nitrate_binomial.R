library(tidyverse)
df = read.csv('GOV2_metadata.csv', header=TRUE)


#HTVC010P
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_1 = tibble(presence=df$HTVC010P>0, nitrate=df$nitrate_median)


model1 <- glm(presence ~ nitrate, family = binomial(link = "logit"),
              data = temp_df_1)
summary(model1)
#significance <0.05

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred1 <- tibble(nitrate = seq(from = min(temp_df_1$nitrate, na.rm=TRUE),
                              to = max(temp_df_1$nitrate, na.rm=TRUE),
                              length.out = 100))

Pred1 <- predict(model1, newdata = pred1, type = "response")

plot(x = temp_df_1$nitrate, y = temp_df_1$presence, main = 'HTVC010P', xlab = 'median nitrate conc. (µmol/l)', ylab = 'phage presence')
lines(pred1$nitrate, Pred1)

#Hödr
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_2 = tibble(presence=df$C6N2_1062_C8_A_0.3>0, nitrate=df$nitrate_median)


model2 <- glm(presence ~ nitrate, family = binomial(link = "logit"),
              data = temp_df_2)
summary(model2)
#significance <0.05

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred2 <- tibble(nitrate = seq(from = min(temp_df_2$nitrate, na.rm=TRUE),
                              to = max(temp_df_2$nitrate, na.rm=TRUE),
                              length.out = 100))

Pred2 <- predict(model2, newdata = pred1, type = "response")

plot(x = temp_df_2$nitrate, y = temp_df_2$presence, main = 'Hödr', xlab = 'median nitrate conc. (µmol/l)', ylab = 'phage presence')
lines(pred2$nitrate, Pred2)

#Baldr
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_3 = tibble(presence=df$C6N2_1062_C8_B_0.3>0, nitrate=df$nitrate_median)


model3 <- glm(presence ~ nitrate, family = binomial(link = "logit"),
              data = temp_df_3)
summary(model3)
#insignificant

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred3 <- tibble(nitrate = seq(from = min(temp_df_3$nitrate, na.rm=TRUE),
                              to = max(temp_df_3$nitrate, na.rm=TRUE),
                              length.out = 100))

Pred3 <- predict(model3, newdata = pred1, type = "response")

plot(x = temp_df_3$nitrate, y = temp_df_3$presence, main = 'Baldr', xlab = 'median nitrate conc. (µmol/l)', ylab = 'phage presence')
lines(pred3$nitrate, Pred3)

#HTVC028P
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_4 = tibble(presence=df$HTVC028P>0, nitrate=df$nitrate_median)


model4 <- glm(presence ~ nitrate, family = binomial(link = "logit"),
              data = temp_df_4)
summary(model4)
#significance >0.1

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred4 <- tibble(nitrate = seq(from = min(temp_df_4$nitrate, na.rm=TRUE),
                              to = max(temp_df_4$nitrate, na.rm=TRUE),
                              length.out = 100))

Pred4 <- predict(model4, newdata = pred4, type = "response")

plot(x = temp_df_4$nitrate, y = temp_df_4$presence, main = 'HTVC028P', xlab = 'median nitrate conc. (µmol/l)', ylab = 'phage presence')
lines(pred4$nitrate, Pred4)

#combined plot

plot(x = temp_df_1$nitrate, y = temp_df_1$presence, type = 'n', main = 'Nitrate Concentration', xlab = 'Median nitrate conc. (µmol/l)', ylab = 'Phage presence', cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.2)
lines(pred1$nitrate, Pred1, col = "steelblue4")
lines(pred2$nitrate, Pred2, col = "aquamarine2")
lines(pred3$nitrate, Pred3, col = "red")
lines(pred4$nitrate, Pred4, col = "yellow3")
legend("topright", legend=c("HTVC010P", "HTVC28P*", "Hödr", "Baldr*"),
       col=c("steelblue4", "yellow3", "aquamarine2", "red"), lty=1, cex=1.2,
       title="Phage", text.font=4, bg='gray60')

#model coef & std error storage
model1_coef_nit <- summary(model1)$coef[2,1]
model2_coef_nit <- summary(model2)$coef[2,1]
model3_coef_nit <- summary(model3)$coef[2,1]
model4_coef_nit <- summary(model4)$coef[2,1]

model1_stderr_nit <- summary(model1)$coef[2,2]
model2_stderr_nit <- summary(model2)$coef[2,2]
model3_stderr_nit <- summary(model3)$coef[2,2]
model4_stderr_nit <- summary(model4)$coef[2,2]

model1_lower_nit <- model1_coef_nit - 1.96*model1_stderr_nit
model1_higher_nit <- model1_coef_nit + 1.96*model1_stderr_nit

model2_lower_nit <- model2_coef_nit - 1.96*model2_stderr_nit
model2_higher_nit <- model2_coef_nit + 1.96*model2_stderr_nit

model3_lower_nit <- model3_coef_nit - 1.96*model3_stderr_nit
model3_higher_nit <- model3_coef_nit + 1.96*model3_stderr_nit

model4_lower_nit <- model4_coef_nit - 1.96*model4_stderr_nit
model4_higher_nit <- model4_coef_nit + 1.96*model4_stderr_nit

model_nit <- c("HTVC010P", "Hödr", "Baldr", "HTVC028P")
cat_nit <- c("Nitrate", "Nitrate", "Nitrate", "Nitrate")
effect_nit <- c(model1_coef_nit, model2_coef_nit, model3_coef_nit, model4_coef_nit)
lower_nit <- c(model1_lower_nit, model2_lower_nit, model3_lower_nit, model4_lower_nit)
upper_nit <- c(model1_higher_nit, model2_higher_nit, model3_higher_nit, model4_higher_nit)#
error_data_nit <- data.frame(model_nit, cat_nit, effect_nit, lower_nit, upper_nit)

#plot effect error

ggplot(data = error_data_nit, aes(y = model_nit, x = effect_nit, xmin = lower_nit, xmax = upper_nit)) +
  geom_point() +
  labs(y = "Phage model", x = "Nitrate model error") +
  theme(axis.title = element_text(size = 19), axis.text = element_text(size = 14)) +
  geom_errorbarh(height = 0.1) 


