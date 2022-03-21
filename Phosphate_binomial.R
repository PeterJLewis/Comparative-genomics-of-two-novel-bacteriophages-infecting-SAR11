library(tidyverse)
df = read.csv('GOV2_metadata.csv', header=TRUE)


#HTVC010P
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_1 = tibble(presence=df$HTVC010P>0, phosphate=df$phosphate_median)


model1 <- glm(presence ~ phosphate, family = binomial(link = "logit"),
              data = temp_df_1)
summary(model1)
#significance <0.05

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred1 <- tibble(phosphate = seq(from = min(temp_df_1$phosphate, na.rm=TRUE),
                              to = max(temp_df_1$phosphate, na.rm=TRUE),
                              length.out = 100))

Pred1 <- predict(model1, newdata = pred1, type = "response")

plot(x = temp_df_1$phosphate, y = temp_df_1$presence, main = 'HTVC010P', xlab = 'median phosphate conc. (µmol/l)', ylab = 'phage presence')
lines(pred1$phosphate, Pred1)

#Hödr
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_2 = tibble(presence=df$C6N2_1062_C8_A_0.3>0, phosphate=df$phosphate_median)


model2 <- glm(presence ~ phosphate, family = binomial(link = "logit"),
              data = temp_df_2)
summary(model2)
#significance <0.05

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred2 <- tibble(phosphate = seq(from = min(temp_df_2$phosphate, na.rm=TRUE),
                                to = max(temp_df_2$phosphate, na.rm=TRUE),
                                length.out = 100))

Pred2 <- predict(model2, newdata = pred2, type = "response")

plot(x = temp_df_2$phosphate, y = temp_df_2$presence, main = 'Hödr', xlab = 'median phosphate conc. (µmol/l)', ylab = 'phage presence')
lines(pred2$phosphate, Pred2)

#Baldr
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_3 = tibble(presence=df$C6N2_1062_C8_B_0.3>0, phosphate=df$phosphate_median)


model3 <- glm(presence ~ phosphate, family = binomial(link = "logit"),
              data = temp_df_3)
summary(model3)
#significance <0.1

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred3 <- tibble(phosphate = seq(from = min(temp_df_3$phosphate, na.rm=TRUE),
                                to = max(temp_df_3$phosphate, na.rm=TRUE),
                                length.out = 100))

Pred3 <- predict(model3, newdata = pred3, type = "response")

plot(x = temp_df_3$phosphate, y = temp_df_3$presence, main = 'Bladr', xlab = 'median phosphate conc. (µmol/l)', ylab = 'phage presence')
lines(pred3$phosphate, Pred3)

#HTVC028P
#pull out the variables of interest
#create a tibble out of variables of interest 
temp_df_4 = tibble(presence=df$HTVC028P>0, phosphate=df$phosphate_median)


model4 <- glm(presence ~ phosphate, family = binomial(link = "logit"),
              data = temp_df_4)
summary(model4)
#significance <0.05

#create new covariate values within observed range for predictions
# This draws the line in the middle
pred4 <- tibble(phosphate = seq(from = min(temp_df_4$phosphate, na.rm=TRUE),
                                to = max(temp_df_4$phosphate, na.rm=TRUE),
                                length.out = 100))

Pred4 <- predict(model4, newdata = pred4, type = "response")

plot(x = temp_df_4$phosphate, y = temp_df_4$presence, main = 'HTVC028P', xlab = 'median phosphate conc. (µmol/l)', ylab = 'phage presence')
lines(pred4$phosphate, Pred4)

#combined plot

plot(x = temp_df_1$phosphate, y = temp_df_1$presence, type = 'n', main = 'Phosphate Concentration', xlab = 'Median phosphate conc. (µmol/l)', ylab = 'Phage presence', cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.2)
lines(pred1$phosphate, Pred1, col = "steelblue4")
lines(pred2$phosphate, Pred2, col = "aquamarine2")
lines(pred3$phosphate, Pred3, col = "red")
lines(pred4$phosphate, Pred4, col = "yellow3")
legend("topright", legend=c("HTVC010P", "HTVC28P*", "Hödr", "Baldr"),
       col=c("steelblue4", "yellow3", "aquamarine2", "red"), lty=1, cex=1.2,
       title="Phage", text.font=4, bg='gray60')

#model coef & std error storage
model1_coef_phos <- summary(model1)$coef[2,1]
model2_coef_phos <- summary(model2)$coef[2,1]
model3_coef_phos <- summary(model3)$coef[2,1]
model4_coef_phos <- summary(model4)$coef[2,1]

model1_stderr_phos <- summary(model1)$coef[2,2]
model2_stderr_phos <- summary(model2)$coef[2,2]
model3_stderr_phos <- summary(model3)$coef[2,2]
model4_stderr_phos <- summary(model4)$coef[2,2]

model1_lower_phos <- model1_coef_phos - 1.96*model1_stderr_phos
model1_higher_phos <- model1_coef_phos + 1.96*model1_stderr_phos

model2_lower_phos <- model2_coef_phos - 1.96*model2_stderr_phos
model2_higher_phos <- model2_coef_phos + 1.96*model2_stderr_phos

model3_lower_phos <- model3_coef_phos - 1.96*model3_stderr_phos
model3_higher_phos <- model3_coef_phos + 1.96*model3_stderr_phos

model4_lower_phos <- model4_coef_phos - 1.96*model4_stderr_phos
model4_higher_phos <- model4_coef_phos + 1.96*model4_stderr_phos

model_phos <- c("HTVC010P", "Hödr", "Baldr", "HTVC028P")
cat_phos <- c("Phosphate", "Phosphate", "Phosphate", "Phosphate")
effect_phos <- c(model1_coef_phos, model2_coef_phos, model3_coef_phos, model4_coef_phos)
lower_phos <- c(model1_lower_phos, model2_lower_phos, model3_lower_phos, model4_lower_phos)
upper_phos <- c(model1_higher_phos, model2_higher_phos, model3_higher_phos, model4_higher_phos)
phos_error_data <- data.frame(model_phos, cat_phos, effect_phos, lower_phos, upper_phos)

#plot effect error

ggplot(data = phos_error_data, aes(y = model_phos, x = effect_phos, xmin = lower_phos, xmax = upper_phos)) +
  geom_point() +
  labs(y = "Phage model", x = "Phosphate model error") +
  theme(axis.title = element_text(size = 19), axis.text = element_text(size = 14)) +
  geom_errorbarh(height = 0.1) 


