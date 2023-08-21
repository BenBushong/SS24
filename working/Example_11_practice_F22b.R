

library(ISLR)
library(tibble)
library(broom)
library(caret)
Default = ISLR::Default

table(Default$default)

Default_clean = Default %>%
  dplyr::mutate(default_int = ifelse(default=='Yes', 1, 0),
                student_int = ifelse(student=='Yes', 1, 0))





model_glm = glm(default_int ~ student_int + balance*income, data = Default_clean, family = "binomial")

summary(model_glm)
tidy(model_glm)

BayesCutoff = .5

model_glm_pred = ifelse(predict(model_glm, type = 'response')>BayesCutoff, 'Yes', 'No')  # Has to match format of Default_clean$default

# 
# calc_class_err = function(actual, predicted) {
#   mean(actual != predicted)
# }



res_tab = table(predicted = model_glm_pred,
                actual = Default_clean$default)  # always put "predicted" first.

res_mat = caret::confusionMatrix(res_tab, positive = 'Yes')
c(res_mat$overall['Accuracy'],
  res_mat$byClass['Sensitivity'],
  res_mat$byClass['Specificity'])

# Let's check these:
# 
# Accuracy
(res_tab[1,1]+res_tab[2,2])/sum(res_tab)

# Sensitivity: True Positive Rate: TP/(TP+FN)
res_tab['Yes','Yes']/(res_tab['Yes','Yes'] + res_tab['No','Yes'])

# Specificity: True Negative Rate: TN/N
res_tab['No','No']/(res_tab['No','No'] + res_tab['Yes','No'])

# Prevalence: 
(res_tab['Yes','Yes']+res_tab['No','Yes'])/sum(res_tab)

















########## Breakout Two ##############
res_list = list()

for(BayesCutoff in seq(from = .001, to = .970, by = .0025)){
print(BayesCutoff)
  
  
model_glm = glm(default_int ~ student_int + balance*income, data = Default_clean, family = "binomial")

model_glm_pred = ifelse(predict(model_glm, type = 'response')>BayesCutoff, 'Yes', 'No')  # Has to match format of Default_clean$default

res_tab = table(predicted = model_glm_pred,
                actual = Default_clean$default)  # always put "predicted" first.

res_mat = caret::confusionMatrix(res_tab, positive = 'Yes')
res = c(BayesCutoff = BayesCutoff,
        res_mat$overall['Accuracy'],
        res_mat$byClass['Sensitivity'],
        res_mat$byClass['Specificity'])

res_list[[paste0('BayesCutoff',BayesCutoff)]] = res
}


res_list = bind_rows(res_list)

res_list = rbind(c(0, NA, 1, 0),
      res_list,
      c(1, NA, 0, 1))

ggplot(res_list, aes(x = Specificity, y = Sensitivity)) + 
  geom_line() +
  #coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(slope = 1, intercept = 1) + 
  scale_x_reverse()


# the AUC
# Change in x-axis (specificity) times the mean of the two values on the y-axis (sensitivity)
auc = 0
for(i in 2:NROW(res_list)){
  auc = auc + ((res_list$Specificity[i] - res_list$Specificity[i-1])* mean(res_list$Sensitivity[c(i-1, i)]))
}
print(auc)

### using pROC
library(pROC)

prob = predict(model_glm, type = 'response')
roc(Default_clean$default_int ~ prob, plot = TRUE, print.auc = TRUE)
