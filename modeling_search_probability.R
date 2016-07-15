library(stringr)

setwd('~/personal_projects/texas-dps/')
data = read.csv('./data/regression_features/regression_features_select_officers.csv')
data$officer_id = str_pad(data$officer_id,5,pad='0')
length(unique_officer_ids)


# filter out rows where searched fields aren't equal
data_filtered = data[data$race %in% c('W','B','H'),]
data_filtered$race = relevel(data_filtered$race,ref='W')
unique_officer_ids = unique(data$officer_id)


sink(file ='./results/officer_models.txt', append=FALSE, type='output')
for (id in unique_officer_ids) {
  data = data_filtered[data_filtered$officer_id == id,]
  print(paste("Officer ID:",id))
  print(paste("Officer Name:",data$name[1]))
  print(paste("Region Worked:",paste(unique(data$region))))
  print("Model Results:")
  model = glm(searched ~ sex + race + age_of_driver + texas_driver + age_of_car, data=data, family = binomial(link='logit'))
  model_summary = summary(model)
  print(model_summary)
  print(exp(cbind(coef(model), confint(model))))
  print("")
}






split_train_test = function(dataset) {
  train_index = sample(1:nrow(dataset), floor(nrow(dataset)*.9),replace=FALSE)
  test_index = !(seq(0,nrow(dataset)) %in% train_index)
  
  train = dataset[train_index,]
  test = dataset[test_index,]
  return(list("train" = train, "test" = test))
}