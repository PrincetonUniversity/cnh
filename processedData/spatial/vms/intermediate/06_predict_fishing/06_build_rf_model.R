# remove NAs
loc_data <- loc_data[complete.cases(feature_data),]
true_data <- true_data[complete.cases(feature_data)]
feature_data <- feature_data[complete.cases(feature_data),]

# remove fishing state == 3
loc_data <- loc_data[-which(true_data==3),]
feature_data <- feature_data[-which(true_data==3),]
true_data <- true_data[-which(true_data==3)]

#normalize
training_dat_norm <- apply(feature_data,2,function(x) (x - min(x))/diff(range(x)))

y = factor(true_data)

all_ids <- unique(loc_data$burst.id)
train_n = floor(length(all_ids)*.7)
trainingids <- sample(all_ids, size = train_n)
testids <- all_ids[-which(all_ids %in% trainingids)]

train_i <- which(loc_data$burst.id %in% trainingids)
test_i <- which(loc_data$burst.id %in% testids)

library(randomForest)
library(rfUtilities)
tuning <- rf.modelSel(xdata = training_dat_norm[train_i,], ydata = y[train_i])

mtrys <- tuneRF(x = training_dat_norm[train_i, tuning$selvars], 
                y = y[train_i])

rf1 <- randomForest(x = training_dat_norm[train_i, tuning$selvars], 
                    y = y[train_i],mtry = 26)

pred_rf1 <-   predict(rf1, training_dat_norm[test_i,tuning$selvars], type = "prob")
colnames(pred_rf1) <- paste0("p",colnames(pred_rf1))
pred_rf1 <- as.data.frame(pred_rf1)
pred_rf1$true <- y[test_i]
pred_rf1$pred <- ifelse(pred_rf1$p1>.75, 1, 0)

table(pred_rf1$true, pred_rf1$pred,deparse.level = 2)

# save results
saveRDS(list(confusion_matrix = table(pred_rf1$true, pred_rf1$pred,deparse.level = 2), 
             rf_model = rf1,
             tuning_results = tuning),
        file="processedData/spatial/vms/intermediate/06_predict_fishing/06_rf_model_list.RDS")