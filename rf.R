
# 清理环境并设置工作目录
rm(list = ls())
setwd("E:/shuinew2/qd/rfnew/quan/")

# 加载必要的库
library(randomForest)
library(caret)
library(ggplot2)
library(mice)
library(MLmetrics)
library(dplyr)
library(iml) 
# 设置随机种子以确保结果可重复
set.seed(123)

# 读取数据
risk_data <- read.delim('zong.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE, quote = '')
global_data <- read.delim('E:/shuinew/qd/qdyz/rfnew/qdys.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE, quote = '')
colnames(global_data) <- make.names(colnames(global_data))
# 查看数据集的结构
str(risk_data)
str(global_data)

# 检查全球数据的缺失情况
missing_values <- sapply(global_data, function(x) sum(is.na(x)))
print(missing_values)

# 处理全球数据缺失值
#imputed_global_data <- mice(global_data, m=10, method='rf', seed=123)

#complete_global_data <- complete(imputed_global_data, 1)

#write.table(complete_global_data,"qdysrf.txt", row.names = F,
#            col.names = T, sep = '\t',na = 'NA',quote = FALSE)

complete_global_data<-read.delim('E:/shuinew/qd/qdyz/rfnew//qdysrf.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE, quote = '')

# 验证插补结果
summary(complete_global_data)

# 将风险数据和全球数据合并
merged_data <- merge(risk_data, complete_global_data, by = "Country")

# 分割合并后的数据集为训练集和测试集
trainData <- merged_data
testData <- complete_global_data

# 检查训练集和测试集中是否还有缺失值
missing_train <- sapply(trainData, function(x) sum(is.na(x)))
print(missing_train)
missing_test <- sapply(testData, function(x) sum(is.na(x)))
print(missing_test)

# 如果训练集仍有缺失值，可以考虑插补这些缺失值
if (any(missing_train > 0)) {
  imputed_trainData <- mice(trainData, m=5, method='rf', seed=123)
  trainData <- complete(imputed_trainData, 1)
}

# 再次检查训练集是否还有缺失值
missing_train <- sapply(trainData, function(x) sum(is.na(x)))
print(missing_train)

# 确保训练集没有缺失值
stopifnot(all(missing_train == 0))

# 标准化数据
preProcValues <- preProcess(trainData[, -which(names(trainData) == "abundance_water")], method = c("center", "scale"))
trainData_scaled <- predict(preProcValues, trainData[, -which(names(trainData) == "abundance_water")])
trainData_scaled <- cbind(abundance_water = trainData$abundance_water, trainData_scaled)

testData_scaled <- predict(preProcValues, testData)

# 设置随机种子以确保结果可重复
set.seed(123)

# 特征选择
ctrl <- rfeControl(functions = rfFuncs, method = "LOOCV")

#ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

x <- trainData_scaled[, -which(names(trainData_scaled) == "abundance_water")]
y <- trainData_scaled$abundance_water


# 执行递归特征消除，指定更多的特征数量

sizes <- seq(1, ncol(x), by = 1)  # 每次减少?个特征
results <- rfe(x, y, sizes = sizes, rfeControl = ctrl)

# 获取不同特征数量下的MSE和R²值
mse_values <- results$results$RMSE^2
r2_values <- results$results$Rsquared

# 创建数据框用于绘图
performance_data <- data.frame(
  Features = results$results$Variables,
  MSE = mse_values,
  R2 = r2_values
)

# 绘制MSE和R²值随特征数量变化的图
ggplot(performance_data, aes(x = Features)) +
  geom_line(aes(y = MSE, color = "MSE")) +
  geom_line(aes(y = R2, color = "R²")) +
  labs(title = "MSE和R²值随特征数量变化的图", x = "特征数量", y = "指标值") +
  scale_color_manual(name = "指标", values = c("MSE" = "red", "R²" = "blue")) +
  theme_minimal()

# 获取最佳特征
best_vars <- predictors(results)

# 手动控制选择的特征数量，例如选择25个特征
desired_num_features <- 16
if (length(best_vars) < desired_num_features) {
  # 如果RFE选择的特征数量少于所需的数量，则选择前desired_num_features个特征
  additional_vars <- setdiff(names(x), best_vars)[2:(desired_num_features - length(best_vars))]
  best_vars <- c(best_vars, additional_vars)
} else {
  # 如果RFE选择的特征数量大于或等于所需的数量，则只选择前desired_num_features个特征
  best_vars <- best_vars[2:desired_num_features]
}

best_vars <- best_vars[!best_vars %in% "Country"]

cat("选择的特征: ", best_vars, "\n")

# 根据选择的最佳特征构建数据集
trainData_best <- trainData[, c("abundance_water", best_vars)]
testData_best <- testData[, best_vars]

# 验证新训练集和测试集的结构
str(trainData_best)
str(testData_best)

# 使用留一法交叉验证
trainControl_loocv <- trainControl(method = "LOOCV")

#trainControl_cv <- trainControl(method = "cv", number = 10)

# 调参
tuneGrid_rf <- expand.grid(mtry = c(2, 4, 6, 8, 10))

# 训练随机森林模型并进行调参
set.seed(123)
rf_tune <- train(abundance_water ~ ., data = trainData_best, method = "rf", 
                 tuneGrid = tuneGrid_rf, trControl = trainControl_loocv, ntree = 500)

# 查看调参结果
print(rf_tune)
plot(rf_tune)

# 最优参数
best_mtry <- rf_tune$bestTune$mtry
cat("最优mtry值: ", best_mtry, "\n")

# 手动优化ntree和nodesize参数
ntree_values <- c(100,200,300,400,500,600,700,800,900,1000)
nodesize_values <- c(1,2,3,4,5,6,7,8,9,10)

best_rf_model <- NULL
best_rf_mse <- Inf

for (ntree in ntree_values) {
  for (nodesize in nodesize_values) {
    set.seed(123)
    rf_model <- randomForest(
      abundance_water ~ ., 
      data = trainData_best, 
      ntree = ntree, 
      mtry = best_mtry, 
      nodesize = nodesize, 
      importance = TRUE
    )
    
    train_predictions <- predict(rf_model, newdata = trainData_best)
    mse <- MSE(train_predictions, trainData_best$abundance_water)
    
    if (mse < best_rf_mse) {
      best_rf_mse <- mse
      best_rf_model <- rf_model
      best_ntree <- ntree
      best_nodesize <- nodesize
    }
  }
}

cat("最优ntree值: ", best_ntree, "\n")
cat("最优nodesize值: ", best_nodesize, "\n")

# 最优随机森林模型
print(best_rf_model)

# 随机森林模型评估
rf_predictions_optimized <- predict(best_rf_model, newdata = trainData_best)
rf_mse_optimized <- MSE(rf_predictions_optimized, trainData_best$abundance_water)
rf_rmse_optimized <- RMSE(rf_predictions_optimized, trainData_best$abundance_water)
rf_mae_optimized <- MAE(rf_predictions_optimized, trainData_best$abundance_water)
rf_r2_optimized <- R2_Score(rf_predictions_optimized, trainData_best$abundance_water)
rf_adj_r2_optimized <- 1 - (1 - rf_r2_optimized) * (nrow(trainData_best) - 1) / (nrow(trainData_best) - length(best_vars) - 1)

cat("随机森林模型优化后的训练集上的MSE: ", rf_mse_optimized, "\n")
cat("随机森林模型优化后的训练集上的RMSE: ", rf_rmse_optimized, "\n")
cat("随机森林模型优化后的训练集上的MAE: ", rf_mae_optimized, "\n")
cat("随机森林模型优化后的训练集上的R平方值: ", rf_r2_optimized, "\n")
cat("随机森林模型优化后的训练集上的调整后R平方值: ", rf_adj_r2_optimized, "\n")

# 在测试集上进行预测
rf_test_predictions_optimized <- predict(best_rf_model, newdata = testData_best)
testData$RF_Predicted_ResistanceRisk_Optimized <- rf_test_predictions_optimized

# 保存随机森林特征重要性
importance_rf <- varImp(best_rf_model, scale = FALSE)
write.csv(best_rf_model$importance, "RF_Feature_Importance.csv", row.names = TRUE)

# 保存最佳随机森林模型
saveRDS(best_rf_model, "best_random_forest_model.rds")


# 计算Shapley值
predictor <- Predictor$new(best_rf_model, data = trainData_best[, -1], y = trainData_best$abundance_water)

# 针对每个样本计算Shapley值
shapley_values <- list()
for (i in 1:nrow(trainData_best)) {
  shapley <- Shapley$new(predictor, x.interest = trainData_best[i, -1])
  shapley_values[[i]] <- shapley$results
}
shapley_data <- do.call(rbind, shapley_values)
# 可视化 Shapley 值
# 将Shapley值汇总以获得每个特征的平均贡献
shapley_data <- shapley_data %>%
  mutate(Direction = ifelse(phi >= 0, "Positive", "Negative"))



# 保存Shapley值
write.csv(shapley_data, "Shapley_Values.csv", row.names = FALSE)


write.csv(testData, "全球预测结果_最终.csv", row.names = FALSE)

cat("脚本运行结束,所有结果已保存。")
