
rm(list = ls()) 

setwd("E:/shuinew2/qd/lgd/")
# Load necessary libraries
library(ggplot2)

# Step 1: Load your data
# 假设数据文件为 'LGD_data.csv'，包含列 'latitude' 和 'ARG_richness'
data<-read.table("plotz.txt",sep="\t",  header = T,stringsAsFactors = FALSE,quote='',na.strings = "NA")
# Step 2: Add a new column 'hemisphere' based on the latitude values
# latitude > 0 为北半球，latitude < 0 为南半球
# Step 3: Fit a quadratic regression model to test the LGD pattern
# 二次多项式回归模型拟合LGD模式
lgd_model <- lm(ARG_richness ~ poly(Latitude, 2), data = data)

# Output model summary to check the significance
summary(lgd_model)

# Step 4: Plot the LGD pattern with hemisphere shape distinction
# 绘制LGD模式图，并用不同形状区分南北半球  "#784101"  "#00BffF"
p <- ggplot(data, aes(x = abs(Latitude), y = ARG_richness, shape = group)) +
  geom_point(size = 3, alpha = 0.7,color = "#00447E") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black", fill = "gray", se = TRUE
             ) +
  scale_shape_manual(values = c("Water" = 16, "Sediment" = 17)) +  # 16为圆点，17为三角形
  labs(x = "Absolute Latitude", y = "ARG Richness") +
  annotate("text", x = 60, y = max(data$ARG_richness) * 0.8, 
           label = paste("R² =", round(summary(lgd_model)$r.squared, 3),
                         "\np-value =", format(summary(lgd_model)$coefficients[3, 4], scientific = TRUE, digits = 3),
                         "\nAIC =", round(AIC(lgd_model), 1)),
           size = 5, hjust = 0) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16, color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# Display the plot
print(p)

# Step 5: Save the plot
# 保存图像
ggsave(p, file = "LGDhemisphere_shapeZZZ.pdf", width = 7, height = 5)
