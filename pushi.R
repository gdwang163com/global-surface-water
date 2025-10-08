rm(list = ls()) 
library(ggplot2)
library(vegan)
library(ggpubr)
setwd("E:/shuinew/qd/pshi/zong//")
# 读取数据
data1 <- read.delim('tax.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE,quote = '')
data2 <- read.delim('arg.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE,quote = '')
group_data <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE,quote = '')
varespec<-t(data1)
varechem<-t(data2)


# 设置自定义颜色
#custom_colors <- c(Lake="#00447E",River="#F34800",Pond="#930026",Reservoir="#3ec1d3")
custom_colors=c(Sediment="#784101",Water="#00BffF")
# 对数据进行降维分析
spe.dist <- vegdist(varespec) # 默认Bray-Curtis
env.dist <- vegdist(varechem)

# NMDS分析
mds.s <- monoMDS(spe.dist)
mds.e <- monoMDS(env.dist)

# 进行Procrustes分析
pro.s.e <- procrustes(mds.s, mds.e, symmetric = TRUE)

set.seed(1)
pro.s.e_t <- protest(mds.s, mds.e, permutations = 999)

# 提取检验结果
pro_test_result <- pro.s.e_t$signif

# 绘制Procrustes分析结果
Pro_Y <- cbind(data.frame(pro.s.e$Yrot), data.frame(pro.s.e$X))
Pro_X <- data.frame(pro.s.e$rotation)

Pro_Y <- cbind(Pro_Y, group_data)

plot_title <- "Correlation between community and environment"

p <- ggplot(data = Pro_Y) +
  geom_segment(aes(x = X1, y = X2, xend = (X1 + MDS1)/2, yend = (X2 + MDS2)/2), 
               arrow = arrow(length = unit(0, 'cm')), color = "gray", size = 0.8) +
  geom_segment(aes(x = (X1 + MDS1)/2, y = (X2 + MDS2)/2, xend = MDS1, yend = MDS2), 
               arrow = arrow(length = unit(0.2, 'cm')), color = "gray", size = 0.8) +
  geom_point(aes(X1, X2, color = Region), size = 2, shape = 17) +
  geom_point(aes(MDS1, MDS2, color = Region), size = 2, shape = 16) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(color = 'black', fill = 'transparent'),
        legend.key = element_rect(fill = 'transparent'),
        axis.ticks.length = unit(0.4,"lines"),
        axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(colour='black', size=14),
        axis.title.y=element_text(colour='black', size=14),
        axis.text=element_text(colour='black',size=12)) +
  labs(x = 'Dimension 1', y = 'Dimension 2', color = '') +
  #  labs(title = plot_title) +
  geom_vline(xintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
  geom_hline(yintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
  geom_abline(intercept = 0, slope = Pro_X[1,2] / Pro_X[1,1], size = 0.3) +
  geom_abline(intercept = 0, slope = Pro_X[2,2] / Pro_X[2,1], size = 0.3) +
  annotate('text', label = paste("Procrustes analysis:\nM2 =", round(pro.s.e$ss, 4), ", p-value <", round(pro_test_result, 4)),
           x = -0.1, y = 0.1, size = 4, hjust = 0) +
  scale_color_manual(values = custom_colors, name = "Group") +
  theme(plot.title = element_text(size = 14, colour = "black", hjust = 0.5, face = "bold"))


p 
