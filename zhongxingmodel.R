
# 清理工作环境中的所有对象
rm(list = ls())
setwd("E:/shuinew2/qd/pls/arg/")
library(devtools)
library(ggplot2)
library(ggimage)
library(plyr)
library(remotes)

#curl::has_internet()                    # TRUE 说明本机能上网
# 以 Clash/V2Ray 为例（默认监听端口 7890）
#Sys.setenv(http_proxy  = "http://127.0.0.1:7890",
#           https_proxy = "http://127.0.0.1:7890")
#httr::GET("https://api.github.com")  # 能成功返回 200 就正常
#remotes::install_github("Russel88/MicEco")
#options(timeout = 60000) 

library(MicEco)

# 读取数据
#read_table <- read.csv(file = "read_table.csv", row.names = 1)
data<-read.table("zong.txt",sep="\t", row.names = 1, header = T,stringsAsFactors = FALSE,quote='',na.strings = "NA")
# 运行中性模型分析
# 出现次数大于等于 2 的基因保留
read_table <- data[rowSums(data > 0) >= 2, ]
# 删除检测到基因数（非零基因数）过少的样本，比如少于10个
read_table <- read_table[, colSums(read_table > 0) >= 10 ]

#read_table <- as.data.frame(apply(table, 2, function(x){x/sum(x)}))

res <- neutral.fit(t(read_table))

m <- res[[1]][1]
N <- res[[1]][4]
Nm<- N*m
r2 <- res[[1]][3]
out <- res[[2]]

# 处理数据
out$group <- with(out, ifelse(freq < Lower, "#cf9198",  #509579
                              ifelse(freq > Upper, "#509579", "#485970")))  #cf9198

# 绘制模型结果图
p1 <- ggplot(data = out) +
  geom_line(aes(x = log(p), y = freq.pred), size = 1.2, linetype = 1) +
  geom_line(aes(x = log(p), y = Lower), size = 1.2, linetype = 2) +
  geom_line(aes(x = log(p), y = Upper), size = 1.2, linetype = 2) +
  geom_point(aes(x = log(p), y = freq, color = group), size = 2) +
  xlab("log10(mean relative abundance)") +
  ylab("Occurrence frequency") +
  scale_colour_manual(values = c("#485970",  "#509579","#cf9198")) +  # 应用新的颜色方案
  annotate("text", x = -4, y = 0.10, label = paste("Nm = ", round(Nm, 3)), size = 7) +
  annotate("text", x = -4, y = 0.20, label = paste("R² = ", round(r2, 3)), size = 7) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 18),
    legend.position = "none",
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 18),
    text = element_text(family = "sans", size = 18),
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # 添加图表边框
  )

write.table(out, "out_zongxing.txt", col.names = TRUE, sep = '\t', quote = FALSE, row.names = T)
p1
###########要是需要计算各分类占比可以使用在图中加入饼图，代码如下#########

# 计算饼图数据(下面不清晰的话可以参考)
#low = nrow(out[out[,6]== "#509579",])
#med = nrow(out[out[,6]== "#485970",])
#high = nrow(out[out[,6]== "#cf9198",])
#type <- c('med','high','low')
#nums <- c(med,high,low)
#df <- data.frame(type = type, nums = nums)

data_summary <- as.data.frame(table(out$group))
colnames(data_summary) <- c("group", "nums")
data_summary$type<-(c("med","high","low"))
data_summary$percentage <- round(data_summary$nums / sum(data_summary$nums) * 100, 1)
data_summary$label <- paste(data_summary$type, paste(data_summary$percentage, "%", sep = ''))

# 绘制饼图
p2 <- ggplot(data_summary, aes(x = "", y = nums, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(
    values = c("#485970","#509579","#cf9198"),  # 应用新的颜色方案
    labels = data_summary$label
  ) +
  theme_void() +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 18)
  )
p2

# 嵌入饼图到主图中
p_final <- p1 + geom_subview(subview = p2, x = -11.5, y = 0.6, w = 6, h = 6)

# 显示最终图形
print(p_final)
