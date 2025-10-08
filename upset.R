# 清理工作区并设置工作目录
rm(list = ls()) 
setwd("E:/shuinew2/arg/veen/zong/")

# 加载必要的库
library(ggplot2)
library(ComplexUpset)

# 读取并预处理数据
movies1 <- read.table("up1.txt", sep = "\t", header = TRUE, check.names = FALSE, quote = "", row.names = 1)
genres <- colnames(movies1)
movies1[movies1 > 0] <- 1
movies1[genres] <- movies1[genres] == 1

# 将预处理后的数据写入新文件
write.table(movies1, "up2.txt", col.names = TRUE, sep = '\t', quote = FALSE, row.names = TRUE)

# 读取用于upset图的数据
movies <- read.table('up3.txt', header = TRUE, sep = '\t', quote = "")
# 创建简单的upset图
upset(
  movies, genres,
  base_annotations = list(
    'Intersection size' = intersection_size(counts = TRUE)
  )
)


# 定义颜色
CC <- c(
  aminoglycoside = '#e6194B', beta_lactam = '#dcbeff', bacitracin = '#3cb44b', chloramphenicol = '#4363d8',
  fosfomycin = '#f58231', MLS = "#D5E4A2FF", multidrug = '#ec748b', mupirocin = '#911eb4', polymyxin = '#075149FF', 
  quinolone = '#fabed4', rifamycin = '#469990', sulfonamide = '#9A6324', tetracycline = '#ffe119',
  trimethoprim = '#000075', novobiocin = '#42d4f4', Others = '#a9a9a9'
)

# 创建带颜色的upset图
upset(
  movies, genres,
  base_annotations = list(
    'Intersection size' = intersection_size(counts = TRUE, mapping = aes(fill = class)) +
      scale_fill_manual(values = CC)
  )
)


# 创建带复杂注释和主题的upset图
upset(
  movies, genres,
  base_annotations = list(
    'Intersection size' = intersection_size(
      text = list(vjust = -0.1, hjust = -0.1, angle = 45), counts = TRUE, mapping = aes(fill = class),
      text_colors = c(on_background = 'black', on_bar = 'black')
    ) + 
      annotate(
        geom = 'text', x = Inf, y = Inf, label = paste('Total:', nrow(movies)),
        vjust = 1, hjust = 1
      ) +
      theme(plot.background = element_rect(fill = 'white')) +
      scale_fill_manual(values = CC)
  ),
  set_sizes = (
    upset_set_size() +
      theme(axis.text.x = element_text(angle = 90))
  ),
  min_size = 5, stripes = c('cornsilk1', 'cornsilk1'),
  width_ratio = 0.1
)







