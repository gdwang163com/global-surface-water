rm(list = ls()) 
setwd("E:/shuinew2/qd/cor/")
library(ggplot2)
library(tidyverse)
library(Hmisc)
library(psych)
library(reshape2)
phy <-read.table(file = "cor.txt", row.names = 1,check.names = F,sep="\t",quote="", header = T)
met <-read.table(file = "cor.txt", sep = "\t", header = T,row.names= 1,quote="")
cor <-corr.test(phy, met, method = "spearman",adjust= "none")
cmt <-cor$r
pmt <- cor$p
head(cmt)
head(pmt)
cmt.out<-cbind(rownames(cmt),cmt)

df <-melt(cmt,value.name= "cor")
df$pvalue <- as.vector(pmt)
df
cor_mat_up <- cmt
cor_mat_up[lower.tri(cor_mat_up, diag = T)] <- NA

cor_mat_up_long <- cor_mat_up %>% 
  as.data.frame() %>% 
  mutate(x = factor(rownames(cor_mat_up), levels = rownames(cor_mat_up))) %>% 
  pivot_longer(cols = !x, names_to = "y", values_to = "cor") %>% 
  mutate(y = factor(y, levels = colnames(cor_mat_up)))

p_mat_lower <- pmt
p_mat_lower[!lower.tri(p_mat_lower, diag = F)] <- 1

p_mat_lower_long <- p_mat_lower %>% 
  as.data.frame() %>% 
  mutate(x = factor(rownames(p_mat_lower), levels = rownames(cor_mat_up))) %>% 
  pivot_longer(cols = !x, names_to = "y", values_to = "p") %>% 
  mutate(y = factor(y, levels = colnames(cor_mat_up)))

# ?????Ա??ǣ?
p_mat_lower_long$p_sig <- as.character(symnum(p_mat_lower_long$p, 
                                              cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                              symbols = c("***", "**", "*", "")))


p<-ggplot() +
  # pֵ??????
  geom_point(data = p_mat_lower_long, 
             aes(x, y, color = -log10(p)),
             size = 8,
             shape = 15)+
  geom_text(data = p_mat_lower_long, 
            aes(x, y, label = p_sig))+
  # ??ɫ??
  scale_color_gradientn(colours = c("#ffffff", "#71bc68", "#c3972e", "#fb7f00"))+
  # ????ͼ??
  geom_point(data = cor_mat_up_long, aes(x, y, size = cor, fill = cor), 
             shape = 21, color = "#c4bcba")+
  # ??ɫ??
  scale_fill_gradient2(low = "#4178a7", mid = "#ffffff", high = "#e2201c")+
  # x????y??  չ
  scale_x_discrete(expand = c(0.025, 0.025))+
  scale_y_discrete(expand = c(0.025, 0.025), position = "right")+
  # ????
  geom_vline(aes(xintercept =seq(0.5, 50.5, 1)), color = "#bbbbbb")+
  geom_hline(aes(yintercept=seq(0.5, 50.5, 1)), color = "#bbbbbb")+
  # ???⣺
  xlab("")+
  ylab("")+
  theme_minimal()+theme(axis.text.y=element_text(size=8,face = "bold",color="black",angle=0)) +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 90,size=8,face = "bold",color="black"),
        legend.position = "top")+
  guides(fill = guide_colorbar("correlation", title.position = "top",
                               title.theme = element_text(face = "bold")),
         color = guide_colorbar("-log10(P value)", title.position = "top",
                                title.theme = element_text(face = "bold")),
         size = "none")
p

ggsave(p,file='p2.pdf',width = 6,height =6)

write.table(cor_mat_up_long,"cor_mat_up_long2.txt", row.names = F,
            col.names = T, sep = '\t', quote = F,na = '0')


write.table(p_mat_lower_long,"p_mat_lower_long2.txt", row.names = F,
            col.names = T, sep = '\t', quote = F,na = '0')

