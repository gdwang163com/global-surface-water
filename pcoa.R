
rm(list = ls()) 
setwd("E:/shuinew2/arg/pcoa/")
# Load package
library(vegan)
library(ggplot2)
library(ggthemes)
# Load data
otu <- read.delim('plot2.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE,quote = '')
otu <- data.frame(otu)


bray_dis <- vegdist(otu, method = 'jaccard',na.rm=TRUE)   ##jaccard   bray

pcoa <- cmdscale(bray_dis, k = (nrow(otu) - 1), eig = TRUE)
site <- data.frame(pcoa$point)[1:2]
site$name <- rownames(site)

map<-read.table('group.txt',header=T,sep="\t",row.names=1)

merged=merge(site,map,by="row.names",all.x=TRUE)

dune.div <- adonis2(otu ~ Group, data = map, permutations = 999, method="jaccard") ### Continent
dune.div
dune.div$R2
dune_adonis <- paste0("P-value< ", dune.div$'Pr(>F)')
#dune_adonis <- paste0("adonis R2: ",round(dune.div$R2,2), "; P-value: ", dune.div$'Pr(>F)')
pcoa_exp <- pcoa$eig/sum(pcoa$eig)
pcoa1 <- paste('PCoA axis1 :', round(100*pcoa_exp[1], 2), '%')
pcoa2 <- paste('PCoA axis2 :', round(100*pcoa_exp[2], 2), '%')


library(ggplot2)
library(ggrepel)

#color = c(Sediment="#784101",Water="#00BffF")
color<-c(Lake="#00447E",River = "#42d4f4")
#color=c(Lake="#00447E",River="#F34800",Pond="#930026",Reservoir="#3ec1d3")
shapes <- c(Water = 19, Sediment =17) 
p<-ggplot(data=merged,aes(x=X1,y=X2,color=Group))+
  theme_bw()+
  geom_point(aes(color=Group,shape = Env),size=1)+
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0,lty="dashed",size=0.8)+
  geom_hline(yintercept = 0,lty="dashed",size=0.8)+
  labs(x = pcoa1, y = pcoa2,title =dune_adonis )+
  scale_color_manual(values = color) +
  scale_shape_manual(values = shapes)+
  theme_classic(base_size = 20)+
  theme(legend.title=element_text(color="black",face="bold",size=12))+ 
  theme(legend.text=element_text(face="bold", color="black",size=12))+
  guides(color=guide_legend(override.aes = list(size=6,alpha=1)))
p
p1<-p+stat_ellipse(data=merged,
                   geom = "polygon",level=0.9,
                   linetype = 1,size=1,
                   aes(fill=Group),
                   alpha=0.05,
                   show.legend = T)+
  scale_fill_manual(values = c(Lake="#00447E",River = "#42d4f4",Pond="#930026",Reservoir="#3ec1d3"))

p1



library(pairwiseAdonis)
dune.pairwise.adonis <- pairwise.adonis(x=otu, factors=map$Env,  ##Group
                                        sim.function = "vegdist",
                                        sim.method = "jaccard",
                                        p.adjust.m = "fdr",
                                        reduce = NULL,
                                        perm = 999)

library(ggpubr)
library(patchwork)
library(tidyverse)
tab2 <- ggtexttable(dune.pairwise.adonis[,c("pairs","R2","p.value","p.adjusted")], rows = NULL, 
                    theme = ttheme("blank")) %>% 
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 1)  %>% 
  tab_add_hline(at.row = nrow(dune.pairwise.adonis)+1, row.side = "bottom", linewidth = 1) 

write.table(dune.pairwise.adonis,"env2pvalue.txt", row.names = F,
            col.names = T, sep = '\t', quote = F,na = 'NA')


