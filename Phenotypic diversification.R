library(readxl)
ST <- read_excel("C:/Users/Si/Desktop/ST2.xlsx", sheet = "Sheet45")
attach(ST)
library(reshape2)
library(dplyr)
library(ggplot2)
library(scales)
melted <- melt(ST, id.vars=c("time", "type"))
grouped <- group_by(melted,time,type)
grouped$time = factor(grouped$time)
df = summarise(grouped, avg = mean(value), sd = sd(value))
df

plot = ggplot(df,aes(y=avg,x=time,group=type))+
  geom_point(size=12,pch=21,stroke=1.5)+
geom_line(size = 5, aes(color=type))+
  scale_color_manual(values=c("#c7e9c0","#238b45","#a6cee3"))+
 geom_errorbar(aes(
    ymin = avg-sd, 
    ymax = avg + sd),
    linewidth = 3, 
    width = 0.4, 
    position = "identity", colour= "black")

plot
plot +xlab("Time (day)")+
  ylab(~ paste("Number of cells ",(mL ^ -1)))+
  scale_y_continuous(
    limits = c(0,2.3*10^5),
     breaks = seq(0, 2.3*10^5, by=5*10^4),
    label= function(x) {ifelse(x==0, "0", 
                               parse(text=gsub("[+]", "", 
                                             gsub("e", " %*% 10^", scientific_format()(x)))))})+  
theme_light()+
  theme(legend.title = element_blank(),legend.position = c(0.26, 0.92),
        legend.text=element_text(size=50))+
  theme(legend.key.size = unit(3,"line"))+  
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.text.y=element_text(size=54, colour = "black"),
        axis.text.x=element_text(size=38, colour = "black"),
        axis.title=element_text(size=65,colour = "black"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.6))+
  
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.ticks = element_line(size = 2,colour = "black"))+
  theme(axis.ticks.length=unit(0.4, "cm"))+
  guides(color = guide_legend(
    override.aes=list(shape = 20)))
ggsave('C:/Users/Si/Desktop/ggplot05.jpeg',width =45, height = 40, units ="cm", dpi = 300)


#The ratio of phenotypes over time

library(readxl)
ST <- read_excel("C:/Users/Si/Desktop/ST2.xlsx", sheet = "Sheet44")
attach(ST)
library(reshape2)
library(dplyr)
melted <- melt(ST, id.vars=c("salinity", "length"))
grouped <- group_by(melted, salinity, length)
grouped$salinity = factor(grouped$salinity)
df = summarise(grouped, avg = mean(value), sd = sd(value))
df
df$y_pos = NA
df$y_pos[df$length == "Non-mobile"] = df$avg[df$length == "Non-mobile"]
df$y_pos[df$length == "Mobile"] = df$avg[df$length == "Non-mobile"] + 
  df$avg[df$length == "Mobile"]
library(ggplot2)
library(scales)
plot = ggplot(df,aes(fill=length,y= avg,x=salinity))+
  geom_bar(stat="identity", color="black",size=1)+
  geom_errorbar(
    aes(ymax = y_pos + sd, ymin=y_pos), 
    position = "identity", width = 0.2, size=1)+
  scale_fill_manual(values = c("#c7e9c0","#238b45"))   
plot

plot + 
  xlab("Time (day)")+
  ylab("Proportion of each phenotype")+
  scale_y_continuous(
    limits = c(0,1.15),
   breaks = seq(0, 1.15, by=0.2))+
  theme_light()+
   theme(legend.position = c(0.16, 0.935),
         legend.text=element_text(size=50), legend.title=element_text(size=35))+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(legend.title = element_blank())+
  theme(axis.text.y=element_text(size=54, colour = "black"),
        axis.text.x=element_text(size=38, colour = "black"),
        axis.title=element_text(size=65,colour = "black"))+
  theme(strip.text.x = element_text(size = 40),strip.text.y = element_text(size = 40))+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.ticks = element_line(size = 2,colour = "black"))+
  theme(axis.ticks.length=unit(0.4, "cm")) 
ggsave('C:/Users/Si/Desktop/ggplot05.jpg', width =40, height =40, units = "cm",dpi = 300)

# Cell size 
library(readxl)
ST <- read_excel("C:/Users/Si/Desktop/ST2.xlsx", sheet = "Sheet47")
attach(ST)
ST
library(reshape2)
library(dplyr)
library(ggplot2)
library(scales)
plot = ggplot(ST, aes(x=size, fill=number)) + geom_density(alpha=.8)+
        scale_fill_manual(values=c("#c7e9c0","#238b45"))+
        xlim(5,52)
        
plot + 
  xlab("Cell diameter (??m)")+
  ylab("Percent of cells")+
  theme_light()+
  theme(legend.position = c(0.83, 0.93),
        legend.text=element_text(size=50), legend.title=element_text(size=35))+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(legend.title = element_blank())+
  theme(axis.text=element_text(size=40, colour = "black"),
        axis.title=element_text(size=50,colour = "black"))+
  theme(strip.text.x = element_text(size = 40),strip.text.y = element_text(size = 40))+
  theme(axis.text.y=element_text(size=54, colour = "black"),
        axis.text.x=element_text(size=50, colour = "black"),
        axis.title=element_text(size=65,colour = "black"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.6))+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.ticks = element_line(size = 2,colour = "black"))+
  theme(axis.ticks.length=unit(0.4, "cm")) 
ggsave('C:/Users/Si/Desktop/ggplot06.jpg', width =40, height =40, units = "cm",dpi = 300)


# Cell size dynamics over time
library(readxl)
library(ggplot2)
library(ggridges)

ST <- read_excel("C:/Users/Si/Desktop/ST1.xlsx", sheet = "Sheet17")
attach(ST)
ST$number <- factor(ST$number)

plot = ggplot(
  ST, 
  aes(x = size, y = number, fill = stat(x))
) +
stat_density_ridges(
  geom = "density_ridges_gradient", calc_ecdf = TRUE,
  quantiles = 4, quantile_lines = TRUE
)+
   scale_fill_viridis_c(name = (~ paste("diameter ",(??m))), option = "C") +
   labs(title = '') 
  
plot + 
  
  xlab("Cell diameter (??m)")+
  ylab("Time (day)")+
  theme_light()+
  theme(legend.position = c(0.85, 0.16),
        legend.text=element_text(size=50), legend.title=element_text(size=40))+
  theme(axis.text=element_text(size=40, colour = "black"),
        axis.title=element_text(size=50,colour = "black"))+
  scale_y_discrete(expand = c(0.005,0))+
  theme(#panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.ticks = element_line(size = 1.2,colour = "black"))+
  theme(axis.ticks.length=unit(0.4, "cm"))+
  theme(legend.key.size = unit(1.5, 'cm'))+
  
  theme(axis.text.y=element_text(size=54, colour = "black"),
        axis.text.x=element_text(size=50, colour = "black"),
        axis.title=element_text(size=65,colour = "black"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.6))
ggsave('C:/Users/Si/Desktop/ggplot06.jpg', width =40, height =40, units = "cm",dpi = 300)
