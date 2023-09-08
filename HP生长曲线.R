library(readxl)
ST <- read_excel("C:/Users/Si/Desktop/ST2.xlsx", sheet = "Sheet45")
attach(ST)

#png('C:/Users/Si/Desktop/ggplot03.png', width= 50*400, height= 40*400, res=1300)
library(reshape2)
library(dplyr)
library(ggplot2)
library(scales)

melted <- melt(ST, id.vars=c("time", "type"))
grouped <- group_by(melted,time,type)
grouped$time = factor(grouped$time)
#grouped$salinity = factor(grouped$salinity)
#grouped$type <- factor(grouped$type, 
#                         levels = c("Total number","Mobile cell", 
#                                    "Non-mobile cell"))
df = summarise(grouped, avg = mean(value), sd = sd(value))
df




plot = ggplot(df,aes(y=avg,x=time,group=type))+
  geom_point(size=12,pch=21,stroke=1.5)+
  #facet_wrap(. ~ medium, 
 # scales = "free"
  #  )+
  
 # scale_y_continuous(limits = c(0,7*10^5),
    #breaks = seq(0, 10^6, by=2*10^7),
   #  expand = c(0, 0),label= function(x) {ifelse(x==0, "0", 
   # parse(text=gsub("[+]", "", 
  # gsub("e", " %*% 10^", scientific_format()(x)))))})+
  
  # scale_y_continuous(
  #limits = c(0,5*10^7),
  #breaks = seq(0, 5*10^7, by=10^7),
  #label= function(x) {ifelse(x==0, "0", parse(text=gsub("[+]", "", 
#  gsub("e", " %*% 10^", scientific_format()(x)))))})+

#scale_color_manual(values = c("BG11" = "#4daf4a", "ASP2" = "#ff7f00"))+
#geom_smooth(se=FALSE,method = 'loess',formula = 'y~x' ,aes(group=df$time))+
# scale_y_continuous(
#limits = c(0,1.3*10^8),
#   breaks = seq(1, 9, by=1))+
#scale_x_continuous(breaks = time)+
geom_line(size = 5, aes(color=type))+
  #geom_line(size = 2.5)+
  scale_color_manual(values=c("#c7e9c0","#238b45","#a6cee3"))+
  
 

  geom_errorbar(aes(
    ymin = avg-sd, 
    ymax = avg + sd),
    linewidth = 3, 
    width = 0.4, 
    position = "identity", colour= "black")
  
             

  # scale_fill_discrete(labels = c("ASP2","ASP2-N",expression(BG11[0]),"BG11", "BG11+NaCl"))
  
# +annotate("rect", xmin = "24", xmax = "72", ymin = 6, ymax = 7.2,
#          alpha = .3,fill = "grey")
plot

plot +xlab("Time (day)")+
  ylab(~ paste("Number of cells ",(mL ^ -1)))+
  #ylab(~ paste("Number of cells ", (mL ^ -1)))+
 # ylab(~ paste("Log(number of cells ", (mL ^ -1),")"))+
  #scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  scale_y_continuous(
    limits = c(0,2.3*10^5),
     breaks = seq(0, 2.3*10^5, by=5*10^4),
  #  #expand = c(0, 0),
    label= function(x) {ifelse(x==0, "0", 
                               parse(text=gsub("[+]", "", 
                                             gsub("e", " %*% 10^", scientific_format()(x)))))})+
  
  #scale_y_log10(
  #              breaks = trans_breaks("log10", function(x) 10^x), 
  #              labels = trans_format("log10", math_format(10^.x)))+
  # annotation_logticks(size=2, 
  #                     short = unit(0.2, "cm"),
  #                     mid = unit(0.4, "cm"),
  #                     long = unit(0.6, "cm"),
  #                     sides = "l")+ 
  #scale_y_discrete(labels=c("6","6.5","7","7.5","8"))+ 
  
theme_light()+
  #theme(legend.position = "none")+
  theme(legend.title = element_blank(),legend.position = c(0.26, 0.92),
        legend.text=element_text(size=50))+
  theme(legend.key.size = unit(3,"line"))+
  # guides(fill=guide_legend(title="Cell morphology"))+
  # theme(legend.position = c(0.14, 0.9),
  #      legend.text=element_text(size=30), legend.title=element_text(size=30))+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2))+
  #theme(legend.title = element_blank())+
  theme(axis.text.y=element_text(size=54, colour = "black"),
        axis.text.x=element_text(size=38, colour = "black"),
        axis.title=element_text(size=65,colour = "black"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.6))+
  
  
  
  #theme(legend.text=element_text(size=25))+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.ticks = element_line(size = 2,colour = "black"))+
  theme(axis.ticks.length=unit(0.4, "cm"))+
  guides(color = guide_legend(
    override.aes=list(shape = 20)))
#dev.off()
ggsave('C:/Users/Si/Desktop/ggplot05.jpeg',width =45, height = 40, units ="cm", dpi = 300)
