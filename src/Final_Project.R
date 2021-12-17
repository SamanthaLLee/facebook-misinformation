# Samantha Lee
# December 16 2021
# Final Project

library(ggplot2)
library(plotly)

setwd("~/R")

# Load data
fbdata <- read.csv("data/facebook-fact-check.csv")


# Manipulate data
fbdata$activity_count = fbdata$share_count + fbdata$reaction_count + fbdata$comment_count
fbdata$Rating_num = ifelse(fbdata$Rating == "mostly true", 3,
                           ifelse(fbdata$Rating == "mixture of true and false", 2,
                                  ifelse(fbdata$Rating == "mostly false", 1, 0)))

# Learn about data
head(fbdata)
nrow(fbdata)
ncol(fbdata)
summary(fbdata)

# Subset into categories of interest
nonpartisan <- subset(fbdata, Category == "mainstream")
liberal <- subset(fbdata, Category == "left")
conservative <- subset(fbdata, Category == "right")

link <- subset(fbdata, Post.Type == "link")
photo <- subset(fbdata, Post.Type == "photo")
text <- subset(fbdata, Post.Type == "text")
video <- subset(fbdata, Post.Type == "video")

mostly_true <- subset(fbdata, Rating_num == "3")
mixture <- subset(fbdata, Rating_num == "2")
mostly_false <- subset(fbdata, Rating_num == "1")
completely_false <- subset(fbdata, Rating_num == "0")

summary(nonpartisan)
summary(liberal)
summary(conservative)

summary(link)
summary(photo)
summary(text)
summary(video)

summary(mostly_true)
summary(mixture)
summary(mostly_false)
summary(completely_false)

# Plot data
p <- ggplot(data=fbdata, mapping = aes(x=Rating, y=activity_count, color=Category)) +
  geom_point(na.rm=T, size=3, shape=4, alpha = .5, position = "jitter")+
  scale_x_discrete(limits = c("no factual content", "mostly false", 
                              "mixture of true and false","mostly true"),
                   labels = c("No Factual \nContent", "Mostly False", 
                              "Mixture of True \nand False","Mostly True"))+
  ggtitle("The Spread of Facebook Misinformation")+
  ylab("Sum of Shares, Comments, and Reactions")+
  xlab("Truth Rating")+
  ylim(0, 150000)+
  scale_color_manual(name = "Partisanship",
                     values=c('blue','black', 'red'),
                     limits = c("left","mainstream","right"),
                     labels = c("Liberal", "Nonpartisan", "Conservative"))+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(plot.margin=unit(c(1,1,.5,1),"cm"))+
  theme(axis.title.x = element_text(margin = margin(t = 20)))+
  theme(axis.title.y = element_text(margin = margin(r = 20)))
p
ggplotly(p)

l <- ggplot(data=liberal, mapping = aes(x=Rating, y=activity_count, shape=Post.Type)) +
  geom_point(na.rm=T, size=3, alpha = .5,color="blue",position = "jitter")+
  scale_x_discrete(limits = c("no factual content", "mostly false", "mixture of true and false","mostly true"),
                   labels = c("No Factual \nContent", "Mostly False", "Mixture of True \nand False","Mostly True"))+
  ggtitle("The Spread of Facebook Misinformation - Liberal Media")+
  ylab("Sum of Shares, Comments, and Reactions")+
  xlab("Truth Rating")+
  ylim(0, 150000)+
  scale_shape_manual(name = "Media Type",
                     values=c(4, 1, 0, 6),
                     limits = c("photo","video","link","text"),
                     labels = c("Photo", "Video","Link","Text"))+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(plot.margin=unit(c(1,1,.5,1),"cm"))+
  theme(axis.title.x = element_text(margin = margin(t = 20)))+
  theme(axis.title.y = element_text(margin = margin(r = 20)))
l
ggplotly(l)

c <- ggplot(data=conservative, mapping = aes(x=Rating, y=activity_count, shape=Post.Type)) +
  geom_point(na.rm=T, size=3, alpha = .5,color="red",position = "jitter")+
  scale_x_discrete(limits = c("no factual content", "mostly false", "mixture of true and false","mostly true"),
                   labels = c("No Factual \nContent", "Mostly False", "Mixture of True \nand False","Mostly True"))+
  ggtitle("The Spread of Facebook Misinformation - Conservative Media")+
  ylab("Sum of Shares, Comments, and Reactions")+
  xlab("Truth Rating")+
  ylim(0, 150000)+
  scale_shape_manual(name = "Media Type",
                     values=c(4, 1, 0, 6),
                     limits = c("photo","video","link","text"),
                     labels = c("Photo", "Video","Link","Text"))+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(plot.margin=unit(c(1,1,.5,1),"cm"))+
  theme(axis.title.x = element_text(margin = margin(t = 20)))+
  theme(axis.title.y = element_text(margin = margin(r = 20)))
c
ggplotly(c)

m <- ggplot(data=nonpartisan, mapping = aes(x=Rating, y=activity_count, shape=Post.Type)) +
  geom_point(na.rm=T, size=3, alpha = .4,color="black",position = "jitter")+
  scale_x_discrete(limits = c("no factual content", "mostly false", "mixture of true and false","mostly true"),
                   labels = c("No Factual \nContent", "Mostly False", "Mixture of True \nand False","Mostly True"))+
  ggtitle("The Spread of Facebook Misinformation - Nonpartisan Media")+
  ylab("Sum of Shares, Comments, and Reactions")+
  xlab("Truth Rating")+
  ylim(0, 10000)+
  scale_shape_manual(name = "Media Type",
                     values=c(4, 1, 0, 6),
                     limits = c("photo","video","link","text"),
                     labels = c("Photo", "Video","Link","Text"))+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(plot.margin=unit(c(1,1,.5,1),"cm"))+
  theme(axis.title.x = element_text(margin = margin(t = 20)))+
  theme(axis.title.y = element_text(margin = margin(r = 20)))
m
ggplotly(m)

