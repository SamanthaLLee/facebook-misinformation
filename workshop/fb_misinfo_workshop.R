# Install or import libraries
# install.packages("ggplot2")
# install.packages("plotly")
library(ggplot2)
library(plotly)

# Load data
fbdata <- read.csv("https://raw.githubusercontent.com/SamanthaLLee/facebook-misinformation/main/data/facebook-fact-check.csv")
summary(fbdata)
View(fbdata)

# Manipulate data
fbdata$activity_count = fbdata$share_count + fbdata$reaction_count + fbdata$comment_count

# Subset into categories of interest
nonpartisan <- subset(fbdata, Category == "mainstream")
liberal <- subset(fbdata, Category == "left")
conservative <- subset(fbdata, Category == "right")

# Plot data - part I
p <- ggplot(data=fbdata, mapping = aes(x=Rating, y=activity_count)) +
  geom_point(na.rm=T)+
  ggtitle("The Spread of Facebook Misinformation")+
  ylab("Sum of Shares, Comments, and Reactions")+
  xlab("Truth Rating")
p

# Plot data - part II
p <- ggplot(data=fbdata, mapping = aes(x=Rating, y=activity_count)) +
  geom_point(na.rm=T, position = "jitter")+
  scale_x_discrete(limits = c("no factual content", "mostly false", 
                              "mixture of true and false","mostly true"),
                   labels = c("No Factual \nContent", "Mostly False", 
                              "Mixture of True \nand False","Mostly True"))+
  ggtitle("The Spread of Facebook Misinformation")+
  ylab("Sum of Shares, Comments, and Reactions")+
  xlab("Truth Rating")+
  ylim(0, 150000)
p

# Plot data - part III
p <- ggplot(data=fbdata, mapping = aes(x=Rating, y=activity_count, color=Category)) +
  geom_point(na.rm=T, size=3, shape=4, alpha = .5, position = "jitter")+
  scale_x_discrete(limits = c("no factual content", "mostly false", 
                              "mixture of true and false","mostly true"),
                   labels = c("No Factual \nContent", "Mostly False", 
                              "Mixture of True \nand False","Mostly True"))+
  scale_color_manual(name = "Partisanship",
                     values=c('blue','black', 'red'),
                     limits = c("left","mainstream","right"),
                     labels = c("Liberal", "Nonpartisan", "Conservative"))+
  ggtitle("The Spread of Facebook Misinformation")+
  ylab("Sum of Shares, Comments, and Reactions")+
  xlab("Truth Rating")+
  ylim(0, 150000)
p

# Generate interactive plot
ggplotly(p)

# Plot liberal data 
l <- ggplot(data=liberal, mapping = aes(x=Rating, y=activity_count, shape=Post.Type)) +
  geom_point(na.rm=T, size=3, alpha = .5, color="blue", position = "jitter")+
  scale_x_discrete(limits = c("no factual content", "mostly false", "mixture of true and false","mostly true"),
                   labels = c("No Factual \nContent", "Mostly False", "Mixture of True \nand False","Mostly True"))+
  ggtitle("The Spread of Facebook Misinformation - Liberal Media")+
  ylab("Sum of Shares, Comments, and Reactions")+
  xlab("Truth Rating")+
  ylim(0, 150000)
l
ggplotly(l)

# Plot conservative data 
c <- ggplot(data=conservative, mapping = aes(x=Rating, y=activity_count, shape=Post.Type)) +
  geom_point(na.rm=T, size=3, alpha = .5, color="red" ,position = "jitter")+
  scale_x_discrete(limits = c("no factual content", "mostly false", "mixture of true and false","mostly true"),
                   labels = c("No Factual \nContent", "Mostly False", "Mixture of True \nand False","Mostly True"))+
  ggtitle("The Spread of Facebook Misinformation - Conservative Media")+
  ylab("Sum of Shares, Comments, and Reactions")+
  xlab("Truth Rating")+
  ylim(0, 150000)
c
ggplotly(c)

# Plot mainstream data 
m <- ggplot(data=nonpartisan, mapping = aes(x=Rating, y=activity_count, shape=Post.Type)) +
  geom_point(na.rm=T, size=3, alpha = .4, color="black" ,position = "jitter")+
  scale_x_discrete(limits = c("no factual content", "mostly false", "mixture of true and false","mostly true"),
                   labels = c("No Factual \nContent", "Mostly False", "Mixture of True \nand False","Mostly True"))+
  ggtitle("The Spread of Facebook Misinformation - Nonpartisan Media")+
  ylab("Sum of Shares, Comments, and Reactions")+
  xlab("Truth Rating")+
  ylim(0, 150000)
m
ggplotly(m)
