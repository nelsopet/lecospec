######Plots of prediction#########
library(tidyverse)
##Reads in dataframe with observed and predicted estimates
Vegplots<-read.csv("Outputs/2_HDW_imagery/2_Models/AK_imagery/Linearplots.csv")

##Lets get the r2values first
Graminoid_coef<-lm(Vegplots$Graminoid_A~Vegplots$Graminoid_P)
##Lets make a graph for each functional group
jpeg('Outputs/2_HDW_Imagery/2_Models/AK_imagery/Graminoid.jpg',width=1200, height=700)
plot(Graminoid_A~Graminoid_P,data=Vegplots,
     pch = 19,cex=3,
     col =Location,
     main = "Graminoid Estimates", cex.main=3,
     xlab ="Predicted",
     ylab ="Actual")
legend(
  xpd=T,
  "topleft",
  legend = c("EightMile","Chatnika"),
  fill =c("red","black"),
  border = FALSE,
  bty = "n",
  cex=2,
  xjust =3,
  horiz = FALSE,
  inset=c(-0.02,-0.13),
  par(cex=0.4))
legend("topright", bty="n", xpd = TRUE,legend=paste("R2 =", 
      format(summary(Graminoid_coef)$adj.r.squared, digits=4)),
      cex=2,
      inset=c(-0.01,-0.10))
##Add regression line
abline(coef(Graminoid_coef),lwd=3)
dev.off()

##Dwarf Shrub
##Lets get the r2values first
DwarfShrub_coef<-lm(Vegplots$DwarfShrub_A~Vegplots$DwarfShrub_P)
##Lets make a graph for each functional group
jpeg('Outputs/2_HDW_Imagery/2_Models/AK_imagery/DwarfShrub.jpg',width=1200, height=700)
plot(DwarfShrub_A~DwarfShrub_P,data=Vegplots,
     pch = 19,cex=2,
     col =Location,
     main = "DwarfShrub Estimates", cex.main=3,
     xlab ="Predicted",
     ylab ="Actual")
legend(
  xpd=T,
  "topleft",
  legend = c("EightMile","Chatnika"),
  fill =c("red","black"),
  border = FALSE,
  bty = "n",
  cex=2,
  xjust =3,
  horiz = FALSE,
  inset=c(-0.02,-0.13),
  par(cex=0.4))
legend("topright", bty="n", xpd = TRUE,legend=paste("R2 =", 
                                                    format(summary(DwarfShrub_coef)$adj.r.squared, digits=4)),
       cex=2,
       inset=c(-0.01,-0.10))
##Add regression line
abline(coef(DwarfShrub_coef),lwd=3)
dev.off()

##Moss
##Lets get the r2values first
Moss_coef<-lm(Vegplots$Moss_A~Vegplots$Moss_P)
##Lets make a graph for each functional group
jpeg('Outputs/2_HDW_Imagery/2_Models/AK_imagery/Moss.jpg',width=1200, height=700)
plot(Moss_A~Moss_P,data=Vegplots,
     pch = 19,cex=2,
     col =Location,
     main = "Moss Estimates", cex.main=3,
     xlab ="Predicted",
     ylab ="Actual")
legend(
  xpd=T,
  "topleft",
  legend = c("EightMile","Chatnika"),
  fill =c("red","black"),
  border = FALSE,
  bty = "n",
  cex=2,
  xjust =3,
  horiz = FALSE,
  inset=c(-0.02,-0.13),
  par(cex=0.4))
legend("topright", bty="n", xpd = TRUE,legend=paste("R2 =", 
                                                    format(summary(Moss_coef)$adj.r.squared, digits=4)),
       cex=2,
       inset=c(-0.01,-0.10))
##Add regression line
abline(coef(Moss_coef),lwd=3)
dev.off()

#Forb
##Lets get the r2values first
Forb_coef<-lm(Vegplots$Forb_A~Vegplots$Forb_P)
##Lets make a graph for each functional group
jpeg('Outputs/2_HDW_Imagery/2_Models/AK_imagery/Forb.jpg',width=1200, height=700)
plot(Forb_A~Forb_P,data=Vegplots,
     pch = 19,cex=2,
     col =Location,
     main = "Forb Estimates", cex.main=3,
     xlab ="Predicted",
     ylab ="Actual")
legend(
  xpd=T,
  "topleft",
  legend = c("EightMile","Chatnika"),
  fill =c("red","black"),
  border = FALSE,
  bty = "n",
  cex=2,
  xjust =3,
  horiz = FALSE,
  inset=c(-0.02,-0.13),
  par(cex=0.4))
legend("topright", bty="n", xpd = TRUE,legend=paste("R2 =", 
                                                    format(summary(Forb_coef)$adj.r.squared, digits=4)),
       cex=2,
       inset=c(-0.01,-0.10))
##Add regression line
abline(coef(Forb_coef),lwd=3)
dev.off()

##Lichen
##Lets get the r2values first
Lichen_coef<-lm(Vegplots$Lichen_A~Vegplots$Lichen_P)
##Lets make a graph for each functional group
jpeg('Outputs/2_HDW_Imagery/2_Models/AK_imagery/Lichen.jpg',width=1200, height=700)
plot(Lichen_A~Lichen_P,data=Vegplots,
     pch = 19,cex=2,
     col =Location,
     main = "Lichen Estimates", cex.main=3,
     xlab ="Predicted",
     ylab ="Actual")
legend(
  xpd=T,
  "topleft",
  legend = c("EightMile","Chatnika"),
  fill =c("red","black"),
  border = FALSE,
  bty = "n",
  cex=2,
  xjust =3,
  horiz = FALSE,
  inset=c(-0.02,-0.13),
  par(cex=0.4))
legend("topright", bty="n", xpd = TRUE,legend=paste("R2 =", 
                                                    format(summary(Lichen_coef)$adj.r.squared, digits=4)),
       cex=2,
       inset=c(-0.01,-0.10))
##Add regression line
abline(coef(Lichen_coef),lwd=3)
dev.off()

##Shrub
##Lets get the r2values first
Shrub_coef<-lm(Vegplots$Shrub_A~Vegplots$Shrub_P)
##Lets make a graph for each functional group
jpeg('Outputs/2_HDW_Imagery/2_Models/AK_imagery/Shrub.jpg',width=1200, height=700)
plot(Shrub_A~Shrub_P,data=Vegplots,
     pch = 19,cex=2,
     col =Location,
     main = "Shrub Estimates", cex.main=3,
     xlab ="Predicted",
     ylab ="Actual")
legend(
  xpd=T,
  "topleft",
  legend = c("EightMile","Chatnika"),
  fill =c("red","black"),
  border = FALSE,
  bty = "n",
  cex=2,
  xjust =3,
  horiz = FALSE,
  inset=c(-0.02,-0.13),
  par(cex=0.4))
legend("topright", bty="n", xpd = TRUE,legend=paste("R2 =", 
                                                    format(summary(Shrub_coef)$adj.r.squared, digits=4)),
       cex=2,
       inset=c(-0.01,-0.10))
##Add regression line
abline(coef(Shrub_coef),lwd=3)
dev.off()

##Tree
##Lets get the r2values first
Tree_coef<-lm(Vegplots$Tree_A~Vegplots$Tree_P)
##Lets make a graph for each functional group
jpeg('Outputs/2_HDW_Imagery/2_Models/AK_imagery/Tree.jpg',width=1200, height=700)
plot(Tree_A~Tree_P,data=Vegplots,
     pch = 19,cex=2,
     col =Location,
     main = "Tree Estimates", cex.main=3,
     xlab ="Predicted",
     ylab ="Actual")
legend(
  xpd=T,
  "topleft",
  legend = c("EightMile","Chatnika"),
  fill =c("red","black"),
  border = FALSE,
  bty = "n",
  cex=2,
  xjust =3,
  horiz = FALSE,
  inset=c(-0.02,-0.13),
  par(cex=0.4))
legend("topright", bty="n", xpd = TRUE,legend=paste("R2 =", 
                                                    format(summary(Tree_coef)$adj.r.squared, digits=4)),
       cex=2,
       inset=c(-0.01,-0.10))
##Add regression line
abline(coef(Tree_coef),lwd=3)
dev.off()