library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(plotly)
library(corrgram)
library(corrplot)
library(sp)
library(sf)
library(leaflet)
library(tidyverse)
library(stringr)
library(here)
library(widgetframe)
library(htmltools)
library(htmlwidgets)
library(tmap)
library(rgdal)
library(caret)
library(ggpubr)
library(car)
library(plyr)
library(PerformanceAnalytics)
library(ggcorrplot)
library(naniar)
library(grid)
library(mice)
library(egg)

cl1 <- read.csv("WQ_CL1.csv")
cl1$Result <- as.numeric(cl1$Result)
str(cl1)
cl3 <- read.csv("WQ_CL3.csv")
cl3$Result <- as.numeric(cl3$Result)
str(cl3)
cl4 <- read.csv("WQ_CL4.csv")
cl4$Result <- as.numeric(cl4$Result)
str(cl4)

##########################################################################
# Combine c samples - data prep
cl1$Sample.Code<-gsub("C","",as.character(cl1$Sample.Code))
cl3$Sample.Code<-gsub("C","",as.character(cl3$Sample.Code))
cl4$Sample.Code<-gsub("C","",as.character(cl4$Sample.Code))

##########################################################################
# Convert Depths - data prep

cl1$Depth_m <- ifelse(cl1$Year <= '1991', round((cl1$Depth * 0.3048),1), cl1$Depth)
cl3$Depth_m <- ifelse(cl3$Year <= '1991', round((cl3$Depth * 0.3048),1), cl3$Depth)
cl4$Depth_m <- ifelse(cl4$Year <= '1991', round((cl4$Depth * 0.3048),1), cl4$Depth)

cl1 <- cl1 %>% 
  mutate(Depth2 = ifelse(Depth_m >= 3, "Deep", "Surface"))
cl3 <- cl3 %>% 
  mutate(Depth2 = ifelse(Depth_m >= 3, "Deep", "Surface"))
cl4 <- cl4 %>% 
  mutate(Depth2 = ifelse(Depth_m >= 3, "Deep", "Surface"))

##########################################################################
# Drop NA depth - data prep
cl1.dropdepth <- cl1[!is.na(cl1$Depth2),]
cl3.dropdepth <- cl3[!is.na(cl3$Depth2),]
cl4.dropdepth <- cl4[!is.na(cl4$Depth2),]

##########################################################################
# Drop notes - data prep
cl1.nonotes <- select(cl1.dropdepth, -'Notes')
cl3.nonotes <- select(cl3.dropdepth, -'Notes')
cl4.nonotes <- select(cl4.dropdepth, -'Notes')

##########################################################################
cl1.filter <- filter(cl1.nonotes, Method != "EPA 352.1 (DWR Modified) [P/A]*")
cl3.filter <- filter(cl3.nonotes, Method != "EPA 352.1 (DWR Modified) [P/A]*")
cl4.filter <- filter(cl4.nonotes, Method != "EPA 352.1 (DWR Modified) [P/A]*")

##########################################################################
# Remove Duplicates - data prep

count(cl1.filter[duplicated(cl1.filter),])
dups1 <- cl1.filter[duplicated(cl1.filter),]
cl1.dup <- distinct(cl1.filter)

count(cl3.filter[duplicated(cl3.filter),])
dups3 <- cl3.filter[duplicated(cl3.filter),]
cl3.dup <- distinct(cl3.filter)

count(cl4.filter[duplicated(cl4.filter),])
dups4 <- cl4.filter[duplicated(cl4.filter),]
cl4.dup <- distinct(cl4.filter)
##########################################################################

cl1.select <- select(cl1.dup, Sample.Code, Date, Analyte, Result, Depth2, Long.Station.Name)
cl3.select <- select(cl3.dup, Sample.Code, Date, Analyte, Result, Depth2, Long.Station.Name)
cl4.select <- select(cl4.dup, Sample.Code, Date, Analyte, Result, Depth2, Long.Station.Name)

##########################################################################
# Format Date - data prep

cl1.select$Date <- as.Date(cl1.select$Date, format =  "%m/%d/%Y")
cl3.select$Date <- as.Date(cl3.select$Date, format =  "%m/%d/%Y")
cl4.select$Date <- as.Date(cl4.select$Date, format =  "%m/%d/%Y")

##########################################################################
#2004 -> 2020 Time Period

cl1.post.2004 <- filter(cl1.select, Date >= "2004-10-28")
cl3.post.2004 <- filter(cl3.select, Date >= "2004-10-28")
cl4.post.2004 <- filter(cl4.select, Date >= "2004-10-28")

##########################################################################
# move data from long to wide - data prep

cl1.table.2004 <- pivot_wider(cl1.post.2004, names_from = Analyte, values_from = Result,
                              values_fn = list(Result = mean))

cl3.table.2004 <- pivot_wider(cl3.post.2004, names_from = Analyte, values_from = Result,
                              values_fn = list(Result = mean))

cl4.table.2004 <- pivot_wider(cl4.post.2004, names_from = Analyte, values_from = Result,
                              values_fn = list(Result = mean))

names(cl1.table.2004) <- gsub(" ", ".", names(cl1.table.2004))
names(cl1.table.2004) <- gsub("-", ".", names(cl1.table.2004))

names(cl3.table.2004) <- gsub(" ", ".", names(cl3.table.2004))
names(cl3.table.2004) <- gsub("-", ".", names(cl3.table.2004))

names(cl4.table.2004) <- gsub(" ", ".", names(cl4.table.2004))
names(cl4.table.2004) <- gsub("-", ".", names(cl4.table.2004))

##########################################################################
#CL NA Graphs
cl1.df.2004.graph <- cl1.table.2004[,order(colnames(cl1.table.2004))]
cl3.df.2004.graph <- cl3.table.2004[,order(colnames(cl3.table.2004))]
cl4.df.2004.graph <- cl4.table.2004[,order(colnames(cl4.table.2004))]


cl1.na.2004.post <- vis_miss(cl1.table.2004)+
  ggtitle("CL1 (2004 - 2020)")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  theme(legend.position = "right")
cl1.na.2004.post

cl3.na.2004.post <- vis_miss(cl3.table.2004)+
  ggtitle("CL3 (2004 - 2020)")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  theme(legend.position = "right")
cl3.na.2004.post

cl4.na.2004.post <- vis_miss(cl4.table.2004)+
  ggtitle("CL4 (2004 - 2020)")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position = "right")+
  scale_x_discrete(position = "bottom")
cl4.na.2004.post

total.cl.graph <- egg::ggarrange(cl1.na.2004.post, cl3.na.2004.post, cl4.na.2004.post,
                                 ncol = 1)
total.cl.graph

##########################################################################
#CL1 Post 2004
cl1.proc.2004 <- select(cl1.table.2004, -c('*No.Lab.Analyses.(Field.Measures.Only)','Sample.Code', 'Date', 'Depth2', 
                                           'Long.Station.Name', 'pH'))
cl1.df.2004 <- as.data.frame(cl1.proc.2004)
summary(cl1.df.2004)
names(cl1.df.2004)[names(cl1.df.2004) == 'Dissolved.Nitrate.+.Nitrite'] <- 'Dissolved.Nitrate.Nitrite'

#Imputation
cl1.post.2004.imp <- mice(cl1.df.2004, m=5, method = "pmm", maxit = 20)
summary(cl1.df.2004$Dissolved.Boron)
summary(cl1.post.2004.imp$imp$Dissolved.Boron)
summary(cl1.post.2004.imp)
final.clean.df.cl1.2004 <- complete(cl1.post.2004.imp, 1)
summary(final.clean.df.cl1.2004)

#Pre-Processing
process.cl1.2004 <-(preProcess (final.clean.df.cl1.2004, method = c("BoxCox", "center", "scale")))
preds.cl1.2004 <- predict(process.cl1.2004, newdata = final.clean.df.cl1.2004)
nearZeroVar(preds.cl1.2004)
preds.remove.cl1.2004 <- preds.cl1.2004[c( -34, -35)]
cor.cl1.2004 <- cor(preds.remove.cl1.2004, use = 'pairwise')

#Correlation Graphing
title.cl1.post <- "CL1 Post 2004"
corrplot(cor.cl1.2004, method = 'circle', type = "upper", na.label = "o", tl.col = 'black',
         order = "hclust", title = title.cl1.post, mar = c(0,0,1,0))

#Find Correlations
highCorr.cl1.post <- findCorrelation(cor.cl1.2004, cutoff = 0.70, names = TRUE)
highCorr.cl1.post

##########################################################################
#CL3 Post 2004
cl3.proc.2004 <- select(cl3.table.2004, -c('Sample.Code', 'Date', 'Depth2', 
                                           'Long.Station.Name'))
cl3.df.2004 <- as.data.frame(cl3.proc.2004)
summary(cl3.df.2004)
names(cl3.df.2004)[names(cl3.df.2004) == 'Dissolved.Nitrate.+.Nitrite'] <- 'Dissolved.Nitrate.Nitrite'

cl3.post.2004.imp <- mice(cl3.df.2004, m=5, method = "pmm",maxit = 20)
summary(cl3.df.2004$Dissolved.Boron)
summary(cl3.post.2004.imp$imp$Dissolved.Boron)
summary(cl3.post.2004.imp)
final.clean.df.cl3.2004 <- complete(cl3.post.2004.imp, 5)
summary(final.clean.df.cl3.2004)

process.cl3.2004 <- preProcess(final.clean.df.cl3.2004, method = "BoxCox", "center", "scale")
preds.cl3.2004 <- predict(process.cl3.2004, newdata = final.clean.df.cl3.2004)
nearZeroVar(preds.cl3.2004)
preds.remove.cl3.2004 <- preds.cl3.2004[c(-3)]
cor.cl3.2004 <- cor(preds.remove.cl3.2004, use = 'pairwise')

title.cl3.post <- "CL3 2004 - 2020"
corrplot(cor.cl3.2004, method = 'circle', type = "upper", na.label = "o", tl.col = 'black',
         order = "hclust", title = title.cl3.post, mar = c(0,0,1,0))

highCorr.cl3.post <- findCorrelation(cor.cl3.2004, cutoff = 0.70, names = TRUE)
highCorr.cl3.post
names(preds.remove.cl3.2004)
##########################################################################
#CL4 Post 2004

cl4.proc.2004 <- select(cl4.table.2004, -c('Sample.Code', 'Date', 'Depth2', 
                                           'Long.Station.Name'))
cl4.df.2004 <- as.data.frame(cl4.proc.2004)
summary(cl4.df.2004)
names(cl4.df.2004)[names(cl4.df.2004) == 'Dissolved.Nitrate.+.Nitrite'] <- 'Dissolved.Nitrate.Nitrite'

cl4.post.2004.imp <- mice(cl4.df.2004, m=5, method = "pmm", maxit = 20)
summary(cl4.df.2004$Dissolved.Lead)
summary(cl4.post.2004.imp$imp$Dissolved.Lead)
hist(cl4.df.2004$Dissolved.Lead)
summary(cl4.df.2004$Dissolved.Boron)
summary(cl4.post.2004.imp$imp$Dissolved.Boron)
final.clean.df.cl4.2004 <- complete(cl4.post.2004.imp, 5)
summary(final.clean.df.cl4.2004)

process.cl4.2004 <- preProcess(cl4.df.2004, method = c("BoxCox", "center", "scale"))
preds.cl4.2004 <- predict(process.cl4.2004, newdata = final.clean.df.cl4.2004)
nearZeroVar(preds.cl4.2004)
preds.remove.cl4.2004 <- preds.cl4.2004[c(-2, -3, -4, -8, -10)]
cor.cl4.2004 <- cor(preds.remove.cl4.2004, use = 'pairwise')

title.cl4.post <- "CL4 Post 2004"
corrplot(cor.cl4.2004, method = 'circle', type = "upper", na.label = "o", tl.col = 'black',
         order = "hclust", tl.cex = 1, title = title.cl4.post, mar = c(0,0,1,0))

highCorr.cl4.post <- findCorrelation(cor.cl4.2004, cutoff = 0.70, names = TRUE)
highCorr.cl4.post
