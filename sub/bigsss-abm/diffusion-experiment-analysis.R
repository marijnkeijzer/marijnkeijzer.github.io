################################################################################
##      _____ _____ _____    ___   ___ ___                                    ##
##     |  _  | __  |     |  |_  | |   |_  |             Keijzer               ##
##     |     | __ -| | | |   _| |_| | |_| |_            BIGSSS Summer School  ##
##     |__|__|_____|_|_|_|  |_____|___|_____|           July 7th, 2023        ##
##                                                                            ##
################################################################################

library('ggplot2')
setwd('/Users/marijnkeijzer/SURFdrive/nnc/ABM101')

   # read in raw data frame, note that we skip the first 6 lines containing contextual information
raw.df <- read.csv('diffusion experiment-table.csv',header=T,skip=6)

   # transform the absolute number of turtles that adopted the trait to a proportion
raw.df$count.turtles.with..color...green. <- raw.df$count.turtles.with..color...green. / raw.df$number.of.nodes

   # function to transform the raw dataset
DiffusionPercentiles <- function(Data=raw.df,Definition=10){
  df <- data.frame(id=c(99),rewiring=c(99),ticks=c(99),percentile=c(99),adopted=c(99))
  iterator <- 1
  
  for(i in unique(Data$X.run.number.)){
    increment <- max(Data$X.step.[Data$X.run.number==i]) * 1 / Definition
    
    for(j in 1:Definition){
      df[iterator,] <- c(
        i,
        Data$rewiring.proportion[Data$X.run.number.==i][1],
        max(Data$X.step.[Data$X.run.number.==i]),
        j,
        mean(Data$count.turtles.with..color...green.[(Data$X.step. > 0 - increment + increment * j) & Data$X.step. < increment * j & Data$X.run.number. == i]))
      iterator <- iterator + 1
    }
  }
  return(df)
}

df <- DiffusionPercentiles(Definition=12)

   # plot the adoption curve observed for different rewiring values
ggplot(data=df,aes(x=percentile,y=adopted)) +
  stat_summary(geom="line", fun.y=mean,col='red') +
  stat_summary(geom="errorbar", fun.data=mean_cl_normal,width=.5,col='red') +
  stat_summary(geom="point", fun.y=mean,size=1) +
  facet_wrap(rewiring ~ .,nrow=3) +
  ylim(0,1) +
  theme_linedraw()

   # plot the convergence times by rewiring
ggplot(data=df,aes(x=rewiring,y=ticks)) +
  stat_summary(geom="bar", fun.y=mean, col='black',fill='lightgrey') +
  stat_summary(geom="errorbar", fun.data=mean_cl_normal,width=.002,col='red') +
  theme_linedraw()


