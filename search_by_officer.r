list.of.packages <- c("ggplot2",'tidyr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(tidyr)

setwd('~/personal_projects/texas-dps/')
by_officer = read.csv('./results/search_by_officer.csv')

search_by_officer = by_officer[by_officer$HA_SEARCHED ==1,]

white_searches = search_by_officer[(search_by_officer$white == 'True'),]
nonwhite_searches = search_by_officer[(search_by_officer$white != 'True'),]

colnames(white_searches)[c(7,8)] = c('white_count','white_pct_searched')
colnames(nonwhite_searches)[c(7,8)] = c('nonwhite_count','nonwhite_pct_searched')

pct_search_by_officer = merge(white_searches[,c('HA_OFFICER_ID','sex','white_pct_searched','white_count')], 
      nonwhite_searches[,c('HA_OFFICER_ID','sex','nonwhite_pct_searched','nonwhite_count')],
      by=c('HA_OFFICER_ID','sex'))

pct_search_by_officer$white_interval = qnorm(.995)*sqrt((1 / pct_search_by_officer$white_count)*pct_search_by_officer$white_pct_searched*(1-pct_search_by_officer$white_pct_searched) )
pct_search_by_officer$nonwhite_interval = qnorm(.995)*sqrt((1 / pct_search_by_officer$nonwhite_count)*pct_search_by_officer$nonwhite_pct_searched*(1-pct_search_by_officer$nonwhite_pct_searched) )


f <- ggplot(data=pct_search_by_officer, aes(x = nonwhite_pct_searched, y = white_pct_searched))
f+geom_abline(slope=1,intercept=0)
#qplot(nonwhite_pct_searched, white_pct_searched, data=pct_search_by_officer) +
  #geom_point(aes(size=1/nonwhite_interval)) + 

ggplot() +
  layer(
    data=pct_search_by_officer, aes(x = nonwhite_pct_searched, y = white_pct_searched),
    geom= 'point', stat='identity', position='identity'
  ) +
  layer(
    geom_abline(slope=1,intercept = 0), stat ='identity', position='identity'
  )
  
point_plot <- ggplot(data=pct_search_by_officer,aes(nonwhite_pct_searched,white_pct_searched, colour=sex)) +
  geom_point()
point_plot +
  geom_abline(slope=1,intercept=0, colour='black')




