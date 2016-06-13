list.of.packages <- c("ggplot2",'tidyr','stringr','dplyr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(tidyr)
library(stringr)
library(dplyr)


setwd('~/personal_projects/texas-dps/')
search_by_officer_loc = read.csv('./data/search_rates_by_officer_race.csv')

search_by_officer = read.csv('./data/officer_service_race_search.csv')
entries_per_officer = summarise(group_by(search_by_officer, officer_id), count = n())
dup_ids = entries_per_officer[entries_per_officer$count > 2,]$officer_id
trouble_rows = search_by_officer[search_by_officer$officer_id %in% dup_ids,]

head(trouble_rows[order(trouble_rows$officer_id),])
# there are some mislabled data apparently. 
# WHere ticket type and for_warning / for_citation don't match up



white_searches = search_by_officer[search_by_officer$white=='true',]
names(white_searches)[names(white_searches)=="cnt"]='white_cnt'
names(white_searches)[names(white_searches)=="interval"]='white_error'
names(white_searches)[names(white_searches)=="search_rate"]='white_search_rate'
names(white_searches)[names(white_searches)=="searched_cnt"]='white_searched_cnt'

nonwhite_searches = search_by_officer[search_by_officer$white=='false',]
names(nonwhite_searches)[names(nonwhite_searches)=="cnt"]='nonwhite_cnt'
names(nonwhite_searches)[names(nonwhite_searches)=="interval"]='nonwhite_error'
names(nonwhite_searches)[names(nonwhite_searches)=="search_rate"]='nonwhite_search_rate'
names(nonwhite_searches)[names(nonwhite_searches)=="searched_cnt"]='nonwhite_searched_cnt'

comp_search_rates = merge(white_searches[,c('officer_id','HA_SERVICE','HA_REGION','HA_DISTRICT','white_search_rate','white_cnt','white_error')], 
                               nonwhite_searches[,c('officer_id','HA_SERVICE','HA_REGION','HA_DISTRICT','nonwhite_search_rate','nonwhite_cnt','nonwhite_error')],
                              by=c('officer_id','HA_SERVICE','HA_REGION', 'HA_DISTRICT'))


comp_search_rates$search_rate_difference = comp_search_rates$nonwhite_search_rate - comp_search_rates$white_search_rate
comp_search_rates_sorted = comp_search_rates[order(-comp_search_rates$search_rate_difference),]


duplicates = comp_search_rates[duplicated(comp_search_rates$officer_id),]

write.csv(comp_search_rates_sorted, file='./results/comp_search_rates.csv')


### PLOTTING ###


point_plot <- ggplot(data=comparing_search_rates,aes(white_search_rate,nonwhite_search_rate, colour='green')) +
  geom_point()
point_plot +
  geom_abline(slope=1,intercept=0, colour='black')


p = ggplot(data=comp_search_rates, aes(nonwhite_search_rate,white_search_rate,colour=HA_SERVICE))
p = p+geom_point()
p = p + geom_abline(slope=1,intercept=0, colour='black')
p 

j1 = ggplot(data=comp_search_rates, aes(x=factor(HA_REGION), y=nonwhite_search_rate,colour=HA_SERVICE))
j1 = j1 + geom_boxplot()

j2 = ggplot(data=comp_search_rates, aes(x=factor(HA_REGION), y=white_search_rate,colour=HA_SERVICE))
j2 = j2 + geom_boxplot()

multiplot(j1, j2, cols=1)

k = ggplot(data=comp_search_rates, aes(x=search_rate_difference, y=officer_id, colour=HA_SERVICE))
k + geom_point()
                         


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}