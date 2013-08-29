library(sqldf)
library(ggplot2)
library(plyr)

datasets <- sqldf("SELECT portal, format, created FROM catalog ORDER BY created;", dbname = '/tmp/catalog.db')
datasets$format <- sub(';.*$', '', datasets$format)
datasets$format <- sub('^.*/', '', datasets$format)
datasets$format <- sub('^x-',  '', datasets$format)
datasets$format <- factor(datasets$format)

datasets$created <- as.Date(datasets$created)

# Proportion CSV over time
datasets$csv <- datasets$format == 'csv'
csv <- ddply(datasets, c('portal', 'csv'), function(df) {
  df$count <- 1:nrow(df)
  df
})

p <- ggplot(csv) + aes(x = created, y = count, group = portal, color = csv) + geom_line()
