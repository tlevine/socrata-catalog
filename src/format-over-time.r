library(sqldf)
library(ggplot2)
library(plyr)

datasets <- sqldf("SELECT portal, format, created FROM catalog ORDER BY created;", dbname = '/tmp/catalog.db')
datasets$format <- sub(';.*$', '', datasets$format)
datasets$format <- sub('^.*/', '', datasets$format)
datasets$format <- sub('^x-',  '', datasets$format)
datasets$format <- factor(datasets$format)

datasets$created <- as.Date(datasets$created)
datasets$csv <- datasets$format == 'csv'

# Proportion CSV by month
datasets$month <- factor(
  strftime(datasets$created, format = '%Y-%m'),
  levels = paste(2009:2013, rep(c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'), 5), sep = '-')
)
csv.monthly <- ddply(datasets, c('portal', 'month'), function(df) {
  c(
    prop.csv = mean(df$csv),
    count = nrow(df)
  )
})
p.monthly <- ggplot(monthly) + aes(x = month, y = prop.csv, group = portal, size = count) + geom_point()
