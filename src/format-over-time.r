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
p.monthly <- ggplot(csv.monthly) + aes(x = month, y = prop.csv, group = portal, size = count) + geom_point()

# Cumulative CSV and non-csv
csv.cum <- ddply(datasets, c('portal'), function(df) {
  df <- df[order(df$created),]
  df$count.csv <- cumsum(df$csv)
  df$count <- 1:nrow(df)
  df
})
csv.cum$prop.csv <- csv.cum$count.csv / csv.cum$count

p.cum <- ggplot(csv.cum) + aes(x = created, y = prop.csv, group = portal, size = count) + geom_line() +
  scale_x_date('Data') + scale_y_continuous('Proportion of datasets that are CSV') + scale_size_continuous('Datasets on the portal') +
  theme(title = element_text('Dataset formats by portal over time'))

