library(sqldf)
library(ggplot2)

datasets <- sqldf("SELECT portal, format, count(*) AS count FROM catalog GROUP BY portal, format;", dbname = '/tmp/catalog.db')
datasets$format <- sub(';.*$', '', datasets$format)
datasets$format <- sub('^.*/', '', datasets$format)
datasets$format <- sub('^x-',  '', datasets$format)


# p.formats <- ggplot(datasets) + aes(x = portal, y = count, group = format, fill = format) + geom_bar(stat = 'identity')
