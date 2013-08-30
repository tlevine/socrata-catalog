library(sqldf)
library(ggplot2)
library(plyr)
library(scales)

datasets <- sqldf("SELECT portal, format, created FROM catalog ORDER BY created;", dbname = '/tmp/catalog.db')
datasets$format <- sub(';.*$', '', datasets$format)
datasets$format <- sub('^.*/', '', datasets$format)
datasets$format <- sub('^x-',  '', datasets$format)
datasets$format <- factor(datasets$format)

datasets$created <- as.Date(datasets$created)
datasets$csv <- datasets$format == 'csv'

# Proportion CSV by month
datasets$month <- as.Date(paste0(strftime(datasets$created, format = '%Y-%m'), '-01'))
csv.monthly <- ddply(datasets, c('portal', 'month'), function(df) {
  c(
    prop.csv = mean(df$csv),
    count = nrow(df)
  )
})
p.monthly <- ggplot(csv.monthly) +
  aes(x = month, y = prop.csv, group = portal, size = count) + geom_point() +
  scale_x_date('Month') + scale_y_continuous('Proportion CSV') +
  scale_size_continuous('Number of datasets')

# Cumulative CSV and non-csv
csv.cum <- ddply(datasets, c('portal'), function(df) {
  df <- df[order(df$created),]
  df$count.csv <- cumsum(df$csv)
  df$count <- 1:nrow(df)
  df
})
csv.cum$prop.csv <- csv.cum$count.csv / csv.cum$count

p.csv.cum <- ggplot(csv.cum) + aes(x = created, y = prop.csv, group = portal, size = count) + geom_line() +
  scale_x_date('Data') + scale_y_continuous('Proportion of datasets that are CSV') + scale_size_continuous('Datasets on the portal') +
  theme(title = element_text('Dataset formats by portal over time'))
p.csv.cum.facet <- p.csv.cum + facet_wrap(~portal)

# Why does Missouri have so few CSVs?
data.mo.gov <- subset(datasets, portal == 'data.mo.gov')
data.mo.gov$format <- factor(data.mo.gov$format, levels = names(sort(table(data.mo.gov$format), decreasing = TRUE)))
p.data.mo.gov <- ggplot(data.mo.gov) + aes(x = format) + geom_bar()


# Cumulative CSV and PDF
datasets$csv.pdf <- datasets$format == 'csv' | datasets$format == 'pdf'
csv.pdf.cum <- ddply(datasets, c('portal'), function(df) {
  df <- df[order(df$created),]
  df$count.csv.pdf <- cumsum(df$csv.pdf)
  df$count <- 1:nrow(df)
  df
})
csv.pdf.cum$prop.csv.pdf <- csv.pdf.cum$count.csv.pdf / csv.cum$count

p.csv.pdf.cum <- ggplot(csv.pdf.cum) + aes(x = created, y = prop.csv.pdf, group = portal, size = count) + geom_line() +
  scale_x_date('Data') + scale_y_continuous('Proportion of datasets that are CSV or PDF') + scale_size_continuous('Datasets on the portal') +
  theme(title = element_text('Dataset formats by portal over time'))
p.csv.pdf.cum.facet <- p.csv.pdf.cum + facet_wrap(~portal)

# Hawaii
data.hawaii.gov <- subset(datasets, portal == 'data.hawaii.gov')
data.hawaii.gov$format <- factor(data.hawaii.gov$format, levels = names(sort(table(data.hawaii.gov$format), decreasing = TRUE)))
p.data.hawaii.gov <- ggplot(data.hawaii.gov) + aes(x = format) + geom_bar()

# Lehman College
bronx.lehman.cuny.edu <- subset(datasets, portal == 'bronx.lehman.cuny.edu')
bronx.lehman.cuny.edu$format <- factor(bronx.lehman.cuny.edu$format, levels = names(sort(table(bronx.lehman.cuny.edu$format), decreasing = TRUE)))
p.bronx.lehman.cuny.edu <- ggplot(bronx.lehman.cuny.edu) + aes(x = format) + geom_bar()

# All of them
# dlply(datasets, 'portal', function(df) {
#   df$format <- factor(df$format, levels = names(sort(table(df$format), decreasing = TRUE)))
#   gplot(df) + aes(x = format) + geom_bar() 
# })

# p.all <- ggplot(datasets) + aes(x = format) + geom_bar() + facet_wrap(~portal)
datasets$main.formats <- factor(datasets$format,
  levels = c('csv', 'pdf', 'msword', 'octet-stream', 'vnd.ms-excel', 'xml', 'other')
)
datasets$main.formats[is.na(datasets$main.formats)] <- 'other'

p.all <- ggplot(datasets) + aes(x = portal, fill = main.formats) +
  geom_bar() + scale_y_continuous('Number of datasets') +
  scale_x_discrete('Portal') + scale_fill_discrete('Dataset format') +
  coord_flip()

# San Francisco has external links.
data.sfgov.org <- subset(datasets, portal == 'data.sfgov.org')
data.sfgov.org$format <- factor(data.sfgov.org$format, levels = names(sort(table(data.sfgov.org$format), decreasing = TRUE)))
p.data.sfgov.org <- ggplot(data.sfgov.org) + aes(x = format) + geom_bar()

library(knitr)
knit('format-over-time.Rmd')
