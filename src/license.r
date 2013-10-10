library(sqldf)

if (!('catalog' %in% ls())) {
  catalog <- sqldf('select * from catalog', dbname = '/tmp/catalog.db')
}

# Public domain
catalog$public.domain <- grepl('Public Domain', catalog$license, ignore.case = TRUE)

# Missouri, because it has a lot of PDFs
catalog$missouri <- catalog$portal == 'data.mo.gov'

# PDFs
catalog$pdf <- grepl('pdf', catalog$format, ignore.case = TRUE)

# The traffic data really are tables
# https://data.mo.gov/Traffic/2012-Traffic-Data-St-John-Ave-and-Topping-Av-PED-X/f8qf-7is2?
# https://data.mo.gov/api/file_data/RUHsZRBZ1NEZZzmMODqg7pXeg911Vsj3ekdk0FovPhk?filename=STJOHNTOPPING_PED-X_12HR_2012B_TMC.pdf
catalog$traffic.data <- grepl('traffic data', catalog$title, ignore.case = TRUE)

# Datasets that should have licenses
interesting <- subset(catalog, !public.domain & pdf & missouri & !traffic.data)
row.names(interesting) <- interesting$identifier
