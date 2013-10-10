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

# ACS data. Lots of facts, but maybe the report can be copyrighted
# https://data.mo.gov/Census/1940-2010-Census-ACS-Council-District-1-Data/wdnq-qmt4?
# https://data.mo.gov/api/file_data/Qrn-OBDtt9fjLzeryhE8uytBNSB7ZndUOisWUwKeIRU?filename=0-0-1980To2010Census_And_2007-2011ACS_Profile_District1.pdf
catalog$acs <- grepl('ACS', catalog$title)

# Datasets that should have licenses
interesting <- subset(catalog, !public.domain & pdf & missouri & !traffic.data)
row.names(interesting) <- interesting$identifier
