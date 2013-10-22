library(sqldf)
library(ggplot2)

if (!('catalog' %in% ls())) {
  catalog <- sqldf('select * from catalog', dbname = '/tmp/catalog.db')
}

# Public domain
catalog$public.domain <- grepl('Public Domain', catalog$license, ignore.case = TRUE)

# table(missouri$public.domain, is.na(missouri$license))

# Missouri, because it has a lot of PDFs
catalog$missouri <- catalog$portal == 'data.mo.gov'

# PDFs
catalog$pdf <- grepl('pdf', catalog$format, ignore.case = TRUE)

# The traffic data really are traffic surveys. They're mostly tables, but there are some diagrams.
# https://data.mo.gov/Traffic/2012-Traffic-Data-St-John-Ave-and-Topping-Av-PED-X/f8qf-7is2?
# https://data.mo.gov/api/file_data/RUHsZRBZ1NEZZzmMODqg7pXeg911Vsj3ekdk0FovPhk?filename=STJOHNTOPPING_PED-X_12HR_2012B_TMC.pdf
catalog$traffic.data <- grepl('traffic data', catalog$title, ignore.case = TRUE) |
  grepl('^[A-Z0-9]* 1?[0-9] HR 20[0-9][0-9] [AB] (TMC|PCW)$', catalog$title)

# Travel to work patterns! Maybe just facts
# https://data.mo.gov/Transportation/1990-Travel-to-Work-Patterns-Summary-Example-Repor/phgg-ybif?
# https://data.mo.gov/api/file_data/fV-nOsd13nYV6SHemxLDPh0w-bSb41ulDTHn3LcZiQc?filename=0-1990_Travel_To_Work_Summary_KCMetro.pdf
catalog$travel.to.work <- grepl('Travel to Work Patterns', catalog$title)

# ACS data. Lots of facts, but maybe the report can be copyrighted
# https://data.mo.gov/Census/1940-2010-Census-ACS-Council-District-1-Data/wdnq-qmt4?
# https://data.mo.gov/api/file_data/Qrn-OBDtt9fjLzeryhE8uytBNSB7ZndUOisWUwKeIRU?filename=0-0-1980To2010Census_And_2007-2011ACS_Profile_District1.pdf
catalog$acs <- grepl('ACS|American Community Survey', catalog$title)

# Building permits. Like ACS: Facts in a report
# c5sq-3hcq
# https://data.mo.gov/api/file_data/x2xCMkYrlmTZvnKRWIHffUu8xuYLvI8WzlZ0JYJLBJo?filename=Building_Permits_Issued_January_to_December_2006.pdf
catalog$building.permits <- grepl('Building Permit', catalog$title)

# Census Block Maps.
# These might be produced by federal government. In this case, they are public domain.
# But we don't know! Also, Missouri could have slightly modified the maps, in which case
# they might not be public domain anymore.
# https://data.mo.gov/Census/2010-Census-Predominant-Ethnicity-by-Census-Block-/6sa7-c4bx?
catalog$census.block.map <- grepl('Census Block', catalog$title)
catalog$census <- grepl('Census', catalog$title)






# Datasets that should have licenses
interesting <- subset(catalog, !public.domain & pdf & missouri &
  !traffic.data & !travel.to.work & !acs & !building.permits & !census.block.map)
row.names(interesting) <- interesting$identifier

# Things that look like reports, meaning a bit more than just facts
# https://data.mo.gov/Census/2000-Population-of-KC-Metro-Area-Cities-and-Counti/wxxx-rjng?
# https://data.mo.gov/api/file_data/feUI8VEC41FnlQ60tXzeOegxMrawSkDffk45-W0Rpd8?filename=2000MetroPlacesReport.pdf
interesting$report <- grepl('^20[0-9][0-9]', interesting$title)

subset(interesting, !report)['title']




# Missouri statistics
missouri <- subset(catalog, missouri)

missouri$expenditures <- grepl('Expenditures',missouri$title) & grepl('20[0-9]{2}',missouri$title)
missouri$employee.pay <- grepl('Employee Pay',missouri$title) & grepl('20[0-9]{2}',missouri$title)
missouri$grants <- grepl('Grant Revenues',missouri$title) & grepl('20[0-9]{2}',missouri$title)
missouri$unemployment <- grepl('Unemployment',missouri$title)
missouri$liquor.licenses <- grepl('(Liquor|Alcohol)', missouri$title)
missouri$injury <- grepl('First Reports of Injury', missouri$title)
missouri$says.missouri <- grepl('Missouri', missouri$title)

a <- subset(missouri, !public.domain & !pdf & !traffic.data & !travel.to.work & !acs & !building.permits & !census &
  !expenditures & !employee.pay & !grants & !unemployment & !liquor.licenses & !injury & !says.missouri)

.binary <- c('pdf','traffic.data','travel.to.work','acs','building.permits','census.block.map','public.domain',
  'expenditures','employee.pay','grantsa','unemployment','liquor.licenses','injury','says.missouri')
for (i in .binary) {
  missouri[,i] <- factor(missouri[,i], levels = c(TRUE, FALSE))
  levels(missouri[,i]) <- c('Yes','No')
}

cross.tabulations <- as.data.frame.table(xtabs(~ traffic.data + travel.to.work + acs + building.permits + census.block.map + pdf + public.domain + expenditures + employee.pay + grants + unemployment + liquor.licenses + injury, data = missouri))
cross.tabulations.2 <- as.data.frame.table(xtabs(~ traffic.data + travel.to.work + acs + building.permits + census.block.map + pdf + public.domain + expenditures + employee.pay + grants + unemployment + liquor.licenses + injury + says.missouri, data = missouri))


# Make this table really fancy with bar graphs on the side
cross.tabulations.sparse <- subset(cross.tabulations, Freq > 0)[c((ncol(cross.tabulations)-1):1, ncol(cross.tabulations))]


#
p1 <- ggplot(missouri) + aes(x = pdf, fill = public.domain) + geom_bar() +
  scale_x_discrete('PDF file?') + scale_y_continuous('Number of datasets')
  ggtitle('PDF datasets in the public domain are quite common')

p2 <- ggplot(missouri) + aes(x = traffic.data, fill = pdf) + geom_bar() +
  scale_fill_discrete('PDF file?') + scale_y_continuous('Number of datasets') +
  scale_x_discrete('Traffic survey?') +
  ggtitle('Most of the PDF files on data.mo.gov are traffic surveys.')

# All-no in cross-tabulation
more.interesting <- subset(catalog, missouri & !public.domain & !pdf & !census.block.map & !building.permits & !acs & !travel.to.work & !traffic.data)
