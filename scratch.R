## Variables
years <- c(seq(1998, 2009))
files <- c('/media/backup/data/nis/NIS_1998_Core.ASC',
	'/media/backup/data/nis/NIS_1999_Core.ASC',
	'/media/backup/data/nis/NIS_2000_Core.ASC',
	'/media/backup/data/nis/NIS_2001_Core.ASC',
	'/media/backup/data/nis/NIS_2002_Core.ASC',
	'/media/backup/data/nis/NIS_2003_Core.ASC',
	'/media/backup/data/nis/NIS_2004_Core.ASC',
	'/media/backup/data/nis/NIS_2005_Core.ASC',
	'/media/backup/data/nis/NIS_2006_Core.ASC',
	'/media/backup/data/nis/NIS_2007_Core.ASC',
	'/media/backup/data/nis/NIS_2008_Core.ASC',
	'/media/backup/data/nis/NIS_2009_Core.ASC'
)



layouts <- getLayouts(layouts.uri = layouts.uri, years = years)
layouts <- addVariables(layouts = layouts, years = years)
layouts <- renameVariables(layouts = layouts, old = old, new = new)
layouts <- mergeLayouts(layouts)
layouts <- cleanLayouts(layouts, remove.capitalization = T)
result <- makeInfileQueries(years = years, files = files, db.table = "core", layouts = layouts)




years <- c(seq(1998, 2009))
files <- c('/media/backup/data/nis/NIS_2009_Core.ASC',
	'/media/backup/data/nis/NIS_2008_Core.ASC',
	'/media/backup/data/nis/NIS_2007_Core.ASC',
	'/media/backup/data/nis/NIS_2006_Core.ASC',
	'/media/backup/data/nis/NIS_2005_Core.ASC',
	'/media/backup/data/nis/NIS_2004_Core.ASC',
	'/media/backup/data/nis/NIS_2003_Core.ASC',
	'/media/backup/data/nis/NIS_2002_Core.ASC',
	'/media/backup/data/nis/NIS_2001_Core.ASC',
	'/media/backup/data/nis/NIS_2000_Core.ASC',
	'/media/backup/data/nis/NIS_1999_Core.ASC',
	'/media/backup/data/nis/NIS_1998_Core.ASC'
)

)

test <- generateNisSQL(years = years, files = files)





years <- c(seq(1998, 2009))
files <- c('/media/backup/data/nis/NIS_1998_Hospital.ASC',
	'/media/backup/data/nis/NIS_1999_Hospital.ASC',
	'/media/backup/data/nis/NIS_2000_Hospital.ASC',
	'/media/backup/data/nis/NIS_2001_Hospital.ASC',
	'/media/backup/data/nis/NIS_2002_Hospital.ASC',
	'/media/backup/data/nis/NIS_2003_Hospital.ASC',
	'/media/backup/data/nis/NIS_2004_Hospital.ASC',
	'/media/backup/data/nis/NIS_2005_Hospital.ASC',
	'/media/backup/data/nis/NIS_2006_Hospital.ASC',
	'/media/backup/data/nis/NIS_2007_Hospital.ASC',
	'/media/backup/data/nis/NIS_2008_Hospital.ASC',
	'/media/backup/data/nis/NIS_2009_Hospital.ASC'
)
layouts.uri <- list(
'2009' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2009_Hospital.TXT',
'2008' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2008_Hospital.TXT',
'2007' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2007_Hospital.TXT',
'2006' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2006_Hospital.TXT',
'2005' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2005_Hospital.TXT',
'2004' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2004_HOSPITAL.TXT',
'2003' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2003_HOSPITAL.TXT',
'2002' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2002_HOSPITAL.TXT',
'2001' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_2001_HOSPITAL.TXT',
'2000' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_2000_HOSPITAL.TXT',
'1999' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_1999_HOSPITALv2.TXT',
'1998' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_1999_HOSPITALv2.TXT')

test <- generateNisSQL(years = years, files = files, type = "hospitals")

layouts <- getLayouts(layouts.uri = layouts.uri, years = years, type = "hospital")
layouts <- addVariables(layouts = layouts, years = years)

layouts <- renameVariables(layouts = layouts, old = old, new = new)
layouts <- mergeLayouts(layouts)
layouts <- cleanLayouts(layouts, remove.capitalization = T)
result <- makeInfileQueries(years = years, files = files, db.table = "core", layouts = layouts)

test <- getLayouts(years = seq(2002, 2009), type = "severity")
test <- getLayouts(years = seq(2005, 2009), type = "group")

test <- generateNisSQL(years = seq(1998, 2009), files = files, type = "core")
test <- generateNisSQL(years = seq(1998, 2009), files = files, type = "hospitals")
test <- generateNisSQL(years = seq(2002, 2009), files = files, type = "severity")
test <- generateNisSQL(years = seq(2005, 2009), files = files, type = "groups")

