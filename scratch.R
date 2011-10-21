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

setwd("c:/Users/jthetzel/Research/nis/")
source("c:/Users/jthetzel/Research/nis/pkg/R/generateSQL.R")

core <- generateSQL(years = years, files = files, type = "core", normalize.dx = T, normalize.pr = T)

cat(core$loadData, file = "loadData.sql")
cat(core$createTable, file = "createTable.sql")


layouts <- getLayouts(years = years, type = "core")
layouts <- addVariables(layouts = layouts, years = years)
layouts <- renameVariables(layouts = layouts, type = "core")
layouts <- mergeLayouts(layouts)
layouts <- cleanLayouts(layouts, remove.capitalization = T)
result <- makeInfileQueries(years = years, files = files, db.table = "core", layouts = layouts)


# normalization SQL
## remember to look into only up to dx15 is showing up
normalizeQuery <- function(year, file, db.table, layouts, pattern, names)
{
	row.key <- grep(pattern = "^key", layouts$variable, ignore.case = T, value = F)
	columns <- grep(year, names(layouts))
	variable.key<- layouts$variable[rows.key]
	start.key <- columns[row.key, 1]
	stop.key <- columns[row.key, 2]
	len.key <- stop.key - start.key + 1
	
	rows <- grep(pattern = pattern, layouts$variable, ignore.case = T, value = F)
	variables <- layouts$variable[rows]
	start <- columns[rows, 1]
	stop <- columns[rows, 2]
	len <- stop - start + 1
	
	if (is.null(names))
	{
		names <- c(variable.key, "icd9", "variable")
	}
	
	for (i in 1:length(variables))
	{
		paste("LOAD DATA LOCAL INFILE ", file, " INTO TABLE ", db.table, " (@var1) SET `", variable.key, "` = substr(@var1 ", start.key, ", ", len.key, "), `", names[2], "` = substr(@var1 ", start[i], ", ", len[i], "), SET `", names[3], "` = `", variables[i], "`;")
	}
}

## Normalize queries
normalizeQueries <- function(years, files, layouts, names, normalize.dx, normalize.pr)
{
	result <- vector()
	result.dx <- vector()
	result.pr <- vector()
	
	if (normalize.dx)
	{
		for (i in 1:length(years))
		{
			result.dx[i] <- normalizeQuery(year = years[i], file = files[1], db.table = "dx",
				layouts = layouts, pattern = "^dx[[:digit:]]+", names = names)
		}
	}
	if (normalize.pr)
	{
		for (i in 1:length(years))
		{
			result.dpr[i] <- normalizeQuery(year = years[i], file = files[1], db.table = "pr",
				layouts = layouts, pattern = "^pr[[:digit:]]+", names = names)
		}
	}
	
	result <- c(result.dx, result.pr)
	if (length(result > 0))
	{
		result <- paste(result, collapse = "\r")
	} else
	{
		result <- NULL
	}
	
	return(result)
}


grep("^dx[[:digit:]]+", layouts$variable, ignore.case = T, value = T)

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



test <- generateSQL(years = years, files = files, type = "core")





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


	## Create example data
	variables <- c("a1", "a2", "b1", "b2", "c1", "c2")
	dat <- data.frame(matrix(rnorm(70), ncol = 7))
	names(dat) <- c("Identity", variables)
	
	## Create formula
	formula <- paste(variables[grep("1$", variables)], variables[grep("2$", variables)], sep = ":", collapse = " + ")
	formula <- paste("Identity ~ ", formula, sep = "")
	
	## Run model
	glm1 <- glm(formula = formula, data = dat)
	summary(glm1)
