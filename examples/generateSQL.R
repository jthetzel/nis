## Create SQL statement to upload NIS Core data from 1998-2009 to MySQL database
# Years to include
years <- c(seq(1998, 2009))

# Location of the NIS ascii flat files
files <- c('/data/nis/NIS_1998_Core.ASC',
	'/data/nis/NIS_1999_Core.ASC',
	'/data/nis/NIS_2000_Core.ASC',
	'/data/nis/NIS_2001_Core.ASC',
	'/data/nis/NIS_2002_Core.ASC',
	'/data/nis/NIS_2003_Core.ASC',
	'/data/nis/NIS_2004_Core.ASC',
	'/data/nis/NIS_2005_Core.ASC',
	'/data/nis/NIS_2006_Core.ASC',
	'/data/nis/NIS_2007_Core.ASC',
	'/data/nis/NIS_2008_Core.ASC',
	'/data/nis/NIS_2009_Core.ASC'
)

# Generate SQL statemebts
sql.core <- generateSQL(years = years, files = files, type = "core")

# Output SQL statements to a file
#cat(sql.core$createTable, file = "makeTableCore.sql")
#cat(sql.core$uploadData, file = "uploadDataCore.sql")