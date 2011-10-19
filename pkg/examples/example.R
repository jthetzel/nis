setwd("c:/Users/jthetzel/Research/nis/")
setwd("/home/jthetzel/Research/nis/example")

## Specify years
years <- seq(1998, 2009)

## Specify locations of ascii flat files from NIS
## Note: must be in same order as years above
files.core <- c('/media/backup/data/nis/NIS_1998_Core.ASC',
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
files.hospitals <- c('/media/backup/data/nis/NIS_1998_Hospital.ASC',
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

## Generate SQL for core table
core <- generateSQL(years = years, files = files.core, type = "core", normalize.dx = T, normalize.pr = T)

## Generate SQL for hosptials table
hospitals <- generateSQL(years = years, files = files.hospitals, type = "hospitals")

## Optional: save SQL to a file
## At shell, these files can be used as:
## $ mysql -u root -p dbname < createTableCore.sql
## where dbname is the name of the database
cat(core$createTable, file = "createTableCore.sql")
cat(core$loadData, file = "loadDataCore.sql")
cat(hospitals$createTable, file = "createTableHospitals.sql")
cat(hospitals$loadData, file = "loadDataHospitals.sql")


## To create database and tables directly from R, use a database connection
require(RMySQL)
drv <- dbDriver('MySQL')
conn <- dbConnect(drv, group='nis2')

dbSendQuery(conn = conn, statement = )


