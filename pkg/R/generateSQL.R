#' Tools for working with data from the Nationwide Inpatient Sample
#' @name nis-package
#' @docType package
NULL 

#' Generate SQL code to convert NIS ascii flat files to SQL database 
#' 
#' The \code{generateSQL} function is a convenience function that returns
#' SQL code to be used for creating appropriate tables for a MySQL
#' database and loading data from the flat ascii files provided by HCUP. 
#' 
#' @param years A vector containing the years of data to include. 
#' 	The order of \code{years} must be the same as file locations
#' 	provided in the \code{files} parameter.  Currently, only years
#' 	1998 through 2009 are supported.
#' @param files A vector containing the file locations of the ascii flat
#' 	files.  The order of \code{files} must be the same as the years
#' 	provided by the \code{years} parameter.
#' @param type The type of NIS data being loaded.  Acceptable values are
#' 	\code{"core"}, \code{"hospitals"}, \code{"severity"}, and 
#' 	\code{"groups"}, for the NIS Core, Hospitals, Severity, and 
#' 	Dx Pr Groups data files, respectively.
#' @param remove.capitalization optional A logical indicating whether the variable
#' 	names should be converted to all lower case. Default is \code{TRUE}.
#' @param db.table optional A character indicating the desired database table name.
#' 	If not specified, will default to the \code{type} parameter (i.e.
#' 	core, hospitals, severity, or groups)
#' @param layouts.uri optional A list specifying the uris to use to fetch the layouts
#' 	of the ascii flat files. This should usually not be specified,
#' 	in which case the default uris are used, as listed in details below.
#' @param old optional A vector of variable names to be replaced by \code{new}. 
#' Must be in the same order as the \code{new} parameter.
#' @param new optional A vector of new variable names to replace those specified
#' 	in \code{old}. Must be in the same order as the \code{old} parameter.
#' @param normalize.dx Logical indicting whether to provide additional SQL statements
#' 	to normalize the diagnosis (dx1, dx2, etc.) variables into a sperate table.
#' 	Default is \code{FALSE}.
#' @param normalize.pr Logical indicting whether to provide additional SQL statements
#' 	to normalize the procedure (pr1, pr2, etc.) variables into a sperate table.
#' 	Default is \code{FALSE}.
#' @param names Character vector giving names for the three columns of the normalized
#' 	diagnosis and procedure tables. Only applicable if \code{normalize.dx} or
#' 	\code{normalize.pr} are \code{TRUE}. Default is NULL, in which case the columns
#' 	are named "keyid", "icd9", and "variable", for the record number, ICD9 code,
#' 	and variable name (i.e. dx1 or dx2), respectively.
#' @return A list of two character vectors:
#' 	\item{createTable}{A SQL statement to create an empty table with
#' 		with variables appropriate for NIS data.}
#' 	\item{loadData}{One or more SQL statements to load data from the
#' 		specified \code{files} into a MySQL table using the 
#' 		\code{load data infile} SQL statement.
#' @section Details: Unless \code{layouts.uri} is specified, \code{generateSQL}
#' 	uses the parses layout information from the NIS website to determine
#' 	the appropriate variable names and fixed-width locations of data
#' 	in the NIS ascii flat files. By default, the \code{layouts.uri} object is
#'  	the following list:
#' 	\code{layouts.uri <- list(
#'				"1998" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_1998_COREv2.TXT',
#'				"1999" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_1999_COREv2.TXT',
#'				"2000" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_2000_CORE.TXT',
#'				"2001" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_2001_CORE.TXT',
#'				"2002" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2002_CORE.TXT',
#'				"2003" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2003_CORE.TXT',
#'				"2004" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2004_CORE.TXT',
#'				"2005" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2005_Core.TXT',
#'				"2006" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2006_Core.TXT',
#'				"2007" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2007_Core.TXT',
#'				"2008" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2008_Core.TXT',
#'				"2009" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2009_Core.TXT'
#'			)}
#' @example examples/generateSQL.R 
#' @references http://www.hcup-us.ahrq.gov/nisoverview.jsp
#' @export
generateSQL <- function(years, files, type, remove.capitalization = T, 
	db.table = NULL, layouts.uri = NULL, old = NULL, new = NULL,
	normalize.dx = F, normalize.pr = F, names = NULL)
{
	
	## If db.table is not specified, use default
	if (is.null(db.table))
	{
		if (type == "core")
		{
			db.table <- "core"
		} else if (type == "hospitals")
		{
			db.table <- "hospitals"
		} else if (type == "severity")
		{
			db.table <- "severity"
		} else if (type == "groups")
		{
			db.table <- "groups"
		}
	}
	
	## Get layouts from the uri
	layouts <- getLayouts(layouts.uri = layouts.uri, years = years, type = type)
	
	## Find variables from past years not included in most recent year,
	## and add them to most recent year 
	layouts <- addVariables(layouts = layouts, years = years)
	
	## Replace old variable names with new variable names
	layouts <- renameVariables(layouts = layouts, type = type, old = old, new = new)
	
	## Merge layouts into single table
	layouts <- mergeLayouts(layouts)
	
	## Remove duplicated variables and, optionally, capitalization 
	layouts <- cleanLayouts(layouts, remove.capitalization = remove.capitalization)
	
	## Generate create table SQL
	createTable <- makeTableQuery(layouts, db.table = db.table)
	
	## Generate data infile SQL
	loadData <- makeInfileQueries(years = years, files = files, db.table = db.table, layouts = layouts)
	
	## Generate create table SQL for normalized tables
	if (normalize.dx | normalize.pr)
	{
		if (is.null(names))
		{
			keyid <- grep("^key", layouts$variable, ignore.case = T, value = T)
			names <- c(keyid, "icd9", "variable")
		}
		if (normalize.dx)
		{
			createTableNormalized <- makeTableQueryNormalized(layouts = layouts, names = names, 
				db.table = "dx", pattern = "^dx[[:digit:]]+")
			createTable <- paste(c(createTable, createTableNormalized), collapse = "\r\r")
		}
		if (normalize.pr)
		{
			createTableNormalized <- makeTableQueryNormalized(layouts = layouts, names = names, 
				db.table = "pr", pattern = "^pr[[:digit:]]+")
			createTable <- paste(c(createTable, createTableNormalized), collapse = "\r\r")
		}
	}
	
	## Generate data infile SQL for normalization
	if (normalize.dx | normalize.pr)
	{
		loadDataNormalized <- makeInfileQueriesNormalized(years = years, files = files, layouts = layouts, 
			names = names, normalize.dx = normalize.dx, normalize.pr = normalize.pr)
		loadData <- paste(c(loadData, loadDataNormalized), collapse = "\r\r")
	}
	
	## Return result
	result <- list(createTable = createTable, loadData = loadData)
	return(result)	
}
	
	
## Get layout files
getLayouts <- function(layouts.uri = NULL, years, type)
{
	## If layouts.uri is not specified, use default uri
	if (is.null(layouts.uri))
	{
		if (type == "core")
		{
			layouts.uri <- list(
				"1998" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_1998_COREv2.TXT',
				"1999" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_1999_COREv2.TXT',
				"2000" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_2000_CORE.TXT',
				"2001" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_2001_CORE.TXT',
				"2002" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2002_CORE.TXT',
				"2003" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2003_CORE.TXT',
				"2004" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2004_CORE.TXT',
				"2005" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2005_Core.TXT',
				"2006" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2006_Core.TXT',
				"2007" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2007_Core.TXT',
				"2008" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2008_Core.TXT',
				"2009" = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2009_Core.TXT'
			)
		} else if (type =="hospitals")
		{
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
		} else if (type =="severity")
		{
			layouts.uri <- list(
				'2009' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2009_Severity.TXT',
				'2008' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2008_Severity.TXT',
				'2007' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2007_Severity.TXT',
				'2006' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2006_Severity.TXT',
				'2005' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2005_Severity.TXT',
				'2004' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2004_SEVERITY.TXT',
				'2003' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2003_SEVERITY.TXT',
				'2002' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2002_SEVERITY.TXT'
			)
		} else if (type =="groups")
		{
			layouts.uri <- list(
				'2009' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2009_DX_PR_GRPS.TXT',
				'2008' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2008_DX_PR_GRPS.TXT',
				'2007' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2007_Dx_Pr_Grps.TXT',
				'2006' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2006_Dx_Pr_Grps.TXT',
				'2005' = 'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2005_Dx_Pr_Grps.TXT'
			)
		}
	}
	
	layouts.connection <- list()
	layouts <- list()
	for (i in years)
	{
		layouts.connection[[as.character(i)]] <- file(layouts.uri[[as.character(i)]])
		if (type == "core")
		{
			if (i <= 2002)
			{
				layouts[[as.character(i)]] <- read.fwf(layouts.connection[[as.character(i)]], 
					widths = c(-30, 19, -1, 3, -1, 3), skip = 20, strip.white = T, as.is = T,
					col.names = c("variable", "start", "end"))
			} else
			{
				layouts[[as.character(i)]] <- read.fwf(layouts.connection[[as.character(i)]], 
					widths = c(-30, 29, -1, 3, -1, 3), skip = 20, strip.white = T, as.is = T,
					col.names = c("variable", "start", "end"))
			}
		} else if (type == "hospitals")
		{
			if (i <= 1999)
			{
				layouts[[as.character(i)]] <- read.fwf(layouts.connection[[as.character(i)]], 
					widths = c(-22, 19, -1, 3, -1, 3), skip = 20, strip.white = T, as.is = T,
					col.names = c("variable", "start", "end"))
			} else if (i <= 2002)
			{
				layouts[[as.character(i)]] <- read.fwf(layouts.connection[[as.character(i)]], 
					widths = c(-30, 19, -1, 3, -1, 3), skip = 20, strip.white = T, as.is = T,
					col.names = c("variable", "start", "end"))
			} else
			{
				layouts[[as.character(i)]] <- read.fwf(layouts.connection[[as.character(i)]], 
					widths = c(-30, 29, -1, 3, -1, 3), skip = 20, strip.white = T, as.is = T,
					col.names = c("variable", "start", "end"))
			}
		} else if (type == "severity")
		{
			if (i < 2002)
			{
				print("Severity Measures File was not included in the Nationwide Inpatient Sample prior to 2002.")
				break()
			} else
			{
				layouts[[as.character(i)]] <- read.fwf(layouts.connection[[as.character(i)]], 
					widths = c(-30, 29, -1, 3, -1, 3), skip = 20, strip.white = T, as.is = T,
					col.names = c("variable", "start", "end"))
			}
		} else if (type == "groups")
		{
			if (i < 2005)
			{
				print("Diagnosis and Procedure Groups file was not included in the Nationwide Inpatient Sample prior to 2005.")
				break()
			} else
			{
				layouts[[as.character(i)]] <- read.fwf(layouts.connection[[as.character(i)]], 
					widths = c(-30, 29, -1, 3, -1, 3), skip = 20, strip.white = T, as.is = T,
					col.names = c("variable", "start", "end"))
			}
		}
		
	}
	
	## Return layouts
	return(layouts)
}


## Get unique names of all variables
uniqueVariables <- function(x)
{
	result <- vector()
	for (i in x)
	{
		result <- append(result, i$variable)
	}
	result <- unique(result)
	return(result)
}


## Find variables from past years not included in most recent year,
## and add them to most recent year
addVariables <- function(layouts, years)
{
	## Find variables from previous years not included in most recent year's layout
	variables.unique <- uniqueVariables(layouts)
	variables.missing <- variables.unique[which(!variables.unique %in% 
				layouts[[as.character(max(years))]][,1])]
	
	## Add missing variables to most recent year's layout
	variables.additional <- matrix(nrow = length(variables.missing),
		ncol = 3)
	variables.additional <- as.data.frame(variables.additional)
	variables.additional[1] <- variables.missing
	names(variables.additional) <- names(layouts[[as.character(max(years))]])
	layouts[[as.character(max(years))]] <- rbind(layouts[[as.character(max(years))]],
		variables.additional)
	
	return(layouts)
	
}


## Rename variables to match 2009 layout
renameVariables <- function(layouts, type, old = NULL, new = NULL)
{
	## If old or new are not specified, use default
	## KEY is changed to KEYID because KEY is a reserved name in MySQL
	if (is.null(old) | is.null(new))
	{
		if (type == "core")
		{
			old <- c("KEY", "ZIPInc_Qrtl")
			new <- c("KEYID","ZIPINC_QRTL")
		} else if (type == "hospitals")
		{
			old <- c()
			new <- c()
		}  else if (type == "severity")
		{
			old <- c("KEY")
			new <- c("KEYID")
		}  else if (type == "groups")
		{
			old <- c("KEY")
			new <- c("KEYID")
		}
	}
	
	for (i in 1:length(old))
	{
		layouts <- lapply(layouts, function(x)
			{
				y <- which(x$variable == old[i])
				if (length(y) > 0)
				{
					x$variable[y] <- new[i]
					return(x)
				} else
				{
					return(x)
				}
			})	
	}
	return(layouts)
}


## Merge layouts to single data frame
mergeLayouts <- function(layouts)
{
	result <- layouts[[1]]
	
	if (length(layouts) > 1)
	{
		for (i in 2:length(layouts))
		{
			result <- merge(result, layouts[[i]], by = "variable", 
				all.x = T, all.y = T, suffixes = c("", paste(".", names(layouts)[i], sep= "")))
		}
	}
	
	names(result)[c(2,3)] <- c(paste("start.", names(layouts)[1], sep = ""), paste("end.", names(layouts)[1], sep = ""))
	
	return(result)
}


## Clean layouts by removing duplicated variables and, optionally, capitalization
cleanLayouts <- function(layouts, remove.capitalization = T)
{
	# Remove rows that are all NA due to renaming of variables above
	rows.to.remove <- apply(layouts[-1], 1, function(x)
		{
			all(is.na(x))
		})
	layouts <- subset(layouts, !rows.to.remove)
	
	# Convert variables to lower case
	if (remove.capitalization)
	{
		layouts$variable <- tolower(layouts$variable)
	}
	
	return(layouts)		
}


## SQL data infile query generator function for single year
makeInfileQuery <- function(year, file, db.table, layouts)
{
	result <- paste("LOAD DATA LOCAL INFILE '", file, "' INTO TABLE ", db.table, " (@var1) SET ", sep="")
	
	columns <- grep(year, names(layouts))
	
	layouts <- subset(layouts, !is.na(layouts[,columns[1]]))
	
	for(i in 1:(nrow(layouts)-1))
	{
		temp.var <- layouts[i,1]
		temp.start <- layouts[i,columns[1]]
		temp.end <- layouts[i,columns[2]]
		temp.length <- temp.end - temp.start + 1
		result <- paste(result, "`", temp.var, "` = substr(@var1, ", temp.start, ", ", temp.length, "), ", sep="")
	}
	
	j <- nrow(layouts)
	temp.var <- layouts[j,1]
	temp.start <- layouts[j,columns[1]]
	temp.end <- layouts[j,columns[2]]  
	temp.length <- temp.end - temp.start + 1
	result <- paste(result, "`", temp.var, "` = substr(@var1, ", temp.start, ", ", temp.length, "); ", sep="")
	
	return(result)
}


## SQL data infile query generator function for multiple years
makeInfileQueries <- function(years, files, db.table, layouts)
{
	result <- vector()
	for (i in 1:length(years))
	{
		result[i] <- makeInfileQuery(year = years[i], file = files[i], 
			db.table = db.table, layouts = layouts)
	}
	
	result <- paste(result, collapse = "\r")
	return(result)
}




## Generate creat table SQL
makeTableQuery <- function(layouts, db.table)
{
	variables <- layouts[1]
	locations <- layouts[-1]
	chars <- matrix(ncol = ncol(locations) / 2, nrow = nrow(locations))
	for (i in 1:ncol(chars))
	{
		chars[,i] <- locations[[2 * i]] - locations[[(2 * i) - 1]] + 1
	}
	
	maxima <- apply(chars, 1, function(x)
		{
			max(x, na.rm = TRUE)
		})
	
	variables.maxima <- data.frame(variables, as.character(maxima))
	
	result <- apply(variables.maxima, 1, function(x)
		{
			paste(x[1], " varchar (", x[2], ")", sep = "")
		})
	result <- paste(result, collapse = ",\n")
	result <- paste("create table ", db.table, " (\n", result, "\n);", sep="")
	return(result)
}



## remember to look into only up to dx15 is showing up
makeInfileQueryNormalized <- function(year, file, db.table, layouts, pattern, names)
{
	row.key <- grep(pattern = "^key", layouts$variable, ignore.case = T, value = F)
	columns <- layouts[, grep(year, names(layouts))]
	variable.key<- layouts$variable[row.key]
	start.key <- columns[row.key, 1]
	stop.key <- columns[row.key, 2]
	len.key <- stop.key - start.key + 1
	
	rows <- grep(pattern = pattern, layouts$variable, ignore.case = T, value = F)
	variables <- layouts$variable[rows]
	start <- columns[rows, 1]
	stop <- columns[rows, 2]
	len <- stop - start + 1
	
	result <- vector()
	for (i in 1:length(variables))
	{
		result[i] <- paste("LOAD DATA LOCAL INFILE '", file, "' INTO TABLE ", db.table, " (@var1) SET `", variable.key, "` = substr(@var1 ", start.key, ", ", len.key, "), `", names[2], "` = substr(@var1 ", start[i], ", ", len[i], "), SET `", names[3], "` = `", variables[i], "`;", sep = "")
	}
	
	result <- paste(result, collapse = "\r")
	return(result)	
}

## Normalize queries
makeInfileQueriesNormalized <- function(years, files, layouts, names, normalize.dx, normalize.pr)
{
	result <- vector()
	result.dx <- vector()
	result.pr <- vector()
	
	if (normalize.dx)
	{
		for (i in 1:length(years))
		{
			result.dx[i] <- makeInfileQueryNormalized(year = years[i], file = files[i], db.table = "dx",
				layouts = layouts, pattern = "^dx[[:digit:]]+", names = names)
		}
	}
	if (normalize.pr)
	{
		for (i in 1:length(years))
		{
			result.pr[i] <- makeInfileQueryNormalized(year = years[i], file = files[i], db.table = "pr",
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



## Generate create table SQL for normalized tables
makeTableQueryNormalized <- function(layouts, names, db.table, pattern)
{
	
	# Find longest width of variables ofer the years	
	variables <- layouts[1]
	locations <- layouts[-1]
	chars <- matrix(ncol = ncol(locations) / 2, nrow = nrow(locations))
	for (i in 1:ncol(chars))
	{
		chars[,i] <- locations[[2 * i]] - locations[[(2 * i) - 1]] + 1
	}
	
	maxima <- apply(chars, 1, function(x)
		{
			max(x, na.rm = TRUE)
		})
	
	variables.maxima <- data.frame(variables, as.numeric(maxima))
	
	# Find lengths of columns
	lengths <- vector()
	lengths[1] <- max((variables.maxima[grep("^key", variables.maxima$variable, ignore.case = T),2]))	
	lengths[2] <- max(variables.maxima[grep(pattern, variables.maxima$variable, ignore.case = T), 2])
	lengths[3] <- max(nchar(grep(pattern, variables.maxima$variable, ignore.case = T, value = T)))
	
	# Generate SQL
	result <- vector()
	for (i in 1:length(lengths))
	{
		result[i] <- paste(names[i], " varchar (", lengths[i], ")", sep = "")
	}
	result <- paste(result, collapse = ",\n")
	result <- paste("create table ", db.table, " (\n", result, "\n);", sep="")
	return(result)
}

