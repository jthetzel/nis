\name{generateSQL}
\alias{generateSQL}
\title{Generate SQL code to convert NIS ascii flat files to SQL database}
\usage{
  generateSQL(years, files, type, remove.capitalization =
  T, db.table = NULL, layouts.uri = NULL, old = NULL, new =
  NULL, normalize.dx = F, normalize.pr = F, names = NULL,
  data_type = "char")
}
\arguments{
  \item{years}{A vector containing the years of data to
  include. The order of \code{years} must be the same as
  file locations provided in the \code{files} parameter.
  Currently, only years 1998 through 2009 are supported.}

  \item{files}{A vector containing the file locations of
  the ascii flat files.  The order of \code{files} must be
  the same as the years provided by the \code{years}
  parameter.}

  \item{type}{The type of NIS data being loaded.
  Acceptable values are \code{"core"}, \code{"hospitals"},
  \code{"severity"}, and \code{"groups"}, for the NIS Core,
  Hospitals, Severity, and Dx Pr Groups data files,
  respectively.}

  \item{remove.capitalization}{optional A logical
  indicating whether the variable names should be converted
  to all lower case. Default is \code{TRUE}.}

  \item{db.table}{optional A character indicating the
  desired database table name. If not specified, will
  default to the \code{type} parameter (i.e. core,
  hospitals, severity, or groups)}

  \item{layouts.uri}{optional A list specifying the uris to
  use to fetch the layouts of the ascii flat files. This
  should usually not be specified, in which case the
  default uris are used, as listed in details below.}

  \item{old}{optional A vector of variable names to be
  replaced by \code{new}. Must be in the same order as the
  \code{new} parameter.}

  \item{new}{optional A vector of new variable names to
  replace those specified in \code{old}. Must be in the
  same order as the \code{old} parameter.}

  \item{normalize.dx}{Logical indicting whether to provide
  additional SQL statements to normalize the diagnosis
  (dx1, dx2, etc.) variables into a sperate table. Default
  is \code{FALSE}.}

  \item{normalize.pr}{Logical indicting whether to provide
  additional SQL statements to normalize the procedure
  (pr1, pr2, etc.) variables into a sperate table. Default
  is \code{FALSE}.}

  \item{names}{Character vector giving names for the three
  columns of the normalized diagnosis and procedure tables.
  Only applicable if \code{normalize.dx} or
  \code{normalize.pr} are \code{TRUE}. Default is NULL, in
  which case the columns are named "keyid", "icd9", and
  "variable", for the record number, ICD9 code, and
  variable name (i.e. dx1 or dx2), respectively.}

  \item{data_type}{A character specifying the MySQL
  data_type.  The default is "char", but "varchar" is an
  appropriate alternative. Theoretically, there are
  performance tradeoffs between "char" and "varchar", but
  the practical benefits for the NIS database have not been
  tested.}
}
\value{
  A list of two character vectors: \item{createTable}{A SQL
  statement to create an empty table with with variables
  appropriate for NIS data.} \item{loadData}{One or more
  SQL statements to load data from the specified
  \code{files} into a MySQL table using the \code{load data
  infile} SQL statement.
}
\description{
  The \code{generateSQL} function is a convenience function
  that returns SQL code to be used for creating appropriate
  tables for a MySQL database and loading data from the
  flat ascii files provided by HCUP.
}
\section{Details}{
  Unless \code{layouts.uri} is specified,
  \code{generateSQL} uses the parses layout information
  from the NIS website to determine the appropriate
  variable names and fixed-width locations of data in the
  NIS ascii flat files. By default, the \code{layouts.uri}
  object is the following list: \code{layouts.uri <- list(
  "1998" =
  'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_1998_COREv2.TXT',
  "1999" =
  'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_1999_COREv2.TXT',
  "2000" =
  'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_2000_CORE.TXT',
  "2001" =
  'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/NIS_2001_CORE.TXT',
  "2002" =
  'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2002_CORE.TXT',
  "2003" =
  'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2003_CORE.TXT',
  "2004" =
  'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2004_CORE.TXT',
  "2005" =
  'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2005_Core.TXT',
  "2006" =
  'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2006_Core.TXT',
  "2007" =
  'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2007_Core.TXT',
  "2008" =
  'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2008_Core.TXT',
  "2009" =
  'http://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2009_Core.TXT'
  )}
}
\examples{
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
}
\references{
  http://www.hcup-us.ahrq.gov/nisoverview.jsp
}

