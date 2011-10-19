## roxygenize
require(roxygen2)

setwd("c:/Users/jthetzel/Research/nis")
package.dir <- "pkg"

roxygenize(package.dir = package.dir)



## Check and build source
setwd("c:/Users/jthetzel/Research/nis/tar")
system('"C:/Program Files/R/R-2.14.0dev/bin/x64/R" CMD check "../pkg/"')
system('"C:/Program Files/R/R-2.14.0dev/bin/x64/R" CMD build "../pkg/" ')

## Build binary
setwd("c:/Users/jthetzel/Research/nis/binary")
system('"C:/Program Files/R/R-2.14.0dev/bin/x64/R" CMD INSTALL "../pkg/" --build')

