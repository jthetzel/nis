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
system('"C:/Program Files/R/R-2.14.0dev/bin/x64/R" CMD build "../pkg/" --binary')


cat('"C:/Program Files/R/R-2.14.0dev/bin/x64/R" CMD build "C:/Users/jthetzel/Research/trapezoid/"')
build <- '"C:/Program Files/R/R-2.14.0dev/bin/x64/R" CMD build "C:/Users/jthetzel/Research/trapezoid/" --binary'
check <- '"C:/Program Files/R/R-2.14.0dev/bin/x64/R" CMD check "C:/Users/jthetzel/Research/trapezoid/"'
rd <- '"C:/Program Files/R/R-2.14.0dev/bin/x64/R" CMD Rd2dvi "C:/Users/jthetzel/Research/trapezoid/man/trapezoid.Rd" --pdf'
system(build)

