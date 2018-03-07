#############################
### Example R file for package Template building
### STAT 3701
### Lecture 9
### Feb 26, 2018
### Zack W. Almquist, University of Minnesota
#############################



library(devtools) # install.packages("devtools")
library(roxygen2) # install.packages("roxygen2")
library(readr) # install.packages("readr")

## Check my current working directory path
getwd()

## Check what is in my current working directory
dir()

##
pckname<-"YuanShiTools"
devtools::create(pckname)
dir(pckname)

loc<-paste(getwd(),pckname,sep="/")
descLoc<-paste(loc,"DESCRIPTION",sep="/")
readLines(descLoc)

## Clean up Description
desc<-readLines(descLoc)
desc[grep("Title",desc)]<-"Title: Tools for STAT 3701"
desc[grep("Version:",desc)]<-"Version: 0.0.1"
desc[grep("Authors",desc)]<-"Authors@R: person(\"Zack\", \"Almquist\", email = \"almquist@umn.edu\", role = c(\"aut\", \"cre\"))"
desc[grep("Description",desc)]<-"Description: This is a set of functions, etc for STAT 370 class."
desc[grep("License",desc)]<-"License: MIT + file LICENSE"
writeLines(desc,con=descLoc)

## Write LICENSE FILE
write_file(x="
YEAR: 2018
COPYRIGHT HOLDER: Yuan Shi
", path=paste(loc,"LICENSE",sep="/"))

# copy R functions to file in package
file.copy(paste(getwd(),"RFunctionsForPackage.R",sep="/"), paste(loc,"R",sep="/"))
file.rename(paste(loc,"R","RFunctionsForPackage.R",sep="/"),paste(loc,"R",paste(pckname,"Rfunctions.R",sep=""),sep="/"))


# Build comments for the package
devtools::document(loc)


# Add some example data
d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
devtools::use_data(d,pkg=loc)

## Document data
write_file(x=
             "
           #' Example data from HW 1
           #'
           #' @author Charlie Geyer \\email{geyer@umn.edu}
           #' @references \\url{http://www.stat.umn.edu/geyer/3701/data/q1p4.txt}
           \"d\" ",path=paste(loc,"R","data.R",sep="/"))

# Build comments for the data
devtools::document(loc)

# Build Test Framework (for unit tests of functions)
use_testthat(loc)

# copy R functions to file in package
file.copy(paste(getwd(),"UnitTestForRfunctions.R",sep="/"), paste(loc,"tests","testthat",sep="/"))
file.rename(paste(loc,"tests","testthat","UnitTestForRfunctions.R",sep="/"),paste(loc,"tests","testthat",paste(pckname,"func1_func2.R",sep=""),sep="/"))


# At this point let's do an R Check
# And an R build
devtools::check(loc)
devtools::build(loc)

# Add function that requires ggplot2

# copy R functions to file in package
file.copy(paste(getwd(),"RFunctionsWdependencies.R",sep="/"), paste(loc,"R",sep="/"))
file.rename(paste(loc,"R","RFunctionsWdependencies.R",sep="/"),paste(loc,"R",paste(pckname,"myplot.R",sep=""),sep="/"))

## Add dependancies to R package
# Dependancies
devtools::use_package("ggplot2",pkg=loc)
devtools::use_package("magrittr",pkg=loc)
# Add Manuals
devtools::document(loc)


# At this point let's do an R Check, again
# And an R build
devtools::check(loc)
devtools::build(loc)



# Let's now document the whole package
devtools::use_package_doc(loc)

# Add Manuals
devtools::document(loc)

## Let's build a vignette
devtools::use_vignette("introduction",pkg=loc)

# At this point let's do an R Check, again
# And an R build
devtools::check(loc)
devtools::build(loc)


# Last let's install it on our machine
devtools::install()

############
## After out template, all future development will be
## through RStudio project, e.g. almquistTools.Rpro
############

############
## Wednesday we will walk through how 
## to add everything to git
############
