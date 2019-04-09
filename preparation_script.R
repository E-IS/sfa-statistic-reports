options("repos"="http://cran.at.r-project.org/")

tryCatch(find.package("doBy"), error=function(e) install.packages("doBy", dependencies=TRUE))
library(doBy)

tryCatch(find.package("RColorBrewer"), error=function(e) install.packages("RColorBrewer", dependencies=TRUE))
library(RColorBrewer)

tryCatch(find.package("xlsx"), error=function(e) install.packages("xlsx", dependencies=TRUE))
library(xlsx)
