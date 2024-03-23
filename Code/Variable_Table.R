setwd("../Data")
rm(list = ls())
load("WTA.RData")
setwd("../Results")

libraries <- c("labelled")
check.libraries <- is.element(libraries, installed.packages()[, 1])==FALSE
libraries.to.install <- libraries[check.libraries]
if (length(libraries.to.install!=0)) 
	{
	install.packages(libraries.to.install)
	}
for (i in libraries) 
	{
	library(i, character.only = TRUE)
	}

sink("Variable_Table.txt")
generate_dictionary(wta)
sink()