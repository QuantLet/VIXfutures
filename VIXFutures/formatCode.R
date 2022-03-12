rm(list = ls(all = TRUE))
# install and load packages
libraries = c("formatR")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
# setwd('C:\')
#tidy_dir("C:\\Joerg\\drive\\github\\josterri\\VIX\\VIXfutures")
tidy_dir("VIXfutures.R")
