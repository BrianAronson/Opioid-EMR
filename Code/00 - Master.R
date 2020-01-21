#0) Prep work space
    library(igraph)
    library(data.table)
    library(stringr)
    library(ggplot2)
    dpath <- "hidden"
    spath <- "hidden"
    
#1 - run scripts
    #1) run once: identify key providers 
        source(file.path(spath, "01 - Identify key providers.R"))
    #2) run once: determine opiates in prescriptions
        source(file.path(spath, "02 - Determine which prescriptions are opiates.R"))
    #3) run once: clean/prep encounter file
        source(file.path(spath, "03 - Prep encounter data.R"))
    #4) run once: clean/prep provider file
        source(file.path(spath, "04 - Prep provider data.R"))
    #5) run once: prep clinic data
        source(file.path(spath, "05 - Prep clinic data.R"))
    
#2 - load relevant data
    encdf <- readRDS(file.path(dpath, "encdf.rds"))
    prescdf <- readRDS(file.path(dpath, "prescdf.rds"))
    provdf <- readRDS(file.path(dpath, "provdf.rds"))
    cdf <- readRDS(file.path(dpath, "cdf.rds"))
    # uniprovs<-readRDS(file.path(dpath, "uniprovs.rds"))
    # genericdf<-readRDS(file.path(dpath, "genericdf.rds"))
