#0) Prep work space
    library(data.table)
    rawpath <- "hidden"
    dpath <- "hidden"
    
#1) load data
    df <- fread(
            file.path(rawpath, "dr1380_encounter.csv"),
            header = T,
            sep = ',',
            select = c("PROVIDERID")
        )
    
#2) identify key provider ids
    uniprovs <- unique(df$PROVIDERID)
    
#3) remove nonreal provider ids
    uniprovs <- sort(uniprovs)
    uniprovs <- uniprovs[uniprovs > 10]

#4) save results
    saveRDS(uniprovs, file.path(dpath, "uniprovs.rds"))
    