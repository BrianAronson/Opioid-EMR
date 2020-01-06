#0) Prep work space
    library(igraph)
    library(data.table)
    library(igraph)
    library(stringr)
    library(ggplot2)
    library(lme4)
    rawpath <- "hidden"
    dpath <- "hidden"

#1) load data
    df <- readRDS(file.path(dpath, "erdf_reg.rds"))
    
#2) gather stats and remove variables to not include in table
    unique.pat <- !duplicated(df$pat_id)
    n.total <- nrow(df)
    n.prescribed <- sum(df$prescribed.opiate == 1)
    n.unprescribed <- sum(df$prescribed.opiate == 0)
    n.unique <- sum(unique.pat)
    df <- df[!names(df) %in% c("pat_id","enc_id","lasthr","prev.opiate.num2","year.reg","age2")]
  
#4) run table
    stats1 <- data.frame(var = 0, mean = 0, meanopiate1 = 0, meanopiate0 = 0, mean.unique = 0)
    for(i in 1:ncol(df)){
      stats1[i, ] <- c(names(df)[i], 
                   mean(df[, i], na.rm = T), 
                   mean(df[, i][df$prescribed.opiate == 1], na.rm = T), 
                   mean(df[, i][df$prescribed.opiate == 0], na.rm = T),
                   mean(df[, i][unique.pat], na.rm = T)
      )
    }
    stats1[nrow(stats1)+1, ] <- c("N", n.total,n.prescribed,n.unprescribed,n.unique)
    

#3) save as csv
    write.csv(stats1, file.path(figpath, "Table 1 - descriptives 10-3.csv"))
    
    
    

    
    
# #double check that last4hr, 24hr, etc. are coded correctly
#     #is last 24hr similar to frequency in same day?
#         tbl <- data.table(as.data.frame(table(df3$admit_date3)))
#         tbl2 <- data.table(data.frame(Var1 = df3$admit_date3[!duplicated(df3$admit_date3)], 
#                                     Freq = df3$last24hr[!duplicated(df3$admit_date3)]))
#         tbl2[, 2] <- c(unlist(tbl2[, 2][-1]), 0)
#         tbl <- tbl[-nrow(tbl), ]
#         tbl2 <- tbl2[-nrow(tbl2), ]
#         cor(tbl$Freq, tbl2$Freq)
#         plot(tbl$Freq, tbl2$Freq)
#             #maybe a little more deviation than I'd hope
#         #add a variable for number patients in calendar day
#             tbl <- data.table(as.data.frame(table(df1$admit_date3), stringsAsFactors  =  F))
#             names(tbl) <- c("admit_date3", "calfreq")
#             tbl$admit_date3 <- as.Date(tbl$admit_date3)
#             df5 <- merge(df1, tbl, by = "admit_date3", all = T)
#             
#     #does last 4 hour make sense?
#         day <- as.numeric(factor(df3$admit_date3))
#         minute <- df3$admit_date2
#         minute2 <- as.numeric(substr(minute, 14, 15))
#         hour <- as.numeric(substr(minute, 11, 12))
#         minute3 <- minute2+hour*60+day*24*60
#         #extract random samples to see if measurement is equivalent
#             samp <- sample(length(minute3), 1)
#             tminute <- minute3[samp]
#             tlast4hr <- sum((tminute-minute3)<240 & (tminute-minute3)>= 0 )
#             tlast4hr
#             df3$last4hr[samp]
#             #yep, usually
#             
#             