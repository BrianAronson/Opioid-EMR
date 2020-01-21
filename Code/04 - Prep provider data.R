#0) Prep work space
    library(data.table)
    rawpath <- "hidden"
    dpath <- "hidden"

#1) load data
    prov_df <- fread(
                  file.path(rawpath, "dr1289_5year_Provider_details.csv"),
                  header = T,
                  sep = '|',
                  select = c(
                    "PROVIDER_KEY",
                    "BEGIN_EFFECTIVE_DATE",
                    "END_EFFECTIVE_DATE",
                    "TITLE_DESC",
                    "PRIMARY_SPECIALTY",
                    "SUB_SPECIALTY",
                    "AFFILIATED_GROUP",
                    "GENDER",
                    "EPIC_DISPLAY_NAME"
                  )
               )
    names(prov_df) <- c("prov_id", "date_begin", "date_end", "title", "specialty", "sub_specialty", "group", "gender", "name")
    encdf <- readRDS(file.path(dpath, "encdf.rds"))
    presc_df <- readRDS(file.path(dpath, "prescdf.rds"))
    
#2) new variables
    prov_df$md <- prov_df$title == "MD"       
    prov_df$pdc <- prov_df$AFFILIATED_GROUP == "PDC"

#3) Link providers in events to events in prescription data
    encdf <- encdf[, c("enc_id", "prov_id", "ref_id")]
    presc_df <- merge(presc_df, encdf, by = "enc_id", all.x = T)
    
#4) identify number of opiates prescribed by provider.
    num_opiates <- presc_df$RX_PROVIDERID[presc_df$is.opiate == 1]
    num_opiates <- as.data.frame(table(num_opiates))
    num_opiates <- num_opiates[order(num_opiates$Freq, decreasing = T), ]
    num_opiates <- num_opiates[-1, ]
    names(num_opiates) <- c("prov_id", "num_opiates")
    num_opiates$prov_id <- as.numeric(as.character(num_opiates$prov_id))
    prov_df <- merge(prov_df, num_opiates, by = "prov_id", all.x = T)
    rm(num_opiates)

#5) identify number of meds prescribed by provider.
    num_presc <- presc_df$RX_PROVIDERID
    num_presc <- as.data.frame(table(num_presc))
    num_presc <- num_presc[order(num_presc$Freq, decreasing = T), ]
    num_presc <- num_presc[-1, ]
    names(num_presc) <- c("prov_id", "num_presc")
    num_presc$prov_id <- as.numeric(as.character(num_presc$prov_id))
    prov_df <- merge(prov_df, num_presc, by = "prov_id", all.x = T)
    rm(num_presc)
    
#6) identify alternate counts of above based on event connections
    #a) identify number of opiates prescribed by provider. (prov_id in events)
        event_num_opiates <- presc_df$prov_id[presc_df$is.opiate == 1]
        event_num_opiates <- as.data.frame(table(event_num_opiates))
        event_num_opiates <- event_num_opiates[order(event_num_opiates$Freq, decreasing = T), ]
        event_num_opiates <- event_num_opiates[-1, ]
        names(event_num_opiates) <- c("prov_id", "event_num_opiates")
        event_num_opiates$prov_id <- as.numeric(as.character(event_num_opiates$prov_id))
        prov_df <- merge(prov_df, event_num_opiates, by = "prov_id", all.x = T)
        rm(event_num_opiates)

    #b) identify number of meds prescribed by provider.  (prov_id in events)
        event_num_presc <- presc_df$prov_id
        event_num_presc <- as.data.frame(table(event_num_presc))
        event_num_presc <- event_num_presc[order(event_num_presc$Freq, decreasing = T), ]
        event_num_presc <- event_num_presc[-1, ]
        names(event_num_presc) <- c("prov_id", "event_num_presc")
        event_num_presc$prov_id <- as.numeric(as.character(event_num_presc$prov_id))
        prov_df <- merge(prov_df, event_num_presc, by = "prov_id", all.x = T)
        rm(a)
        
    #c) identify number of opiates prescribed by provider.  (ref_id in events)
        ref_num_opiates <- presc_df$ref_id[presc_df$is.opiate == 1]
        ref_num_opiates <- as.data.frame(table(ref_num_opiates))
        ref_num_opiates <- ref_num_opiates[order(ref_num_opiates$Freq, decreasing = T), ]
        ref_num_opiates <- ref_num_opiates[-1, ]
        names(ref_num_opiates) <- c("prov_id", "ref_num_opiates")
        ref_num_opiates$prov_id <- as.numeric(as.character(ref_num_opiates$prov_id))
        prov_df <- merge(prov_df, ref_num_opiates, by = "prov_id", all.x = T)
        rm(ref_num_opiates)
        
    #d) identify number of meds prescribed by provider.   (ref_id in events)
        ref_num_presc <- presc_df$ref_id
        ref_num_presc <- as.data.frame(table(ref_num_presc))
        ref_num_presc <- ref_num_presc[order(ref_num_presc$Freq, decreasing = T), ]
        ref_num_presc <- ref_num_presc[-1, ]
        names(ref_num_presc) <- c("prov_id", "ref_num_presc")
        ref_num_presc$prov_id <- as.numeric(as.character(ref_num_presc$prov_id))
        prov_df <- merge(prov_df, ref_num_presc, by = "prov_id", all.x = T)
        rm(ref_num_presc)
        
#7) save
    saveRDS(prov_df, file.path(dpath, "provdf.rds"))
  