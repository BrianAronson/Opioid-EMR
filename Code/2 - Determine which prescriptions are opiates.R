#0) Prep work space
    library(data.table)
    rawpath <- "hidden"
    dpath <- "hidden"
  
#1) load data
    df <- fread(
             file.path(rawpath, "dr1380_prescribing.csv"), 
             header = T, 
             sep = ', ', 
             drop = c(
                "RAW_RXNORM_CUI", 
                "RAW_RX_BASIS", 
                "RX_QUANTITY", 
                "RX_REFILLS", 
                "RX_DAYS_SUPPLY", 
                "RX_FREQUENCY", 
                "RX_BASIS", 
                "RXNORM_CUI", 
                "RAW_SEDI_UNITS_PER_DOSE"
             )
           )
    
    opioids <- fread(
                  file.path(dpath, "opiods list.csv"), 
                  header = F, 
                  sep = ', '
               )
    
#2) prepare opioids list
    #a) convert to lower case
        opioids <- opioids$V1
        meds <- df$RAW_RX_MED_NAME
        meds <- tolower(meds)
        opioids <- tolower(opioids)
        
    #b) add and remove opioids as appropriate
        opioids <- opioids[opioids!="aspirin" & opioids!="ibuprofen" & opioids!="naltrexone" & opioids!="naloxone"]
        opioids <- opioids[!(opioids %in% c("abstral", "co-gesic", "hydromorphone hydrochloride", "hysingla", "liquicet", "morphabond", "oxaydo", "oxycodone hydrochloride", "reprexain", "roxanol-t", "tussicaps", "vituz", "xodol", "zohydro er", "zutripro", "anexsia", "embeda", "hyrocodone", "ibudone", "maxidone", "onsolis", "oxycet", "palladone", "rezira", "targiniq er", "tylenol #3 and #4", "xartemis xr", "xtampza er", "zolvit", "acetaminophen", "pseudoephedrine", "homatropine", ""))]
        opioids <- c(opioids, "xartemis", "xtampza", "targiniq", "roxanol", "zohydro")

                
#3) identify opiods in meds
    #a) create an empty matrix with a column for each opiate
          medmat <- matrix(nrow=length(meds), ncol=length(opioids))
    #b) find each opioid in prescription data
          for(i in 1:length(opioids)){
            medmat[, i] <- grepl(opioids[i], meds)
            print(i)
          }
          medmat <- as.data.frame(medmat)
          names(medmat) <- opioids

          
#4) identify generics
    #a) create data frame of generics
        generics <- sort(c("fentanyl", "morphine", "buprenorphine", "meperidine", "hydromorphone", "methadone", "hydrocodone", "tapentadol", "oxycodone", "codeine"))
        names(medmat)[names(medmat)=="butrans"] <- "buprenorphine"
        names(medmat)[names(medmat)=="demerol"] <- "meperidine"
        genericdf <- medmat[, generics]
    #b) make manual fixes
        genericdf$hydromorphone[medmat$dilaudid] <- T
        genericdf$oxycodone[medmat$percocet] <- T
        genericdf$hydrocodone[medmat$vicodin] <- T
        genericdf$hydrocodone[medmat$tussionex] <- T
        genericdf$hydrocodone[medmat$norco] <- T

    
#5) update prescription df
    #a) identify whether an opiate
        is.opiate <- rowSums(genericdf)
        df$is.opiate <- is.opiate
    #b) rename encounter id
        df$enc_id <- df$ENCOUNTERID
        df$ENCOUNTERID <- NULL
        
        
#6) save
    saveRDS(genericdf, file.path(dpath, "genericdf.rds"))
    saveRDS(genericdf, file.path(dpath, "prescdf.rds"))
  