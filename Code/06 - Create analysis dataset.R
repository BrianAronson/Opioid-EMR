#0) Prep work space
    library(igraph)
    library(data.table)
    library(igraph)
    library(stringr)
    library(ggplot2)
    rawpath <- "hidden"
    dpath <- "hidden"

#1) read data
    encdf <- readRDS(file.path(dpath,  "encdf.rds"))
    prescdf <- readRDS(file.path(dpath,  "prescdf.rds"))
    provdf <- readRDS(file.path(dpath,  "provdf.rds"))

#2) subset data
    #a) identify visits that were within emergency room
        df_ed <- encdf[encdf$enc_type=="ED", ]
        ERpatdat <- paste(df_ed$pat_id, round(df_ed$admit_date, 2))
    #b) identify visits were inpatient stays    
        df_ip <- encdf[encdf$enc_type=="IP", ]
        IDpatdat <- paste(df_ip$pat_id, round(df_ip$admit_date, 2))
    #c) Exclude ER visits that resulted in inpatient stay (these are rare but could bias results)
        df <- df_ed[!ERpatdat %in% IDpatdat, ]  
    #d) Remove outliers
        df <- df[df$facility_id != "",]
        df <- df[df$clinic_specialty == "",]
        df <- df[df$clinic_location == "",]
        df <- df[is.na(df$visit_type_key),]
    #e) exclude meaningless info
        #i) identify meaningless variables
            meaningless <- sapply(df, function(x) length(unique(x)))
        #ii) identify redundant variables
            meaningless[names(meaningless)=="site_id"] <- 1
        #iii) remove meaningless
            df <- df[, c(meaningless!=1), with=F]
    #e) free up RAM
        rm(df_ed, df_ip, encdf)
        gc()
        
#3) bring in useful prescription data        
    #a) identify prescriptions from those visits
        prescdf <- prescdf[prescdf$enc_id %in% df$enc_id, ]
    #b) was patient prescribed anything that visit?
        df$prescibed.any <- ifelse(df$enc_id %in% prescdf$enc_id, 1, 0)
    #c) was patient prescribed an opiate?
        df$prescibed.opiate <- ifelse(df$enc_id %in% prescdf$enc_id[prescdf$is.opiate==1], 1, 0)
    #d) Identify who prescribed opioid (not used in current analyses, but could be useful in future)
        #i) subset precription data data
            prescdf <- prescdf[, c("enc_id", "RX_PROVIDERID", "is.opiate", "RAW_RX_MED_NAME", "RAW_RX_FREQUENCY", "RAW_SEDI_DRUG_STRENGTH", "RAW_SEDI_DISCRETE_DOSE_UNITS", "RAW_SEDI_DISCRETE_DOSE_AMOUNT")]
        #ii) find people with multiple prescribers
            dem_df <- unique(prescdf[, 1:3])
            a <- data.table(table(dem_df$enc_id))
            a <- a$V1[a$N>1]
            clean_dem_df <- dem_df[dem_df$enc_id %in% a]
            a <- data.table(table(dem_df$enc_id))
            a <- a$V1[a$N==1]
            encs0 <- dem_df[dem_df$enc_id %in% a]
        #iii) for those with multiple prescribers but only one real one,  just take the name of the real one
            tdf3 <- clean_dem_df[clean_dem_df$RX_PROVIDERID!=-1, ]
            a <- data.table(table(tdf3$enc_id))
            a <- a$V1[a$N==1]
            encs1 <- tdf3[tdf3$enc_id %in% a]
        #iv) for those with multiple presibers and no real ones,  keep the first
            b <- clean_dem_df[!clean_dem_df$enc_id %in% tdf3$enc_id]
            encs2 <- b[!duplicated(b$enc_id), ]
        #v) for those with multiple prescribers that are real and one opioid prescriber, keep the opioid prescriber
            a <- data.table(table(tdf3$enc_id))
            a <- a$V1[a$N>1]
            b <- tdf3[tdf3$enc_id %in% a]
            encs3 <- b[b$is.opiate==1, ] #some are still duplicated
            encs3 <- encs3[!duplicated(encs3$enc_id), ]
        #vi) otherwise just take the first presciber named
            b <- b[!b$enc_id %in% encs3$enc_id, ]
            encs4 <- b[!duplicated(b$enc_id), ]
            encs <- rbind(encs0, encs1, encs2, encs3, encs4)
        #vii) merge prescriber id into df
            encs <- encs[, 1:2]
            names(encs)[2] <- "rx.provider"
            df <- merge(df, encs, by="enc_id", all=T)
    #e) what identify what was prescribed and how much (prioritizing opioids)
        prescdf <- prescdf[order(prescdf$enc_id, prescdf$is.opiate, decreasing=T), ]
        prescdf <- prescdf[!duplicated(prescdf$enc_id), ]
        prescdf <- prescdf[, c(1, 4:7)]
        names(prescdf)[2:5] <- c("rx.med", "rx.freq", "rx.strength", "rx_units")
        df <- merge(df, prescdf, by="enc_id", all=T)
    #f) free up RAM
        rm(prescdf)
        gc()
        
#4) link encounter info with diagnosis info
    diagdf <- fread(file.path(rawpath, "dr1380_diagnosis.csv", header=T, sep=', ', select=c("DX", "RAW_DX_TYPE", "ENCOUNTERID")))
    names(diagdf) <- c("dx", "dx_type", "enc_id")
    diagdf <- diagdf[diagdf$enc_id %in% df$enc_id]
    #a) remove duplicate diagnoses
        diagdf <- unique(diagdf)
    #b) since I won't be able to trace back custom diagnoses,  just remove those
        diagdf <- diagdf[diagdf$dx_type=="ICD-9-CM"]
        diagdf$dx_type <- NULL
    #c) because many diagnoses per encounter; keep all diagnoses for now
        dem_df <- split(diagdf$dx, factor(diagdf$enc_id))
    #d) turn to table
        dem_df <- data.table(enc_id=as.integer(names(dem_df)), diagnosis=dem_df)
    #e) merge with encounter info
        df <- merge(df, dem_df, by="enc_id", all=T)
        
#5) link enounter info with demographics info
    #a) read demographics
      dem_df <- fread(file.path(rawpath, "dr1380_demographic.csv", header=T, sep=c(', '), select=c("RAW_SEX", "RAW_HISPANIC", "RAW_RACE", "RAW_SEDI_CURRENT_MARITAL_STS", "PATID", "BIRTH_DATE")))
    #b) alter/create variables
      clean_dem_df <- dem_df[, 1]
      names(clean_dem_df) <- "female"
      #gender
          clean_dem_df$female[clean_dem_df$female=="FEMALE"] <- 1
          clean_dem_df$female[clean_dem_df$female=="MALE"] <- 0
          clean_dem_df$female[clean_dem_df$female!="1" & clean_dem_df$female!="0"] <- NA
          clean_dem_df$female <- as.numeric(clean_dem_df$female)
      #race
          clean_dem_df$race <- dem_df$RAW_RACE
          clean_dem_df$race[clean_dem_df$race=="UNAVAILABLE" | clean_dem_df$race=="DECLINED"] <- NA
          clean_dem_df$race[clean_dem_df$race=="WHITE OR CAUCASIAN"] <- "white"
          clean_dem_df$race[clean_dem_df$race=="BLACK OR AFRICAN AMERICAN"] <- "black"
          clean_dem_df$race[clean_dem_df$race=="ASIAN"] <- "asian"
          clean_dem_df$race[clean_dem_df$race=="MULTIRACIAL"] <- "multiracial"
          clean_dem_df$race[!clean_dem_df$race %in% c("white", "black", "asian", "multiracial") & !is.na(clean_dem_df$race)] <- "other"
          clean_dem_df$race[dem_df$RAW_HISPANIC=="HISPANIC OR LATINO"] <- "latino"
      #marital
          clean_dem_df$marital <- tolower(dem_df$RAW_SEDI_CURRENT_MARITAL_STS)
          clean_dem_df$marital[clean_dem_df$marital=="unknown" | clean_dem_df$marital==""] <- NA
          clean_dem_df$marital[clean_dem_df$marital=="legally separated"] <- "separated"
      #patid
          clean_dem_df$pat_id <- dem_df$PATID
      #birth date
          day <- as.numeric(substr(dem_df$BIRTH_DATE, 1, 2))
          month <- factor(substr(dem_df$BIRTH_DATE, 3, 5))
          levels(month) <- c(4, 8, 12, 2, 1, 7, 6, 3, 5, 11, 10, 9)
          month <- as.numeric(as.character(month))
          year <- as.numeric(substr(dem_df$BIRTH_DATE, 6, 9))
          clean_dem_df$birth_date <- year+(round((month-1)*(365/12))+day)/365
    #c) merge with enounter info
        df <- merge(df, clean_dem_df, by="pat_id", all=T)
        
#6) Make additional variables
    #a) age
        df$age <- df$admit_date-df$birth_date
    #b) count previously prescribed meds
        temp <- df$prescibed.opiate
        temp[is.na(temp)] <- 0
        prev.opiate <- data.table(temp=temp, pat_id=df$pat_id)
        prev.opiate <- prev.opiate[, cumsum(temp), by=pat_id]
        df$prev.opiate.num <- prev.opiate$V1
        df$prev.opiate.any <- prev.opiate$V1>1 | (prev.opiate$V1==1 & df$prescibed.opiate!=1)
    #c) estimate how crowded was ER
        #i) order and subset data into just crowding info
            df <- df[order(df$admit_date, df$admit_time), ]
            er_crowding <- df[, c("admit_date", "admit_time", "facility_id")]
        #ii) identify the day, hour, and minute of each patient visit
            er_crowding$hour <- as.numeric(substr(er_crowding$admit_time, 1, 2))
            er_crowding$minute <- as.numeric(substr(er_crowding$admit_time, 4, 5))
            er_crowding$day <- er_crowding$admit_date * 365
            er_crowding$hr_min <- er_crowding$hour * 60 + er_crowding$minute
            er_crowding$day_hr_min <- er_crowding$day * 24 * 60
            er_crowding$day_hr_min <- er_crowding$day_hr_min + er_crowding$hr_min
            day_hr_min <- er_crowding$day_hr_min
            facility_id <- er_crowding$facility_id
        #iii) prepare loop to count how busy ER was during a given time interval
            lasthr <- vector(length = length(day_hr_min))
            last4hr <- vector(length = length(day_hr_min))
            last12hr <- vector(length = length(day_hr_min))
            last24hr <- vector(length = length(day_hr_min))
            last168hr <- vector(length = length(day_hr_min))
        #iv) run loop counting number of people in ER of a given facility during a given interval of time
            for(i in 1:length(day_hr_min)){
                rows <- i-3000
                rows <- ifelse(rows<1, 1, rows)
                fac <- facility_id[i]==facility_id[rows:i]
                dif <- (day_hr_min[i]-day_hr_min[rows:i])
                dif <- dif[fac]
                lasthr[i] <- sum(dif<60 & dif>0)
                last4hr[i] <- sum(dif<60*4 & dif>0)
                last12hr[i] <- sum(dif<60*12 & dif>0)
                last24hr[i] <- sum(dif<60*24 & dif>0)
                last168hr[i] <- sum(dif<60*168 & dif>0)
                print(paste(round((i/nrow(er_crowding)*100), 2), "%", sep=""))
            }
        #v) append crowding data to encounter df
            df$lasthr <- lasthr
            df$last4hr <- last4hr
            df$last12hr <- last12hr
            df$last24hr <- last24hr
            df$last168hr <- last168hr
        #vi) force day_hr_min info during early days of the analysis data to NA (since there is missing info)
            a <- day_hr_min - min(day_hr_min, na.rm = T)
            df$lasthr[a < 60] <- NA
            df$last4hr[a < 60 * 4] <- NA
            df$last12hr[a < 60 * 12] <- NA
            df$last24hr[a < 60 * 24] <- NA
            df$last168hr[a < 60 * 168] <- NA
            
#6) save
    saveRDS(df, file.path(dpath,  "erdf.rds"))
        
