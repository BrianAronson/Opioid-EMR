#0) Prep work space
    library(data.table)
    rawpath <- "hidden"
    dpath <- "hidden"

#1) read data
    df <- fread(
            file.path(rawpath, "dr1380_encounter.csv"),
            header = T,
            sep = c(','),
            select = c(
              "FACILITYID",
              "ENC_TYPE",
              "PROVIDERID",
              "ENCOUNTERID",
              "PATID",
              "RAW_SEDI_CLINIC_SVC_SPECIALTY",
              "RAW_SEDI_CLINIC_LOC",
              "RAW_SEDI_REFERRING_PROV_KEY",
              "RAW_ENC_TYPE",
              "RAW_SITEID",
              "RAW_SEDI_CLINIC_DEPT",
              "ADMIT_DATE",
              "ADMIT_TIME",
              "RAW_SEDI_CLINIC_VISIT_TYPE"
            )
          )
    
#2) split character variables
    #a) visit type
        temp <- strsplit(df$RAW_SEDI_CLINIC_VISIT_TYPE, "[| ]")
        df$visit_type_key <- sapply(temp, function(x) x[2])
        df$visit_type_desc <- sapply(temp, function(x) paste(x[8], x[9]))
    #b) clinic dept
        temp <- strsplit(df$RAW_SEDI_CLINIC_DEPT, "[| ]")
        df$clinic_dept_key <- sapply(temp, function(x) x[2])
        df$clinic_dept_name <- sapply(temp, function(x) x[8])
        df <- df[, names(df) != "RAW_SEDI_CLINIC_VISIT_TYPE" &
                   names(df) != "RAW_SEDI_CLINIC_DEPT", with = F]
        rm(temp)
    
#3) rename variables
    names(df) <- c("facility_id", "enc_type", "prov_id", "enc_id", "pat_id", "clinic_specialty", "clinic_location", "ref_id", "enc_type", "site_id", "admit_date", "admit_time", "visit_type_key", "visit_type_desc", "clinic_dept_key", "clinic_dept_name")

#4) convert admit date to number
    day <- as.numeric(substr(df$admit_date, 1, 2))
    month <- factor(substr(df$admit_date, 3, 5))
    levels(month) <- c(4, 8, 12, 2, 1, 7, 6, 3, 5, 11, 10, 9)
    month <- as.numeric(as.character(month))
    year <- as.numeric(substr(df$admit_date, 6, 9))
    df$admit_date <- year + (round((month - 1) * (365 / 12)) + day) / 365

#5) remake the location/specialty variable
    df$clinic_ls <- paste(df$clinic_location, df$clinic_specialty, sep=" | ")

#6) save
    saveRDS(df, file.path(dpath, "encdf.rds"))
    