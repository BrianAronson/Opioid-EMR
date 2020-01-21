#0) Prep work space
    library(data.table)
    rawpath  <-  "hidden"
    dpath  <-  "hidden"

#1) load necessary data
    encdf <- readRDS(file.path(dpath, "encdf.rds"))
    prescdf <- readRDS(file.path(dpath, "prescdf.rds"))
    provdf <- readRDS(file.path(dpath, "provdf.rds"))
    encdf <- data.table(encdf)
    prescdf <- data.table(prescdf)
    provdf <- data.table(provdf)
    
#2) identify bad refs
    badref <- data.table(sort(table(encdf$ref_id), decreasing = T))
    badref <- badref$V1[badref$N > 10000]

#3) create clinic level datasets of everything
    #enc
        l_encdf <- split(encdf, by="clinic_ls")
    #presc
        temp <- encdf[, c("enc_id", "clinic_ls")]
        prescdf <- merge(prescdf, temp, by="enc_id")
        l_prescdf <- split(prescdf, by="clinic_ls")
    #prov1
        temp <- encdf[, c("prov_id", "clinic_ls")]
        lprov1 <- merge(provdf, temp, by="prov_id")
        lprov1 <- split(lprov1, by="clinic_ls")
    #prov2
        temp <- encdf[, c("ref_id", "clinic_ls")]
        names(temp) <- c("prov_id", "clinic_ls2")
        num_ref_provs <- merge(provdf, temp, by="prov_id")
        num_ref_provs$clinic_ls <- num_ref_provs$clinic_ls2
        num_ref_provs$clinic_ls2 <- NULL
        num_ref_provs <- split(num_ref_provs, by="clinic_ls")

#4) initiate a clinic-level dataset
    cdf <- data.table(clinic = unique(encdf$clinic_ls))
    
#5) grab basic clinic descriptives
    cdf$specialty <- sapply(l_encdf, function(x) x$clinic_specialty[1])
    cdf$location <- sapply(l_encdf, function(x) x$clinic_location[1])
    
#6) fill out cdf based on encounter dataset
    #a) identify providers,  patients,  departments,  and referals by clinic
        temp <- list()
        temp2 <- list()
        temp3 <- list()
        temp4 <- list()
        for(i in 1:length(l_encdf)){
          tdf <- l_encdf[[i]]
          temp[[i]] <- unique(tdf$prov_id)
          temp2[[i]] <- unique(tdf$ref_id)
          temp3[[i]] <- unique(tdf$pat_id)
          temp4[[i]] <- unique(tdf$clinic_dept_key)
        }
        cdf$prov_ids <- temp
        cdf$ref_ids <- temp2
        cdf$pat_ids <- temp3
        cdf$clinic_dept_keys <- temp4
        #i) kill bad ids
            cdf$prov_ids <- lapply(cdf$prov_ids, function(x) x[x>10])
            cdf$ref_ids <- lapply(cdf$ref_ids, function(x) x[x>10])
            cdf$ref_ids <- lapply(cdf$ref_ids, function(x) x[!(x %in% badref)])
    #b) create more clinic level variables
        cdf$num_provs <- sapply(cdf$prov_ids, length)
        cdf$num_refs <- sapply(cdf$ref_ids, length)
        cdf$num_pats <- sapply(cdf$pat_ids, length)
        cdf$num_depts <- sapply(cdf$clinic_dept_keys, length)
        cdf$events <- sapply(l_encdf, nrow)
        #i) department keys and number of departments is useless
            cdf <- cdf[, -"clinic_dept_keys"]
            cdf <- cdf[, -"num_depts"]
    
#7) fill out cdf based on presc dataset
    #a) identify providers,  patients,  departments,  and referals by clinic
        temp1 <- list()
        temp2 <- list()
        for(i in 1:length(l_prescdf)){
          tdf <- l_prescdf[[i]]
          temp1[[i]] <- unique(tdf$RX_PROVIDERID)
          temp2[[i]] <- unique(tdf$PATID)
        }
        rx_prov_ids <- temp1
        rx_pat_ids <- temp2
        rx_prov_ids <- lapply(rx_prov_ids, function(x) x[x>10])
    #b) create more clinic level variables
        num_presc <- as.numeric(sapply(l_prescdf, nrow))
        num_rx_pats <- as.numeric(sapply(rx_pat_ids, function(x)length(unique(x))))
        num_rx_provs <- as.numeric(sapply(rx_prov_ids, function(x)length(unique(x))))
        num_opiates <- as.numeric(sapply(l_prescdf, function(x)sum(x$is.opiate)))
        num_pats_on_opiates <- as.numeric(sapply(l_prescdf, function(x) sum(x[, .(b=sum(is.opiate)), by=list(PATID)]$b>0)))
        num_pats_on_opiates2P <- as.numeric(sapply(l_prescdf, function(x) sum(x[, .(b=sum(is.opiate)), by=list(PATID)]$b>1)))
    #c) make ordering match
        temp <- data.table(clinic=names(l_prescdf), 
                         rx_prov_ids, 
                         rx_pat_ids, 
                         num_presc, 
                         num_rx_pats, 
                         num_rx_provs, 
                         num_opiates, 
                         num_pats_on_opiates, 
                         num_pats_on_opiates2P)
        cdf <- merge(cdf, temp, by="clinic", all=T)
     
#8) fill out cdf based on prov dataset
    #a) identify providers,  patients,  departments,  and referals by clinic
        temp1 <- list()
        temp2 <- list()
        temp3 <- list()
        for(i in 1:length(lprov1)){
          tdf <- lprov1[[i]]
          temp1[[i]] <- unique(tdf$prov_id)
          temp2[[i]] <- unique(tdf$specialty)
          temp3[[i]] <- unique(tdf$group)
        }
        prov_prov_ids <- temp1
        prov_specialties <- temp2
        prov_groups <- temp3
        prov_prov_ids <- lapply(prov_prov_ids, function(x) x[x>10])
    #b) create more clinic level variables
        num_prov_provs <- as.numeric(sapply(prov_prov_ids, length))
        num_prov_men <- as.numeric(sapply(lprov1, function(x)sum(x$gender=="MALE") ))
        num_prov_md <- as.numeric(sapply(lprov1, function(x)sum(x$md)))
        num_prov_num_presc <- as.numeric(sapply(lprov1, function(x)sum(x$md) ))
        num_prov_num_opiates <- as.numeric(sapply(lprov1, function(x)sum(x$md) ))
        num_prov_event_num_presc <- as.numeric(sapply(lprov1, function(x)sum(x$md) ))
        num_prov_event_num_opiates <- as.numeric(sapply(lprov1, function(x)sum(x$md) ))
    #c) make ordering match
        temp <- data.table(clinic=names(lprov1), 
                         prov_prov_ids, 
                         prov_specialties, 
                         prov_groups, 
                         num_prov_provs, 
                         num_prov_md, 
                         num_prov_num_presc, 
                         num_prov_num_opiates, 
                         num_prov_event_num_presc, 
                         num_prov_event_num_opiates)
        cdf <- merge(cdf, temp, by="clinic", all=T)
        
#9) fill out cdf based on ref dataset
    clinic <- names(num_ref_provs)
    num_ref_provs <- as.numeric(sapply(num_ref_provs, nrow))
    temp <- data.table(clinic = clinic, num_ref_provs = num_ref_provs)
    cdf <- merge(cdf, temp, by="clinic", all=T)

#9 - remove word "null" from specialties and groups
    cdf$prov_specialties <- lapply(cdf$prov_specialties, function(x) x[x!="(null)"])
    cdf$prov_groups <- lapply(cdf$prov_groups, function(x) x[x!="(null)"])

#10 - location
    cdf$location <- sapply(l_encdf, function(x) x$clinic_location[1])
    
#11 - save
    saveRDS(cdf, file.path(dpath, "cdf.rds"))
    

