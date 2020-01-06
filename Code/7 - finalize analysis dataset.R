#0) Prep work space
    library(igraph)
    library(data.table)
    library(igraph)
    library(stringr)
    library(ggplot2)
    library(lme4)
    rawpath <- "hidden"
    dpath <- "hidden"
    
#1) read data
    df <- readRDS(file.path(dpath, "erdf.rds"))
    
#2) delete bad data / fix typo
    df <- df[df$facility_id=="hidden", ]
    df <- df[!is.na(df$admit_date), ]
    df <- df[!is.na(df$admit_time), ]
    df <- df[as.numeric(substr(df$admit_date2, 6, 9))>2006, ]
    df <- df[!is.na(df$race), ]
    df <- df[!is.na(df$marital), ]
    df$prescribed.opiate <- df$prescibed.opiate

#2) subset data and break out all categorical variables
    #a) break out categorical variables
        races <- unique(df$race)
        ncols <- ncol(df)
        df <- as.data.frame(df)
        for(i in 1:length(races)){
          df[, (ncols+i)] <- df$race==races[i]
          names(df)[ncols+i] <- races[i]
        }

        maritals <- unique(df$marital)
        ncols <- ncol(df)
        df <- as.data.frame(df)
        for(i in 1:length(maritals)){
          df[, (ncols+i)] <- df$marital==maritals[i]
          names(df)[ncols+i] <- maritals[i]
        }

        facility_ids <- unique(df$facility_id)
        ncols <- ncol(df)
        df <- as.data.frame(df)
        for(i in 1:length(facility_ids)){
          df[, (ncols+i)] <- df$facility_id==facility_ids[i]
          names(df)[ncols+i] <- facility_ids[i]
        }

    #b) create useful alternative variables
        df$admit_date3 <- substr(df$admit_date2, 1, 9)
        df$admit_date3 <- as.Date(df$admit_date3, "%d%b%Y")
        #day of week
            df$day <- weekdays(df$admit_date3)
            df$day <- factor(df$day, levels=unique(df$day))
            df$day <- as.numeric(df$day)
        #time of day
            df$admit_time2 <- as.numeric(substr(df$admit_time, 1, 2))+as.numeric(substr(df$admit_time, 4, 5))/60
            df$morning <- df$admit_time2<8
            df$afternoon <- df$admit_time2>=8 & df$admit_time2<16
            df$evening <- df$admit_time2>=16
            df$admit_time <- as.numeric(substr(df$admit_time, 1, 2))+as.numeric(substr(df$admit_time, 4, 5))/60

    #c) exclude unusuable variables
        df$diagnosis <- NULL
        df$enc_type <- NULL
        df$rx.med <- NULL
        df$rx.freq <- NULL
        df$rx.strength <- NULL
        df$rx_units <- NULL
        df$marital <- NULL
        df$race <- NULL
        df$facility_id <- NULL
        df$NA.1 <- NULL
        # df$pat_id <- NULL
        # df$enc_id <- NULL
        df$prov_id <- NULL
        df$ref_id <- NULL
        df$rx.provider <- NULL
        df$admit_time2 <- NULL
        # df$admit_time <- NULL

    #e) kill NAs
        df <- df[!apply(df, 1, function(x)any(is.na(x))), ]


#2) load diagnoses
    mat <- readRDS(file.path(dpath, "erdiagnoses.rds"))

#3) make predicted crowding variable
    df1$year2 <- factor(year(df1$admit_date3))
    df1$admit_time2 <- factor(floor(df1$admit_time))
    df1$day2 <- factor(df1$day)
    # model <- lm(last4hr~ day2+admit_time2+year2, data=df1)
    # predcr <- predict(model)
    # df1$crowding <- df1$last4hr/predcr

#4) make/amend other variables
    df1$birth_date[df1$birth_date<1900]
    df1$age[df1$age>100] <- 100
    df1$age2 <- df1$age^2
    df1$prev.opiate.num2 <- df1$prev.opiate.num^2
    df1$time <- as.numeric(as.character(df1$year))

#5) final edits
    #kill 2007
        df1 <- df1[df1$year2!=2007, ]
    #merge other and multiracial
        df1$other <- df1$other | df1$multiracial
        df1$multiracial <- NULL
    #merge life partner with single
        df1$single <- df1$single | df1$`life partner`
    #make continuous version of admit time starting at 5am
        df1$time.5am <- as.numeric(as.character(df1$admit_time2))
        df1$time.5am <- df1$time.5am-5
        df1$time.5am[df1$time.5am < 0] <- max(df1$time.5am) - df1$time.5am[df1$time.5am < 0]  
        df1$time.dummy <- ifelse(df1$time.5am <= 6, 1, 0)
    #make continuous day of week and month
        df1$day.week <- as.numeric(as.character(df1$day2))-1
        df1$day.month <- as.numeric(format(df1$admit_date3, "%d"))-1 
        df1$day.week.dummy <- ifelse(df1$day.week > 4, 1, 0)
    #kill unreliable ages
        df1 <- df1[df1$birth_date > 1900, ]
    #rename and remove unstudied vars
        df1$year <- df1$year2
        df1 <- df1[, !names(df1) %in% c("birth_date", "prescibed.any", "prescibed.opiate", "admit_date", "admit_time", "admit_date2", "last12hr", "last24hr", "last168hr", "admit_date3", "day", "morning", "afternoon", "evening", "year2", "admit_time2", "day2", "crowding", "time", "day3", "life partner", "time.dummy", "day.week.dummy")]
    #cap outliers
        df1$last4hr[df1$last4hr>30] <- 30
        df1$lasthr[df1$lasthr>10] <- 10
        df1$prev.opiate.num[df1$prev.opiate.num > 30] <- 30
        df1$prev.opiate.num2 <- df1$prev.opiate.num^2
    #change year to continuous
        df1$year <- as.numeric(as.character(df1$year))
        df1$year.reg <- df1$year - min(df1$year)
        df1$age.00.30.dummy <- ifelse(df1$age < 30, 1, 0)
        df1$age.30.60.dummy <- ifelse(df1$age >= 30 & df1$age < 60, 1, 0)
        df1$age.60.100.dummy <- ifelse(df1$age >= 60, 1, 0)

#6) save
    saveRDS(df1, file.path(dpath, "erdf_reg.rds"))
