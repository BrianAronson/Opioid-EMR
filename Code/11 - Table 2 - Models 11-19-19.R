#0) Prep work space
    .libPaths(c("C:/Users/bda13/Documents/R/win-library/3.5",.libPaths()))
    library(igraph)
    library(data.table)
    library(igraph)
    library(stringr)
    library(ggplot2)
    library(lme4)
    library(plm)
    library(bife)
    library(margins)
    rawpath <- "hidden"
    dpath <- "hidden"  
    
#1) read data
    df1 <- readRDS(file.path(dpath, "erdf_reg.rds"))
    mat <- readRDS(file.path(dpath, "erdiagnoses.rds"))
    
#2) create last minute variables
    df1$time.5am2 <- df1$time.5am^2
    df1$year.reg <- as.numeric(as.character(df1$year2))
    df1$year.reg <- df1$year.reg - min(df1$year.reg, na.rm = T)
    df1 <- data.table(df1)    
    df1 <- df1[, c("pat_id", "enc_id", "female", "prev.opiate.num", "last4hr", "prescribed.opiate", "black", "white", "other", "latino", "asian", "divorced", "widowed", "married", "separated", "time.dummy", "day.week.dummy", "age.30.60.dummy", "age.60.100.dummy", "year.reg")]
    df1 <- df1[, lapply(.SD, as.numeric)]
    df1[, ':='(
      black.last4hr = black * last4hr,
      female.black = female * black,
      female.other = female * other,
      female.latino = female * latino,
      female.asian = female * asian)
    ]
    
#3) append diagnosis to df1 for use in model 2
    #a) reorder subset mat to match df1
        temp <- match(mat$enc_id, df1$enc_id)
        mat <- mat[!is.na(temp), ]
    #b) remove rare diagnoses
        temp <- which(sapply(mat, sum) > 50)
        mat <- mat[, temp, with=F]
    #c) merge into df1
        df2 <- merge(df1, mat, by="enc_id")
        df2 <- df2[, lapply(.SD, as.numeric)]
    #d) free up RAM
        rm(mat); gc()
    
#4) prepare fixed effects dataframe for use in model 3
    #a) remove people who only visted the ED once
        id.table <- data.table(table(df1$pat_id))
        id.table <- id.table[N > 1]
        df3 <- df2[df2$pat_id %in% id.table$V1,]
    # #b) remove icd9 variables that did not change over period
    #     a <- unlist(df3[, lapply(.SD, function(x) length(unique(x))), .SDcols = vnames])
    #     badvars <- names(a)[a == 1]
        
#5) create functions to print key results for each model
    formfun<-function(model){
      a<-summary(model)$coefficient
      a<-as.data.frame(a)
      a1<-a[,c(1,4)]
      a1[,2]<-ifelse(a1[,2]<.001,"***",ifelse(a1[,2]<.01,"**",ifelse(a1[,2]<.05,"*","")))
      a1[,1]<-paste(ifelse(sign(a1[,1])==1,"","-"),format(round(abs(a1[,1]),3),nsmall=3),sep="")
      names(a1)[2]<-"pr"
      row.names(a1)<-gsub("TRUE","",row.names(a1))
      return(a1)
    }

    formfun2<-function(model){
      a<-summary(model)$cm
      a<-as.data.frame(a)
      a1<-a[,c(1,4)]
      a1[,2]<-ifelse(a1[,2]<.001,"***",ifelse(a1[,2]<.01,"**",ifelse(a1[,2]<.05,"*","")))
      a1[,1]<-paste(ifelse(sign(a1[,1])==1,"","-"),format(round(abs(a1[,1]),3),nsmall=3),sep="")
      names(a1)[2]<-"pr"
      row.names(a1)<-gsub("TRUE","",row.names(a1))
      return(a1)
    }
    
    formfun3<-function(model){
      a<-summary(model)
      a<-a[1:nrow(a),]
      a<-as.data.frame(a)
      a1<-a[,c(1,4)]
      a1[,2]<-ifelse(a1[,2]<.001,"***",ifelse(a1[,2]<.01,"**",ifelse(a1[,2]<.05,"*","")))
      a1[,1]<-paste(ifelse(sign(a1[,1])==1,"","-"),format(round(abs(a1[,1]),3),nsmall=3),sep="")
      names(a1)[2]<-"pr"
      row.names(a1)<-gsub("TRUE","",row.names(a1))
      return(a1)
    }
    
#6) create formulas for each model
    f.m1 <- formula(prescribed.opiate ~ last4hr + black.last4hr +
          age.30.60.dummy + age.60.100.dummy +
          year.reg*(prev.opiate.num + I(prev.opiate.num ^2 )) +
          black + other + latino + asian +
          female + female.black + female.other + female.latino + female.asian +
          married + divorced + widowed + separated +
          time.dummy + day.week.dummy + (1|pat_id))

    f.m2 <- formula(prescribed.opiate ~ last4hr + black.last4hr +
                      age.30.60.dummy + age.60.100.dummy +
                      year.reg*(prev.opiate.num + I(prev.opiate.num ^2 )) +
                      black + other + latino + asian +
                      female + female.black + female.other + female.latino + female.asian +
                      married + divorced + widowed + separated +
                      time.dummy + day.week.dummy + 
      icd9.008 + icd9.009 + icd9.034 + icd9.041 + icd9.042 + icd9.053 + 
      icd9.054 + icd9.070 + icd9.078 + icd9.079 + icd9.099 + icd9.110 + 
      icd9.112 + icd9.131 + icd9.133 + icd9.135 + icd9.153 + icd9.154 + 
      icd9.155 + icd9.157 + icd9.162 + icd9.174 + icd9.180 + icd9.183 + 
      icd9.185 + icd9.191 + icd9.197 + icd9.198 + icd9.199 + icd9.202 + 
      icd9.203 + icd9.204 + icd9.209 + icd9.214 + icd9.218 + icd9.225 + 
      icd9.238 + icd9.239 + icd9.241 + icd9.242 + icd9.244 + icd9.246 + 
      icd9.249 + icd9.250 + icd9.251 + icd9.255 + icd9.266 + icd9.268 + 
      icd9.272 + icd9.273 + icd9.274 + icd9.275 + icd9.276 + icd9.277 + 
      icd9.278 + icd9.280 + icd9.281 + icd9.282 + icd9.285 + icd9.286 + 
      icd9.287 + icd9.288 + icd9.289 + icd9.290 + icd9.291 + icd9.292 + 
      icd9.293 + icd9.294 + icd9.295 + icd9.296 + icd9.297 + icd9.298 + 
      icd9.299 + icd9.300 + icd9.301 + icd9.303 + icd9.304 + icd9.305 + 
      icd9.307 + icd9.308 + icd9.309 + icd9.310 + icd9.311 + icd9.312 + 
      icd9.314 + icd9.317 + icd9.318 + icd9.319 + icd9.327 + icd9.331 + 
      icd9.332 + icd9.333 + icd9.337 + icd9.338 + icd9.339 + icd9.340 + 
      icd9.342 + icd9.343 + icd9.344 + icd9.345 + icd9.346 + icd9.348 + 
      icd9.349 + icd9.350 + icd9.351 + icd9.354 + icd9.355 + icd9.356 + 
      icd9.357 + icd9.361 + icd9.362 + icd9.364 + icd9.365 + icd9.366 + 
      icd9.368 + icd9.369 + icd9.370 + icd9.371 + icd9.372 + icd9.373 + 
      icd9.374 + icd9.375 + icd9.376 + icd9.379 + icd9.380 + icd9.381 + 
      icd9.382 + icd9.386 + icd9.388 + icd9.389 + icd9.401 + icd9.403 + 
      icd9.410 + icd9.411 + icd9.412 + icd9.413 + icd9.414 + icd9.415 + 
      icd9.416 + icd9.420 + icd9.423 + icd9.424 + icd9.425 + icd9.426 + 
      icd9.427 + icd9.428 + icd9.429 + icd9.432 + icd9.433 + icd9.434 + 
      icd9.435 + icd9.437 + icd9.438 + icd9.440 + icd9.441 + icd9.443 + 
      icd9.447 + icd9.451 + icd9.453 + icd9.454 + icd9.455 + icd9.456 + 
      icd9.457 + icd9.458 + icd9.459 + icd9.460 + icd9.461 + icd9.462 + 
      icd9.463 + icd9.464 + icd9.465 + icd9.466 + icd9.473 + icd9.475 + 
      icd9.477 + icd9.478 + icd9.482 + icd9.486 + icd9.487 + icd9.490 + 
      icd9.491 + icd9.492 + icd9.493 + icd9.494 + icd9.496 + icd9.511 + 
      icd9.514 + icd9.515 + icd9.517 + icd9.518 + icd9.519 + icd9.520 + 
      icd9.521 + icd9.522 + icd9.523 + icd9.524 + icd9.525 + icd9.526 + 
      icd9.527 + icd9.528 + icd9.529 + icd9.530 + icd9.533 + icd9.535 + 
      icd9.536 + icd9.540 + icd9.541 + icd9.550 + icd9.553 + icd9.555 + 
      icd9.556 + icd9.558 + icd9.560 + icd9.562 + icd9.564 + icd9.565 + 
      icd9.566 + icd9.569 + icd9.571 + icd9.572 + icd9.573 + icd9.574 + 
      icd9.575 + icd9.576 + icd9.577 + icd9.578 + icd9.583 + icd9.584 + 
      icd9.585 + icd9.586 + icd9.590 + icd9.591 + icd9.592 + icd9.593 + 
      icd9.595 + icd9.596 + icd9.597 + icd9.599 + icd9.600 + icd9.601 + 
      icd9.603 + icd9.604 + icd9.607 + icd9.608 + icd9.611 + icd9.614 + 
      icd9.616 + icd9.617 + icd9.620 + icd9.623 + icd9.625 + icd9.626 + 
      icd9.627 + icd9.632 + icd9.633 + icd9.634 + icd9.637 + icd9.640 + 
      icd9.641 + icd9.642 + icd9.643 + icd9.646 + icd9.648 + icd9.649 + 
      icd9.651 + icd9.654 + icd9.674 + icd9.680 + icd9.681 + icd9.682 + 
      icd9.685 + icd9.686 + icd9.691 + icd9.692 + icd9.693 + icd9.695 + 
      icd9.696 + icd9.698 + icd9.700 + icd9.701 + icd9.703 + icd9.704 + 
      icd9.705 + icd9.706 + icd9.707 + icd9.708 + icd9.709 + icd9.710 + 
      icd9.714 + icd9.715 + icd9.716 + icd9.718 + icd9.719 + icd9.721 + 
      icd9.722 + icd9.723 + icd9.724 + icd9.726 + icd9.727 + icd9.728 + 
      icd9.729 + icd9.730 + icd9.733 + icd9.736 + icd9.737 + icd9.738 + 
      icd9.741 + icd9.747 + icd9.753 + icd9.756 + icd9.758 + icd9.759 + 
      icd9.780 + icd9.781 + icd9.782 + icd9.783 + icd9.784 + icd9.785 + 
      icd9.786 + icd9.787 + icd9.788 + icd9.789 + icd9.790 + icd9.791 + 
      icd9.792 + icd9.793 + icd9.794 + icd9.796 + icd9.799 + icd9.801 + 
      icd9.802 + icd9.805 + icd9.807 + icd9.808 + icd9.810 + icd9.812 + 
      icd9.813 + icd9.814 + icd9.815 + icd9.816 + icd9.820 + icd9.822 + 
      icd9.823 + icd9.824 + icd9.825 + icd9.826 + icd9.831 + icd9.834 + 
      icd9.836 + icd9.840 + icd9.841 + icd9.842 + icd9.843 + icd9.844 + 
      icd9.845 + icd9.846 + icd9.847 + icd9.848 + icd9.850 + icd9.852 + 
      icd9.870 + icd9.872 + icd9.873 + icd9.874 + icd9.875 + icd9.876 + 
      icd9.879 + icd9.880 + icd9.881 + icd9.882 + icd9.883 + icd9.884 + 
      icd9.886 + icd9.890 + icd9.891 + icd9.892 + icd9.893 + icd9.905 + 
      icd9.906 + icd9.907 + icd9.910 + icd9.911 + icd9.912 + icd9.913 + 
      icd9.914 + icd9.915 + icd9.916 + icd9.917 + icd9.918 + icd9.919 + 
      icd9.920 + icd9.921 + icd9.922 + icd9.923 + icd9.924 + icd9.927 + 
      icd9.930 + icd9.931 + icd9.933 + icd9.935 + icd9.938 + icd9.941 + 
      icd9.942 + icd9.943 + icd9.944 + icd9.945 + icd9.948 + icd9.949 + 
      icd9.955 + icd9.958 + icd9.959 + icd9.965 + icd9.967 + icd9.969 + 
      icd9.977 + icd9.987 + icd9.989 + icd9.994 + icd9.995 + icd9.996 + 
      icd9.997 + icd9.998 + icd9.999 + icd9.E00 + icd9.E01 + icd9.E81 + 
      icd9.E82 + icd9.E84 + icd9.E85 + icd9.E86 + icd9.E87 + icd9.E88 + 
      icd9.E89 + icd9.E90 + icd9.E91 + icd9.E92 + icd9.E93 + icd9.E94 + 
      icd9.E95 + icd9.E96 + icd9.E97 + icd9.E98 + icd9.V01 + icd9.V03 + 
      icd9.V04 + icd9.V06 + icd9.V08 + icd9.V10 + icd9.V11 + icd9.V12 + 
      icd9.V13 + icd9.V15 + icd9.V17 + icd9.V22 + icd9.V23 + icd9.V28 + 
      icd9.V40 + icd9.V42 + icd9.V43 + icd9.V44 + icd9.V45 + icd9.V46 + 
      icd9.V49 + icd9.V53 + icd9.V54 + icd9.V55 + icd9.V56 + icd9.V58 + 
      icd9.V60 + icd9.V62 + icd9.V64 + icd9.V65 + icd9.V67 + icd9.V68 + 
      icd9.V70 + icd9.V71 + icd9.V72 + icd9.V73 + icd9.V77 + icd9.V82 + 
      icd9.V85 + icd9.V87 + icd9.V88 + icd9.V90 +(1|pat_id))
    
    f.m3 <-
      formula(
        prescribed.opiate ~ last4hr + black.last4hr +
          year.reg*(prev.opiate.num + I(prev.opiate.num ^2 )) +
          time.dummy + day.week.dummy +
          icd9.008 + icd9.009 + icd9.034 + icd9.041 + icd9.042 + icd9.053 +
          icd9.054 + icd9.070 + icd9.078 + icd9.079 + icd9.099 + icd9.110 +
          icd9.112 + icd9.131 + icd9.133 + icd9.135 + icd9.153 + icd9.154 +
          icd9.155 + icd9.157 + icd9.162 + icd9.174 + icd9.180 + icd9.183 +
          icd9.185 + icd9.191 + icd9.197 + icd9.198 + icd9.199 + icd9.202 +
          icd9.203 + icd9.204 + icd9.209 + icd9.214 + icd9.218 + icd9.225 +
          icd9.238 + icd9.239 + icd9.241 + icd9.242 + icd9.244 + icd9.246 +
          icd9.249 + icd9.250 + icd9.251 + icd9.255 + icd9.266 + icd9.268 +
          icd9.272 + icd9.273 + icd9.274 + icd9.275 + icd9.276 + icd9.277 +
          icd9.278 + icd9.280 + icd9.281 + icd9.282 + icd9.285 + icd9.286 +
          icd9.287 + icd9.288 + icd9.289 + icd9.290 + icd9.291 + icd9.292 +
          icd9.293 + icd9.294 + icd9.295 + icd9.296 + icd9.297 + icd9.298 +
          icd9.299 + icd9.300 + icd9.301 + icd9.303 + icd9.304 + icd9.305 +
          icd9.307 + icd9.308 + icd9.309 + icd9.310 + icd9.311 + icd9.312 +
          icd9.314 + icd9.317 + icd9.318 + icd9.319 + icd9.327 + icd9.331 +
          icd9.332 + icd9.333 + icd9.337 + icd9.338 + icd9.339 + icd9.340 +
          icd9.342 + icd9.343 + icd9.344 + icd9.345 + icd9.346 + icd9.348 +
          icd9.349 + icd9.350 + icd9.351 + icd9.354 + icd9.355 + icd9.356 +
          icd9.357 + icd9.361 + icd9.362 + icd9.364 + icd9.365 + icd9.366 +
          icd9.368 + icd9.369 + icd9.370 + icd9.371 + icd9.372 + icd9.373 +
          icd9.374 + icd9.375 + icd9.376 + icd9.379 + icd9.380 + icd9.381 +
          icd9.382 + icd9.386 + icd9.388 + icd9.389 + icd9.401 + icd9.403 +
          icd9.410 + icd9.411 + icd9.412 + icd9.413 + icd9.414 + icd9.415 +
          icd9.416 + icd9.420 + icd9.423 + icd9.424 + icd9.425 + icd9.426 +
          icd9.427 + icd9.428 + icd9.429 + icd9.432 + icd9.433 + icd9.434 +
          icd9.435 + icd9.437 + icd9.438 + icd9.440 + icd9.441 + icd9.443 +
          icd9.447 + icd9.451 + icd9.453 + icd9.454 + icd9.455 + icd9.456 +
          icd9.457 + icd9.458 + icd9.459 + icd9.460 + icd9.461 + icd9.462 +
          icd9.463 + icd9.464 + icd9.465 + icd9.466 + icd9.473 + icd9.475 +
          icd9.477 + icd9.478 + icd9.482 + icd9.486 + icd9.487 + icd9.490 +
          icd9.491 + icd9.492 + icd9.493 + icd9.494 + icd9.496 + icd9.511 +
          icd9.514 + icd9.515 + icd9.517 + icd9.518 + icd9.519 + icd9.520 +
          icd9.521 + icd9.522 + icd9.523 + icd9.524 + icd9.525 + icd9.526 +
          icd9.527 + icd9.528 + icd9.529 + icd9.530 + icd9.533 + icd9.535 +
          icd9.536 + icd9.540 + icd9.541 + icd9.550 + icd9.553 + icd9.555 +
          icd9.556 + icd9.558 + icd9.560 + icd9.562 + icd9.564 + icd9.565 +
          icd9.566 + icd9.569 + icd9.571 + icd9.572 + icd9.573 + icd9.574 +
          icd9.575 + icd9.576 + icd9.577 + icd9.578 + icd9.583 + icd9.584 +
          icd9.585 + icd9.586 + icd9.590 + icd9.591 + icd9.592 + icd9.593 +
          icd9.595 + icd9.596 + icd9.597 + icd9.599 + icd9.600 + icd9.601 +
          icd9.603 + icd9.604 + icd9.607 + icd9.608 + icd9.611 + icd9.614 +
          icd9.616 + icd9.617 + icd9.620 + icd9.623 + icd9.625 + icd9.626 +
          icd9.627 + icd9.632 + icd9.633 + icd9.634 + icd9.637 + icd9.640 +
          icd9.641 + icd9.642 + icd9.643 + icd9.646 + icd9.648 + icd9.649 +
          icd9.651 + icd9.654 + icd9.674 + icd9.680 + icd9.681 + icd9.682 +
          icd9.685 + icd9.686 + icd9.691 + icd9.692 + icd9.693 + icd9.695 +
          icd9.696 + icd9.698 + icd9.700 + icd9.701 + icd9.703 + icd9.704 +
          icd9.705 + icd9.706 + icd9.707 + icd9.708 + icd9.709 + icd9.710 +
          icd9.714 + icd9.715 + icd9.716 + icd9.718 + icd9.719 + icd9.721 +
          icd9.722 + icd9.723 + icd9.724 + icd9.726 + icd9.727 + icd9.728 +
          icd9.729 + icd9.730 + icd9.733 + icd9.736 + icd9.737 + icd9.738 +
          icd9.741 + icd9.747 + icd9.753 + icd9.756 + icd9.758 + icd9.759 +
          icd9.780 + icd9.781 + icd9.782 + icd9.783 + icd9.784 + icd9.785 +
          icd9.786 + icd9.787 + icd9.788 + icd9.789 + icd9.790 + icd9.791 +
          icd9.792 + icd9.793 + icd9.794 + icd9.796 + icd9.799 + icd9.801 +
          icd9.802 + icd9.805 + icd9.807 + icd9.808 + icd9.810 + icd9.812 +
          icd9.813 + icd9.814 + icd9.815 + icd9.816 + icd9.820 + icd9.822 +
          icd9.823 + icd9.824 + icd9.825 + icd9.826 + icd9.831 + icd9.834 +
          icd9.836 + icd9.840 + icd9.841 + icd9.842 + icd9.843 + icd9.844 +
          icd9.845 + icd9.846 + icd9.847 + icd9.848 + icd9.850 + icd9.852 +
          icd9.870 + icd9.872 + icd9.873 + icd9.874 + icd9.875 + icd9.876 +
          icd9.879 + icd9.880 + icd9.881 + icd9.882 + icd9.883 + icd9.884 +
          icd9.886 + icd9.890 + icd9.891 + icd9.892 + icd9.893 + icd9.905 +
          icd9.906 + icd9.907 + icd9.910 + icd9.911 + icd9.912 + icd9.913 +
          icd9.914 + icd9.915 + icd9.916 + icd9.917 + icd9.918 + icd9.919 +
          icd9.920 + icd9.921 + icd9.922 + icd9.923 + icd9.924 + icd9.927 +
          icd9.930 + icd9.931 + icd9.933 + icd9.935 + icd9.938 + icd9.941 +
          icd9.942 + icd9.943 + icd9.944 + icd9.945 + icd9.948 + icd9.949 +
          icd9.955 + icd9.958 + icd9.959 + icd9.965 + icd9.967 + icd9.969 +
          icd9.977 + icd9.987 + icd9.989 + icd9.994 + icd9.995 + icd9.996 +
          icd9.997 + icd9.998 + icd9.999 + icd9.E00 + icd9.E01 + icd9.E81 +
          icd9.E82 + icd9.E84 + icd9.E85 + icd9.E86 + icd9.E87 + icd9.E88 +
          icd9.E89 + icd9.E90 + icd9.E91 + icd9.E92 + icd9.E93 + icd9.E94 +
          icd9.E95 + icd9.E96 + icd9.E97 + icd9.E98 + icd9.V01 + icd9.V03 +
          icd9.V04 + icd9.V06 + icd9.V08 + icd9.V10 + icd9.V11 + icd9.V12 +
          icd9.V13 + icd9.V15 + icd9.V17 + icd9.V22 + icd9.V23 + icd9.V28 +
          icd9.V40 + icd9.V42 + icd9.V43 + icd9.V44 + icd9.V45 + icd9.V46 +
          icd9.V49 + icd9.V53 + icd9.V54 + icd9.V55 + icd9.V56 + icd9.V58 +
          icd9.V60 + icd9.V62 + icd9.V64 + icd9.V65 + icd9.V67 + icd9.V68 +
          icd9.V70 + icd9.V71 + icd9.V72 + icd9.V73 + icd9.V77 + icd9.V82 +
          icd9.V85 + icd9.V87 + icd9.V88 + icd9.V90 | pat_id
      )
    
#7) run models, format, and save results
  #model 1
    m1 <- glmer(f.m1, df1, family = "binomial", nAGQ = 0)
    m11 <- formfun(m1)
    saveRDS(m1, file.path(figpath, "m1-11-19-19.rds"))
  #model 2
    m2 <- glmer(f.m2, df2, family = "binomial", nAGQ = 0)
    m21 <- formfun(m2)
    m21 <- m21[!grepl("icd9", row.names(m21)),]
    saveRDS(m2, file.path(figpath, "m2-11-19-19.rds"))
  #model 3
    m3 <- bife(f.m3, data = df3, "logit")
    m31 <- formfun2(m3)
    m31 <- m31[!grepl("icd9", row.names(m31)), ]
    saveRDS(m3, file.path(figpath, "m3-11-19-19.rds"))

#8) merge all models, format, and save results
    #a) remove row names
        m11$var <- row.names(m11)
        m21$var <- row.names(m21)
        m31$var <- row.names(m31)
    #b) merge regression results
        ml <- list(m11, m21, m31)
        merge.all <- function(x, y) {
          merge(x, y, by = "var", all = T)
        }
        all.mods <- Reduce(merge.all, ml)
        all.mods <- sapply(all.mods, as.character)
        all.mods[is.na(all.mods)] <- ""
        all.mods <- as.data.frame(all.mods, stringsAsFactors = F)
        all.mods <- sapply(all.mods, function(x) paste("'", x,sep=""))
    #c) save results
        write.csv(all.mods, file.path(figpath, "models-11-19-19.csv"))

#9) create efficient functions for estimating marginal effects (i.e. without estimating significance; takes days otherwise)
    #a) glmer
        f.margins <- function(model, data, var.names){
            margeff <- vector()
            for(i in 1:length(var.names)){
              t.var <- var.names[i]
              data2 <- copy(data)
              data2[,(t.var) := lapply(.SD, function(x) x + 1/10000000), .SDcols = t.var]
              a <- predict(model, data)
              print((i-.5)/length(var.names))
              b <- predict(model, data2)
              a <- exp(a)/(exp(a)+1)
              b <- exp(b)/(exp(b)+1)
              margeff[i] <- mean(b-a) * 10000000
              print(i/length(var.names))
            }
            return(margeff)
        }
      #b) bife
        f.bife.margins <- function(model, var.names){
          margeff <- vector()
          for(i in 1:length(var.names)){
            t.var <- var.names[i]
            model2 <- copy(model)
            model2$data[,(t.var) := lapply(.SD, function(x) x + 1/10000000), .SDcols = t.var]
            a <- predict(model)
            print((i-.5)/length(var.names))
            b <- predict(model2)
            a <- exp(a)/(exp(a)+1)
            b <- exp(b)/(exp(b)+1)
            margeff[i] <- mean(b-a) * 10000000
            print(i/length(var.names))
          }
          return(margeff)
        }
        
#10) estimate marginal effects for key variables        
    #a) identify key variables
        glmer.vars <- names(df1)
        glmer.vars <- setdiff(glmer.vars, c("enc_id", "pat_id","prescribed.opiate"))
        bife.vars <- c("last4hr", "black.last4hr", "year.reg", "prev.opiate.num", "time.dummy", "day.week.dummy")
    #b) estimate marginal effects for each model
        system.time(marg1 <- f.margins(m1, df1, glmer.vars))
        system.time(marg2 <- f.margins(m2, df2, glmer.vars))
        system.time(marg3 <- f.bife.margins(model = m3, var.names = bife.vars))

#11) save marginal effects
    saveRDS(marg1, file.path(figpath, "marg1-11-19-19.rds"))
    saveRDS(marg2, file.path(figpath, "marg2-11-19-19.rds"))
    saveRDS(marg3, file.path(figpath, "marg3-11-19-19.rds"))
      
#12) merge marg tables and save results
    #a) format/order marg tables identically to reg tables
        #i) pull in variable order
            dt1 <- data.table(var = glmer.vars, marg = marg1)
            dt1 <- merge(dt1, m11, by = "var")
            dt2 <- data.table(var = glmer.vars, marg = marg2)
            dt2 <- merge(dt2, m21, by = "var")
            dt3 <- data.table(var = bife.vars, marg = marg3)
            dt3 <- merge(dt3, m31, by = "var")
        #ii) remove reg coefficients
            dt1$Estimate <- NULL
            dt2$Estimate <- NULL
            dt3$Estimate <- NULL
        #iii) rename marg estimate variable
            setnames(dt1, "marg", "Estimate")
            setnames(dt2, "marg", "Estimate")
            setnames(dt3, "marg", "Estimate")
    #b) merge marg tables
        ml <- list(dt1, dt2, dt3)
        merge.all <- function(x, y) {
          merge(x, y, by = "var", all = T)
        }
        all.mods <- Reduce(merge.all, ml)
    #c) format marg tables
        all.mods <- sapply(all.mods, as.character)
        all.mods[is.na(all.mods)] <- ""
        all.mods <- as.data.frame(all.mods, stringsAsFactors = F)
        all.mods <- sapply(all.mods, function(x) paste("'", x,sep=""))
    #d) save marg tables
        write.csv(all.mods, file = file.path(figpath, "marg.models-11-19-19.csv"))
    


# #13) estimate predicted probabilities 
#     #a) find median values of everything
#         df.normal.str <- df[1, ]
#         df.normal <- as.data.frame(t(sapply(df, median)))
#         for(i in 1:ncol(df.normal)){
#             class(df.normal[,i]) <- class(df.normal.str[,i])
#         }
#         
#     #b) for each variable to manipulate, find predicted values
#         #i) last 4 hour
#             # df.last4hr <- df.normal[rep(1,31), ]     
#             # df.last4hr$last4hr <- 0:30 
#             # predictions <- predict(m2, newdata = df.last4hr)
#             # df.pred1 <- data.table(y = predictions, x = df.last4hr$last4hr, variable = "ER visits in last 4 hours")
#         #ii) time of dat
#             df.time.5am <- df.normal[rep(1,24), ]     
#             df.time.5am$time.5am <- 0:23
#             df.time.5am$time.5am2 <- df.time.5am$time.5am^2 
#             predictions <- predict(m2, newdata = df.time.5am)
#             df.pred2 <- data.table(y = predictions, x = df.time.5am$time.5am, variable = "Time of day (hour)")
#         #iii) previously prescribed num
#             # df.prev.opiate.num <- df.normal[rep(1,21), ]     
#             # df.prev.opiate.num$prev.opiate.num <- 0:20
#             # df.prev.opiate.num$prev.opiate.num2 <- df.prev.opiate.num$prev.opiate.num^1
#             # predictions <- predict(m2, newdata = df.prev.opiate.num)
#             # df.pred3 <- data.table(y = predictions, x = df.prev.opiate.num$prev.opiate.num, variable = "Previous opioid prescriptions at ER")
#         #iv) age
#             df.age <- df.normal[rep(1,81), ]     
#             df.age$age <- 0:80
#             df.age$age2 <- df.age$age^2
#             predictions <- predict(m2, newdata = df.age)
#             df.pred4 <- data.table(y = predictions, x = df.age$age, variable = "Patient Age (years)")
#     #c) combine predictions to list
#         df.pred2$x <- df.pred2$x + 5
#         df.pred2$x[df.pred2$x > 23] <- df.pred2$x[df.pred2$x > 23] - 24
#         l.pred <- list(df.pred2,df.pred4)
#         df.pred <- rbindlist(l.pred)
#         
#         # df.pred <- df.pred[df.pred$variable %in% c("Patient Age", "Time of day"), ]
#     #d) plot predictions
#         library(ggplot2)
#         library(ggthemes)
#         p <- ggplot(df.pred, aes(x = x, y = y)) +
#           geom_line(size = 2) +
#           theme_pander() +
#           theme(
#             text = element_text(size=11.5, family = "serif"),
#             plot.title = element_text(size=20, hjust=0),
#             axis.title.x = element_text(size = 13, face = "bold", margin = margin(10,0,0,0)),
#             axis.title.y = element_text(size = 13.5, face = "bold", margin = margin(0,15,0,0)),
#             strip.text.x =  element_text(size = 12, face = "bold"),
#             axis.ticks = element_blank(),
#             panel.spacing = unit(1.5, "lines")
#           ) +
#           facet_wrap(~variable, nrow = 2, scales = "free") +
#           labs(x = "", y = "Log odds of being prescribed opioid")
# 
#         p
#     #e) save plot
#         ggsave(tsave1 <- "file.path(figpath, "Figure 2 10-2.png"),
#                p, width = 6, height = 5, units = "in")
#         # browseURL(tsave1)
# 

    
