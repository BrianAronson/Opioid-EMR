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
    df <- readRDS(file.path(dpath, "erdf.rds"))

#2) pull diagnoses
    diagn <- df$diagnosis
    
#3) create diagnosis dataframe
    #a) identify common diagnoses
        keeps <- data.table(table(substr(unlist(diagn),1,3)))
        keeps <- keeps[keeps$N>75,]
    #b) convert to vector; remove blank
        diagns <- keeps$V1
        diagns <- diagns[-1]
    #c) Convert common diagnoses to dataframe
        mat <- t(sapply(diagn, function(x) diagns %in% substr(x,1,3)))
        mat <- data.table(mat)
        names(mat) <- paste("icd9.",diagns,sep="")
    #d) add encounter id
        mat[, enc_id := df$enc_id]
        setcolorder(mat, order(names(mat)))
        
#4) save
    saveRDS(mat,file.path(dpath, "erdiagnoses.rds"))

    
    
    
    
    
##X) Look into icds
# library(icd)
#     #find most common icds
#         a<-table(unlist(diagn))
#         com<-sort(a,decreasing=T)[1:10]
#         explain_code(names(com))
#     #find most common icds where given opiate
#         a1<-table(unlist(df2$diagnosis[df2$prescribed.opiate==1]))
#         com1<-sort(a1,decreasing=T)[1:10]
#         explain_code(names(com1)) #basically the same
#     #find most common icds where not given opiate
#         a2<-table(unlist(df2$diagnosis[df2$prescribed.opiate!=1]))
#         com2<-sort(a2,decreasing=T)[1:10]
#         explain_code(names(com2)) #basically the same
#     #find icds most associated with opiate
# 
#         b1<-data.table(a1)
#         b2<-data.table(a2)
#         names(b1)[2]<-"yes"
#         names(b2)[2]<-"no"
#         b3<-merge(b1,b2,all=T,by="V1")
#         b3[is.na(b3)]<-0
#         b3$sum<-b3$yes+b3$no
#             #half of icds are probably too rare to bother with; a quarter should definitely go).
#         b3$prop<-b3$yes/b3$sum
#         b3<-b3[order(b3$prop,decreasing = T),]
#         b3<-b3[b3$sum>10,]
#             explain_code(head(b3$V1))
#         b3<-b3[b3$sum>100,]
#             explain_code(head(b3$V1))
#             explain_code(tail(b3$V1))
#         b3<-b3[b3$sum>500,]
#             explain_code(head(b3$V1))
#             explain_code(tail(b3$V1))
#         b3<-b3[b3$sum>1000,]
#             explain_code(head(b3$V1))
#             explain_code(tail(b3$V1))
#         b3<-b3[b3$sum>2000,]
#             explain_code(head(b3$V1))
#             explain_code(tail(b3$V1))
#         b3<-b3[b3$sum>5000,]
#             explain_code(head(b3$V1))
#             explain_code(tail(b3$V1))
#             explain_code('V58.69')
#         b3<-b3[b3$sum>10000,]
#             explain_code(head(b3$V1))
#             explain_code(tail(b3$V1))
#             explain_code(b3$V1)
#         #subset data and run code above
#             diagnosis<-b3$V1[1:2]
#             keeps<-sapply(diagn,function(x) any(x %in% diagnosis))
#             df1<-df1[keeps,]
#             #compare
#         temp<-cbind(names(com),names(com1),names(com2))
# 
# #5) create propensity scores
#     #reduce number of diagnoses
#         keeps<-data.table(table(unlist(diagn)))
#         keeps<-keeps[keeps$N>50,]
#     #create data table of diagnoses
#         diagns<-keeps$V1
#         #kill first diagnosis (a blank)
#           diagns<-diagns[-1]
#         #slow way
#           # mat<-matrix(0,nrow=nrow(df2),ncol=length(diagns))
#           # for(i in 1:length(diagns)){
#           # icd<-diagns[i]
#           # mat[,i]<-sapply(diagn, function(x) icd %in% x)
#           # print(i/length(diagns))
#           #   }
