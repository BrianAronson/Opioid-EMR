#0) read data
    library(igraph)
    library(data.table)
    library(igraph)
    library(stringr)
    library(ggplot2)
    rawpath <- "hidden"
    dpath <- "hidden"  
    df<-readRDS(file.path(dpath, "erdf.rds"))
                
#1) load data
    cdf<-readRDS(file.path(dpath, "cdf.rds"))
    cdf<-cdf[cdf$num_provs>3 & cdf$events>100,]
    cdf<-cdf[cdf$clinic!=" | ",]

#2) make a df of all possible ties by provider
    edges<-data.table(id1=rep(cdf$clinic,each=nrow(cdf)),id2=rep(cdf$clinic,nrow(cdf)),
                      p1=rep(cdf$prov_ids,each=nrow(cdf)),p2=rep(cdf$prov_ids,nrow(cdf)))
    edges<-edges[,.(tie=sum(unlist(p1) %in% unlist(p2)),a=length(unlist(p1)), b=length(unlist(p2))),by=list(id1,id2)]
    edges$p.tie1<-edges$tie/edges$a
    edges$p.tie2<-edges$tie/edges$b
    edges$p.tie3<-edges$tie/edges$a * edges$tie/edges$b

    # #many clinics are probably specialized subsets of larger ones
    #     temp<-edges[edges$p.tie1>.75 & edges$id1!=edges$id2,c(1:3,8)]
    #     length(unique(temp$id1))


#3) graph ties with raw sample
    g<-graph.adjacency(matrix(edges$p.tie3,nrow=sqrt(nrow(edges))),weighted = T,diag=F)
    #convert back to ggplot
        df<-as.data.frame(layout_with_fr(g))
        df$name<-edges$id2[1:sqrt(nrow(edges))]
        df$name<-gsub("LEGACY","",df$name)
        df$name<-gsub(" [/|] ","",df$name)
        df$V1[df$V1>quantile(df$V1,.95,na.rm=T)]<-quantile(df$V1,.95,na.rm=T)
        df$V1[df$V1<quantile(df$V1,.05,na.rm=T)]<-quantile(df$V1,.05,na.rm=T)
        df$V2[df$V2>quantile(df$V2,.95,na.rm=T)]<-quantile(df$V2,.95,na.rm=T)
        df$V2[df$V2<quantile(df$V2,.05,na.rm=T)]<-quantile(df$V2,.05,na.rm=T)
        ggplot(df,aes(x=V1,y=V2,label=name))+
          geom_point()+
          theme_void()+
          geom_label()

#4) collapse identical clinics (as some clinics are probably identical and just named differently)
    temp<-edges[edges$p.tie3>.5 & edges$id1!=edges$id2,c(1:3,8)]
    toremove<-temp$id1[1:(nrow(temp)/2)]
    edges<-edges[!(edges$id1 %in% toremove | edges$id2 %in% toremove),]
    cdf<-cdf[!(cdf$clinic %in% toremove),]    
        
    g<-graph.adjacency(matrix(edges$p.tie3,nrow=sqrt(nrow(edges))),weighted = T,diag=F)
    #convert back to ggplot
        df<-as.data.frame(layout_with_fr(g))
        df$name<-edges$id2[1:sqrt(nrow(edges))]
        df$name<-gsub("LEGACY","",df$name)
        df$name<-gsub(" [/|] ","",df$name)
        df$V1[df$V1>quantile(df$V1,.95,na.rm=T)]<-quantile(df$V1,.95,na.rm=T)
        df$V1[df$V1<quantile(df$V1,.05,na.rm=T)]<-quantile(df$V1,.05,na.rm=T)
        df$V2[df$V2>quantile(df$V2,.95,na.rm=T)]<-quantile(df$V2,.95,na.rm=T)
        df$V2[df$V2<quantile(df$V2,.05,na.rm=T)]<-quantile(df$V2,.05,na.rm=T)
        ggplot(df,aes(x=V1,y=V2,label=name))+
          geom_point()+
          theme_void()+
          geom_label()
    
        
#5) prov_id nesting
    g<-graph.adjacency(matrix(edges$tie,nrow=sqrt(nrow(edges))),weighted = T,diag=F)
    #convert back to ggplot
    df<-as.data.frame(layout_with_fr(g))
    df$name<-edges$id2[1:sqrt(nrow(edges))]
    df$name<-gsub("LEGACY","",df$name)
    df$name<-gsub(" [/|] ","",df$name)
    df$V1[df$V1>quantile(df$V1,.95,na.rm=T)]<-quantile(df$V1,.95,na.rm=T)
    df$V1[df$V1<quantile(df$V1,.05,na.rm=T)]<-quantile(df$V1,.05,na.rm=T)
    df$V2[df$V2>quantile(df$V2,.95,na.rm=T)]<-quantile(df$V2,.95,na.rm=T)
    df$V2[df$V2<quantile(df$V2,.05,na.rm=T)]<-quantile(df$V2,.05,na.rm=T)
    ggplot(df,aes(x=V1,y=V2,label=name))+
      geom_point()+
      theme_void()+
      geom_label()
    

#6) make a df of all possible ties by patient
    edges<-data.table(id1=rep(cdf$clinic,each=nrow(cdf)),id2=rep(cdf$clinic,nrow(cdf)),
                      p1=rep(cdf$pat_ids,each=nrow(cdf)),p2=rep(cdf$pat_ids,nrow(cdf)))
    edges<-edges[,.(tie=sum(unlist(p1) %in% unlist(p2)),a=length(unlist(p1)), b=length(unlist(p2))),by=list(id1,id2)]
    edges$p.tie1<-edges$tie/edges$a
    edges$p.tie2<-edges$tie/edges$b
    edges$p.tie3<-edges$tie/edges$a * edges$tie/edges$b


    #a) graph ties with raw sample
        g<-graph.adjacency(matrix(edges$p.tie2,nrow=sqrt(nrow(edges))),weighted = T,diag=F)
        #convert back to ggplot
            df<-as.data.frame(layout_with_fr(g))
            df$name<-edges$id2[1:sqrt(nrow(edges))]
            df$name<-gsub("LEGACY","",df$name)
            df$name<-gsub(" [/|] ","",df$name)
            df$V1[df$V1>quantile(df$V1,.95,na.rm=T)]<-quantile(df$V1,.95,na.rm=T)
            df$V1[df$V1<quantile(df$V1,.05,na.rm=T)]<-quantile(df$V1,.05,na.rm=T)
            df$V2[df$V2>quantile(df$V2,.95,na.rm=T)]<-quantile(df$V2,.95,na.rm=T)
            df$V2[df$V2<quantile(df$V2,.05,na.rm=T)]<-quantile(df$V2,.05,na.rm=T)
            ggplot(df,aes(x=V1,y=V2,label=name))+
              geom_point()+
              theme_void()+
              geom_label()
        

        #b) Insert colors to indicate important attributes.
            df$recurringpatients<-1-cdf$num_pats/cdf$events
            df$prescriptions<-1-cdf$num_rx_pats/cdf$num_pats
            df$prescriptions[df$prescriptions==min(df$prescriptions,na.rm=T)]<-sort(df$prescriptions)[2]
            df$percentopiates<-1-cdf$num_opiates/cdf$num_presc
            df$percentopiates2<-1-cdf$num_pats_on_opiates/cdf$num_pats
            df$percentopiates3<-1-cdf$num_pats_on_opiates2P/cdf$num_pats_on_opiates
            temp<-!duplicated(df$name)
            
            edges$x_beg<-rep(df$V1,each=91)
            edges$y_beg<-rep(df$V2,each=91)
            edges$x_end<-rep(df$V1,91)
            edges$y_end<-rep(df$V2,91)
            edges$name1<-rep(df$name,each=91)
            edges$name2<-rep(df$name,91)
            


            edges$color=ifelse(edges$p.tie3>=quantile(edges$p.tie3,.9,na.rm=T),"black",NA)
            
                        
            ggplot()+
              geom_segment(data=edges,aes(x=x_beg,y=y_beg,xend=x_end,yend=y_end),size=.0001,color=edges$color)+
              theme_void()+
              geom_label(data=df,aes(x=V1,y=V2,label=name,color=percentopiates3))+
              scale_color_gradient(high="grey70",low="black",na.value ="grey80")+
              theme(legend.position = "none")
            
            
            
                    
        # #remove clinics with less than  3 unique provider or less than 100 events
        #     a<-cdf$clinic[cdf$num_provs<3 | cdf$events<100]
        #     df<-df[!(df$clinic_ls %in% a)]
        # #remove clinics focused on kiddos
        #     a<-cdf$clinic[grepl("pedi",tolower(cdf$clinic))]
        #     df<-df[!(df$clinic_ls %in% a)]
        
        # df<-data.table(
        #   xbeg,
        #   ybeg,
        #   xend,
        #   yend
        # )
        