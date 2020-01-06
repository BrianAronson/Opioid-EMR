#1 - Load Libraries
    library(igraph)
    library(data.table)
    library(igraph)
    library(stringr)
    library(ggplot2)
    rawpath <- "hidden"
    dpath <- "hidden"  
    
#referrals among key providers:
        table(uniprovs %in% df$RAW_SEDI_REFERRING_PROV_KEY[df$RAW_SEDI_REFERRING_PROV_KEY!=df$PROVIDERID])
            #it seems that most providers served as referrals for other providers. Still not 100% sure what to think of this variable.
    #convert to edgelist
        tedg<-df[df$PROVIDERID %in% uniprovs & df$RAW_SEDI_REFERRING_PROV_KEY %in% uniprovs & df$RAW_SEDI_REFERRING_PROV_KEY!=df$PROVIDERID,]
            #a large proportion of referrals are through other providers
        tedg<-tedg[,c("PROVIDERID","RAW_SEDI_REFERRING_PROV_KEY")] #,"ADMIT_DATE"
        a<-data.table(table(tedg))
        a<-a[a$N!=0,]
        names(a)<-c("id2","id1","n")
        refmat<-data.table(id1=rep(uniprovs,each=length(uniprovs)), id2=rep(uniprovs,length(uniprovs)),n=0)
        refmat<-refmat[!(paste(refmat$id1,refmat$id2) %in% paste(a$id1,a$id2)),]
        refmat<-rbind(refmat,a)
        refmat<-data.table(sapply(refmat,as.numeric))
        refmat<-refmat[order(refmat$id1,refmat$id2),]
    #graph
        g<-graph.adjacency(matrix(refmat$n,nrow=sqrt(nrow(refmat))))
        # b<-as.matrix(a[,c(2,1)])
        # g<-graph.edgelist(b)
        g<-decompose.graph(g)[[1]]
        # d<-b[sample(1:nrow(b),1000),]
        # g<-graph.edgelist(d)
        # g<-decompose.graph(g)[[1]]
        # plot(g,vertex.label=NA,edge.arrow.size=0,edge.width=NA,vertex.size=1,vertex.color="black") #edge.color="black",
        
        
#Providers by shared patients
    tedg<-df[df$PROVIDERID %in% uniprovs,]
    tedg<-tedg[,c("PROVIDERID","PATID")] #,"ADMIT_DATE"
    tedg<-tedg[!duplicated(tedg)]
    tedg$PATID<-paste("p",tedg$PATID)
    #project to one mode
        #create list of projection replacements
            # tedg2<-tedg[sample(1:nrow(tedg),100000),]
            a<-split(tedg,by="PATID")
            a<-sapply(a,function(x) as.numeric(unlist(x[,1])))
        #create vector of projected receivers
            b<-a[c(tedg$PATID)]
            newr<-as.numeric(unlist(b))
        #duplicate senders accordingly
            b<-as.numeric(sapply(b,length))
            news<-rep(tedg$PROVIDERID,b)
        #turn into edge list
            tedg3<-data.table(id1=newr,id2=news)
            rm(a,b,newr,news,df3)
        #since non-directed, make a copy in opposite order
            # system.time({
            #   tedg3<-data.table(id1=(apply(tedg3,1,min)),id2=(apply(tedg3,1,max)))
            # })
            tedg4<-tedg3
            tedg4$id1<-tedg3$id2
            tedg4$id2<-tedg3$id1
            tedg3<-rbind(tedg3,tedg4)
        #count duplicates
            tedg3<-as.data.frame(table(tedg3))
            tedg3<-tedg3[tedg3$Freq!=0,]    
            tedg3<-data.table(tedg3)
            tedg3<-tedg3[order(tedg3$id1,tedg3$id2)]
            tedg3<-tedg3[tedg3$id1!=tedg3$id2]
            names(tedg3)[3]<-"n"
            length(unique(tedg3$id2))
                #almost everyone is tied to at least one other provider
            
        
            
        #add 0s for all empty edges
            provmat<-data.table(id1=rep(uniprovs,each=length(uniprovs)), id2=rep(uniprovs,length(uniprovs)),n=0)
            provmat<-provmat[!(paste(provmat$id1,provmat$id2) %in% paste(tedg3$id1,tedg3$id2)),]
            provmat<-rbind(provmat,tedg3)
            provmat$id1<-as.numeric(as.character(provmat$id1))
            provmat$id2<-as.numeric(as.character(provmat$id2))
            provmat<-data.table(provmat)
            provmat<-provmat[order(provmat$id1,provmat$id2),]
            rm(tedg,tedg3,tedg4)
        
        #convert to two matrices; one that counts all edges, one that counts if any edge
            a<-matrix(provmat$n,nrow=sqrt(nrow(provmat)))
            b<-a
            b[b>0]<-1

    
        #get some network stats
            g<-graph.adjacency(b,mode="undirected")
            #degree distributions
            temp<-degree(g)
            summary(temp)    
            plot(density(temp))
            
            temp<-evcent(g)[[1]]
            summary(temp)    
            plot(density(temp))
            
                # plot(density(degree(g,mode=c("in"))))
            
            g<-decompose.graph(g)[[1]]
            png(file=paste(Directory,"Patient Network",sep=""),height=8.5,width=11,units="in",res=300)
            plot(g,vertex.label=NA,edge.arrow.size=0,edge.width=0,vertex.size=2,vertex.color="black") #edge.color="black",
            dev.off()
            
            
    # #the above method is way faster than the alternative with igraph
    #     g<-graph.edgelist(as.matrix(tedg))
    #     V(g)$type<-bipartite.mapping(g)$type
    #     g<-bipartite.projection(g)$proj1
    #     a<-as.data.frame(get.edgelist(g))
    #     a<-(sapply(a,function(x) as.numeric(as.character(x))))


    


              
