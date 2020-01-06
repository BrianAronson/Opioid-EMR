#0) Prep work space
    library(data.table)
    library(igraph)
    library(stringr)
    library(ggplot2)
    library(lme4)
    rawpath <- "hidden"
    dpath <- "hidden"  
    figpath <- "hidden"
    df <- readRDS(file.path(dpath, "erdf_reg.rds"))

#1) delete bad data
    df <- df[df$facility_id != "hidden", ]
    df <- df[!is.na(df$admit_date), ]
    df <- df[!is.na(df$admit_time), ]
    df <- df[as.numeric(substr(df$admit_date2, 6, 9)) > 2006, ]
    df <- df[!is.na(df$race), ]
    df <- df[!is.na(df$marital), ]
    df$prescribed.opiate <- df$prescibed.opiate

#2) subset data and break out all categorical variables
    #a) break out categorical variables
        races<-unique(df$race)
        ncols<-ncol(df)
        df<-as.data.frame(df)
        for(i in 1:length(races)){
          df[,(ncols+i)]<-df$race==races[i]
          names(df)[ncols+i]<-races[i]
        }

        maritals<-unique(df$marital)
        ncols<-ncol(df)
        df<-as.data.frame(df)
        for(i in 1:length(maritals)){
          df[,(ncols+i)]<-df$marital==maritals[i]
          names(df)[ncols+i]<-maritals[i]
        }

        facility_ids<-unique(df$facility_id)
        ncols<-ncol(df)
        df<-as.data.frame(df)
        for(i in 1:length(facility_ids)){
          df[,(ncols+i)]<-df$facility_id==facility_ids[i]
          names(df)[ncols+i]<-facility_ids[i]
        }

    #b) create useful alternative variables
        df$admit_date3<-substr(df$admit_date2,1,9)
        df$admit_date3<-as.Date(df$admit_date3,"%d%b%Y")
        #day of week
            df$day<-weekdays(df$admit_date3)
            df$day<-factor(df$day,levels=unique(df$day))
            df$day<-as.numeric(df$day)
        #time of day
            df$admit_time2<-as.numeric(substr(df$admit_time,1,2))+as.numeric(substr(df$admit_time,4,5))/60
            df$morning<-df$admit_time2<8
            df$afternoon<-df$admit_time2>=8 & df$admit_time2<16
            df$evening<-df$admit_time2>=16
            df$admit_time<-as.numeric(substr(df$admit_time,1,2))+as.numeric(substr(df$admit_time,4,5))/60

    #c) exclude unusuable variables
        df$diagnosis<-NULL
        df$enc_type<-NULL
        df$rx.med<-NULL
        df$rx.freq<-NULL
        df$rx.strength<-NULL
        df$rx_units<-NULL
        df$marital<-NULL
        df$race<-NULL
        df$facility_id<-NULL
        df$NA.1<-NULL
        # df$pat_id<-NULL
        # df$enc_id<-NULL
        df$prov_id<-NULL
        df$ref_id<-NULL
        df$rx.provider<-NULL
        df$admit_time2<-NULL
        # df$admit_time<-NULL

    #e) kill NAs
        df<-df[!apply(df,1,function(x)any(is.na(x))),]

    #optionals)
        #Kill DRH
            df1<-df[df$hidden==1,]
        #extract and kill enc_ids
            encs<-df1$enc_id

#2) load diagnoses
    mat<-readRDS(file.path(dpath, "erdiagnoses.rds"))

#3) make predicted crowding variable
    df1$year2<-factor(year(df1$admit_date3))
    df1$admit_time2<-factor(floor(df1$admit_time))
    df1$day2<-factor(df1$day)
    model<-lm(last4hr~ day2+admit_time2+year2,data=df1)
    predcr<-predict(model)
    df1$crowding<-df1$last4hr/predcr

#4) make/amend other variables
    df1$birth_date[df1$birth_date<1900]
    df1$age[df1$age>100]<-100
    df1$age2<-df1$age^2
    df1$prev.opiate.num2<-df1$prev.opiate.num^2
    df1$time<-as.numeric(as.character(df1$year))

    df1$crowding

    
sub1<-df1$last4hr<10
sub2<-df1$last4hr<15 & df1$last4hr>=10
sub3<-df1$last4hr<20 & df1$last4hr>=15
sub4<-df1$last4hr<30 & df1$last4hr>=20
sub5<-df1$last4hr>=30
cor(df1$prescibed.opiate[sub5],df1$last4hr[sub5])

perc.rank<-function(x) rank(x)/length(x)
sumfun<-function(x,y){
  df1$t1<-perc.rank(x)
  df1$t1<-round(df1$t1*20)/20
  df2<-data.table(df1)
  df3<-df2[,.(mean(prescribed.opiate)),by="t1"]
  df3<-df3[order(df3$t1),]
  if(sum(df3$t1==1)==0){
    df3<-rbind(df3,df3[nrow(df3),])
    df3[nrow(df3),]$t1<-1
  }
  df3[df3$t1==1]$V1<-df3[df3$t1==.95]$V1
  df3$group<-y
  return(df3)
}
names(df1$lasthr)

gdf<-sumfun(df1$last4hr,"Patients in last 4 hrs")
gdf<-rbind(gdf,sumfun(df1$last24hr,"Patients in last 24 hrs"))
gdf<-rbind(gdf,sumfun(df1$crowding,"Weighted Crowding"))
gdf$group<-factor(gdf$group,levels = c("Patients in last 4 hrs","Patients in last 24 hrs","Weighted Crowding"))

p<-ggplot(data=gdf,aes(y = V1, x = t1,color=group,linetype=group))+
  geom_smooth(size = 2,se = F)+
  theme_bw()+
  scale_color_grey()+
  coord_cartesian(ylim = c(.3,.38))+
  labs(x="Crowding (percentile)",y="Patients Prescribed (percentage)",color="Crowding Measure",linetype="Crowding Measure",title = "Figure 2: Percent of ER Patients Prescribed Opiates by Degree of ER Crowding")+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(text=element_text("serif"))
  
  
ggsave(file.path(figpath, "Figure 2 - Opiate by Crowding.pdf"),plot=p,width = 8.5,height=3,units="in")

