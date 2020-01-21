#0) Prep work space
    library(igraph)
    library(data.table)
    library(igraph)
    library(stringr)
    library(ggplot2)
    library(scales)
    library(cowplot)
    library(gridExtra)
    rawpath <- "hidden"
    dpath <- "hidden"    
    figpath <- "hidden"

#1) read data
    df1 <- readRDS(file.path(dpath, "erdf_reg.rds"))
    
#2) Convert admit date to date format
    df1$admit_date3 <- substr(df1$admit_date2, 1, 9)
    df1$admit_date3 <- as.Date(df1$admit_date3, "%d%b%Y")
    df1$admit_time2 <- as.numeric(substr(df1$admit_time, 1, 2)) + as.numeric(substr(df1$admit_time, 4, 5)) / 60
    
#3) kill bad data
    df <- df1[df1$facility_id != "hidden", ]
    df <- df[!is.na(df$admit_date), ]
    df <- df[!is.na(df$admit_time), ]
    df <- df[as.numeric(substr(df$admit_date2, 6, 9)) > 2007, ]
    df <- df[!is.na(df$race), ]
    df <- df[!is.na(df$marital), ]
    df$prescribed.opiate <- df$prescibed.opiate
    
#4) prep graph data
    #a) visits per day
        g1<-df[,.(.N),by="admit_date3"]
        names(g1)[1]<-"days5"
    #b) visits per day of year
        g2<-g1
        g2$days5<-as.character(g2$days5)
        g2$days5<-paste(2007,substr(g2$days5,5,nchar(g2$days5[1])),sep="")
        g2$days5<-as.Date(g2$days5)
        g2<-g2[!is.na(g2$days5),]
        g2<-g2[,.(mean(N)),by="days5"]
    #c) visits by day of month
        g3<-g1
        g3$days5<-format(g3$days5,"%d")
        g3<-g3[,.(mean(N)),by="days5"]
        g3$days5<-as.numeric(g3$days5)
    #d) visits by day of week
        g4<-g1
        g4$days5<-weekdays(g4$days5)
        g4<-g4[,.(mean(N)),by="days5"]
        g4$days5<-factor(g4$days5,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
        levels(g4$days5) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    #e) visit by hour of day
        df$admit_time3<-floor(df$admit_time2)
        g5<-df[,.(.N),by=c("admit_date3","admit_time3")]
        names(g5)[2]<-"days5"
        g5<-g5[,.(mean(N)),by="days5"]
        g5<-g5[order(g5$days5),]
        names(g5)[2]<-"N"

#5) make graphs
        gr1 <- ggplot(g1, aes(x = days5, y = N)) +
            geom_point() +
            geom_smooth(color = "red", size = 2, se = F) +
            theme_bw() +
            theme(text = element_text("serif")) +
            labs(x = "", y = "") +
            ggtitle("Number ED Visits by Day across all Years") +
            coord_cartesian(ylim = c(0, 105))
        gr2 <- ggplot(g2, aes(x = days5, y = V1)) +
            geom_point(color = "grey80") +
            geom_smooth(color = "black", size = 2, se = F) +
            theme_bw() +
            theme(text = element_text("serif"),
                  axis.text = element_text(size = 13)) +
            scale_x_date(labels = date_format("%b")) +
            labs(x = "", y = "") +
            ggtitle("Average Number ED Visits by Day of Year") +
            coord_cartesian(ylim = c(50, 90),
                            xlim = as.Date(c('10/1/2007', '21/12/2007'), format = "%d/%m/%Y")) +
            scale_y_continuous(breaks = c(50, 60, 70, 80, 90))
        gr3 <- ggplot(g3, aes(x = days5, y = V1)) +
            geom_point(color = "grey80", size = 2) +
            geom_smooth(color = "black", size = 2, se = F) +
            theme_bw() +
            theme(text = element_text("serif"),
                  axis.text = element_text(size = 13)) +
            labs(x = "", y = "") +
            ggtitle("Average Number ED Visits by Day of Month") +
            coord_cartesian(ylim = c(60, 80)) +
            scale_y_continuous(breaks = c(60, 65, 70, 75, 80))
        gr4 <- ggplot(g4, aes(x = days5, y = V1, group = 1)) +
            geom_point(color = "grey80", size = 2) +
            geom_smooth(
                color = "black",
                size = 1.5,
                se = F,
                method = "lm"
            ) +
            theme_bw() +
            theme(text = element_text("serif"),
                  axis.text = element_text(size = 13)) +
            labs(x = "", y = "") +
            ggtitle("Average Number ED Visits by Day of Week") +
            coord_cartesian(ylim = c(60, 80), xlim = c(1.5, 6.5)) +
            scale_y_continuous(breaks = c(60, 65, 70, 75, 80))
        gr5 <- ggplot(g5, aes(x = days5, y = N)) +
            geom_point(color = "grey80", size = 2) +
            geom_smooth(
                color = "black",
                size = 2,
                se = F,
                span = .6
            ) +
            theme_bw() +
            theme(text = element_text("serif"),
                  axis.text = element_text(size = 13)) +
            labs(x = "", y = "") +
            ggtitle("Average Number ED Visits by Hour of Day") +
            coord_cartesian(xlim = c(0.5, 22.5), ylim = c(0, 5))
        
#6) append all graphs to one png
        p <- plot_grid(gr2, gr3, gr4, gr5, ncol = 2) #gr1
        # title<-ggdraw()+draw_label("Figure 1: Frequency of ED Visits by Unit of Time",fontfamily = "serif",fontface = "bold",size = 20)
        # p<-plot_grid(title,p,nrow=2,rel_heights=c(.1,.9))
        ggsave(file.path(figpath, "Figure 1 - ED Crowding.png"), 
            plot = p,
            width = 10,
            height = 6,
            units = "in"
        )
        
        