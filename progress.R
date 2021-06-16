# Install Packages and Load
packages<-c("rtweet","curl","ggplot2","jsonlite")
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg)
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)

# Set time
accessed_date<-format(as.POSIXlt(Sys.time(), "EST5EDT" ),"%b %d")
accessed_date<-gsub(" 0"," ",accessed_date)
accessed_time<-format(as.POSIXlt(Sys.time(), "EST5EDT" ),"%H:%M")
accessed<-paste0(accessed_date," at ",accessed_time," ET")

# Progress Bar
latest_data<-as.data.frame(fromJSON('https://api.covid19tracker.ca/summary'))
df<-data.frame(
  type=c("first","second"),
  value=c((as.numeric(latest_data$data.total_vaccinations)-
             as.numeric(latest_data$data.total_vaccinated))/38328048, # latest real-time estimate from https://www150.statcan.gc.ca/n1/pub/71-607-x/71-607-x2018005-eng.htm
          as.numeric(latest_data$data.total_vaccinated)/38328048)
)
ggplot(df,aes(type,value,fill=type))+
  geom_col(aes(y=1),fill='gray90',width=0.5)+
  geom_col(width=0.5)+
  annotate('text',x=1.4,y=0,size=5,label=paste("At least one dose:  "),hjust=0)+
  annotate('text',x=2.4,y=0,size=5,label=paste("Fully vaccinated (% of Total Pop.):  "),hjust=0)+
  annotate('text',x=1.4,y=1,size=5,label=paste0(round(100*df[1,2],1),"%"),hjust=1)+
  annotate('text',x=2.4,y=1,size=5,label=paste0(round(100*df[2,2],1),"%"),hjust=1)+
  coord_flip()+
  theme_void()+
  theme(panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = 'none')+
  scale_fill_manual(values=c('dodgerblue','#CC2529'),'gray')+
  scale_y_continuous(limit=c(0,1))+
  labs(x="",y="")
ggsave('Plots/progress.png',width=5.5,height=3,dpi=400)

# Tweet the Graph
vaccineplots_token <- rtweet::create_token(
  app = "vaccineplots",
  consumer_key =    Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token =    Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret =   Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET"),
)
post_tweet(paste("Canada's COVID vaccination progress as of",accessed),
           media="Plots/progress.png",
           token=vaccineplots_token)
