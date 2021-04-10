# wipes previous workspace
rm(list=ls(all=TRUE)) 

# Install Packages and Load
packages<-c("rtweet","curl","scales","zoo","dplyr",
            "ggplot2","ggthemes","tidyr","jsonlite")
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg)
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)

# Your preferred color scheme (https://www.color-hex.com/color-palette/33490)
col<-c("#CC2529","#396ab1","#3E9651","#DA7C30","#535154","#6B4C9A","#922428","#948B3D")
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = col)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = col)
}

# Set time
accessed<-.POSIXct(Sys.time(), "America/Denver")

# Progress Bar
canada<-as.data.frame(fromJSON('https://api.covid19tracker.ca/reports'))
plotdata<-canada %>%
  select(date=data.date,doses=data.total_vaccinations,dist=data.total_vaccines_distributed,
         full=data.total_vaccinated) %>%
  mutate(date=as.Date(date,"%Y-%m-%d")) %>%
  filter(date>="2020-12-13") %>%
  gather(type,number,-date) %>% 
  filter(date==max(date)) %>%
  select(type,number) %>%
  spread(type,number) %>%
  mutate(total=38008005,
         first=(doses-2*full)/total,
         second=full/total,
         any=first+second) %>%
  select(any,second,total,first)
df<-data.frame(
  first=plotdata$any,
  second=plotdata$second
) %>%
  gather(type,value) %>%
  mutate(total=1)
ggplot(df,aes(type,value,fill=type))+
  geom_col(aes(y=total),fill='gray90',width=0.4)+
  geom_col(width=0.4)+
  annotate('text',x=1.35,y=0,size=5,label=paste("At least one dose:  "),hjust=0)+
  annotate('text',x=2.35,y=0,size=5,label=paste("Fully vaccinated:  "),hjust=0)+
  annotate('text',x=1.35,y=1,size=5,label=paste0(round(100*plotdata$any,1),"%"),hjust=1)+
  annotate('text',x=2.35,y=1,size=5,label=paste0(round(100*plotdata$second,1),"%"),hjust=1)+
  coord_flip()+
  theme_void()+
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = 'none')+
  scale_fill_manual(values=c('dodgerblue',col[1]),'gray')+
  scale_y_continuous(label=percent_format(accuracy = 1),breaks=pretty_breaks(6),limit=c(0,1))+
  labs(x="",y="")
ggsave('progress.png',width=6,height=3,dpi=400)

# Tweet the Graph
vaccineplots_token <- rtweet::create_token(
  app = "vaccineplots",
  consumer_key =    Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token =    Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret =   Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET"),
)
#post_tweet(" ",
#           media="progress.png",
#           token=vaccineplots_token)

