library(maptools)
library(sp)
library(ggplot2)
library(tidyr)
library(dplyr)
library(httr)
library(rjson)
library(stringr)
library(RColorBrewer)

#setwd("C:/Users/Kwezi Family/OneDrive - CDC/r/shiny/apps/nCoV_tracker_ug")

#Download eIDSR data
usr<-"cdcuganda"
psw<-"Cdcug@2020"
urlA<-"https://eidsr.health.go.ug/api/29/analytics/events/query/o6TN8Sr45CZ.json?"
urlB<-"dimension=pe:THIS_YEAR&dimension=ou:akV6429SUqu&dimension=HAZ7VQ730yn&
  dimension=hUvDUjcFmFB&dimension=rpkGPScBEus&dimension=bZGmeF1bpcx&dimension=FQjRK8ei8Ue&
  dimension=CW4bo1xDbto&dimension=sB1IHYu2xQT&dimension=JgbTeRB32lX&dimension=oUqWGeHjj5C&
  dimension=A5HreEUqgcj&dimension=Rq4qM2wKYFL&dimension=fctSQp5nAYl&dimension=j6sEr8EcULP&
  dimension=g4LJbkM0R24&dimension=ttjZl7jvrRj&dimension=v24me96F6XA&dimension=Za0xkyQDpxA&
  dimension=oVFYcqtwPY9&dimension=KxosXJrC3bh&dimension=Rx2fEI9zDJ3&dimension=Fs892x4qy0d&
  dimension=P2gE4lKybQG&dimension=CvJsiC7uXFY&dimension=JTWovJaXgxS&dimension=s52IJ2zlEol&
  dimension=IuwaCGsUr7w&dimension=jMlpubCBAqQ&dimension=TzqawmlPkI5&dimension=oomj0HzoQB5&
  dimension=oPEL0yUsFJ6&dimension=mW7DtDdQBv3&dimension=v4bkZ9UZsXr&dimension=tVntAinnj0j&
  dimension=y4oilvA8mVy&dimension=ymPAOiwkqT0&dimension=ypw02tLMPFQ&dimension=nsohnMrNmaF&
  dimension=HcxURmewqhC&dimension=X30kPftDe6M&dimension=kWrFMAyg6Wd&dimension=qIjb1K7RBSQ&
  dimension=OO9CGZqnlHg&dimension=aWmPaghPPiX&dimension=USNWlaqJYfj&dimension=SwPVr4XgPpH&
  dimension=o7djkXfYZQH&dimension=xKuGAfa9VwT&dimension=oREeMbJdOOi&dimension=B459pxTvHeR&
  dimension=LnR3mUby5q3&dimension=fFfdr1uvr8O&dimension=s3eoonJ8OJb&dimension=z38VCNZh0qV&
  dimension=lRpToX3dhff&dimension=Nedj4eHxkDA&dimension=oR1I0GFQLeB&dimension=m7MH14roDiB&
  dimension=jBkw8u7sVkG&dimension=bo5e2Y6HPK3&dimension=dA6OfHRlWwB&dimension=t1PAsSxkv0e&
  dimension=R7pCw3iE9KA&dimension=a9a5FvQ1mlS&dimension=Ok06T0bZnQP&dimension=qZbnrWP7sAc&
  dimension=grMYEotvVoG&dimension=EcvciwIVAtL&dimension=IUFQDU68rS6&dimension=hCZ3F0EuzdS&
  dimension=VdiTxORMZ24&dimension=NxFYllr5RpU&dimension=fAefJDDlCgz&dimension=sb4e0RdaCy1&
  dimension=xUX5BfGUHjG&dimension=sJeIFfhX8BE&dimension=TRanjlwhC84&dimension=zkzuwNw4Aq0&
  dimension=coUb4QcuVKI&dimension=SBRCbEvwCjZ&dimension=j6LrBVzW3k4&dimension=TMwKRNAYuYB&
  dimension=IGQdwCEWqxs&dimension=hcHUkwpjcDc&dimension=DbbrWq4rFPh&dimension=IliCwhlgfJq&
  dimension=UmXYz5bN9is&dimension=uNNfUKaj0gY&dimension=AHlLUcC9l3T&dimension=tAqxn8xSJG5&
  dimension=OY35IARTLYJ&dimension=eB3mTHxMtLe&dimension=bPBdafWqhyg&dimension=jSCr3wfmnzH&
  dimension=BGIl7Un4Far&dimension=E4Z7JQV0QFt&dimension=kJHY6C05MoS&stage=ChsOd5Hb5hA&displayProperty=NAME&
  outputType=EVENT&desc=eventdate&paging=false]"
eIDSRurl<-URLencode(paste0(urlA,urlB))
IDSR_raw<-httr::GET(eIDSRurl, httr::authenticate(usr,psw)) %>% 
  httr::content("text") %>%
  jsonlite::fromJSON(flatten = TRUE)

cnames<-IDSR_raw$headers[[2]] #Extract column names
IDSR_df<-as.data.frame(IDSR_raw$rows) #Extract data content
names(IDSR_df)<-cnames #Add column names
IDSR_df<-tbl_df(IDSR_df)%>%
  select(Geometry:"v3: Date of death, if applicable") %>%
  separate("District/Sub-county", c("District", "Sub-county"), sep = " : ")%>%
  mutate(District=ifelse(District %in% "" & IDSR_df$`Organisation unit name`=="Kampala District","Kampala District",District)) %>%
  mutate(District=ifelse(District %in% "" & IDSR_df$`Organisation unit name`=="1 Test Facility","Kampala District",District)) %>%
  mutate(District=ifelse(District %in% "" & IDSR_df$`Organisation unit name`=="Mulago National Referral Hospital","Kampala District",District)) %>%
  mutate(District=ifelse(District %in% "" & IDSR_df$`Organisation unit name`=="Wakiso District","Wakiso District",District)) %>%
  mutate(District=ifelse(District %in% "" & IDSR_df$`Organisation unit name`=="Mukono District","Mukono District",District)) %>%
  mutate(District=ifelse(District %in% "Kampala" & IDSR_df$`Organisation unit name`=="Kampala District","Kampala District",District))

  
# Suspects by district (for map)
dist_sum<-tbl_df(data.frame(table(IDSR_df$District))) %>%
  separate(Var1,c("DistName",NA)) %>%
  group_by(DistName) %>%
  summarize(Freq=sum(Freq)) %>%
  mutate_all(funs(toupper)) %>%
  rename(DName2019=DistName)


#Trend line data
counts<- data.frame(table(IDSR_df$`Date of symptoms onset`))
  counts$Var1<-as.Date(counts$Var1)

#Preparing Map data
  ug_map<-rgdal::readOGR("./datasources/uganda_districts_2019_i.shp")
  ug_map@data <- as.data.frame(apply(ug_map@data, 2, function(x) iconv(x, "latin1", "UTF-8"))) %>%
    merge(dist_sum, by = "DName2019", all = TRUE) 
  cut <- cut(as.numeric(ug_map$Freq), breaks = c(0, 25, 50, 75, 100, 150, 200))
  ug_map$cut<- cut
  colors <- colorRampPalette(brewer.pal(9, "YlGnBu"))(6)
  cutColors <- cut(as.numeric(ug_map$Freq), breaks = c(0, 25, 50, 75, 100, 150,200), labels = colors)
  ug_map$colors<-cutColors