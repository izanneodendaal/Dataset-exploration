#get vessels
library(dplyr)
agrihubvessel.imo<-read_xlsx("Desktop/agrihubvessel_imo.xlsx")

agrihubvessel.imo<-agrihubvessel.imo %>% arrange(desc(importance))

dimves<-agrihubvessel.imo[1:25,]
agrihubvessel.imo$id_vessel[agrihubvessel.imo$id_vessel=='173'] <- '403'
agrihubvessel.imo<-agrihubvessel.imo[ !duplicated(agrihubvessel.imo[, c("id_vessel", "imo")]),]
View(agrihubvessel.imo)
dimves$importance<-NULL
###########################################################################################################
#get stack
data_stackdates<-read_xlsx("Desktop/data_stackdates.xlsx")

stack<-data_stackdates %>%
  group_by(documentdate, id_vessel) %>%
  arrange(documentdate,time)
stack$TimeDiff<- difftime(stack$stackdateopen_clean ,stack$documentdate , units = c("days"))

stack<-stack[stack$TimeDiff <= 7 & stack$TimeDiff>=0,]
View(stackdates1)

stack$id<-NULL
stack$time<-NULL
stack$vesselname<-NULL


stackdates<-stack[ !duplicated(stack[, c("id_vessel", "documentdate")], fromLast=T),]
stackdates1<-stackdates %>% 
  select(documentdate, id_vessel, stackdateopen_clean, stackdateclose_clean, TimeDiff) %>%
  filter(id_vessel == "1174" | id_vessel == "1519" | id_vessel == "1664" | id_vessel == "1684" | id_vessel == "2086" | id_vessel == "2208" |
           id_vessel == "2222" | id_vessel == "166" | id_vessel == "403" | id_vessel == "423" | id_vessel == "426" |
           id_vessel == "476" | id_vessel == "477" | id_vessel == "501" | id_vessel == "536" | id_vessel == "544" |
           id_vessel == "546" | id_vessel == "629" | id_vessel == "699" | id_vessel == "700" | id_vessel == "702" |
           id_vessel == "706" | id_vessel == "707" | id_vessel == "708" | id_vessel == "710" )
colnames(stackdates1)[1] <- "Date"
stackdates1$Date<-as.Date(stackdates1$Date)
stackdates1$id_vessel<-as.character(stackdates1$id_vessel)
###########################################################################################################
#get berth
databerthing<-read_xlsx("Desktop/finaldataberthing.xlsx")
duplicated(db)
db<-databerthing[ !duplicated(databerthing[, c("IMO", "DOCK_TIMESTAMP_LT")]),]
View(db)

db$BERTH_ID<-NULL
db$TERMINAL_ID<-NULL
###########################################################################################################
#base
base<-seq(as.Date("2019-01-01"), as.Date("2020-09-16"), "days")
base<-as.data.frame.Date(base)
View(base)


base1<-cbind(id, base)
colnames(base) <- "Date"
base$Date<-as.Date(base$Date)
###########################################################################################################
#containers
containers<-read_xlsx("Desktop/finalcontainerinfo.xlsx")
View(containers)
containers1<-containers %>% 
  select(id_vessel, DateTimeInHarbour, NumberOfContainers, VesselETA, VesselATA, VesselETD, VesselATD) %>%
  filter(id_vessel == "1174" | id_vessel == "1519" | id_vessel == "1664" | id_vessel == "1684" | id_vessel == "2086" | id_vessel == "2208" |
           id_vessel == "2222" | id_vessel == "166" | id_vessel == "403" | id_vessel == "423" | id_vessel == "426" |
           id_vessel == "476" | id_vessel == "477" | id_vessel == "501" | id_vessel == "536" | id_vessel == "544" |
           id_vessel == "546" | id_vessel == "629" | id_vessel == "699" | id_vessel == "700" | id_vessel == "702" |
           id_vessel == "706" | id_vessel == "707" | id_vessel == "708" | id_vessel == "710" )
###########################################################################################################
j1<-merge(base, dimves)
View(fill1)
lj = left_join(j1, stackdates1, by = c("Date", "id_vessel"))
lj$TimeUntilStackOpen<- difftime(lj$stackdateopen_clean, lj$Date , units = c("days"))
fill1<-lj %>% 
  group_by(id_vessel, Date) %>% 
  fill(stackdateopen_clean, stackdateclose_clean) %>% #default direction down
  fill(stackdateopen_clean, stackdateclose_clean, .direction = "down")

ljj<-lj[lj$id_vessel != "173",]
View(ljj)
library(runner)
lj %>%
  group_by(id_vessel) %>%
  mutate(across(stackdateopen_clean, ~ coalesce(.x, last(.x[!is.na(.x)]))))
write_xlsx(as.data.frame(lj), path = "lj.xlsx")
###################################################################################################################
sb<-read_xlsx("Desktop/stackberth.xlsx")
View(sb)
sum(is.na(csbw))
sb1<-sb[sb$id_vessel != "173",]
View(sb1)
colnames(containers1)[2] <- "date"
csb = left_join(sb1, containers1, by = c("date", "id_vessel"))
containers1$date<-as.Date(containers1$date)
sb1$date<-as.Date(sb1$date)
containers1$id_vessel<-as.character(containers1$id_vessel)
sb1$id_vessel<-as.character(sb1$id_vessel)
View(csb)
csb$VesselETA<-NULL
csb$VesselATA<-NULL
csb$VesselETD<-NULL
csb$VesselATD<-NULL

csb$NumberOfContainers[is.na(csb$NumberOfContainers)] <- 0

wind<-read_xlsx("Desktop/finalwind.xlsx")
View(csbw)
colnames(wind)[1] <- "Date"
colnames(wind)[2]<-"Windspeed (Knots)"
wind$date<-as.Date(wind$date)
csbw = left_join(csb, wind, by = ("date"))
View(c)

csbw$next_stackopen_date<-as.Date(csbw$next_stackopen_date)
#403
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '403' & days_stack_open < 0), as.Date("2020-08-16"), as.Date(next_stackopen_date))))
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '403' & days_dock < 0), as.Date("2021-01-02"), as.Date(next_dock_date))))
#423
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '423' & days_dock < 0), as.Date("2020-09-29"), as.Date(next_dock_date))))
#426
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '426' & days_dock < 0), as.Date("2021-01-06"), as.Date(next_dock_date))))
#476
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '476' & days_stack_open < 0), as.Date("2020-08-27"), as.Date(next_stackopen_date))))
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '476' & days_dock < 0), as.Date("2020-08-30"), as.Date(next_dock_date))))
#477
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '477' & days_stack_open < 0), as.Date("2020-08-31"), as.Date(next_stackopen_date))))
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '477' & days_dock < 0), as.Date("2020-10-18"), as.Date(next_dock_date))))
#501
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '501' & days_dock == 1000), as.Date("2019-05-31"), as.Date(next_dock_date))))
#536
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '536' & days_stack_open < 0), as.Date("2019-07-18"), as.Date(next_stackopen_date))))
#544
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '544' & days_stack_open < 0), as.Date("2020-05-05"), as.Date(next_stackopen_date))))
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '544' & days_dock < 0), as.Date("2020-12-07"), as.Date(next_dock_date))))
#546
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '546' & days_stack_open < 0), as.Date("2020-05-05"), as.Date(next_stackopen_date))))
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '546' & days_dock < 0), as.Date("2020-03-29"), as.Date(next_dock_date))))
#629
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '629' & days_stack_open < 0), as.Date("2020-05-06"), as.Date(next_stackopen_date))))
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '629' & days_dock < 0), as.Date("2020-09-25"), as.Date(next_dock_date))))
#699
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '699' & days_dock == 1000), as.Date("2020-09-13"), as.Date(next_dock_date))))
#700
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '700' & days_stack_open < 0), as.Date("2020-09-07"), as.Date(next_stackopen_date))))
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '700' & days_dock < 0), as.Date("2020-11-12"), as.Date(next_dock_date))))
#702
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '702' & days_stack_open < 0), as.Date("2020-09-21"), as.Date(next_stackopen_date))))
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '702' & days_dock < 0), as.Date("2020-11-12"), as.Date(next_dock_date))))
#706
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '706' & days_dock < 0), as.Date("2020-09-20"), as.Date(next_dock_date))))
#707
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '707' & days_dock < 0), as.Date("2020-09-24"), as.Date(next_dock_date))))
#708
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '708' & days_stack_open < 0), as.Date("2020-09-24"), as.Date(next_stackopen_date))))
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '708' & days_dock < 0), as.Date("2020-09-27"), as.Date(next_dock_date))))
#710
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '710' & days_stack_open < 0), as.Date("2020-08-24"), as.Date(next_stackopen_date))))
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '710' & days_dock < 0), as.Date("2020-10-10"), as.Date(next_dock_date))))
#1174
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '1174' & days_stack_open < 0), as.Date("2019-09-05"), as.Date(next_stackopen_date))))
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '1174' & days_dock < 0), as.Date("2019-09-09"), as.Date(next_dock_date))))
#1519
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '1519' & days_stack_open < 0), as.Date("2020-09-08"), as.Date(next_stackopen_date))))
#1664
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '1664' & days_stack_open < 0), as.Date("2020-09-10"), as.Date(next_stackopen_date))))
#1684
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '1684' & days_stack_open < 0), as.Date("2020-07-13"), as.Date(next_stackopen_date))))
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '1684' & days_dock < 0), as.Date("2020-07-19"), as.Date(next_dock_date))))
#2086
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '2086' & days_stack_open < 0), as.Date("2020-04-14"), as.Date(next_stackopen_date))))
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '2086' & days_dock < 0), as.Date("2020-04-19"), as.Date(next_dock_date))))
#2208
csbw<-csbw %>%
  mutate(next_stackopen_date = as.Date(ifelse((id_vessel == '2208' & days_stack_open == 1000), as.Date("2020-09-14"), as.Date(next_stackopen_date))))
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '2208' & days_dock == 1000), as.Date("2020-09-19"), as.Date(next_dock_date))))
#2222
csbw<-csbw %>%
  mutate(next_dock_date = as.Date(ifelse((id_vessel == '2222' & days_dock == 1000), as.Date("2020-11-05"), as.Date(next_dock_date))))


csbw$Time_UntilNext_StackDate<- difftime(csbw$next_stackopen_date ,csbw$date , units = c("days"))
csbw$Time_UntilNext_DockDate<- difftime(csbw$next_dock_date ,csbw$date , units = c("days"))

csbw$days_stack_open<-NULL
csbw$days_dock<-NULL


colnames(csbw)[1] <- "Date"
colnames(csbw)[2] <- "IMO"
colnames(csbw)[3] <- "ID_Vessel"
colnames(csbw)[4] <- "Next_StackOpen_Date"
colnames(csbw)[5] <- "Next_Dock_Date"
colnames(csbw)[6] <- "Number_Of_Containers"
colnames(csbw)[7] <- "Wind_Knots"
sum(is.na(csbw))

write_xlsx(as.data.frame(csbw), path = "FinalMerge.xlsx")
###################################################################################################################
contves<-csbw %>%
  group_by(ID_Vessel) %>%
  summarise(Number_Of_Containers = sum(Number_Of_Containers))

csbw$Number_Of_Containers<-as.numeric(csbw$Number_Of_Containers)
class(csbw$Number_Of_Containers)

ggplot(contves, aes(x=ID_Vessel, y=Number_Of_Containers)) + 
  geom_bar(stat="identity", fill="sea green") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs( x="Vessel ID", y="Number of containers")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))

windd<-unique(csbw$Wind_Knots)
plot(wind)

mfs<-read_xlsx("Desktop/FinalMFS.xlsx")
View(mfs)

ggplot(mfs, aes(x=Date, y=NumberOfContainers)) + 
  geom_bar(stat="identity", fill="coral2") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs( x="Date", y="Number of containers")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))

week<-mfs$Week
ggplot(mfs, aes(x=factor(Week), y=NumberOfContainers)) + 
  geom_bar(stat="identity", fill="skyblue3") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs( x="Week ", y="Number of containers")+
  scale_x_discrete(breaks = week[c(T,F,F)]) +
  theme(legend.position = "bottom",
        axis.text.x = element_text( angle = 90, size=5, face=1))

ggplot(mfs, aes(x=factor(PublicHolidays), y=NumberOfContainers)) + 
  geom_bar(stat="identity", fill="darkseagreen2") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs( x="Public Holidays", y="Number of containers")+
  theme(legend.position = "bottom")

ggplot(mfs, aes(x= Month, y=NumberOfContainers)) + 
  geom_bar(stat="identity", fill="mediumpurple1") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs( x="Month", y="Number of containers")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(mfs, aes(x= Year, y=NumberOfContainers)) + 
  geom_bar(stat="identity", fill="mediumaquamarine") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs( x="Year", y="Number of containers")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))

plot(mfs$Date, mfs$NumberOfContainers)

##########################################################################################################
efs<-read_xlsx("Desktop/FinalMerge.xlsx")
mfs<-read_xlsx("Desktop/FinalMFS.xlsx")
View(mfs)

ggplot(efs, aes(x= Time_UntilNext_DockDate, y=Number_Of_Containers)) + 
  geom_bar(stat="identity", fill="mediumaquamarine") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs( x="Year", y="Number of containers")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))






















