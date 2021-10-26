agrihubvessel.imo<-read_xlsx("Desktop/agrihubvessel_imo.xlsx")
View(agrihubvessel.imo)
summary(agrihubvessel.imo)
agrihubvessel.imo$id_vessel<-as.character(agrihubvessel.imo$id_vessel)
agrihubvessel.imo %>% 
  filter(!is.na(imo)) %>% 
  group_by(imo) %>% 
  count()
imo<-table(agrihubvessel.imo$imo)
write_xlsx(as.data.frame(imo), path = "vesselimo.xlsx")
sapply(agrihubvessel.imo, function(x) length(unique(x)))
agrihubvessel.imo %>% 
  filter(!is.na(id_vessel)) %>% 
  group_by(id_vessel) %>% 
  count()
vid<-table(agrihubvessel.imo$id_vessel)
vid
barplot(vid, xlab="id",ylab="occurance", main="Agrihub vessel id")
write_xlsx(as.data.frame(vid), path = "vesselid.xlsx")
aa<-table(agrihubvessel.imo$id_vessel,agrihubvessel.imo$imo)
aa
write_xlsx(as.data.frame(aa), path = "vesselid.xlsx")
#delete:
agrihubvessel.imo$importance<-NULL
agrihubvessel.imo$id_vessel[agrihubvessel.imo$id_vessel=="2190"]<-"2216"
agrihubvessel.imo$id_vessel[agrihubvessel.imo$id_vessel=="662"]<-"2082"
agrihubvessel.imo$id_vessel[agrihubvessel.imo$id_vessel=="1640"]<-"2239"
agrihubvessel.imo$id_vessel[agrihubvessel.imo$id_vessel=="173"]<-"403"
is.na(agrihubvessel.imo$imo)<-agrihubvessel.imo$imo == "1111111"
agrihub<-na.omit(agrihubvessel.imo)
View(agrihub)
View(agri)
dimves<-read_xlsx("Desktop/dimves.xlsx")
View(dv)
dv<-dimves[complete.cases(dimves), ]
dv$VesselName<-NULL
dv$VesselType<-NULL
unique(dv$IMO)
table(dv$IMO)
#####################################################################################################################
containerinfo<-read_xlsx("Desktop/containerinfo.xlsx")
View(containerinfo)
summary(containerinfo)
containerinfo$Agrihub_Ref<-as.character(containerinfo$Agrihub_Ref)
table(containerinfo$Agrihub_Ref)
containerinfo$id_port<-as.character(containerinfo$id_port)
containerinfo$id_vessel<-as.character(containerinfo$id_vessel)
#delete columns with NA's equal to 282699
containerinfo$FreightKind<-NULL
containerinfo$Power<-NULL
containerinfo$VesselFacility<-NULL
containerinfo$LinerPODETA<-NULL
containerinfo$LinerPODATA<-NULL
containerinfo$LinerVesselName<-NULL
containerinfo$LinerVoyage<-NULL
containerinfo$LinerPOD<-NULL
table(containerinfo$Agrihub_Ref)
table(containerinfo$ContainerNo)
#see cardinality
sapply(containerinfo, function(x) length(unique(x)))
sapply(containerinfo$ShipLine, function(x) sum(is.na(x)))
#date harbour in and date harbour out
sum(complete.cases(containerinfo$DateTimeInHarbour))
#shipline
sum(complete.cases(containerinfo$ShipLine))
is.na(containerinfo$ShipLine) <- containerinfo$ShipLine == "_"
is.na(containerinfo$ShipLine) <- containerinfo$ShipLine == "0"
sum(is.na(containerinfo$ShipLine))
# to see the mode and 2nd mode of shipline
shipline<-na.omit(containerinfo$ShipLine)
table(shipline)
#vessel visit ref
sum(complete.cases(containerinfo$VesselVisitRef))
is.na(containerinfo$VesselVisitRef) <- containerinfo$VesselVisitRef == " "
sum(is.na(containerinfo$VesselVisitRef))
#to see mode and second mode of vesselvisit ref
vesselvisit<-na.omit(containerinfo$VesselVisitRef)
table(vesselvisit)
#Category
is.na(containerinfo$Category) <- containerinfo$Category == " "
sum(is.na(containerinfo$Category))
containerinfo$Category[containerinfo$Category=="Export"]<-"EXPORT"  #make the same
containerinfo$Category[containerinfo$Category=="Through"]<-"THROUGH" #make the same
containerinfo$Category[containerinfo$Category=="Transship"]<-"TRANSSHIP" #make the same
#to see mode and second mode
cat<-na.omit(containerinfo$Category)
table(cat)
#HarbourStatus
is.na(containerinfo$HarbourStatus) <- containerinfo$HarbourStatus == " "
sum(is.na(containerinfo$HarbourStatus))
containerinfo$HarbourStatus[containerinfo$HarbourStatus=="Departed"]<-"DEPARTED" #make the same
containerinfo$HarbourStatus[containerinfo$HarbourStatus=="Inbound"]<-"INBOUND" #make the same
containerinfo$HarbourStatus[containerinfo$HarbourStatus=="Loaded"]<-"LOADED" #make the same
containerinfo$HarbourStatus[containerinfo$HarbourStatus=="Yard"]<-"YARD" #make the same
#to see mode
hs<-na.omit(containerinfo$HarbourStatus)
table(hs)
#PODcode
is.na(containerinfo$PODCode) <- containerinfo$PODCode == " "
sum(is.na(containerinfo$PODCode))
#to see modes
podc<-na.omit(containerinfo$PODCode)
table(podc)
#PODDescrip
is.na(containerinfo$PODDescrip) <- containerinfo$PODDescrip == " "
sum(is.na(containerinfo$PODDescrip))
containerinfo$PODDescrip[containerinfo$PODDescrip=="Abu Dhabi"]<-"ABU DHABI"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Algeciras"]<-"ALGECIRAS"
containerinfo$PODDescrip[containerinfo$PODDescrip=="ALGECIRAS (ML TERMINAL)"]<-"ALGECIRAS"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Antwerpen"]<-"ANTWERPEN"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Apapa"]<-"APAPA"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Bremerhaven"]<-"BREMERHAVEN"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Busan (ex Pusan)"]<-"BUSAN (EX PUSAN)"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Cape Town"]<-"CAPE TOWN"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Colombo"]<-"COLOMBO"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Dar es Salaam"]<-"DAR ES SALAAM"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Durban"]<-"DURBAN"
containerinfo$PODDescrip[containerinfo$PODDescrip=="East London"]<-"EAST LONDON"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Freeport, Grand Bahama"]<-"FREEPORT, GRAND BAHAMA"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Hamburg"]<-"HAMBURG"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Hong Kong"]<-"HONG KONG"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Jebel Ali"]<-"JEBEL ALI"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Kaohsiung (Taoshing)"]<-"KAOHSIUNG (TAOSHING)"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Keelung (Chilung)"]<-"KEELUNG (CHILUNG)"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Khor al Fakkan"]<-"KHOR AL FAKKAN"
containerinfo$PODDescrip[containerinfo$PODDescrip=="King Abdullah "]<-"KING ABDULLAH"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Kobe"]<-"KOBE"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Kribi"]<-"KRIBI"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Lagos"]<-"LAGOS"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Las Palmas"]<-"LAS PALMAS"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Le Havre"]<-"LE HAVRE"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Lisboa"]<-"LISBOA"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Lome"]<-"LOME"
containerinfo$PODDescrip[containerinfo$PODDescrip=="London Gateway"]<-"LONDON GATEWAY"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Luanda"]<-"LUANDA"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Maputo"]<-"MAPUTO"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Mombasa"]<-"MOMBASA"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Mundra"]<-"MUNDRA"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Nansha"]<-"NANSHA"
containerinfo$PODDescrip[containerinfo$PODDescrip=="New York"]<-"NEW YORK"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Newark"]<-"NEWARK"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Newark Apt/New York"]<-"NEWARK APT/NEW YORK"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Ngqura"]<-"NGQURA"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Nhava Sheva (Jawaharlal Nehru)"]<-"NHAVA SHEVA (JAWAHARLAL NEHRU)"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Ningbo"]<-"NINGBO"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Onne"]<-"ONNE"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Philadelphia"]<-"PHILADELPHIA"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Pointe Noire"]<-"POINTE NOIRE"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Port de Pointe des Galets"]<-"PORT DE POINTE DES GALETS"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Port Elizabeth"]<-"PORT ELIZABETH"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Port Kelang"]<-"PORT KELANG"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Port Klang"]<-"PORT KLANG"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Port Louis"]<-"PORT LOUIS"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Reunion"]<-"REUNION"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Rotterdam"]<-"ROTTERDAM"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Saint Petersburg (ex Leningrad)"]<-"SAINT PETERSBURG (EX LENINGRAD)"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Shanghai"]<-"SHANGHAI"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Shekou"]<-"SHEKOU"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Sines"]<-"SINES"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Singapore"]<-"SINGAPORE"
containerinfo$PODDescrip[containerinfo$PODDescrip=="South"]<-"SOUTH"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Tanjong Pelepas"]<-"TANJONG PELEPAS"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Tema"]<-"TEMA"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Tianjinxingang"]<-"TIANJINXINGANG"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Tilbury"]<-"TILBURY"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Tincan / Tincan Island"]<-"TINCAN / TINCAN ISLAND"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Vigo"]<-"VIGO"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Vlissingen"]<-"VLISSINGEN"
containerinfo$PODDescrip[containerinfo$PODDescrip=="West Port Kelang"]<-"WEST PORT KELANG"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Xiamen"]<-"XIAMEN"
containerinfo$PODDescrip[containerinfo$PODDescrip=="Yokohama"]<-"YOKOHAMA"
#find modes
podd<-na.omit(containerinfo$PODDescrip)
table(podd) # issues with capital and lower case letters
#Vesselname
is.na(containerinfo$VesselName) <- containerinfo$VesselName == " "
is.na(containerinfo$VesselName) <- containerinfo$VesselName == "NULL"
sum(is.na(containerinfo$VesselName))
#to see modes
vn<-na.omit(containerinfo$VesselName)
table(vn)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(vn)
vnn<-vn[vn !="SANTA ROSA"]
Mode(vnn)
#VesselStatus
is.na(containerinfo$VesselStatus) <- containerinfo$VesselStatus == " "
is.na(containerinfo$VesselStatus) <- containerinfo$VesselStatus == "NULL"
sum(is.na(containerinfo$VesselStatus))
#find modes
vs<-na.omit(containerinfo$VesselStatus)
table(vs)
containerinfo$VesselStatus[containerinfo$VesselStatus=="Arrived"]<-"ARRIVED"
containerinfo$VesselStatus[containerinfo$VesselStatus=="Closed"]<-"CLOSED"
containerinfo$VesselStatus[containerinfo$VesselStatus=="Complete"]<-"COMPLETE"
containerinfo$VesselStatus[containerinfo$VesselStatus=="Departed"]<-"DEPARTED"
containerinfo$VesselStatus[containerinfo$VesselStatus=="Inbound"]<-"INBOUND"
containerinfo$VesselStatus[containerinfo$VesselStatus=="Queried"]<-"QUERIED"
containerinfo$VesselStatus[containerinfo$VesselStatus=="Working"]<-"WORKING"
#InboundVoyage
is.na(containerinfo$InboundVoyage) <- containerinfo$InboundVoyage == " "
sum(is.na(containerinfo$InboundVoyage))
#find modes
iv<-na.omit(containerinfo$InboundVoyage)
table(iv)
Mode(iv)
ivv<-iv[iv !="190B"]
Mode(ivv)
#OutboundVoyage
is.na(containerinfo$OutboundVoyage) <- containerinfo$OutboundVoyage == " "
sum(is.na(containerinfo$OutboundVoyage))
#find modes
ov<-na.omit(containerinfo$OutboundVoyage)
table(ov)
Mode(ov)
ovv<-ov[ov != "203B"]
Mode(ovv)
#LoadPort
is.na(containerinfo$LoadPort) <- containerinfo$LoadPort == " "
sum(is.na(containerinfo$LoadPort))
table(containerinfo$LoadPort)
# find modes
lp<-na.omit(containerinfo$LoadPort)
table(lp)
#id_time_depart
is.na(containerinfo$id_time_depart) <- containerinfo$id_time_depart == " "
is.na(containerinfo$id_time_depart) <- containerinfo$id_time_depart == "0"
sum(is.na(containerinfo$id_time_depart))
#find modes
itd<-na.omit(containerinfo$id_time_depart)
Mode(itd)
table(itd)
itdd<-itd[itd != "5551"]
Mode(itdd)
#idPort
is.na(containerinfo$id_port) <- containerinfo$id_port == " "
sum(is.na(containerinfo$id_port))
#find modes
Mode(containerinfo$id_port)
table(containerinfo$id_port)
it<-containerinfo$id_port[containerinfo$id_port != "40"]
Mode(it)
#datasource
is.na(containerinfo$DataSource) <- containerinfo$DataSource == " "
sum(is.na(containerinfo$DataSource))
#find modes
table(containerinfo$DataSource)
#id_vessel
Mode(containerinfo$id_vessel)
table(containerinfo$id_vessel)
iv<-containerinfo$id_vessel[containerinfo$id_vessel != "0"]
Mode(iv)
#continuous featres std 
#data visulaisations: categorical
#barplot(table(containerinfo$Agrihub_Ref))
#table(containerinfo$ContainerNo)
#barplot(table(containerinfo$Category), xlab="Category", ylab="Count",main="Categories")
#table(containerinfo$Category)
#barplot(table(containerinfo$HarbourStatus), xlab="harbour status", ylab="count", main="Harbour status")
#barplot(table(containerinfo$PODCode))
#barplot(table(containerinfo$id_port))
#hist(intake$Date)

#delete
containerinfo$ContainerNo<-NULL
containerinfo$ShipLine<-NULL
containerinfo$VesselVisitRef<-NULL
containerinfo$PODDescrip<-NULL
containerinfo$VesselName<-NULL
containerinfo$InboundVoyage<-NULL
containerinfo$OutboundVoyage<-NULL
containerinfo$id_time_depart<-NULL
containerinfo$DataSource<-NULL
containerinfo$AgrihubFileName<-NULL
containerinfo$DateTimeRequested<-NULL
containerinfo$Intake_Date<-NULL
containerinfo$LastMove<-NULL
containerinfo$DatetimeOutHarbour<-NULL

#filter loadport to only ZACPT
containerinfo1<-containerinfo[(containerinfo$LoadPort=="ZACPT"),]
containerinfo1$DateTimeInHarbour<-as.Date(containerinfo1$DateTimeInHarbour)
View(containerinfo1)
summary(containerinfo1)
# sort dates and complete cases
containerinfo11<-containerinfo1[complete.cases(containerinfo1), ]
View(containerinfo11)
containerinfo1<- arrange(containerinfo1, DateTimeInHarbour)

containerinfo1$DateTimeInHarbour[containerinfo1$DateTimeInHarbour == "1899-12-31 00:00:00"] <- NA
containerinfo1$DateTimeInHarbour[containerinfo1$DateTimeInHarbour == "1899-12-31"] <- NA
containerinfo1$DateTimeInHarbour[containerinfo1$DateTimeInHarbour <= "2019-01-01"]<-NA
containerinfo1$VesselETA[containerinfo1$VesselETA=="1899-12-31"]<-NA
containerinfo1$VesselATA[containerinfo1$VesselATA=="1899-12-31"]<-NA
containerinfo1$VesselETD[containerinfo1$VesselETD == "1899-12-31"]<-NA
containerinfo1$VesselATD[containerinfo1$VesselATD == "1899-12-31"]<-NA
containerinfo1$VesselETA<-as.Date(containerinfo1$VesselETA)
containerinfo1$VesselATA<-as.Date(containerinfo1$VesselATA)
containerinfo1$VesselETD<-as.Date(containerinfo1$VesselETD)
containerinfo1$VesselATD<-as.Date(containerinfo1$VesselATD)
summary(containerinfo11)
#knowledge discovery

containerinfo1$DateTimeInHarbour<-as.Date(containerinfo1$DateTimeInHarbour)
class(containerinfo1$DateTimeInHarbour)


#replace duplicates
containerinfo11$id_vessel[containerinfo11$id_vessel=="2190"]<-"2216"
containerinfo11$id_vessel[containerinfo11$id_vessel=="662"]<-"2082"
containerinfo11$id_vessel[containerinfo11$id_vessel=="1640"]<-"2239"
containerinfo11$id_vessel[containerinfo11$id_vessel=="173"]<-"403"

con<-containerinfo11
con$Agrihub_Ref<-NULL
con$DateTimeInHarbour<-NULL
View(con)
con<-con %>% 
  distinct(id_vessel, .keep_all = TRUE)

conty<-containerinfo11 %>%
  group_by(DateTimeInHarbour, id_vessel) %>%
  summarise(Agrihub_Ref = n())
View(conty)

finalcontainerinfo <- merge(conty, con, by.x = "id_vessel", 
                   by.y = "id_vessel", all.x = TRUE, all.y = FALSE)
View(finalcontainerinfo)
finalcontainerinfo<- arrange(finalcontainerinfo, DateTimeInHarbour)
#rename cols
names(finalcontainerinfo)[names(finalcontainerinfo) == 'Agrihub_Ref'] <- 'NumberOfContainers'
finalcontainerinfo$LoadPort<-NULL
finalcontainerinfo$id_port<-NULL
#export excel file
write_xlsx(as.data.frame(finalcontainerinfo), path = "FinalContainerinfo.xlsx")
#plots
library(scales)
library(ggplot2)
ggplot(dates_mos, aes(x=Date, y=Frequency)) + 
  geom_bar(stat="identity", fill="blue") +
  scale_x_continuous(breaks = unique(dates_mos$Date)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))
table(containerinfo11$DateTimeInHarbour)
t<-unique(containerinfo111$id_vessel)
View(t)
tabe <- table(cut(containerinfo111$VesselETA, 'month'))
eta<-data.frame(Date=format(as.Date(names(tabe)), '%d/%m/%Y'),
                 Frequency=as.vector(tabe))

veta<-containerinfo111 %>% 
  group_by(VesselETA) %>% 
  summarise(uniqueid = n_distinct(id_vessel))
View(veta)
ggplot(veta, aes(x=VesselETA, y=uniqueid)) + 
  geom_bar(stat="identity", fill="red") +
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs( x="Estimated month of arrival", y="Number of vessels arriving")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))

vata<-containerinfo111 %>% 
  group_by(VesselATA) %>% 
  summarise(uniqueid = n_distinct(id_vessel))
View(vata)
ggplot(vata, aes(x=VesselATA, y=uniqueid)) + 
  geom_bar(stat="identity", fill="red") +
  labs( x="Month of arrival", y="Number of vessels arriving")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))
vetd<-containerinfo111 %>% 
  group_by(VesselETD) %>% 
  summarise(uniqueid = n_distinct(id_vessel))
ggplot(vetd, aes(x=VesselETD, y=uniqueid)) + 
  geom_bar(stat="identity", fill="purple") +
  labs( x="Month of depart", y="Number of vessels departing")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))
vatd<-containerinfo111 %>% 
  group_by(VesselATD) %>% 
  summarise(uniqueid = n_distinct(id_vessel))
ggplot(vatd, aes(x=VesselATD, y=uniqueid)) + 
  geom_bar(stat="identity", fill="purple") +
  labs( x="Month of depart", y="Number of vessels departing")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))
xy<-barplot(table(containerinfo111$PODCode),las=2, cex.names=.5,cex.axis=0.5,  xlab = "Destination code", ylab="Frequency of destination exports", col="pink")
View(xy)



#####################################################################################################################
databerthing<-read_xlsx("Desktop/data_berthing.xlsx")
View(databerthing)
summary(databerthing)
databerthing$SHIP_ID<-as.character(databerthing$SHIP_ID)
#see cardinality
sapply(databerthing, function(x) length(unique(x)))
#SHIP_ID
Mode(databerthing$SHIP_ID)
table(databerthing$SHIP_ID)
sid<-databerthing$SHIP_ID[databerthing$SHIP_ID != "300164"]
Mode(sid)
#MMSI
Mode(databerthing$MMSI)
table(databerthing$MMSI)
mmsii<-databerthing$MMSI[databerthing$MMSI != "538090563"]
Mode(mmsii)
#IMO
Mode(databerthing$IMO)
table(databerthing$IMO)
imo<-databerthing$IMO[databerthing$IMO != "9151917"]
Mode(imo)
#DOCK_TIMESTAMP_OFFSET
Mode(databerthing$DOCK_TIMESTAMP_OFFSET)
#UNDOCK_TIMESTAMP_OFFSET
Mode(databerthing$UNDOCK_TIMESTAMP_OFFSET)
is.na(databerthing$UNDOCK_TIMESTAMP_OFFSET) <- databerthing$UNDOCK_TIMESTAMP_OFFSET == " "
sum(is.na(databerthing$UNDOCK_TIMESTAMP_OFFSET))
table(databerthing$UNDOCK_TIMESTAMP_OFFSET)
#SHIP_NAME
table(databerthing$SHIPNAME)
Mode(databerthing$SHIPNAME)
sn<-databerthing$SHIPNAME[databerthing$SHIPNAME != "BARRIER"]
Mode(sn)
#Type_Name
table(databerthing$TYPE_NAME)
#DWT
table(databerthing$DWT)
Mode(databerthing$DWT)
dwt<-databerthing$DWT[databerthing$DWT != "93405"]
Mode(dwt)
#GRT
table(databerthing$GRT)
Mode(databerthing$GRT)
grt<- databerthing$GRT[databerthing$GRT != "85676"]
Mode(grt)
#Flag
table(databerthing$FLAG)
Mode(databerthing$FLAG)
flag<-databerthing$FLAG[databerthing$FLAG != "LR"]
Mode(flag)
#Year built
is.na(databerthing$YEAR_BUILT)<-databerthing$YEAR_BUILT == " "
sum(is.na(databerthing$YEAR_BUILT))
Mode(databerthing$YEAR_BUILT)
table(databerthing$YEAR_BUILT)
#BERTH_ID
table(databerthing$BERTH_ID)
Mode(databerthing$BERTH_ID)
bid<-databerthing$BERTH_ID[databerthing$BERTH_ID != "417"]
Mode(bid)
#BERTH_NAME
table(databerthing$BERTH_NAME)
#TERMINAL_ID
table(databerthing$TERMINAL_ID)
#TERMINAL_NAME
table(databerthing$TERMINAL_NAME)
#PORT_ID
table(databerthing$PORT_ID)
#PORT_NAME
table(databerthing$PORT_NAME)
#UNLOCODE
table(databerthing$UNLOCODE)
#COUNTRY_CODE
table(databerthing$COUNTRY_CODE)
#DESTINATION_ID
table(databerthing$DESTINATION_ID)
is.na(databerthing$DESTINATION_ID)<- databerthing$DESTINATION_ID == " "
sum(is.na(databerthing$DESTINATION_ID))
Mode(databerthing$DESTINATION_ID)
#DESTINATION_NAME
is.na(databerthing$DESTINATION)<- databerthing$DESTINATION == " "
sum(is.na(databerthing$DESTINATION))
table(databerthing$DESTINATION)
Mode(databerthing$DESTINATION)
#ARR_DRAUGHT
is.na(databerthing$ARR_DRAUGHT)<- databerthing$ARR_DRAUGHT == " "
sum(is.na(databerthing$ARR_DRAUGHT))
table(databerthing$ARR_DRAUGHT)
Mode(databerthing$ARR_DRAUGHT)
ad<-na.omit(databerthing$ARR_DRAUGHT)
Mode(ad)
add<-ad[ad != "90"]
Mode(add)
#ARR_LOAD_STATUS
is.na(databerthing$ARR_LOAD_STATUS)<- databerthing$ARR_LOAD_STATUS == " "
sum(is.na(databerthing$ARR_LOAD_STATUS))
als<-na.omit(databerthing$ARR_LOAD_STATUS)
Mode(als)
table(databerthing$ARR_LOAD_STATUS)
#PREVIOUS_NOANCH_ID
table(databerthing$PREVIOUS_NOANCH_ID)
is.na(databerthing$PREVIOUS_NOANCH_ID)<- databerthing$PREVIOUS_NOANCH_ID == " "
sum(is.na(databerthing$PREVIOUS_NOANCH_ID))
Mode(databerthing$PREVIOUS_NOANCH_ID)
pnid<-databerthing$PREVIOUS_NOANCH_ID[databerthing$PREVIOUS_NOANCH_ID != "452"]
Mode(pnid)
#PREVIOUS_NOANCH_NAME
sum(is.na(databerthing$PREVIOUS_NOANCH_NAME))
Mode(databerthing$PREVIOUS_NOANCH_NAME)
table(databerthing$PREVIOUS_NOANCH_NAME)
#DEP_DRAUGHT
sum(is.na(databerthing$DEP_DRAUGHT))
Mode(databerthing$DEP_DRAUGHT)
dd<-na.omit(databerthing$DEP_DRAUGHT)
Mode(dd)
table(databerthing$DEP_DRAUGHT)
ddd<-dd[dd != "118"]
Mode(ddd)
#DEP_LOAD_STATUS
sum(is.na(databerthing$DEP_LOAD_STATUS))
Mode(databerthing$DEP_LOAD_STATUS)
table(databerthing$DEP_LOAD_STATUS)
#PORT_OPERATION
sum(is.na(databerthing$PORT_OPERATION))
table(databerthing$PORT_OPERATION)
#Continuous feautures
#databerthing$DOCK_TIMESTAMP_LT <- as.Date(databerthing$DOCK_TIMESTAMP_LT, format="%d/%m/%Y")
#databerthing$DOCK_TIMESTAMP_LT<-as.Date(databerthing$DOCK_TIMESTAMP_LT)
sd(databerthing$DOCK_TIMESTAMP_LT)
sum(is.na(databerthing$DOCK_TIMESTAMP_LT))
#databerthing$DOCK_TIMESTAMP_UTC<-as.Date(databerthing$DOCK_TIMESTAMP_UTC)
#databerthing$UNDOCK_TIMESTAMP_LT<-as.Date(databerthing$UNDOCK_TIMESTAMP_LT)
#databerthing$UNDOCK_TIMESTAMP_UTC<-as.Date(databerthing$UNDOCK_TIMESTAMP_UTC)
databerthing$ARR_TIMESTAMP_LT<-as.Date(databerthing$ARR_TIMESTAMP_LT)
databerthing$ARR_TIMESTAMP_UTC<-as.Date(databerthing$ARR_TIMESTAMP_UTC)
databerthing$DISTANCE_TRAVELLED<-as.numeric(databerthing$DISTANCE_TRAVELLED)
databerthing$VOYAGE_SPEED_AVG<-as.numeric(databerthing$VOYAGE_SPEED_AVG)
databerthing$VOYAGE_SPEED_MAX<-as.numeric(databerthing$VOYAGE_SPEED_MAX)
databerthing$VOYAGE_IDLE_TIME_MINS<-as.numeric(databerthing$VOYAGE_IDLE_TIME_MINS)
databerthing$PREVIOUS_NOANCH_TIMESTAMP<-as.Date(databerthing$PREVIOUS_NOANCH_TIMESTAMP)
databerthing$ELAPSED_NOANCH<-as.numeric(databerthing$ELAPSED_NOANCH)
databerthing$DEP_TIMESTAMP_LT<-as.Date(databerthing$DEP_TIMESTAMP_LT)
databerthing$DEP_TIMESTAMP_UTC<-as.Date(databerthing$DEP_TIMESTAMP_UTC)
#delete following
databerthing$SHIP_ID<-NULL
databerthing$MMSI<-NULL
databerthing$DOCK_TIMESTAMP_OFFSET<-NULL
databerthing$UNDOCK_TIMESTAMP_OFFSET<-NULL
databerthing$SHIPNAME<-NULL
databerthing$TYPE_NAME<-NULL
databerthing$DWT<-NULL
databerthing$GRT<-NULL
databerthing$FLAG<-NULL
databerthing$YEAR_BUILT<-NULL
databerthing$BERTH_NAME<-NULL
databerthing$TERMINAL_NAME<-NULL
databerthing$PORT_NAME<-NULL
databerthing$UNLOCODE<-NULL
databerthing$COUNTRY_CODE<-NULL
databerthing$DESTINATION<-NULL
databerthing$ARR_DRAUGHT<-NULL
databerthing$ARR_LOAD_STATUS<-NULL
databerthing$PREVIOUS_NOANCH_ID<-NULL
databerthing$PREVIOUS_NOANCH_NAME<-NULL
databerthing$DEP_DRAUGHT<-NULL
databerthing$DEP_LOAD_STATUS<-NULL
databerthing$PORT_OPERATION<-NULL
databerthing$ARR_TIMESTAMP_LT<-NULL
databerthing$ARR_TIMESTAMP_UTC<-NULL
databerthing$DISTANCE_TRAVELLED<-NULL
databerthing$VOYAGE_SPEED_AVG<-NULL
databerthing$VOYAGE_SPEED_MAX<-NULL
databerthing$VOYAGE_IDLE_TIME_MINS<-NULL
databerthing$PREVIOUS_NOANCH_TIMESTAMP<-NULL
databerthing$ELAPSED_NOANCH<-NULL
databerthing$DEP_TIMESTAMP_LT<-NULL
databerthing$DEP_TIMESTAMP_UTC<-NULL
databerthing$TIME_AT_BERTH<-NULL
databerthing$TIME_AT_PORT<-NULL
databerthing$DOCK_TIMESTAMP_UTC<-NULL
databerthing$UNDOCK_TIMESTAMP_UTC<-NULL
#names(databerthing)[names(databerthing) == "IMO"] <- "imo"

barplot(table(databerthing$BERTH_ID),  xlab="Berth ID", ylab="Frequency", col = "Lavender")
#databerthing$diff_in_days<- difftime(databerthing$DOCK_TIMESTAMP_LT ,databerthing$UNDOCK_TIMESTAMP_LT, units = c("days"))
databerthingg<-databerthing[complete.cases(databerthing), ]
View(databerthingg)

databerthingg$TimeAtBerth<- difftime(databerthingg$UNDOCK_TIMESTAMP_LT ,databerthingg$DOCK_TIMESTAMP_LT , units = c("days"))
databerthing$DOCK_TIMESTAMP_LT<-as.POSIXct(databerthing$DOCK_TIMESTAMP_LT)
databerthing$UNDOCK_TIMESTAMP_LT<-as.POSIXct(databerthing$UNDOCK_TIMESTAMP_LT)
databerthinggg<-databerthingg[order(databerthingg$DOCK_TIMESTAMP_LT),]
View(databerthinggg)
summary(databerthinggg)
sapply(databerthinggg, function(x) length(unique(x)))
sd(databerthinggg$TimeAtBerth)
databerthinggg$TimeAtBerth<-as.integer(databerthinggg$TimeAtBerth)
databerthinggg$DOCK_TIMESTAMP_LT[databerthinggg$DOCK_TIMESTAMP_LT <= "2019-01-01"]<-NA
databerthinggg<-databerthinggg[complete.cases(databerthinggg), ]
databerthinggg$DOCK_TIMESTAMP_LT<-as.Date(databerthinggg$DOCK_TIMESTAMP_LT)
databerthinggg$UNDOCK_TIMESTAMP_LT<-as.Date(databerthinggg$UNDOCK_TIMESTAMP_LT)
databerthinggg$PORT_ID<-NULL

write_xlsx(as.data.frame(databerthinggg), path = "finaldataberthing.xlsx")





#####################################################################################################################
#kan hierdie een uitlos

data_sail_date<-read_xlsx("Desktop/data_sail_date.xlsx")
View(data_sail_date)
summary(data_sail_date)
#check cardinality
sapply(data_sail_date, function(x) length(unique(x)))
#id
data_sail_date$id<-as.factor(data_sail_date$id)
#VesselName
Mode(data_sail_date$Vesselname)
table(data_sail_date$Vesselname)
vn<-data_sail_date$Vesselname[data_sail_date$Vesselname != "BOUNDARY"]
getOption("max.print")
Mode(vn)
#POL
Mode(data_sail_date$POL)
sum(is.na(data_sail_date$POL))
table(data_sail_date$POL)
#id_vessel
Mode(data_sail_date$id_vessel)
sum(is.na(data_sail_date$id_vessel))
table(data_sail_date$id_vessel)
iv<-data_sail_date$id_vessel[data_sail_date$id_vessel != "0"]
Mode(iv)
#id_load_port
Mode(data_sail_date$id_loadport)
table(data_sail_date$id_loadport)
#continuous

#####################################################################################################################
data_stackdates<-read_xlsx("Desktop/data_stackdates.xlsx")
View(data_stackdates)
summary(data_stackdates)
#check cardinality
sapply(data_stackdates, function(x) length(unique(x)))
#id
data_stackdates$id<-as.factor(data_stackdates$id)
#vesselname hier is baie foute
sum(is.na(data_stackdates$vesselname))
Mode(data_stackdates$vesselname)
table(data_stackdates$vesselname)
vesname<-data_stackdates$vesselname[data_stackdates$vesselname != "Santa Isabel"]
Mode(vesname)
#id_vessel
data_stackdates$id_vessel<-as.factor(data_stackdates$id_vessel)
Mode(data_stackdates$id_vessel)
table(data_stackdates$id_vessel)
ives<-data_stackdates$id_vessel[data_stackdates$id_vessel != "706"]
Mode(ives)
#continuous
data_stackdates$time<-as.POSIXct(data_stackdates$time,format="%H:%M:%S")
#delete
data_stackdates$id<-NULL
data_stackdates$time<-NULL
data_stackdates$vesselname<-NULL

data_stackdates$stackdateopen_clean<-as.POSIXct(data_stackdates$stackdateopen_clean)
data_stackdates$stackdateclose_clean<-as.POSIXct(data_stackdates$stackdateclose_clean)
data_stackdates$TimeBetween<- difftime(data_stackdates$stackdateclose_clean ,data_stackdates$stackdateopen_clean , units = c("days"))
data_stackdates$TimeBetween<-as.integer(data_stackdates$TimeBetween)


#eronious entries 
data_stackdates$TimeBetween[data_stackdates$TimeBetween < "0"]<-NA
data_stackdates<-data_stackdates[complete.cases(data_stackdates), ]
data_stackdates$documentdate<-as.Date(data_stackdates$documentdate)
data_stackdates$stackdateopen_clean<-as.Date(data_stackdates$stackdateopen_clean)
data_stackdates$stackdateclose_clean<-as.Date(data_stackdates$stackdateclose_clean)
data_stackdates$id_vessel[data_stackdates$id_vessel=="2190"]<-"2216"
data_stackdates$id_vessel[data_stackdates$id_vessel=="662"]<-"2082"
data_stackdates$id_vessel[data_stackdates$id_vessel=="1640"]<-"2239"
data_stackdates$id_vessel[data_stackdates$id_vessel=="173"]<-"403"
data_stackdates$id_vessel[data_stackdates$id_vessel=="0"]<-NA
write_xlsx(as.data.frame(data_stackdates), path = "finalstackdates.xlsx")
#####################################################################################################################
data_sscc2<-read.csv("Desktop/data_sscc2.csv")
View(data_sscc2)
summary(data_sscc2)
#check cardinality
sapply(data_sscc2, function(x) length(unique(x)))
#id
sum(is.na(data_sscc2$id))
Mode(data_sscc2$id)
table(data_sscc2$id)
#sscc
sum(is.na(data_sscc2$sscc))
Mode(data_sscc2$sscc)
table(data_sscc2$sscc)
is.na(data_sscc2$sscc)<- data_sscc2$sscc == " "
ghytr<-data_sscc2$sscc[data_sscc2$sscc == "260099003444052422"]
ghytrr<-data_sscc2$sscc[data_sscc2$sscc != "260099003444052422"]
Mode(ghytrr)
ghytrrr<-ghytrr[ghytrr=="260099003444052439"]
ghytrrr
#season
Mode(data_sscc2$season)
sum(is.na(data_sscc2$season))
table(data_sscc2$season)
#commodity
table(data_sscc2$commodity)
Mode(data_sscc2$commodity)
com<-data_sscc2$commodity[data_sscc2$commodity != "OR"]
data_sscc2$commodity[data_sscc2$commodity=="pr"]<-"PR"
Mode(com)
#puc
table(data_sscc2$puc)
Mode(data_sscc2$puc)
sum(is.na(data_sscc2$puc))
puc<-data_sscc2$puc[data_sscc2$puc != "D0147"]
Mode(puc)
pucc<-puc[puc=="V0238"]
table(pucc)
#loadport
sum(is.na(data_sscc2$loadport))
table(data_sscc2$loadport)
Mode(data_sscc2$loadport)
#continuous
data_sscc2$intake_date<-as.Date(data_sscc2$intake_date)
data_sscc2$harbourin_date<-as.Date(data_sscc2$harbourin_date)
datasscc<-data_sscc2[(data_sscc2$loadport == "ZACPT"),]
View(datasscc)

datass<-datasscc[(datasscc >= "2019-01-01" & datasscc$intake_date <= "2019-12-31"),]
View(datass)
ggplot(datass, aes(x=commodity, y=Frequency)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="Number of containers per month.")+
  scale_x_continuous(breaks = unique(datass$intake_date)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))
table(datass$commodity)
data_sscc2$id<-NULL
data_sscc2$loadport<-NULL
data_sscc2$intake_date<-NULL
#####################################################################################################################
data_stock_pareto<-read_xlsx("Desktop/data_stock_pareto.xlsx")
View(data_stock_pareto)
summary(data_stock_pareto)
#check cardinality
sapply(data_stock_pareto, function(x) length(unique(x)))
#LOCN_CODE
table(data_stock_pareto$LOCN_CODE)
Mode(data_stock_pareto$LOCN_CODE)
sum(is.na(data_stock_pareto$LOCN_CODE))
#PPECB_COMMODITY
table(data_stock_pareto$PPECB_COMMODITY)
Mode(data_stock_pareto$PPECB_COMMODITY)
sum(is.na(data_stock_pareto$PPECB_COMMODITY))
#continuous
sum(is.na(data_stock_pareto$pallets_stock))
data_stock_pareto$stock_date<-as.Date(data_stock_pareto$stock_date)
#delete
table(data_sscc2$commodity)
#####################################################################################################################
data_stock_tm_pareto<-read.csv("Desktop/data_stock_tm_pareto.csv")
View(data_stock_tm_pareto)
summary(data_stock_tm_pareto)
#check cardinality
sapply(data_stock_tm_pareto, function(x) length(unique(x)))
#SSCC
sum(is.na(data_stock_tm_pareto$SSCC))
Mode(data_stock_tm_pareto$SSCC)
sscc<-data_stock_tm_pareto$SSCC[data_stock_tm_pareto$SSCC == "660091600182068188"]
table(sscc)
sssc<-data_stock_tm_pareto$SSCC[data_stock_tm_pareto$SSCC !="660091600182068188"]
Mode(sssc)
ssccc<-sssc[sssc=="660091600182068195"]
table(ssccc)
#SEASON
sum(is.na(data_stock_tm_pareto$SEASON))
table(data_stock_tm_pareto$SEASON)
data_stock_tm_pareto$SEASON[data_stock_tm_pareto$SEASON==""]<-NA
#TARGET_COUNTRY
sum(is.na(data_stock_tm_pareto$TARGET_COUNTRY))
table(data_stock_tm_pareto$TARGET_COUNTRY)
Mode(data_stock_tm_pareto$TARGET_COUNTRY)
is.na(data_stock_tm_pareto$TARGET_COUNTRY)<- data_stock_tm_pareto$TARGET_COUNTRY == "  "
data_stock_tm_pareto$TARGET_COUNTRY[data_stock_tm_pareto$TARGET_COUNTRY==""]<-NA
tc<-na.omit(data_stock_tm_pareto$TARGET_COUNTRY)
Mode(tc)
tcc<-tc[tc!="GB"]
Mode(tcc)
data_stock_tm_pareto$TARGET_COUNTRY[data_stock_tm_pareto$TARGET_COUNTRY=="ni"]<-"NI"
#TARGET_REGION
data_stock_tm_pareto$TARGET_REGION[data_stock_tm_pareto$TARGET_REGION==""]<-NA
sum(is.na(data_stock_tm_pareto$TARGET_REGION))
Mode(data_stock_tm_pareto$TARGET_REGION)
table(data_stock_tm_pareto$TARGET_REGION)
tr<-data_stock_tm_pareto$TARGET_REGION[data_stock_tm_pareto$TARGET_REGION!="EUN"]
Mode(tr)
data_stock_tm_pareto$TARGET_REGION[data_stock_tm_pareto$TARGET_REGION=="U-KI"]<-"UKI"
data_stock_tm_pareto$TARGET_REGION[data_stock_tm_pareto$TARGET_REGION=="UkI"]<-"UKI"
#date
data_stock_tm_pareto$date<-as.Date(data_stock_tm_pareto$date)
#delete
data_stock_tm_pareto$TARGET_COUNTRY<-NULL
#####################################################################################################################
data_stockage<-read_xlsx("Desktop/data_stockage.xlsx")
View(data_stockage)
summary(data_stockage)
#check cardinality
sapply(data_stockage, function(x) length(unique(x)))
#index
data_stockage$index<-as.factor((data_stockage$index))
Mode(data_stockage$index)
table(data_stockage$index)
#commodity
table(data_stockage$commodity)
Mode(data_stockage$commodity)
data_stockage$commodity[data_stockage$commodity=="pr"]<-"PR"
comm<-data_stockage$commodity[data_stockage$commodity != "AP"]
Mode(comm)
#sscc
sum(is.na(data_stockage$sscc))
Mode(data_stockage$sscc)
table(data_stockage$sscc)
sssccc<-data_stockage$sscc[data_stockage$sscc!="1"]
Mode(sssccc)
#date
data_stockage$date<-as.Date(data_stockage$date)
#age
sd(data_stockage$age)
typeof(data_stockage$age)


#####################################################################################################################
# los uit hierdie dataset
data_vesselmeta<-read_xlsx("Desktop/data_vesselmeta.xlsx")
View(data_vesselmeta)
summary(data_vesselmeta)
#check cardinality
sapply(data_vesselmeta, function(x) length(unique(x)))
#id_vessel
sum(is.na(data_vesselmeta$id_vessel))
table(data_vesselmeta$id_vessel)
#MMSI
sum(is.na(data_vesselmeta$MMSI))
table(data_vesselmeta$MMSI)
Mode(data_vesselmeta$MMSI)
mmsi<-data_vesselmeta$MMSI[data_vesselmeta$MMSI != "477764700"]
Mode(mmsi)
#IMO
Mode(data_vesselmeta$IMO)
table(data_vesselmeta$IMO)
imo<-data_vesselmeta$IMO[data_vesselmeta$IMO != "9526904"]
Mode(imo)
#Name
Mode(data_vesselmeta$NAME)
table(data_vesselmeta$NAME)
name<-data_vesselmeta$NAME[data_vesselmeta$NAME != "CYPRESS"]
Mode(name)
#PLACE_OF_BUILD 
sum(is.na(data_vesselmeta$PLACE_OF_BUILD))
table(data_vesselmeta$PLACE_OF_BUILD)
#BUILD
sum(is.na(data_vesselmeta$BUILD))
table(data_vesselmeta$BUILD)
Mode(data_vesselmeta$BUILD)
#CALLSIGN
sum(is.na(data_vesselmeta$CALLSIGN))
Mode(data_vesselmeta$CALLSIGN)
table(data_vesselmeta$CALLSIGN)
#FLAG
table(data_vesselmeta$FLAG)
sum(is.na(data_vesselmeta$FLAG))
#LIQUID_OIL
sum(is.na(data_vesselmeta$LIQUID_OIL))
#OWNER
table(data_vesselmeta$OWNER)
sum(is.na(data_vesselmeta$OWNER))
Mode(data_vesselmeta$OWNER)
own<-na.omit(data_vesselmeta$OWNER)
Mode(own)
ownn<-own[own != "MOLLER A P"]
Mode(ownn)
#MANAGER
table(data_vesselmeta$MANAGER)
sum(is.na(data_vesselmeta$MANAGER))
man<-na.omit(data_vesselmeta$MANAGER)
Mode(man)
mann<-man[man != "MEDITERRANEAN SHIPPING CO MSC"]
Mode(mann)
#MANAGER_OWNER
sum(is.na(data_vesselmeta$MANAGER_OWNER))
manown<-na.omit(data_vesselmeta$MANAGER_OWNER)
Mode(manown)
table(data_vesselmeta$MANAGER_OWNER)
manownn<-manown[manown!="MEDITERRANEAN SHIPPING CO MSC"]
Mode(manownn)
#VESSEL_TYPE
sum(is.na(data_vesselmeta$VESSEL_TYPE))
table(data_vesselmeta$VESSEL_TYPE)
#CONTINUOUS
summary(data_vesselmeta)
data_vesselmeta$id_vessel<-as.factor(data_vesselmeta$id_vessel)
data_vesselmeta$BREADTH_EXTREME<-as.numeric(data_vesselmeta$BREADTH_EXTREME)
sd(data_vesselmeta$BREADTH_EXTREME)
data_vesselmeta$SUMMER_DWT<-as.numeric(data_vesselmeta$SUMMER_DWT)
sd(data_vesselmeta$SUMMER_DWT)
data_vesselmeta$DISPLACEMENT_SUMMER<-as.numeric(data_vesselmeta$DISPLACEMENT_SUMMER)
sd(disp)
disp<-na.omit(data_vesselmeta$DISPLACEMENT_SUMMER)
data_vesselmeta$DRAUGHT<-as.numeric(data_vesselmeta$DRAUGHT)
dr<-na.omit(data_vesselmeta$DRAUGHT)
sd(dr)
data_vesselmeta$LENGTH_OVERALL<-as.numeric(data_vesselmeta$LENGTH_OVERALL)
sd(data_vesselmeta$LENGTH_OVERALL)
data_vesselmeta$FUEL_CONSUMPTION<-as.numeric(data_vesselmeta$FUEL_CONSUMPTION)
fc<-na.omit(data_vesselmeta$SPEED_MAX)
sd(fc)
data_vesselmeta$SPEED_MAX<-as.numeric(data_vesselmeta$SPEED_MAX)
data_vesselmeta$SPEED_SERVICE<-as.numeric(data_vesselmeta$SPEED_SERVICE)
ss<-na.omit(data_vesselmeta$SPEED_SERVICE)
sd(ss)
#####################################################################################################################
data_wind<-read_xlsx("Desktop/data_wind.xlsx")
View(data_wind)
summary(data_wind)
#find cardinality
sapply(data_wind, function(x) length(unique(x)))
#id
data_wind$id<-as.factor(data_wind$id)
#measure
sum(is.na(data_wind$Measure))
table(data_wind$Measure)
#continuous
sd(data_wind$Knots)
#delete
data_wind$Measure<-NULL
plot(data_wind$DateTime, data_wind$Knots)
data_wind$DateTime<-as.Date(data_wind$DateTime)
#rather take the max
wind<-data_wind %>% 
  group_by(DateTime) %>% 
        summarise(Knots = max(Knots))

wind$DateTime<-as.Date(wind$DateTime)
View(wind)
wind<-wind[order(wind$DateTime),]
plot(wind$DateTime, wind$Knots,  xlab="Date", ylab=" Avergae windspeed (knots)")

write_xlsx(as.data.frame(wind), path = "finalwind.xlsx")
#####################################################################################################################
mt_target_regions<-read_xlsx("Desktop/mt_target_regions.xlsx")
View(mt_target_regions)
summary(mt_target_regions)
#check cardinality
sapply(mt_target_regions, function(x) length(unique(x)))
#REGION
table(mt_target_regions$REGION)
#delete
mt_target_regions$DESTINATION<-NULL
#####################################################################################################################
#start merging datasets:
finalcontainerinfo$id_vessel[finalcontainerinfo$id_vessel=="342"]<-"346"

joined_ds <- merge(finalcontainerinfo1, dv, by.x = "id_vessel", 
                   by.y = "id_vessel", all.x = TRUE, all.y = FALSE)
View(joined_ds)
summary(joined_ds)

join<-join[complete.cases(join), ]

join<-merge(join1, databerthinggg, by.x = "DateTimeInHarbour", 
            by.y = "DOCK_TIMESTAMP_LT", all.x = TRUE )
names(join)[names(join) == 'IMO.y'] <- 'IMODockedVessel'

View(join)
join<-join %>%
  group_by(DateTimeInHarbour) %>%
  mutate_at(vars(id_vessel:NumberOfContainers), funs(replace(., duplicated(.), NA)))

join$id_vessel[is.na(join$id_vessel)]<- 0
join$NumberOfContainers[is.na(join$NumberOfContainers)]<- 0

join[join$id_vessel == 0 & join$NumberOfContainers == 0, "Category"] <- "NONE"
join[join$id_vessel == 0 & join$NumberOfContainers == 0, "HarbourStatus"] <- "NONE"
join[join$id_vessel == 0 & join$NumberOfContainers == 0, "PODCode"] <- "NONE"
join[join$id_vessel == 0 & join$NumberOfContainers == 0, "VesselStatus"] <- "NONE"
join[join$id_vessel == 0 & join$NumberOfContainers == 0, "VesselETA"] <- NA
join[join$id_vessel == 0 & join$NumberOfContainers == 0, "VesselATA"] <- NA
join[join$id_vessel == 0 & join$NumberOfContainers == 0, "VesselETD"] <- NA
join[join$id_vessel == 0 & join$NumberOfContainers == 0, "VesselATD"] <- NA

#handling missing values
join$UNDOCK_TIMESTAMP_LT[is.na(join$UNDOCK_TIMESTAMP_LT)] <- join$DateTimeInHarbour[is.na(join$UNDOCK_TIMESTAMP_LT)]
join$IMODockedVessel[is.na(join$IMODockedVessel)]<- 0
join$BERTH_ID[is.na(join$BERTH_ID)]<- 0
join$TERMINAL_ID[is.na(join$TERMINAL_ID)]<- 0
join$DESTINATION_ID[is.na(join$DESTINATION_ID)]<- 0
join$TimeAtBerth[is.na(join$TimeAtBerth)]<- 0
sum(is.na(join))
join$id_vessel[is.na(join$id_vessel)]<- 0
join$NumberOfContainers[is.na(join$NumberOfContainers)]<- 0
join$id_port[is.na(join$id_port)]<- 0
join$IMO.x[is.na(join$IMO.x)]<- 0
join$Category[is.na(join$Category)]<- "NONE"
join$HarbourStatus[is.na(join$HarbourStatus)]<- "NONE"
join$PODCode[is.na(join$PODCode)]<- "NONE"
join$VesselStatus[is.na(join$VesselStatus)]<- "NONE"
join$VesselETA[is.na(join$VesselETA)] <- join$DateTimeInHarbour[is.na(join$VesselETA)]
join$VesselATA[is.na(join$VesselATA)] <- join$DateTimeInHarbour[is.na(join$VesselATA)]
join$VesselETD[is.na(join$VesselETD)] <- join$DateTimeInHarbour[is.na(join$VesselETD)]
join$VesselATD[is.na(join$VesselATD)] <- join$DateTimeInHarbour[is.na(join$VesselATD)]


#################################################################################################################

joinn<-merge(join, finalstackdates1, by.x = c("id_vessel" ,"DateTimeInHarbour"), 
             by.y = c( "id_vessel","documentdate") , all.x = TRUE)
View(joinn)
joinnn<-joinn[complete.cases(joinn), ]

#################################################################################################################
names(joinn)[names(joinn) == 'id_vessel.y'] <- 'StackIDVessel'
joinn$UNDOCK_TIMESTAMP_LT[is.na(joinn$UNDOCK_TIMESTAMP_LT)] <- joinn$DateTimeInHarbour[is.na(joinn$UNDOCK_TIMESTAMP_LT)]
joinn$IMODockedVessel[is.na(joinn$IMODockedVessel)]<- 0
joinn$BERTH_ID[is.na(joinn$BERTH_ID)]<- 0
joinn$TERMINAL_ID[is.na(joinn$TERMINAL_ID)]<- 0
joinn$DESTINATION_ID[is.na(joinn$DESTINATION_ID)]<- 0
joinn$TimeAtBerth[is.na(joinn$TimeAtBerth)]<- 0
sum(is.na(joinn))
joinn$id_vessel[is.na(joinn$id_vessel)]<- 0
joinn$NumberOfContainers[is.na(joinn$NumberOfContainers)]<- 0
joinn$id_port[is.na(joinn$id_port)]<- 0
joinn$IMO.x[is.na(joinn$IMO.x)]<- 0
joinn$Category[is.na(joinn$Category)]<- "NONE"
joinn$HarbourStatus[is.na(joinn$HarbourStatus)]<- "NONE"
joinn$PODCode[is.na(joinn$PODCode)]<- "NONE"
joinn$VesselStatus[is.na(joinn$VesselStatus)]<- "NONE"
joinn$VesselETA[is.na(joinn$VesselETA)] <- joinn$DateTimeInHarbour[is.na(joinn$VesselETA)]
joinn$VesselATA[is.na(joinn$VesselATA)] <- joinn$DateTimeInHarbour[is.na(joinn$VesselATA)]
joinn$VesselETD[is.na(joinn$VesselETD)] <- joinn$DateTimeInHarbour[is.na(joinn$VesselETD)]
joinn$VesselATD[is.na(joinn$VesselATD)] <- joinn$DateTimeInHarbour[is.na(joinn$VesselATD)]
joinn$id_vessel<-NULL
joinn$StackIDVessel[is.na(joinn$StackIDVessel)]<- 0
joinn$TimeBetween[is.na(joinn$TimeBetween)]<- 0
joinn$stackdateopen_clean[is.na(joinn$stackdateopen_clean)] <- joinn$DateTimeInHarbour[is.na(joinn$stackdateopen_clean)]
joinn$stackdateclose_clean[is.na(joinn$stackdateclose_clean)] <- joinn$DateTimeInHarbour[is.na(joinn$stackdateclose_clean)]
joinn$id_vessel.x[is.na(joinn$id_vessel.x)]<-0


joinnn<-merge(joinn, wind, by.x = "DateTimeInHarbour", 
                 by.y = "DateTime", all.x = TRUE, all.y = TRUE)
joinnn$UNDOCK_TIMESTAMP_LT[is.na(joinnn$UNDOCK_TIMESTAMP_LT)] <- joinnn$DateTimeInHarbour[is.na(joinnn$UNDOCK_TIMESTAMP_LT)]
joinnn$IMODockedVessel[is.na(joinnn$IMODockedVessel)]<- 0
joinnn$BERTH_ID[is.na(joinnn$BERTH_ID)]<- 0
joinnn$TERMINAL_ID[is.na(joinnn$TERMINAL_ID)]<- 0
joinnn$DESTINATION_ID[is.na(joinnn$DESTINATION_ID)]<- 0
joinnn$TimeAtBerth[is.na(joinnn$TimeAtBerth)]<- 0
sum(is.na(joinnn))
joinnn$id_vessel.x[is.na(joinnn$id_vessel.x)]<- 0
joinnn$NumberOfContainers[is.na(joinnn$NumberOfContainers)]<- 0
joinnn$id_port[is.na(joinnn$id_port)]<- 0
joinnn$IMO.x[is.na(joinnn$IMO.x)]<- 0
joinnn$Category[is.na(joinnn$Category)]<- "NONE"
joinnn$HarbourStatus[is.na(joinnn$HarbourStatus)]<- "NONE"
joinnn$PODCode[is.na(joinnn$PODCode)]<- "NONE"
joinnn$VesselStatus[is.na(joinnn$VesselStatus)]<- "NONE"
joinnn$VesselETA[is.na(joinnn$VesselETA)] <- joinnn$DateTimeInHarbour[is.na(joinnn$VesselETA)]
joinnn$VesselATA[is.na(joinnn$VesselATA)] <- joinnn$DateTimeInHarbour[is.na(joinnn$VesselATA)]
joinnn$VesselETD[is.na(joinnn$VesselETD)] <- joinnn$DateTimeInHarbour[is.na(joinnn$VesselETD)]
joinnn$VesselATD[is.na(joinnn$VesselATD)] <- joinnn$DateTimeInHarbour[is.na(joinnn$VesselATD)]
joinnn$id_vessel<-NULL
joinnn$StackIDVessel[is.na(joinnn$StackIDVessel)]<- 0
joinnn$TimeBetween[is.na(joinnn$TimeBetween)]<- 0
joinnn$stackdateopen_clean[is.na(joinnn$stackdateopen_clean)] <- joinnn$DateTimeInHarbour[is.na(joinnn$stackdateopen_clean)]
joinnn$stackdateclose_clean[is.na(joinnn$stackdateclose_clean)] <- joinnn$DateTimeInHarbour[is.na(joinnn$stackdateclose_clean)]
joinnn$Knots[is.na(joinnn$Knots)]<-"NEED"
names(joinnn)[names(joinnn) == 'id_vessel.x'] <- 'id_vessel.container'
View(joinnn)


write_xlsx(as.data.frame(joinnn), path = "ABT.xlsx")




############################################################################################################################


write_xlsx(as.data.frame(mfs), path = "mfs.xlsx")

mfs<-containerinfo1 %>%
  group_by(DateTimeInHarbour) %>%
  summarise(Agrihub_Ref = n())
View(date)
date<-mfs$DateTimeInHarbour
date<-na.omit(date)
mfs<-separate(mfs, "DateTimeInHarbour", c("Year", "Month", "Day"), sep = "-")
mfs$Week<-strftime(mfs$DateTimeInHarbour, format = "%V")
mfs$WeekDay <- weekdays(as.Date(mfs$DateTimeInHarbour))

names(mfs)[names(mfs) == 'Agrihub_Ref'] <- 'NumberOfContainers'
plot(mfs$NumberOfContainers)

mfs<-mfs[complete.cases(mfs), ]
names(mfs)[names(mfs) == 'date'] <- 'Date'
class(finalwind$DateTime)
finalwind$DateTime<-as.Date(finalwind$DateTime)
mfs<-cbind(date, mfs)
mfs<-merge(mfs, finalwind, by.x = "Date", 
            by.y = "DateTime", all.x = TRUE)
mfsno<-na.omit(mfs)
View(mfs1)
mfsno$Date<-as.Date(mfsno$Date)
mfsno$WeekDay<-as.factor(mfsno$WeekDay)
mf$Date<-as.Date(mf$Date)

mf<-rbind(data.frame(Date = as.Date(2019/01/01), Year = 2019, Month = '01', Day = '01', NumberOfContainers = 0, Week = '01', WeekDay = 'Tuesday', Knots = 18), mfsno)
mf$Date[mf$Date=="1975-07-13"]<-"2019-01-01"
mfs<-mf

PublicHolidays<-c(1:734)
mfs<-cbind(mfs, PublicHolidays)
mfs$PublicHolidays<-is.na(mfs$PublicHolidays)
mfs$PublicHolidays[mfs$PublicHolidays=="FALSE"]<-0
mfs[mfs$Date == "2019-01-01" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2019-03-21" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2019-04-19" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2019-04-22" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2019-04-27" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2019-05-01" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2019-05-08" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2019-06-17" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2019-08-09" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2019-09-24" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2019-12-16" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2019-12-25" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2019-12-26" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2020-01-01" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2020-03-21" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2020-04-10" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2020-04-13" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2020-04-27" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2020-05-01" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2020-06-16" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2020-08-10" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2020-09-24" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2020-12-16" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2020-12-25" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2020-12-26" , "PublicHolidays"] <- 1
mfs[mfs$Date == "2021-01-01" , "PublicHolidays"] <- 1
mfs1<-merge(data.frame(Date= as.Date(min(mfs$Date):max(mfs$Date),"1970-1-1")),
          mfs, by = "Date", all = TRUE)
mfs1$NumberOfContainers[is.na(mfs1$NumberOfContainers)] <- 0
sum(is.na(mfs1$NumberOfContainers))
missing<-subset(mfs1, NumberOfContainers == 0)
View(mfs)
mfs<-rbind(data.frame(Date = as.Date('2019/05/08', format="%Y/%m/%d"), Year = 2019, Month = '05', Day = '08', NumberOfContainers = 0, Week = '19', WeekDay = 'Wednesday', Knots = 17, PublicHolidays = 1), mfs)
mfs<-rbind(data.frame(Date = as.Date('2019/09/22', format="%Y/%m/%d"), Year = 2019, Month = '09', Day = '22', NumberOfContainers = 0, Week = '38', WeekDay = 'Sunday', Knots = 18, PublicHolidays = 0), mfs)
mfs<-rbind(data.frame(Date = as.Date('2019/09/29', format="%Y/%m/%d"), Year = 2019, Month = '09', Day = '29', NumberOfContainers = 0, Week = '39', WeekDay = 'Sunday', Knots = 28, PublicHolidays = 0), mfs)
mfs[1, 8] = 31
mfs<-rbind(data.frame(Date = as.Date('2019/10/13', format="%Y/%m/%d"), Year = 2019, Month = '10', Day = '13', NumberOfContainers = 0, Week = '41', WeekDay = 'Sunday', Knots = 22, PublicHolidays = 0), mfs)
mfs<-rbind(data.frame(Date = as.Date('2019/11/16', format="%Y/%m/%d"), Year = 2019, Month = '11', Day = '16', NumberOfContainers = 0, Week = '46', WeekDay = 'Saturday', Knots = 31, PublicHolidays = 0), mfs)
mfs<-rbind(data.frame(Date = as.Date('2019/12/25', format="%Y/%m/%d"), Year = 2019, Month = '12', Day = '25', NumberOfContainers = 0, Week = '52', WeekDay = 'Wednesday', Knots = 17, PublicHolidays = 1), mfs)
mfs<-rbind(data.frame(Date = as.Date('2020/01/01', format="%Y/%m/%d"), Year = 2020, Month = '01', Day = '01', NumberOfContainers = 0, Week = '01', WeekDay = 'Wednesday', Knots = 25, PublicHolidays = 1), mfs)
mfs<-rbind(data.frame(Date = as.Date('2020/01/18', format="%Y/%m/%d"), Year = 2020, Month = '01', Day = '18', NumberOfContainers = 0, Week = '03', WeekDay = 'Saturday', Knots = 35, PublicHolidays = 0), mfs)
mfs<-rbind(data.frame(Date = as.Date('2020/03/09', format="%Y/%m/%d"), Year = 2020, Month = '03', Day = '09', NumberOfContainers = 0, Week = '11', WeekDay = 'Monday', Knots = 26, PublicHolidays = 0), mfs)
mfs<-rbind(data.frame(Date = as.Date('2020/05/01', format="%Y/%m/%d"), Year = 2020, Month = '05', Day = '01', NumberOfContainers = 0, Week = '18', WeekDay = 'Friday', Knots = 4, PublicHolidays = 1), mfs)
mfs<-rbind(data.frame(Date = as.Date('2020/09/06', format="%Y/%m/%d"), Year = 2020, Month = '09', Day = '06', NumberOfContainers = 0, Week = '36', WeekDay = 'Sunday', Knots = 22, PublicHolidays = 0), mfs)
mfs<-rbind(data.frame(Date = as.Date('2020/09/27', format="%Y/%m/%d"), Year = 2020, Month = '09', Day = '27', NumberOfContainers = 0, Week = '39', WeekDay = 'Sunday', Knots = 6, PublicHolidays = 0), mfs)
mfs<-rbind(data.frame(Date = as.Date('2020/10/31', format="%Y/%m/%d"), Year = 2020, Month = '10', Day = '31', NumberOfContainers = 0, Week = '44', WeekDay = 'Saturday', Knots = 27, PublicHolidays = 0), mfs)
mfs<-rbind(data.frame(Date = as.Date('2020/11/28', format="%Y/%m/%d"), Year = 2020, Month = '11', Day = '28', NumberOfContainers = 0, Week = '48', WeekDay = 'Saturday', Knots = 23, PublicHolidays = 0), mfs)
mfs<-rbind(data.frame(Date = as.Date('2020/12/25', format="%Y/%m/%d"), Year = 2020, Month = '12', Day = '25', NumberOfContainers = 0, Week = '52', WeekDay = 'Friday', Knots = 9, PublicHolidays = 1), mfs)
mfs<-rbind(data.frame(Date = as.Date('2021/01/01', format="%Y/%m/%d"), Year = 2021, Month = '01', Day = '01', NumberOfContainers = 0, Week = '53', WeekDay = 'Friday', Knots = 32, PublicHolidays = 1), mfs)
sum(is.na(mfs))
mfs<- arrange(mfs, Date)
write_xlsx(as.data.frame(mfs), path = "FinalMFS.xlsx")
##################################################################################################################
finalcontainerinfo1<-read_xlsx("Desktop/finalcontainerinfo.xlsx")
finaldataberthing1<-read_xlsx("Desktop/finaldataberthing.xlsx")
databerthinggg$Week<-strftime(databerthinggg$DOCK_TIMESTAMP_LT, format = "%V")
finalstackdates1<-read_xlsx("Desktop/finalstackdates.xlsx")
finalwind<-read_xlsx("finalwind.xlsx")
dimves<-read_xlsx("Desktop/dimves.xlsx")
class(finalstackdates1$documentdate)
dv<-dimves[complete.cases(dimves), ]
dv$VesselName<-NULL
dv$VesselType<-NULL
View(finalcontainerinfo1)
finalcontainerinfo1$id_vessel[finalcontainerinfo1$id_vessel=="342"]<-"346"

join1 <- merge(finalcontainerinfo, dv, by.x = "id_vessel", 
                   by.y = "id_vessel", all.x = TRUE, all.y = FALSE)
join1<- arrange(join1, DateTimeInHarbour)
join1$Week<-strftime(join1$DateTimeInHarbour, format = "%V")

join2<-merge(join, finalstackdates1, by.x = "DateTimeInHarbour", 
             by.y = "documentdate", all.x = TRUE, all.y = FALSE)


write_xlsx(as.data.frame(joinn), path = "container_stack.xlsx")
joinn<- arrange(joinn, DateTimeInHarbour)
write_xlsx(as.data.frame(join1), path = "containerimo.xlsx")
write_xlsx(as.data.frame(joinn), path = "container_berth_stack.xlsx")
