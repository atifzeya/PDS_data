library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library(lubridate)

pds<-read_excel('D:/PDS_DATA_SCRAPED.xlsx')
pds$iden<-paste0(tolower(pds$state_name),tolower(pds$dfso))

lgd<-read_excel("D:/pds_district_lgd_codes.xlsx",sheet = "Sheet1")
lgd$iden<-paste0(tolower(lgd$state_name),tolower(lgd$dfso))
lgd1<-lgd %>% select(iden,D_CODE.x,id)
lgd1<-lgd1[!duplicated(lgd1$iden),]

df<-left_join(pds,lgd1,by="iden")
data<-select(df,-c(dfso,iden))
data1<-data[!duplicated(data),]   #removing all duplicates

#ck<-data[duplicated(data),]

#aggregating distributed
data2<-data1 %>% group_by(month_year,state_code,state_name,D_CODE.x,
                          total_rice_allocated,total_wheat_allocated,
                          total_qty_allocated) %>% summarise(total_rice_distributed_unautomated=sum(total_rice_distributed_unautomated),
                                                             total_wheat_distributed_unautomated=sum(total_wheat_distributed_unautomated),
                                                             total_qty_distributed_unautomated=sum(total_qty_distributed_unautomated),
                                                             total_rice_distributed_automated=sum(total_rice_distributed_automated),
                                                             total_wheat_distributed_automated=sum(total_wheat_distributed_automated),
                                                             total_qty_distributed_automated=sum(total_qty_distributed_automated),
                                                             total_qty_allocated_automated=sum(total_qty_allocated_automated),
                                                             total_rice_distributed=sum(total_rice_distributed),
                                                             total_wheat_distributed=sum(total_wheat_distributed))


data2_max<-data2 %>% filter(state_name %in% c('ASSAM','GOA','GUJARAT',
                                              'HARYANA','HIMACHAL PRADESH',
                                              'JHARKHAND','LAKSHADWEEP',
                                              'ODISHA','SIKKIM','TELANGANA',
                                              'UTTAR PRADESH'))

data2_sum<-data2 %>% filter(state_name %in% c('ANDAMAN & NICOBAR ISLANDS',
                                              'ANDHRA PRADESH','KARNATAKA',
                                              'ARUNACHAL PRADESH','KERALA',
                                              'BIHAR','JAMMU AND KASHMIR',
                                              'CHHATTISGARH','LADAKH',
                                              'DADRA & NAGAR HAVELI',
                                              'DAMAN & DIU','MANIPUR',
                                              'DELHI','MAHARASHTRA',
                                              'MADHYA PRADESH','MEGHALAYA',
                                              'MIZORAM','NAGALAND',
                                              'PUNJAB','RAJASTHAN',
                                              'TAMIL NADU','TRIPURA',
                                              'UTTARAKHAND','WEST BENGAL'))

data3_1<-data2_max %>% group_by(month_year,state_code,state_name,D_CODE.x) %>% summarise(total_rice_allocated=max(total_rice_allocated),
                                                                                         total_wheat_allocated=max(total_wheat_allocated),
                                                                                         total_qty_allocated=max(total_qty_allocated),
                                                                                         total_rice_distributed_unautomated=sum(total_rice_distributed_unautomated),
                                                                                         total_wheat_distributed_unautomated=sum(total_wheat_distributed_unautomated),
                                                                                         total_qty_distributed_unautomated=sum(total_qty_distributed_unautomated),
                                                                                         total_rice_distributed_automated=sum(total_rice_distributed_automated),
                                                                                         total_wheat_distributed_automated=sum(total_wheat_distributed_automated),
                                                                                         total_qty_distributed_automated=sum(total_qty_distributed_automated),
                                                                                         total_qty_allocated_automated=sum(total_qty_allocated_automated),
                                                                                         total_rice_distributed=sum(total_rice_distributed),
                                                                                         total_wheat_distributed=sum(total_wheat_distributed))

data3_2<-data2_sum %>% group_by(month_year,state_code,state_name,D_CODE.x) %>% summarise(total_rice_allocated=sum(total_rice_allocated),
                                                                                         total_wheat_allocated=sum(total_wheat_allocated),
                                                                                         total_qty_allocated=sum(total_qty_allocated),
                                                                                         total_rice_distributed_unautomated=sum(total_rice_distributed_unautomated),
                                                                                         total_wheat_distributed_unautomated=sum(total_wheat_distributed_unautomated),
                                                                                         total_qty_distributed_unautomated=sum(total_qty_distributed_unautomated),
                                                                                         total_rice_distributed_automated=sum(total_rice_distributed_automated),
                                                                                         total_wheat_distributed_automated=sum(total_wheat_distributed_automated),
                                                                                         total_qty_distributed_automated=sum(total_qty_distributed_automated),
                                                                                         total_qty_allocated_automated=sum(total_qty_allocated_automated),
                                                                                         total_rice_distributed=sum(total_rice_distributed),
                                                                                         total_wheat_distributed=sum(total_wheat_distributed))

data3<-rbind(data3_1,data3_2)

#final data preparation - mapping of lgd codes
lgd_official<-read_excel("D:/LGD_v1_17Oct19.xlsx")
lgd_official<-lgd_official %>% select(D_CODE,D_NAME)

fin<-left_join(data3,lgd_official,by=c("D_CODE.x"="D_CODE"))
final<-fin %>% select(month_year,state_name,state_code,D_NAME,D_CODE.x,
                      total_rice_allocated,total_wheat_allocated,
                      total_rice_distributed,total_wheat_distributed)

final$month_year <- format(final$month_year, "%d-%m-%Y")
final2<-final %>% filter(D_CODE.x>0)

colnames(final2)<-c("date","state_name","state_code","district_name",
                    "district_code","total_rice_allocated",
                    "total_wheat_allocated","total_rice_distributed",
                    "total_wheat_distributed")

write.csv(final2,"D:/pds_district_data.csv")

