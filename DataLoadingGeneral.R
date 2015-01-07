######################################################################
# After testing, should make function source files for each function #
######################################################################

### Import data
rm(list=ls()) # clear datalist
options(scipen=999) # disable scientic option

setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid") 
Irvine.GTM=read.table("./RawData/IrvineALLGTMasterlist.txt", header=T, fill=T)
Irvine.WIMRec=read.table("./RawData/IrvineALLWIM.txt", header=T, fill=T)

siglink=read.table("./RawData/signaturelink.txt", header=T, fill=T)
wimsiglink=read.table("./RawData/wimsignaturelink.txt", header=T, fill=T)
matching=merge(siglink, wimsiglink, by="vehid")
########################### Matching ###################################
#### import matching id - Oct 02

siglink_Oct02 <- subset (siglink, substr(siglink[,1],3,12) > 1349161200 & substr(siglink[,1],3,12) < 1349247600 )
wimsiglink_Oct02 <- subset (wimsiglink, substr(wimsiglink[,1],3,12) > 1349161200 & substr(wimsiglink[,1],3,12) < 1349247600 )
matching_Oct02 = merge (siglink_Oct02, wimsiglink_Oct02, by="vehid")

write.table(matching_Oct02, "./TruckReid/ProcessedData/matching_Oct02.txt", sep="\t")

#### import matching id - Mar 20
siglink_Mar20 <- subset (siglink, substr(siglink[,1],3,12) > 1363762800 & substr(siglink[,1],3,12) < 1363849200 )
wimsiglink_Mar20 <- subset (wimsiglink, substr(wimsiglink[,1],3,12) > 1363762800 & substr(wimsiglink[,1],3,12) < 1363849200 )
matching_Mar20 = merge (siglink_Mar20, wimsiglink_Mar20, by="vehid")



write.table(matching_Mar20, "./ProcessedData/matching_Mar20.txt", sep="\t")

### import matching id - Jan 0910 (SO-LC)
# from query (v1)
rm(LCLP)
options(scipen=999)
install.packages("RPostgreSQL")
library( RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- f.dbinfo (drv)

LCLP <- dbGetQuery(con, 
      " SELECT wimsignaturelink.wimsigid , groundtruthmasterlist.licenseplate 
			FROM gtsystem.groundtruthmasterlist, gtsystem.wimlink, gtsystem.wimsignaturelink, gtsystem.wimrecords
			where groundtruthmasterlist.vehid = wimlink.vehid 
			and groundtruthmasterlist.vehid = wimsignaturelink.vehid
			and  wimlink.wimid = wimrecords.wim_id
			and groundtruthmasterlist.station = 84")

SOLP <- dbGetQuery(con, 
     " SELECT wimsignaturelink.wimsigid , groundtruthmasterlist.licenseplate 
  		FROM gtsystem.groundtruthmasterlist, gtsystem.wimlink, gtsystem.wimsignaturelink, gtsystem.wimrecords
			where groundtruthmasterlist.vehid = wimlink.vehid 
			and groundtruthmasterlist.vehid = wimsignaturelink.vehid
			and  wimlink.wimid = wimrecords.wim_id
			and groundtruthmasterlist.station = 810")



LCLP <- LCLP[order (LCLP[,1]),]
SOLP <- SOLP[order (SOLP[,1]),]
LCLP[,2] <- as.character(LCLP[,2])
SOLP[,2] <- as.character(SOLP[,2])


LCLP <- cbind( SOLP[,2][match(LCLP[,2], SOLP[,2])], SOLP[,1][match(LCLP[,2], SOLP[,2])], LCLP )
matching_SOLC <- subset(LCLP[,2:3], !is.na( LCLP[,1]) )
colnames(matching_SOLC) <- c("SO", "LC")
matching_SOLC <- format(matching_SOLC, scientific=FALSE)

    # from text (v2)
#     matching_SOLC=read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/RawData/LCJan/MatchingIDSOLC.txt")
#     colnames(matching_SOLC) <- c("SO", "LC")
#     matching_SOLC <- format(matching_SOLC, scientific=FALSE)

# matching subset (ts)
matching <- subset(matching_SOLC , substr(SO, 3, 13) <  substr(LC, 3, 13))


#gt masterlist

LCGTML <- dbGetQuery(con, 
            "SELECT  groundtruthmasterlist.vehid, wimlink.wimid, wimsignaturelink.wimsigid, 
            groundtruthmasterlist.station, groundtruthmasterlist.lane,
            veh_class,  num_axles, wimrecords.ts,  veh_len, gross_weight, axle_1_2_spacing, axle_2_3_spacing,
            axle_3_4_spacing, axle_4_5_spacing,  axle_5_6_spacing, 
            axle_6_7_spacing, axle_7_8_spacing, axle_8_9_spacing, 
            axle_1_rt_weight, axle_1_lt_weight, axle_2_rt_weight, axle_2_lt_weight, 
            axle_3_rt_weight, axle_3_lt_weight, axle_4_rt_weight, axle_4_lt_weight,
            axle_5_rt_weight, axle_5_lt_weight, axle_6_rt_weight, axle_6_lt_weight,
            axle_7_rt_weight, axle_7_lt_weight, axle_8_rt_weight, axle_8_lt_weight, 
            axle_9_rt_weight, axle_9_lt_weight
            FROM  gtsystem.wimlink, gtsystem.wimsignaturelink, gtsystem.wimrecords,gtsystem.groundtruthmasterlist
            where groundtruthmasterlist.vehid = wimlink.vehid 
            and groundtruthmasterlist.vehid = wimsignaturelink.vehid
            and wimlink.wimid = wimrecords.wim_id
            and station = 84
            order by wimrecords.ts")

SOGTML <- dbGetQuery(con, 
             "SELECT  groundtruthmasterlist.vehid, wimlink.wimid, wimsignaturelink.wimsigid, 
          groundtruthmasterlist.station, groundtruthmasterlist.lane,
            veh_class,  num_axles, wimrecords.ts,  veh_len, gross_weight, axle_1_2_spacing, axle_2_3_spacing, 
            axle_3_4_spacing, axle_4_5_spacing, axle_5_6_spacing, 
            axle_6_7_spacing, axle_7_8_spacing, axle_8_9_spacing, 
            axle_1_rt_weight, axle_1_lt_weight, axle_2_rt_weight, axle_2_lt_weight, 
            axle_3_rt_weight, axle_3_lt_weight, axle_4_rt_weight, axle_4_lt_weight,
            axle_5_rt_weight, axle_5_lt_weight, axle_6_rt_weight, axle_6_lt_weight,
            axle_7_rt_weight, axle_7_lt_weight, axle_8_rt_weight, axle_8_lt_weight, 
            axle_9_rt_weight, axle_9_lt_weight
            FROM gtsystem.groundtruthmasterlist, gtsystem.wimlink, gtsystem.wimsignaturelink, gtsystem.wimrecords
            where groundtruthmasterlist.vehid = wimlink.vehid 
            and groundtruthmasterlist.vehid = wimsignaturelink.vehid
            and  wimlink.wimid = wimrecords.wim_id
            and station = 810
            order by wimrecords.ts")
dbDisconnect(con)

SOJan_v1 <- SOGTML 
LCJan_v1 <- LCGTML

      # from text
      #     SOJan_v1 <- read.table("./RawData/SOJan/SOJan_v1.txt",fill=T)
      #     LCJan_v1 <- read.table("./RawData/LCJan/LCJan_v1.txt",fill=T)