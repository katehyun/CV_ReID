rm(list=ls())
# install.packages("RPostgreSQL")
library( RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con_v2 <- f.dbinfo_v2 (drv)


sigsite_up <- 75
sigsite_down <- 46 
wimsite_up <- 75
wimsite_down <- 46
  
sigts_up_from <- "2015-06-17 00:00:000" # & 6/22-24
sigts_up_to <- "2015-06-19 23:59:000" # & 6/22-24
 - 
wimts_up_from <-   1434956400000 # 1434956400000   1434524400000
wimts_up_to <-    143521554000 #  1435215540000   1434783599000


SIG_Up <- dbGetQuery(con_v2, 
      " SELECT signaturearchive_6_2015.id , signaturearchive_6_2015.detstaid,  signaturearchive_6_2015.timestamp_full,
      signaturearchive_6_2015.duration, signaturearchive_6_2015.rawsig
  		FROM archive.signaturearchive_6_2015		
        where signaturearchive_6_2015.detstaid = 75
      and timestamp_full >= '2015-06-22 00:00:000' and timestamp_full < '2015-06-24 23:59:000'
			and signaturearchive_6_2015.duration > 0 and lane_dir=1")

SIG_Down <- dbGetQuery(con_v2, 
      " SELECT signaturearchive_6_2015.id , signaturearchive_6_2015.detstaid, signaturearchive_6_2015.timestamp_full,
      signaturearchive_6_2015.duration, signaturearchive_6_2015.rawsig
    	FROM archive.signaturearchive_6_2015
			where signaturearchive_6_2015.detstaid = 46
      and timestamp_full >= '2015-06-22 00:00:000' and timestamp_full < '2015-06-24 23:59:000'
			and signaturearchive_6_2015.duration > 0 and lane_dir=1")



WIM_Up <- dbGetQuery(con_v2, 
     " SELECT wimarchive_6_2015.id , wimarchive_6_2015.site_no, wimarchive_6_2015.ts_fieldunit, wimarchive_6_2015.numaxles,
      wimarchive_6_2015.speedkmh, wimarchive_6_2015.vehlencm, wimarchive_6_2015.axlespccm, wimarchive_6_2015.axlewtltkg, 
      wimarchive_6_2015.axlewtrtkg
  		FROM archive.wimarchive_6_2015
			where wimarchive_6_2015.site_no = 75 
      and wimarchive_6_2015.ts_fieldunit >  1434956400000 and wimarchive_6_2015.ts_fieldunit <  1435215540000 
			and wimarchive_6_2015.vehlencm > 0 and lane<3")


WIM_Down <- dbGetQuery(con_v2, 
     " SELECT wimarchive_6_2015.id , wimarchive_6_2015.site_no, wimarchive_6_2015.ts_fieldunit, wimarchive_6_2015.numaxles,
      wimarchive_6_2015.speedkmh, wimarchive_6_2015.vehlencm, wimarchive_6_2015.axlespccm, wimarchive_6_2015.axlewtltkg, 
      wimarchive_6_2015.axlewtrtkg
    	FROM archive.wimarchive_6_2015
			where wimarchive_6_2015.site_no = 46 
      and wimarchive_6_2015.ts_fieldunit > 1434956400000 and wimarchive_6_2015.ts_fieldunit < 1435215540000 
			and wimarchive_6_2015.vehlencm > 0 and lane>=3")


minid <- sort(WIM_Up$id)[1]
print(minid)
maxid <- sort(WIM_Up$id)[length(WIM_Up$id)]
print(maxid)

WIM_SIG_pair <- dbGetQuery(con_v2, 
      " SELECT * 
      FROM public.wim_sig_pairs
			where wim_id > 22880189 and wim_id < 23489130 ")

# WIM_SIG_pair <- WIM_SIG_pair[order(WIM_SIG_pair[,1]),] 
