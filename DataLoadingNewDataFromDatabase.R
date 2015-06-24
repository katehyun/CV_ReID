install.packages("RPostgreSQL")
library( RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con_v2 <- f.dbinfo_v2 (drv)


sigsite_up <- 75
sigsite_down <- 46 
wimsite_up <- 75
wimsite_down <- 46
  
sigts_up_from <- "2015-05-10 00:00:000"
sigts_up_to <- "2015-05-10 23:00:000"

wimts_up_from <-    1431241200000
wimts_up_to <-   1431324000000 


SIG_Up <- dbGetQuery(con_v2, 
      " SELECT signaturearchive_5_2015.id , signaturearchive_5_2015.detstaid, signaturearchive_5_2015.timestamp_full,
      signaturearchive_5_2015.duration, signaturearchive_5_2015.rawsig
  		FROM archive.signaturearchive_5_2015
			where signaturearchive_5_2015.detstaid = 75
      and timestamp_full >= '2015-05-20 00:00:000' and timestamp_full < '2015-05-21 00:00:000'
			and signaturearchive_5_2015.duration > 0")

SIG_Down <- dbGetQuery(con_v2, 
      " SELECT signaturearchive_5_2015.id , signaturearchive_5_2015.detstaid, signaturearchive_5_2015.timestamp_full,
      signaturearchive_5_2015.duration, signaturearchive_5_2015.rawsig
    	FROM archive.signaturearchive_5_2015
			where signaturearchive_5_2015.detstaid = 46
      and timestamp_full >= '2015-05-20 00:00:000' and timestamp_full < '2015-05-21 00:00:000'
			and signaturearchive_5_2015.duration > 0")



WIM_Up <- dbGetQuery(con_v2, 
     " SELECT wimarchive_5_2015.id , wimarchive_5_2015.site_no, wimarchive_5_2015.ts_fieldunit, wimarchive_5_2015.numaxles,
      wimarchive_5_2015.speedkmh, wimarchive_5_2015.vehlencm, wimarchive_5_2015.axlespccm, wimarchive_5_2015.axlewtltkg, 
      wimarchive_5_2015.axlewtrtkg
  		FROM archive.wimarchive_5_2015
			where wimarchive_5_2015.site_no = 46 
      and wimarchive_5_2015.ts_fieldunit > 1431241200000 and wimarchive_5_2015.ts_fieldunit < 1431324000000 
			and wimarchive_5_2015.vehlencm > 0")


WIM_Down <- dbGetQuery(con_v2, 
     " SELECT wimarchive_5_2015.id , wimarchive_5_2015.site_no, wimarchive_5_2015.ts_fieldunit, wimarchive_5_2015.numaxles,
      wimarchive_5_2015.speedkmh, wimarchive_5_2015.vehlencm, wimarchive_5_2015.axlespccm, wimarchive_5_2015.axlewtltkg, 
      wimarchive_5_2015.axlewtrtkg
    	FROM archive.wimarchive_5_2015
			where wimarchive_5_2015.site_no = 75 
      and wimarchive_5_2015.ts_fieldunit > 1431241200000 and wimarchive_5_2015.ts_fieldunit < 1431324000000
			and wimarchive_5_2015.vehlencm > 0")

minid <- sort(WIM_Up$id)[1]
print(minid)
maxid <- sort(WIM_Up$id)[length(WIM_Up$id)]
print(maxid)

WIM_SIG_pair <- dbGetQuery(con_v2, 
      " SELECT * 
      FROM public.wim_sig_pairs
			where wim_id > 13204143 and wim_id <  13449592  ")

