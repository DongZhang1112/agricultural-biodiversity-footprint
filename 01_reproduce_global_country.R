# Global and country-level calculations from the archived harmonized country-year input.
# Usage from archive root: Rscript scripts/01_reproduce_global_country.R
# Outputs are placed in reproduced_results/; original archived results are not overwritten.
options(stringsAsFactors=FALSE)
root <- Sys.getenv('NF_DATA_DIR', '.')
out <- file.path(root,'reproduced_results'); dir.create(out,showWarnings=FALSE,recursive=TRUE)
a <- read.csv(file.path(root,'data','01_country_and_year.csv'),check.names=FALSE)
needed <- c('ISO3','Country','Year','Outcrop_Q','Outanim_Q','Output_CL_Q','Agricultural_land_km2','BF_crop_km2eq','BF_livestock_km2eq','BF_total_km2eq')
stopifnot(all(needed %in% names(a)), nrow(a)==3507, length(unique(a$ISO3))==167,
          !anyDuplicated(a[c('ISO3','Year')]), all(a$Year %in% 2000:2020))
stopifnot(max(abs(a$Outcrop_Q + a$Outanim_Q - a$Output_CL_Q))<1e-4)
annual <- aggregate(a[c('Output_CL_Q','Agricultural_land_km2','BF_total_km2eq')],list(Year=a$Year),sum)
annual <- annual[order(annual$Year),]
annual$LI <- annual$Agricultural_land_km2/annual$Output_CL_Q
annual$BFI <- annual$BF_total_km2eq/annual$Output_CL_Q
for(n in c('Output_CL_Q','Agricultural_land_km2','BF_total_km2eq','LI','BFI')) annual[[paste0(n,'_index')]] <- annual[[n]]/annual[[n]][1]*100
write.csv(annual,file.path(out,'global_annual_recomputed.csv'),row.names=FALSE)
trend <- function(y,yrs) { if(any(!is.finite(y)) || any(y<=0)) return(NA_real_); (exp(unname(coef(lm(log(y)~yrs))[2]))-1)*100 }
byct <- split(a,a$ISO3)
country <- do.call(rbind,lapply(byct,function(d) {
 d<-d[order(d$Year),]; Y<-d$Output_CL_Q; BF<-d$BF_total_km2eq; A<-d$Agricultural_land_km2
 data.frame(ISO3=d$ISO3[1],Country=d$Country[1],R_Y_pct_yr=trend(Y,d$Year),R_A_pct_yr=trend(A,d$Year),R_BF_pct_yr=trend(BF,d$Year),R_LI_pct_yr=trend(A/Y,d$Year),R_BFI_pct_yr=trend(BF/Y,d$Year))
}))
country$Decoupling_category <- with(country,ifelse(R_Y_pct_yr <= 0,'Other',ifelse(R_BF_pct_yr<0,'Strong',ifelse(R_BF_pct_yr<R_Y_pct_yr,'Relative','No'))))
write.csv(country,file.path(out,'country_trends_recomputed.csv'),row.names=FALSE)
windows <- list(c(2000,2005),c(2005,2010),c(2010,2015),c(2015,2020))
period <- do.call(rbind,lapply(byct,function(d) do.call(rbind,lapply(windows,function(w) { q<-d[d$Year>=w[1] & d$Year<=w[2],]; data.frame(ISO3=d$ISO3[1],Country=d$Country[1],Period=paste(w,collapse='-'),Annual_trend_pct_yr=trend(q$BF_total_km2eq/q$Output_CL_Q,q$Year)) }))))
write.csv(period,file.path(out,'country_BFI_periods_recomputed.csv'),row.names=FALSE)
message('Global 2000-2020 change: output=',round(tail(annual$Output_CL_Q_index,1)-100,3),'%, land=',round(tail(annual$Agricultural_land_km2_index,1)-100,3),'%, BF=',round(tail(annual$BF_total_km2eq_index,1)-100,3),'%')
