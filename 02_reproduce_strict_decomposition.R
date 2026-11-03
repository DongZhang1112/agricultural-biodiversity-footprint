# Recompute strict-matched commodity decomposition from country-level matched input.
# Does NOT reproduce the alternative missing-BF-as-zero scenario without original missing rows.
root <- Sys.getenv('NF_DATA_DIR','.')
out <- file.path(root,'reproduced_results');dir.create(out,showWarnings=FALSE,recursive=TRUE)
d <- read.csv(file.path(root,'data','08_four_commodity_matched_country.csv'))
stopifnot(all(c('Commodity','ISO3','BF_2000','BF_2020','Q_2000_t','Q_2020_t') %in% names(d)))
calc <- function(x) {
 q0<-x$Q_2000_t; q1<-x$Q_2020_t;b0<-x$BF_2000;b1<-x$BF_2020
 stopifnot(all(q0>0),all(q1>0));Q0<-sum(q0);Q1<-sum(q1)
 I0<-b0/q0;I1<-b1/q1;w0<-q0/Q0;w1<-q1/Q1
 W0<-sum(w0*I0);W1<-sum(w1*I1)
 quantity<-(Q1-Q0)*(W0+W1)/2
 within<-(Q0+Q1)/2 * sum((w0+w1)/2*(I1-I0))
 between<-(Q0+Q1)/2 * sum((I0+I1)/2*(w1-w0))
 delta<-sum(b1)-sum(b0)
 data.frame(Commodity=x$Commodity[1],N=nrow(x),BF_change=delta,quantity_effect=quantity,within_country_intensity_effect=within,between_country_production_share_effect=between,balance_error=delta-quantity-within-between)
}
byc<-split(d,d$Commodity)
res<-do.call(rbind,lapply(byc,calc));stopifnot(max(abs(res$balance_error))<1e-6)
write.csv(res,file.path(out,'commodity_strict_decomposition_recomputed.csv'),row.names=FALSE)
# Leave-one-country-out rerun for Fig S3.
loo<-do.call(rbind,lapply(byc,function(x)do.call(rbind,lapply(seq_len(nrow(x)),function(i){ r<-calc(x[-i,,drop=FALSE]);r$excluded_ISO3<-x$ISO3[i];r }))))
write.csv(loo,file.path(out,'commodity_LOO_recomputed.csv'),row.names=FALSE)
print(res)
