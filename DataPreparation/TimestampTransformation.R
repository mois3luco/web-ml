#
# Feature engineering: Timestamp -> Time and Midweek
#

require(lubridate)

dates <- as.POSIXct(data$timestamp, origin="1970-01-01", tz=data$timezone)

## MIDWEEK: TRUE si es un día entre semana

data$midweek <- ! weekdays(as.Date(dates)) %in% c("sábado","domingo")

## TIME: madrugada 0-5, mañana 6-12, tarde 13-18, noche 19-23

hours <- hour(dates)

times <- factor(levels=c("madrugada","mañana","tarde","noche"))  

times[ which(0 <= hours & hours <= 5) ] <- "madrugada"
times[ which(6 <= hours & hours <= 12) ] <- "mañana"
times[ which(13 <= hours & hours <= 18) ] <- "tarde"
times[ which(19 <= hours & hours <= 23) ] <- "noche"

data$time <- times

## VISUALIZE RESULTS

require(ggplot2)
ggplot(data) + geom_histogram(aes(x = long_url), fill = time, color = "white")+ 
facet_grid(midweek~time)+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=10))

