#
# Adapt state data to choroplethr package
#

# Replace abbreviation with name
data[["Geo-Region"]] <- state.regions$region[match(data[["Geo-Region"]], state.regions$abb)]

# NASA data_frame
nasa <- data[which(data[["LongURL"]] == "www.nasa.gov"),]
tmp <- table(nasa[["Geo-Region"]])
nasa_clics <- data.frame(value=as.numeric(tmp), region=names(tmp))
rm(nasa)

# PLD data_frame
pld <- data[which(data[["LongURL"]] == "pld.dpi.wi.gov"),]
tmp <- table(pld[["Geo-Region"]])
pld_clics <- data.frame(value=as.numeric(tmp), region=names(tmp))
rm(pld)

# FBI data_frame
fbi <- data[which(data[["LongURL"]] == "www.fbi.gov"),]
tmp <- table(fbi[["Geo-Region"]])
fbi_clics <- data.frame(value=as.numeric(tmp), region=names(tmp))
rm(fbi)

# Graph map
require(choroplethr)
state_choropleth(map_data, title="Clics totales desde el 1 de enero al 17 de mayo de 2013",
                 legend="Nº total de clics", num_colors=1)
