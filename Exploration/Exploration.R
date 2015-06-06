#
# Several plots
#

## INITIAL EXPLORATION

require(ggplot2)
require(gridExtra)

# City

g1 <- ggplot()+geom_line(aes(x=1:length(city_amounts),y=accumulated),colour="blue")+labs(x = "Ciudades", y="Densidad acumulada")

g2 <- ggplot() + geom_bar(aes(x=factor(names(city_amounts)[1:5]),y=city_amounts[1:5]),stat="identity",fill="springgreen3", color="white") + labs(x = "5 ciudades con más clics", y="Nº de clics") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=7)) + scale_x_discrete(limits=names(city_amounts)[1:5])

grid.arrange(g1,g2,ncol=2)

# Known user

ggplot() + geom_bar(aes(x=1, y=as.numeric(known), fill=legend), stat="identity", colour="white") + coord_polar(theta="y")+theme(axis.ticks=element_blank(),  axis.title=element_blank(), axis.text.y=element_blank())


## MODIFIED DATA

# UA, OS & DEV

nombres_ua <- names(datos_ua)
nombres_os <- names(datos_os)
nombres_dev <- names(datos_dev)

g1 <- ggplot() + geom_bar(aes(x=nombres_ua,y=datos_ua),stat="identity",fill="steelblue3",colour="white") + labs(x = "Agentes de usuario", y="")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=7)) + scale_x_discrete(limits=nombres_ua)
g2 <- ggplot() + geom_bar(aes(x=nombres_os,y=datos_os),stat="identity",fill="darkorange1",colour="white") + labs(x = "Sistemas operativos", y="")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=7))+ scale_x_discrete(limits=nombres_os)
g3 <- ggplot() + geom_bar(aes(x=c(nombres_dev[1:4],"Android device"),y=datos_dev),stat="identity",fill="springgreen3",colour="white") + labs(x = "Dispositivos", y="")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=7))+ scale_x_discrete(limits=c(nombres_dev[1:4],"Android device"))
grid.arrange(g1,g2,g3,ncol=3)

# UA profiles

profiles <- sort(table(data$ua_profiles),decreasing=TRUE)[1:10]

ggplot() + geom_bar(aes(x=names(profiles),y=profiles),stat="identity",fill="darkgoldenrod2",colour="white") + labs(y = "10 perfiles más utilizados", x="" )+ scale_x_discrete(limits=factor(names(profiles))) + coord_flip()


# Long & Referring URL

long_url <- as.numeric(sort(table(data$long_url), decreasing=T))
names_long_url <- names(data$long_url)
ref_url <- as.numeric(sort(table(data$referring_url), decreasing=T))
names_ref_url <- names(data$referring_url)

options(scipen=5)
g1 <- ggplot() + geom_bar(aes(x=names_ref_url,y=ref_url),stat="identity",fill="light blue",colour="white") + labs(x = "URLs de origen", y="")+theme(axis.text.y=element_text(size=7)) +theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=7))+ scale_x_discrete(limits=names_ref_url)

g2 <- ggplot() + geom_bar(aes(x=names_long_url,y=long_url),stat="identity",fill="springgreen3",colour="white") + labs(x = "URLs de destino", y="")+theme(axis.text.y=element_text(size=7)) +theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=5.8))+ scale_x_discrete(limits=names_long_url)

legend <- c("Segura (HTTPS)", "No segura (HTTP)")
g3 <- ggplot() + geom_bar(aes(x=1, y=table(data$seg_url, fill=data$seg_url), stat="identity", colour="white")+labs(y="",x="% Tipo de conexión")+theme(axis.text.y=element_text(size=7))+ coord_polar(theta="y")+theme(axis.ticks=element_blank(),  axis.title=element_blank(), axis.text.y=element_blank())+scale_fill_discrete(guide=F)+ggtitle("% Conexiones seguras")

grid.arrange(g1,g2,g3,ncol=3)


## DESCRIPTIVE STATISTICS

# EEUU Map

load(".//mapaClicsTotales.RData")
require(choroplethr)
state_choropleth(map_data, title="Clics totales (01/01/13 a 17/05/13)",
                 legend="Nº total de clics", num_colors=1)


# Time & Midweek

ggplot(data) + geom_histogram(aes(x = long_url,fill = time), color = "white")+ 
facet_grid(midweek~time)+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=5))+labs(y="Entre semana",x="URLs de destino")+scale_fill_discrete(guide=F)


# Popular UA profiles

ggplot(data)+geom_bar(aes(x=long_url,fill=ua_profiles),color="white",position ="fill")+labs(y="",x="")+theme(axis.text.y=element_text(size=8),legend.text=element_text(size=6),legend.key.height=unit(.5,"line"),legend.key.width=unit(.5,"line"))+coord_flip()+guides(fill=guide_legend(title="User agents"))


# Referring URLs

ggplot(data)+geom_bar(aes(x=long_url,fill=referring_url),color="white",position ="fill")+labs(y="",x="")+theme(axis.text.y=element_text(size=8),legend.text=element_text(size=6),legend.key.height=unit(.5,"line"),legend.key.width=unit(.5,"line"))+coord_flip()+guides(fill=guide_legend(title="URLs de origen"))
