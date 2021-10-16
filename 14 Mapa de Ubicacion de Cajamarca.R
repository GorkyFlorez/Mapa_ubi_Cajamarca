# Librerias
library(raster)
library(sf)
library(ggspatial)
library(sp)
library(ggplot2)
library(ggrepel)
# Cargamos data
Pe          <- getData('GADM', country='Peru', level=0) %>%st_as_sf()
Per         <- getData('GADM', country='Peru', level=1) %>%st_as_sf() 
Peru        <- getData('GADM', country='Peru', level=2) %>%st_as_sf() 
Peru_dis    <- getData('GADM', country='Peru', level=3) %>%st_as_sf()
Cajamarca   =  subset(Per, NAME_1 == "Cajamarca")
Cajamar_pro =  subset(Peru, NAME_1 == "Cajamarca")
Cajamar_box = st_as_sfc(st_bbox(Cajamarca))
dff <- st_centroid(Cajamar_pro) 
df <- cbind(dff, st_coordinates(st_centroid(dff$geometry)))

SA          =  subset(Cajamar_pro, NAME_2 == "San Pablo")
CA          =  subset(Cajamar_pro, NAME_2 == "Celendín")
SM          =  subset(Cajamar_pro, NAME_2 == "San Marcos")
SZ          =  subset(Cajamar_pro, NAME_2 == "Contumazá")
SJ          =  subset(Cajamar_pro, NAME_2 == "Cajamarca")
SB          =  subset(Cajamar_pro, NAME_2 == "Cajabamba")

sites <- data.frame(longitude = c(-79.00224,-78.5 ,-78.46855  ),
                    latitude = c(-5.6,-6.437740,-7.3),
                    name= c("Jaen","Chota","Cajamarca" ))

g1= ggplot()+
  geom_sf(data = Per, fill="gray", color="white")+
  geom_sf(data = Pe, fill=NA)+
  geom_sf(data = Cajamar_box, fill=NA, size=1, color="deepskyblue4")+
  theme_void()
g1.grob  <- ggplotGrob(g1)

Map=ggplot()+
  geom_sf(data = Cajamar_pro, fill=NA, color="black")+
  geom_sf(data = SA, fill="cadetblue1", color="black")+
  geom_sf(data = CA, fill="cadetblue1", color="black")+
  geom_sf(data = SM, fill="cadetblue1", color="black")+
  geom_sf(data = SZ, fill="cadetblue1", color="black")+
  geom_sf(data = SJ, fill="cadetblue1", color="black")+
  geom_sf(data = SB, fill="cadetblue1", color="black")+
  geom_sf_text(data = st_as_sf(Cajamar_pro ), aes(label =  NAME_2), size = 3.5,family="serif") +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 2,
             shape = 19)+
  coord_sf(xlim = c(-80,-77.5), ylim = c( -7.763202 ,-4.622213))+
  geom_segment(aes(x=-79.00224, xend=-77.7, y=-5.6, yend=-5.6), 
               linetype = "solid", color = "deepskyblue4", size = 0.3) +
  geom_segment(aes(x=-78.5 , xend=-77.6, y=-6.437740, yend=-6.437740), 
               linetype = "solid", color = "deepskyblue4", size = 0.3) +
  geom_segment(aes(x=-78.46855 , xend=-77.4, y=-7.3, yend=-7.3), 
               linetype = "solid", color = "deepskyblue4", size = 0.3) +
  annotate(geom = "text", x = -78.2, y = -5.6, label = "Unidad de Negocio Jaen \nELECTRO ORIENTE S.A", 
           fontfamily = "serif", color = "grey22", size = 3,face="bold")+
  annotate(geom = "text", x = -78, y = -6.4, label = "Unidad de Negocio \nCajamarca Centro \nELECTRONORTE S.A", 
           fontfamily = "serif", color = "grey22", size = 3,face="bold")+
  annotate(geom = "text", x = -77.6, y = -7.3, label = "Unidad de Negocio \nCajamarca \nHIDRANDINA S.A", 
           fontfamily = "serif", color = "grey22", size = 3,face="bold")+
  annotate(geom = "text", x = -79, y = -7.7, label = "Ing.Gorky Florez", 
           fontfamily = "serif", color = "black", size = 3,face="bold")+
  theme_void()+
  theme(panel.background = element_rect(fill = "white"))+
  annotation_custom(g1.grob, xmin = -80, xmax = -79.3, ymin =-7.763202, ymax=-6.8)+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")

ggsave(plot = Map ,"MAPAS/Mapa Cajamarca.png", 
       units = "cm", width = 21,height = 29, dpi = 1000) 


