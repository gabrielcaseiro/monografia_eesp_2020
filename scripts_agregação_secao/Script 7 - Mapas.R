#############
### Mapas ###
#############

rm(list=ls()) 

library(sp)
library(lubridate)
library(dplyr)
library(raster)
library(rgdal)
library(SDraw)
library(leaflet)
library(htmlwidgets)
library(sf)

#Abrir base de escolas da administração direta:

base_escolas_mun<-readRDS("base_escolas_mun.rds")

base_escolas_mun<-SpatialPointsDataFrame(coords = base_escolas_mun[,33:32],data = base_escolas_mun[,c(1:31,34:37)] )

#Abrir dados dos locais de votação:

  res_lv_sec_t1<-readRDS("res_lv_sec_t1.rds")
  res_lv_sec_t2<-readRDS("res_lv_sec_t2.rds")
  
  res_lv<-bind_rows(res_lv_sec_t1[,c(1:10,15)],res_lv_sec_t2[,c(1:10,15)])
  
  res_lv$NR_SECAO<-NULL
  
  res_lv<-aggregate(res_lv$QTD_VOTOS_P,by=as.list(res_lv[,c(1,4,8:9)]),FUN=mean)
  
  colnames(res_lv)[5]<-"QTD_VOTOS_P"

  rm(list = ls(pattern = "res_lv_sec"))

  #Criar objeto de pontos espaciais com locais únicos:
  
  lv_sp<-SpatialPointsDataFrame(coords = res_lv[,4:3],data = res_lv[,c(1,2,5)] )
  

#Abrir mapa de São Paulo:

  map_sp<-readOGR("C:/Users/gcase/Documents/Projetos_R/CEPESP_LOCVOT_SP/LimiteMunicipal","LimiteMunicipalPolygon")
  map_sp<-map_sp[map_sp$Cod_ibge==3550308,]
  
#Padronizar projeções:  
  
  proj4string(lv_sp)<-proj4string(map_sp)
  proj4string(base_escolas_mun)<-proj4string(map_sp)
  

#Criar Mapa com os locais de votação: 
 
  anos_pref<-seq(2004,2016,4)

  lv_pl_04<-lv_sp[lv_sp$NR_ANO==2004&lv_sp$NR_TURNO==1,]
  lv_pl_04<-voronoi.polygons(lv_pl_04,bounding.polygon = map_sp) #Criar polígonos de voronoi no entorno dos locais de votação para facilitar a vizualização
  lv_pl_04@data<-cbind(lv_pl_04@data,lv_sp[lv_sp$NR_ANO==2004&lv_sp$NR_TURNO==1,]@data)
  
  lv_pl_08<-lv_sp[lv_sp$NR_ANO==2008&lv_sp$NR_TURNO==1,]
  lv_pl_08<-voronoi.polygons(lv_pl_08,bounding.polygon = map_sp) #Criar polígonos de voronoi no entorno dos locais de votação para facilitar a vizualização
  lv_pl_08@data<-cbind(lv_pl_08@data,lv_sp[lv_sp$NR_ANO==2008&lv_sp$NR_TURNO==1,]@data)
  
  lv_pl_12<-lv_sp[lv_sp$NR_ANO==2012&lv_sp$NR_TURNO==1,]
  lv_pl_12<-voronoi.polygons(lv_pl_12,bounding.polygon = map_sp) #Criar polígonos de voronoi no entorno dos locais de votação para facilitar a vizualização
  lv_pl_12@data<-cbind(lv_pl_12@data,lv_sp[lv_sp$NR_ANO==2012&lv_sp$NR_TURNO==1,]@data)
  
  lv_pl_16<-lv_sp[lv_sp$NR_ANO==2016&lv_sp$NR_TURNO==1,]
  lv_pl_16<-voronoi.polygons(lv_pl_16,bounding.polygon = map_sp) #Criar polígonos de voronoi no entorno dos locais de votação para facilitar a vizualização
  lv_pl_16@data<-cbind(lv_pl_16@data,lv_sp[lv_sp$NR_ANO==2016&lv_sp$NR_TURNO==1,]@data)
  
  pal<-colorNumeric(palette = "YlGnBu", 
                  domain = res_lv$QTD_VOTOS_P) #Cores
  icons <- awesomeIcons(icon = 'ios-close',iconColor = 'black',library = 'ion',markerColor = "lightred")
  
map_t1<-leaflet()%>%
  addTiles()%>%
  
  addPolygons(data = map_sp,fillOpacity = 0,color = "black")%>%
  
  #Resultados Eleitorais dos incumbentes nos locais de votação:
  
  addPolygons(data=lv_pl_04,color="lightblack",fillColor= ~pal(QTD_VOTOS_P)
              , smoothFactor = 0.5, fillOpacity = 0.8,group = "Eleição de 2004")%>%
  addPolygons(data=lv_pl_08,color="lightblack",fillColor= ~pal(QTD_VOTOS_P)
              , smoothFactor = 0.5, fillOpacity = 0.8,group = "Eleição de 2008")%>%
  addPolygons(data=lv_pl_12,color="lightblack",fillColor= ~pal(QTD_VOTOS_P)
              , smoothFactor = 0.5, fillOpacity = 0.8,group = "Eleição de 2012")%>%
  addPolygons(data=lv_pl_16,color="lightblack",fillColor= ~pal(QTD_VOTOS_P)
              , smoothFactor = 0.5, fillOpacity = 0.8,group = "Eleição de 2016")%>%
  
  addLegend("bottomright", pal = pal, values = res_lv$QTD_VOTOS_P,
            title = c("Votação do Incumbente - 1º Turno"),
            opacity = 1)%>%
 
  #Adicionar os pontos dos locais de votação:
  
  addCircleMarkers(data=lv_sp[lv_sp$NR_ANO==2004&lv_sp$NR_TURNO==1,], color = "black",stroke = F,radius=3, label=~QTD_VOTOS_P,group = "Eleição de 2004")%>%
  addCircleMarkers(data=lv_sp[lv_sp$NR_ANO==2008&lv_sp$NR_TURNO==1,], color = "black",stroke = F,radius=3, label=~QTD_VOTOS_P,group = "Eleição de 2008")%>%
  addCircleMarkers(data=lv_sp[lv_sp$NR_ANO==2012&lv_sp$NR_TURNO==1,], color = "black",stroke = F,radius=3, label=~QTD_VOTOS_P,group = "Eleição de 2012")%>%
  addCircleMarkers(data=lv_sp[lv_sp$NR_ANO==2016&lv_sp$NR_TURNO==1,], color = "black",stroke = F,radius=3, label=~QTD_VOTOS_P,group = "Eleição de 2016")%>%
  
  #Escolas abertas em cada legislatura:
  
   addAwesomeMarkers(data=base_escolas_mun[base_escolas_mun$ini_func_ano%in%seq(2001,2004)&base_escolas_mun$dif_ano>=0&!is.na(base_escolas_mun$dif_ano),],
             label = ~tipoesc, group = "Eleição de 2004",icon = icons)%>%
   addAwesomeMarkers(data=base_escolas_mun[base_escolas_mun$ini_func_ano%in%seq(2005,2008)&base_escolas_mun$dif_ano>=0&!is.na(base_escolas_mun$dif_ano),],
             label = ~tipoesc, group = "Eleição de 2008",icon = icons)%>%
   addAwesomeMarkers(data=base_escolas_mun[base_escolas_mun$ini_func_ano%in%seq(2009,2012)&base_escolas_mun$dif_ano>=0&!is.na(base_escolas_mun$dif_ano),],
             label = ~tipoesc, group = "Eleição de 2012",icon = icons)%>%
   addAwesomeMarkers(data=base_escolas_mun[base_escolas_mun$ini_func_ano%in%seq(2013,2016)&base_escolas_mun$dif_ano>=0&!is.na(base_escolas_mun$dif_ano),],
             label = ~tipoesc, group = "Eleição de 2016",icon = icons)%>%
    
  #Escolas criadas pelo incumbente em cada legislatura:
  
  addCircleMarkers(data=base_escolas_mun[base_escolas_mun$dom_ano%in%seq(2001,2004)&base_escolas_mun$dif_ano>=0&!is.na(base_escolas_mun$dif_ano),],
             group = "Eleição de 2004",color="red",radius = 5)%>%
  addCircleMarkers(data=base_escolas_mun[base_escolas_mun$dom_ano%in%seq(2005,2008)&base_escolas_mun$dif_ano>=0&!is.na(base_escolas_mun$dif_ano),],
             group = "Eleição de 2008",color="red",radius = 5)%>%
  addCircleMarkers(data=base_escolas_mun[base_escolas_mun$dom_ano%in%seq(2009,2012)&base_escolas_mun$dif_ano>=0&!is.na(base_escolas_mun$dif_ano),],
             group = "Eleição de 2012",color="red",radius = 5)%>%
  addCircleMarkers(data=base_escolas_mun[base_escolas_mun$dom_ano%in%seq(2013,2016)&base_escolas_mun$dif_ano>=0&!is.na(base_escolas_mun$dif_ano),],
             group = "Eleição de 2016",color="red",radius = 5)%>%
  
  #Escolas abertas em legislaturas anteriores
  
  addCircles(data=base_escolas_mun[base_escolas_mun$ini_func_ano<2001&!is.na(base_escolas_mun$ini_func_ano),],
                   group = "Escolas abertas (até 2000)",color="white",label = ~tipoesc)%>%
  addCircles(data=base_escolas_mun[base_escolas_mun$ini_func_ano<2005&!is.na(base_escolas_mun$ini_func_ano),],
                   group = "Escolas abertas (até 2004)",color="white",label = ~tipoesc)%>%
  addCircles(data=base_escolas_mun[base_escolas_mun$ini_func_ano<2009&!is.na(base_escolas_mun$ini_func_ano),],
                   group = "Escolas abertas (até 2008)",color="white",label = ~tipoesc)%>%
  addCircles(data=base_escolas_mun[base_escolas_mun$ini_func_ano<2013&!is.na(base_escolas_mun$ini_func_ano),],
                   group = "Escolas abertas (até 2012)",color="white",label = ~tipoesc)%>%
  
  addLayersControl(baseGroups = c("Eleição de 2004","Eleição de 2008","Eleição de 2012","Eleição de 2016"),
                   overlayGroups = c("Escolas abertas (até 2000)","Escolas abertas (até 2004)","Escolas abertas (até 2008)","Escolas abertas (até 2012)"),
                   options = layersControlOptions(collapsed = FALSE)) %>%

  hideGroup(c("Escolas abertas (até 2000)","Escolas abertas (até 2004)","Escolas abertas (até 2008)","Escolas abertas (até 2012)")) %>% 
  
  addScaleBar("bottomleft")

saveWidget(map_t1,file = "mapa_t1.html")
