###########################################################
#### Processamento dos dados dos censos de 2000 e 2010 ####
###########################################################

rm(list = ls())

library(sp)
library(lubridate)
library(dplyr)
library(raster)
library(rgdal)
library(SDraw)
library(leaflet)
library(htmlwidgets)
library(sf)
library(readxl)

#Abrir dados dos locais de votação:

res_lv_sec_t1<-readRDS("res_lv_sec_t1.rds")
res_lv_sec_t2<-readRDS("res_lv_sec_t2.rds")

  res_lv<-bind_rows(res_lv_sec_t1[,1:10],res_lv_sec_t2[,1:10])
  res_lv$NR_TURNO<-NULL
  res_lv<-unique(res_lv)
  
  rm(list = ls(pattern = "res_lv_sec"))

  #Criar objeto de pontos espaciais com locais únicos:
  
  res_lv<-unique(res_lv[,c("lon","lat")])
  
  res_lv$ID_lv<-1:nrow(res_lv)
  
  res_lv_<-res_lv

  #Padronizar projeção dos pontos Sirgas2000:
  
  res_lv <- st_as_sf(res_lv,coords = c("lon","lat"), crs = 4326) %>% 
    st_transform(5880)
  
  #Criar áreas de k metros de raio dos locais de votação:
  
  k<-1000
  res_lv_circles <- st_buffer(res_lv, dist = k)
  
###Método 1:  
  
#Censo 2000:
  
  #Shapefile dos distritos censitários:
  mapa_cs_2000<-readOGR("bases_originais/censo_2000")
   mapa_cs_2000<-st_as_sf(mapa_cs_2000)%>% 
     st_transform(5880)
   
   mapa_cs_2000$area_sc<-as.numeric(st_area(mapa_cs_2000))
   
  #Encontrar interseção entre os círculos dos locais de votação e os setores censitários 

   res_lv_circles_2000<-st_intersection(res_lv_circles,mapa_cs_2000)
   
    res_lv_circles_2000$area_intersec<-as.numeric(st_area(res_lv_circles_2000))
  
    res_lv_circles_2000$area_intersec_p<-res_lv_circles_2000$area_intersec/res_lv_circles_2000$area_sc
    
    #Manter como parte da área ao redor dos locais de votação apenas os setores com 50% ou mais de sua área dentro do perímetro
    
      #Ajustar casos de locais sem setor censitário pelo critério dos 50%: considerar apenas o setor de maior área no perímetro
      teste<-unique(res_lv_circles_2000$ID_lv)
        teste<-teste[!teste%in%unique(res_lv_circles_2000[res_lv_circles_2000$area_intersec_p>=0.5,]$ID_lv)]
        teste<-res_lv_circles_2000[res_lv_circles_2000$ID_lv%in%teste,]
        teste<-teste[order(teste$area_intersec_p,decreasing = T),]
        teste<-teste[!duplicated(teste$ID_lv),]
    
    res_lv_circles_2000<-res_lv_circles_2000[res_lv_circles_2000$area_intersec_p>=0.5,]
    
    res_lv_circles_2000<-bind_rows(res_lv_circles_2000,teste)

  #Abrir dados do Censo 2000 agregados por setor censitário:
    
    basico <- read_excel("bases_originais/censo_2000/basico_SP1.XLS")
      basico<-basico[basico$Cod_setor%in%mapa_cs_2000$CODSETOR,]
      colnames(basico)[1]<-"CODSETOR"
      basico[,2:17]<-NULL
    pessoa1 <- read_excel("bases_originais/censo_2000/Pessoa1_SP1.XLS")
      pessoa1<-pessoa1[pessoa1$Cod_setor%in%mapa_cs_2000$CODSETOR,]
      colnames(pessoa1)[1]<-"CODSETOR"
      pessoa1[,2:3]<-NULL
      
    res_lv_2000<-res_lv_circles_2000
      res_lv_2000$geometry<-NULL
      
    res_lv_2000<-left_join(res_lv_2000,basico)
      res_lv_2000<-left_join(res_lv_2000,pessoa1)
  
    res_lv_2000$N_sc<-1
    
    rm(list = c("pessoa1","basico"))
    
    #Agregar setores censitários dentro do perímetro k do local de votação
    
    res_lv_2000<-aggregate(res_lv_2000[,12:ncol(res_lv_2000)],by=list(res_lv_2000$ID_lv),FUN=function(x){sum(x,na.rm = T)})
    
    colnames(res_lv_2000)[1]<-"ID_lv"
    
    res_lv_2000<-left_join(res_lv_,res_lv_2000)
    
      saveRDS(res_lv_2000,paste("res_lv_2000_",k,"m.rds",sep = ""))
    
#Censo 2010:
    
    #Shapefile dos distritos censitários:
    mapa_cs_2010<-readOGR("bases_originais/censo_2010")
      mapa_cs_2010<-st_as_sf(mapa_cs_2010)%>% 
        st_transform(5880)
    
    mapa_cs_2010$area_sc<-as.numeric(st_area(mapa_cs_2010))
    
    #Encontrar interseção entre os círculos dos locais de votação e os setores censitários 
    
      res_lv_circles_2010<-st_intersection(res_lv_circles,mapa_cs_2010)
    
      res_lv_circles_2010$area_intersec<-as.numeric(st_area(res_lv_circles_2010))
    
      res_lv_circles_2010$area_intersec_p<-res_lv_circles_2010$area_intersec/res_lv_circles_2010$area_sc
    
    #Manter como parte da área ao redor dos locais de votação apenas os setores com 50% ou mais de sua área dentro do perímetro
    
    #Ajustar casos de locais sem setor censitário pelo critério dos 50%: considerar apenas o setor de maior área no perímetro
     teste<-unique(res_lv_circles_2010$ID_lv)
       teste<-teste[!teste%in%unique(res_lv_circles_2010[res_lv_circles_2010$area_intersec_p>=0.5,]$ID_lv)]
       teste<-res_lv_circles_2010[res_lv_circles_2010$ID_lv%in%teste,]
       teste<-teste[order(teste$area_intersec_p,decreasing = T),]
       teste<-teste[!duplicated(teste$ID_lv),]
    
      res_lv_circles_2010<-res_lv_circles_2010[res_lv_circles_2010$area_intersec_p>=0.5,]
    
      res_lv_circles_2010<-bind_rows(res_lv_circles_2010,teste)
    
    #Abrir dados do Censo 2010 agregados por setor censitário:
    
    basico <- read_excel("bases_originais/censo_2010/basico_SP1.XLS")
      basico$Cod_setor<-as.character(basico$Cod_setor)
      basico<-basico[basico$Cod_setor%in%mapa_cs_2010$CODSETOR,]
      colnames(basico)[1]<-"CODSETOR"
      basico[,2:19]<-NULL
      colnames(basico)[2:ncol(basico)]<-paste("bas_",colnames(basico)[2:ncol(basico)],sep = "")
      
    pessoa13 <- read_excel("bases_originais/censo_2010/Pessoa13_SP1.xls")
      pessoa13$Cod_setor<-as.character(pessoa13$Cod_setor)
      pessoa13<-pessoa13[pessoa13$Cod_setor%in%mapa_cs_2010$CODSETOR,]
      colnames(pessoa13)[1]<-"CODSETOR"
      pessoa13[,2]<-NULL
      colnames(pessoa13)[2:ncol(pessoa13)]<-paste("p13_",colnames(pessoa13)[2:ncol(pessoa13)],sep = "")
    
    responsavel2<-read_excel("bases_originais/censo_2010/Responsavel02_SP1.xls")
      responsavel2$Cod_setor<-as.character(responsavel2$Cod_setor)
      responsavel2<-responsavel2[responsavel2$Cod_setor%in%mapa_cs_2010$CODSETOR,]
      colnames(responsavel2)[1]<-"CODSETOR"
      responsavel2[,2]<-NULL
      colnames(responsavel2)[2:ncol(responsavel2)]<-paste("res2_",colnames(responsavel2)[2:ncol(responsavel2)],sep = "")
      
    res_lv_2010<-res_lv_circles_2010
      res_lv_2010$geometry<-NULL
      
    res_lv_2010<-left_join(res_lv_2010,basico)
      res_lv_2010<-left_join(res_lv_2010,pessoa13)
      res_lv_2010<-left_join(res_lv_2010,responsavel2)
    
    res_lv_2010$N_sc<-1
    
    rm(list = c("pessoa13","basico","responsavel2"))
    
    #Agregar setores censitários dentro do perímetro k do local de votação
    
    for (i in 12:ncol(res_lv_2010)) {
      res_lv_2010[,i]<-as.numeric(res_lv_2010[,i])
    }
    
    res_lv_2010$bas_V005_<-res_lv_2010$bas_V001*res_lv_2010$bas_V005
    res_lv_2010$bas_V007_<-res_lv_2010$bas_V001*res_lv_2010$bas_V007
    
    res_lv_2010<-aggregate(res_lv_2010[,12:ncol(res_lv_2010)],by=list(res_lv_2010$ID_lv),FUN=function(x){sum(x,na.rm = T)})
    
    colnames(res_lv_2010)[1]<-"ID_lv"
    
    res_lv_2010<-left_join(res_lv_,res_lv_2010)
    
      saveRDS(res_lv_2010,paste("res_lv_2010_",k,"m.rds",sep = ""))
      

      
###Mapas - Exemplos:
      
      library(ggplot2)
      
      ggplot(data = mapa_cs_2000) +
      geom_sf() + geom_sf(data=res_lv[res_lv$ID_lv==2,], size = 4, shape = 23, fill = "darkred") +
        geom_sf(data=res_lv_circles[res_lv_circles$ID_lv==2,], fill = NA, colour = "darkred",size=1)+
        coord_sf(xlim = c(5746308.84 , 5752035.64 ), ylim = c(7371491.47,7377152.16), expand = F)
     
    p<-st_transform(res_lv[res_lv$ID_lv==2,],4326)
    c<-st_transform(res_lv_circles[res_lv_circles$ID_lv==2,],4326)
       
    leaflet()%>%
      addTiles()%>%
      addPolygons(data = c)%>%
      addMarkers(data = p)%>%
      addScaleBar("bottomleft")
    
      
      