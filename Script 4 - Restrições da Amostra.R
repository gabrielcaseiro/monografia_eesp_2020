####################################################
#### Testes de robustez - Restrições da amostra ####
####################################################

library(dplyr)
library(sp)

####################################################################################################################
# 1.Restringir amostra a apenas locais de votação em que o status de tratamento variou durante o período analisado #
####################################################################################################################

rm(list = ls())

#Definir parâmetro de distância e turno:
k<-1
n_dist<-"1Km"
t<-1


res_lv_sec<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))

res_lv_sec$COD2<-paste(res_lv_sec$NR_ZONA,res_lv_sec$NR_LOCVOT,sep = "_")


#Tratamento 1: Abertura de Escolas
  
  res_lv_sec$I_esc_cri<-res_lv_sec[,paste("I_esc_ini_fun",n_dist,sep = "_")]
  
  teste<-aggregate(res_lv_sec$I_esc_cri ,list(res_lv_sec$COD2),FUN=sum)
  teste<-res_lv_sec[res_lv_sec$COD2%in%teste[teste[,2]>0,]$Group.1,]
  
  saveRDS(teste,file = paste("restr_1_1_",n_dist,".RDS",sep = ""))
  
#Tratamento 2: Abertura de escolas criadas pelo incumbente
  
  res_lv_sec$I_esc_cri<-res_lv_sec[,paste("I_esc_ini_fun_incumb",n_dist,sep = "_")]
  
  teste<-aggregate(res_lv_sec$I_esc_cri ,list(res_lv_sec$COD2),FUN=sum)
  teste<-res_lv_sec[res_lv_sec$COD2%in%teste[teste[,2]>0,]$Group.1,]
  
  saveRDS(teste,file = paste("restr_1_2_",n_dist,".RDS",sep = ""))
  
#Tratamento 3: Abertura de escolas no ano da eleição
  
  res_lv_sec$I_esc_cri<-res_lv_sec[,paste("I_esc_ini_fun_a4",n_dist,sep = "_")]
  
  teste<-aggregate(res_lv_sec$I_esc_cri ,list(res_lv_sec$COD2),FUN=sum)
  teste<-res_lv_sec[res_lv_sec$COD2%in%teste[teste[,2]>0,]$Group.1,]
  
  saveRDS(teste,file = paste("restr_1_3_",n_dist,".RDS",sep = ""))


######################################################################################################################################################################
# 2.Restringir amostra a pares de locais de votação tradados/controles próximos (a menos de 3km) e com a menor diferença na votação do incumbente na eleição anterior#
######################################################################################################################################################################  
  
#Tratamento 1: Abertura de Escolas
  
rm(list = ls())
  
  #Definir parâmetro de distância e turno:
  k<-1
  n_dist<-"1Km"
  t<-1
  
res_lv_sec<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))

  res_lv_sec$COD2<-paste(res_lv_sec$NR_ZONA,res_lv_sec$NR_LOCVOT,sep = "_")
  
  res_lv_sec<-res_lv_sec[!is.na(res_lv_sec$QTD_VOTOS_P_lag),]
  
res_lv<-unique(res_lv_sec[,c("NR_ANO","COD2","lat","lon","QTD_VOTOS_P_lag",paste("I_esc_ini_fun",n_dist,sep = "_"))])

name<-paste("restr_2_",n_dist,sep = "")

anos<-unique(res_lv_sec$NR_ANO)

teste<-NULL

for (g in 1:4) {
  
  temp<-res_lv[res_lv$NR_ANO==anos[g]&res_lv[,paste("I_esc_ini_fun",n_dist,sep = "_")]==1,]
  
  temp2<-res_lv[res_lv$NR_ANO==anos[g]&res_lv[,paste("I_esc_ini_fun",n_dist,sep = "_")]==0,]
  
  dist<-matrix(NA,nrow(temp2),nrow(temp))  
  
  for (i in 1:nrow(temp)) {
    
    dist[,i]<-spDistsN1(as.matrix(temp2[,c("lon","lat")]),as.numeric(temp[i,c("lon","lat")]),longlat = T)
    
  }
  
  dist<-1*(dist<3)
    dist[dist==0]<-NA
  
  lag<-abs(outer(temp2$QTD_VOTOS_P_lag,temp$QTD_VOTOS_P_lag,"-"))
  
  dist<-lag*dist
  
  min<-apply(dist, 2, function(x) {if (all(is.na(x))) {NA} else {which.min(x)} })
  
  temp2<-temp2[min[!is.na(min)],]
  
  temp2$id<-1:nrow(temp2)
  
  temp<-temp[!is.na(min),]
  
  temp$id<-1:nrow(temp)
  
  teste<-bind_rows(teste,temp,temp2)
  
}


teste<-left_join(teste,res_lv_sec)

  saveRDS(teste,file = paste(name,".RDS",sep = ""))

  
#Tratamento 2: Abertura de escolas criadas pelo incumbente
  
rm(list = ls())

#Definir parâmetro de distância e turno:
k<-1
n_dist<-"1Km"
t<-1

res_lv_sec<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))

res_lv_sec$COD2<-paste(res_lv_sec$NR_ZONA,res_lv_sec$NR_LOCVOT,sep = "_")

res_lv_sec<-res_lv_sec[!is.na(res_lv_sec$QTD_VOTOS_P_lag),]

res_lv<-unique(res_lv_sec[,c("NR_ANO","COD2","lat","lon","QTD_VOTOS_P_lag",paste("I_esc_ini_fun_incumb",n_dist,sep = "_"))])

name<-paste("restr_3_",n_dist,sep = "")

anos<-unique(res_lv_sec$NR_ANO)

teste<-NULL

for (g in 1:4) {
  
  temp<-res_lv[res_lv$NR_ANO==anos[g]&res_lv[,paste("I_esc_ini_fun_incumb",n_dist,sep = "_")]==1,]
  
  temp2<-res_lv[res_lv$NR_ANO==anos[g]&res_lv[,paste("I_esc_ini_fun_incumb",n_dist,sep = "_")]==0,]
  
  dist<-matrix(NA,nrow(temp2),nrow(temp))  
  
  for (i in 1:nrow(temp)) {
    
    dist[,i]<-spDistsN1(as.matrix(temp2[,c("lon","lat")]),as.numeric(temp[i,c("lon","lat")]),longlat = T)
    
  }
  
  dist<-1*(dist<3)
  dist[dist==0]<-NA
  
  lag<-abs(outer(temp2$QTD_VOTOS_P_lag,temp$QTD_VOTOS_P_lag,"-"))
  
  dist<-lag*dist
  
  min<-apply(dist, 2, function(x) {if (all(is.na(x))) {NA} else {which.min(x)} })
  
  temp2<-temp2[min[!is.na(min)],]
  
  temp2$id<-1:nrow(temp2)
  
  temp<-temp[!is.na(min),]
  
  temp$id<-1:nrow(temp)
  
  teste<-bind_rows(teste,temp,temp2)

  
}

teste<-left_join(teste,res_lv_sec)


saveRDS(teste,file = paste(name,".RDS",sep = ""))


#Tratamento 3: Abertura de escolas no ano da eleição

  rm(list = ls())
  
  #Definir parâmetro de distância e turno:
  k<-1
  n_dist<-"1Km"
  t<-1
  
  res_lv_sec<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))
  
  res_lv_sec$COD2<-paste(res_lv_sec$NR_ZONA,res_lv_sec$NR_LOCVOT,sep = "_")
  
  res_lv_sec<-res_lv_sec[!is.na(res_lv_sec$QTD_VOTOS_P_lag),]
  
  res_lv<-unique(res_lv_sec[,c("NR_ANO","COD2","lat","lon","QTD_VOTOS_P_lag",paste("I_esc_ini_fun_a4",n_dist,sep = "_"))])
  
  name<-paste("restr_4_",n_dist,sep = "")
  
  anos<-unique(res_lv_sec$NR_ANO)
  
  teste<-NULL
  
  for (g in 1:4) {
    
    temp<-res_lv[res_lv$NR_ANO==anos[g]&res_lv[,paste("I_esc_ini_fun_a4",n_dist,sep = "_")]==1,]
    
    temp2<-res_lv[res_lv$NR_ANO==anos[g]&res_lv[,paste("I_esc_ini_fun_a4",n_dist,sep = "_")]==0,]
    
    dist<-matrix(NA,nrow(temp2),nrow(temp))  
    
    for (i in 1:nrow(temp)) {
      
      dist[,i]<-spDistsN1(as.matrix(temp2[,c("lon","lat")]),as.numeric(temp[i,c("lon","lat")]),longlat = T)
      
    }
    
    dist<-1*(dist<3)
    dist[dist==0]<-NA
    
    lag<-abs(outer(temp2$QTD_VOTOS_P_lag,temp$QTD_VOTOS_P_lag,"-"))
    
    dist<-lag*dist
    
    min<-apply(dist, 2, function(x) {if (all(is.na(x))) {NA} else {which.min(x)} })
    
    temp2<-temp2[min[!is.na(min)],]
    
    temp2$id<-1:nrow(temp2)
    
    temp<-temp[!is.na(min),]
    
    temp$id<-1:nrow(temp)
    
    teste<-bind_rows(teste,temp,temp2)
    
  }
  
  teste<-left_join(teste,res_lv_sec)
  
  saveRDS(teste,file = paste(name,".RDS",sep = ""))
