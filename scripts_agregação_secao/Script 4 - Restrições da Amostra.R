####################################################
#### Testes de robustez - Restrições da amostra ####
####################################################

library(dplyr)
library(sp)


# 1.Restringir amostra apenas às seções em locais de votação em que se abriu ou se criou alguma escola durante o período analisado:

rm(list = ls())

  #Definir parâmetro de distância e turno:
  k<-1
  n_dist<-"1Km"
  t<-1
  
res_lv_sec<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))
  
  res_lv_sec$COD<-paste(res_lv_sec$NR_ZONA,res_lv_sec$NR_SECAO,sep = "_")
  res_lv_sec$COD2<-paste(res_lv_sec$NR_ZONA,res_lv_sec$NR_LOCVOT,sep = "_")
  
res_lv_sec$I_esc_cri<-res_lv_sec[,paste("I_esc_ini_fun",n_dist,sep = "_")]+res_lv_sec[,paste("I_esc_nao_fun_incumb",n_dist,sep = "_")]+res_lv_sec[,paste("I_esc_nao_fun_lag",n_dist,sep = "_")]

  teste<-aggregate(res_lv_sec$I_esc_cri ,list(res_lv_sec$COD),FUN=sum)
  teste<-res_lv_sec[res_lv_sec$COD%in%teste[teste$x>0,]$Group.1,]

  saveRDS(teste,file = paste("restr_1_",n_dist,".RDS",sep = ""))

  
# 2.Restringir amostra apenas a pares de locais de votação tradados/controles (abertura de escolas pelo incumbente em cada legislatura):
  
rm(list = ls())
  
  #Definir parâmetro de distância e turno:
  k<-1
  n_dist<-"1Km"
  t<-1
  
res_lv_sec<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))

  res_lv_sec$COD2<-paste(res_lv_sec$NR_ZONA,res_lv_sec$NR_LOCVOT,sep = "_")
  
  res_lv_sec<-res_lv_sec[!is.na(res_lv_sec$QTD_VOTOS_P_lag),]
  
res_lv<-unique(res_lv_sec[,c("NR_ANO","COD2","lat","lon",paste("I_esc_ini_fun",n_dist,sep = "_"))])

name<-paste("restr_2_",n_dist,sep = "")

anos<-unique(res_lv_sec$NR_ANO)

teste<-NULL

for (g in 1:4) {
  
  temp<-res_lv[res_lv$NR_ANO==anos[g]&res_lv[,paste("I_esc_ini_fun",n_dist,sep = "_")]==1,]
  
  temp2<-res_lv[res_lv$NR_ANO==anos[g]&res_lv[,paste("I_esc_ini_fun",n_dist,sep = "_")]==0,]
  
  dist<-matrix(NA,nrow(temp2),nrow(temp))  
  
  for (i in 1:nrow(temp)) {
    
    dist[,i]<-spDistsN1(as.matrix(temp2[,c("lon","lat")]),as.numeric(temp[i,c("lon","lat")]),longlat = T)
    dist[!temp2$NR_ZONA==temp[i,]$NR_ZONA,i]<-NA
    dist[dist[,i]>5,i]<-NA
    
    
  }
  
  min<-apply(dist, 2, function(x) {if (all(is.na(x))) {NA} else {which.min(x)} })
  
  for (h in 1:length(min)) {
    
    if(is.na(min[h])==F){
      
      t1<-left_join(temp[h,],res_lv_sec)
      t2<-left_join(temp2[min[h],],res_lv_sec)
      
      t<-abs(outer(t1$QTD_VOTOS_P_lag,t2$QTD_VOTOS_P_lag,"-"))
      
      min_t<-apply(t, 1, function(x) {if (all(is.na(x))) {NA} else {which.min(x)} })
      
      t2<-t2[na.omit(min_t),]
      
      t2$id<-paste(g,h,1:nrow(t2))
      
      t1<-t1[!is.na(min_t),]
      
      t1$id<-paste(g,h,1:nrow(t1))
      
      teste<-rbind(teste,t1,t2)
      
    }
      
    }
  
}

  saveRDS(teste,file = paste(name,".RDS",sep = ""))

  
  
# 3.Restringir amostra apenas a pares de locais de votação tradados/controles (abertura de escolas criadas pelo incumbente em cada legislatura):
  
rm(list = ls())

#Definir parâmetro de distância e turno:
k<-1
n_dist<-"1Km"
t<-1

res_lv_sec<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))

res_lv_sec$COD2<-paste(res_lv_sec$NR_ZONA,res_lv_sec$NR_LOCVOT,sep = "_")

res_lv_sec<-res_lv_sec[!is.na(res_lv_sec$QTD_VOTOS_P_lag),]

res_lv<-unique(res_lv_sec[,c("NR_ANO","COD2","lat","lon",paste("I_esc_ini_fun_incumb",n_dist,sep = "_"))])

name<-paste("restr_3_",n_dist,sep = "")

anos<-unique(res_lv_sec$NR_ANO)

teste<-NULL

for (g in 1:4) {
  
  temp<-res_lv[res_lv$NR_ANO==anos[g]&res_lv[,paste("I_esc_ini_fun_incumb",n_dist,sep = "_")]==1,]
  
  temp2<-res_lv[res_lv$NR_ANO==anos[g]&res_lv[,paste("I_esc_ini_fun_incumb",n_dist,sep = "_")]==0,]
  
  dist<-matrix(NA,nrow(temp2),nrow(temp))  
  
  for (i in 1:nrow(temp)) {
    
    dist[,i]<-spDistsN1(as.matrix(temp2[,c("lon","lat")]),as.numeric(temp[i,c("lon","lat")]),longlat = T)
    dist[!temp2$NR_ZONA==temp[i,]$NR_ZONA,i]<-NA
    dist[dist[,i]>5,i]<-NA
    
    
  }
  
  min<-apply(dist, 2, function(x) {if (all(is.na(x))) {NA} else {which.min(x)} })
  
  for (h in 1:length(min)) {
    
    if(is.na(min[h])==F){
      
      t1<-left_join(temp[h,],res_lv_sec)
      t2<-left_join(temp2[min[h],],res_lv_sec)
      
      t<-abs(outer(t1$QTD_VOTOS_P_lag,t2$QTD_VOTOS_P_lag,"-"))
      
      min_t<-apply(t, 1, function(x) {if (all(is.na(x))) {NA} else {which.min(x)} })
      
      t2<-t2[na.omit(min_t),]
      
      t2$id<-paste(g,h,1:nrow(t2))
      
      t1<-t1[!is.na(min_t),]
      
      t1$id<-paste(g,h,1:nrow(t1))

      teste<-rbind(teste,t1,t2)
      
    }
    
  }

  
}


saveRDS(teste,file = paste(name,".RDS",sep = ""))


# 4.Restringir amostra apenas a pares de locais de votação tradados/controles (abertura de escolas criadas pelo incumbente em cada legislatura):
  
  rm(list = ls())
  
  #Definir parâmetro de distância e turno:
  k<-1
  n_dist<-"1Km"
  t<-1
  
  res_lv_sec<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))
  
  res_lv_sec$COD2<-paste(res_lv_sec$NR_ZONA,res_lv_sec$NR_LOCVOT,sep = "_")
  
  res_lv_sec<-res_lv_sec[!is.na(res_lv_sec$QTD_VOTOS_P_lag),]
  
  res_lv<-unique(res_lv_sec[,c("NR_ANO","COD2","lat","lon",paste("I_esc_ini_fun_a4",n_dist,sep = "_"))])
  
  name<-paste("restr_4_",n_dist,sep = "")
  
  anos<-unique(res_lv_sec$NR_ANO)
  
  teste<-NULL
  
  for (g in 1:4) {
    
    temp<-res_lv[res_lv$NR_ANO==anos[g]&res_lv[,paste("I_esc_ini_fun_a4",n_dist,sep = "_")]==1,]
    
    temp2<-res_lv[res_lv$NR_ANO==anos[g]&res_lv[,paste("I_esc_ini_fun_a4",n_dist,sep = "_")]==0,]
    
    dist<-matrix(NA,nrow(temp2),nrow(temp))  
    
    for (i in 1:nrow(temp)) {
      
      dist[,i]<-spDistsN1(as.matrix(temp2[,c("lon","lat")]),as.numeric(temp[i,c("lon","lat")]),longlat = T)
      dist[!temp2$NR_ZONA==temp[i,]$NR_ZONA,i]<-NA
      dist[dist[,i]>5,i]<-NA
      
      
    }
    
    min<-apply(dist, 2, function(x) {if (all(is.na(x))) {NA} else {which.min(x)} })
    
    for (h in 1:length(min)) {
      
      if(is.na(min[h])==F){
        
        t1<-left_join(temp[h,],res_lv_sec)
        t2<-left_join(temp2[min[h],],res_lv_sec)
        
        t<-abs(outer(t1$QTD_VOTOS_P_lag,t2$QTD_VOTOS_P_lag,"-"))
        
        min_t<-apply(t, 1, function(x) {if (all(is.na(x))) {NA} else {which.min(x)} })
        
        t2<-t2[na.omit(min_t),]
        
        t2$id<-paste(g,h,1:nrow(t2))
        
        t1<-t1[!is.na(min_t),]
        
        t1$id<-paste(g,h,1:nrow(t1))
        
        teste<-rbind(teste,t1,t2)
        
      }
        
      }
    
  }
  
  
  saveRDS(teste,file = paste(name,".RDS",sep = ""))
