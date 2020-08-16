#################################################################
#### Processamento da Base de Escolas e Criação de Variáveis ####
#################################################################

library(tidyr)
library(dplyr)
library(readr)
library(sp)
library(lubridate)

rm(list = ls())

#Abrir base de escolas:

base_escolas <- read_delim("bases_originais/escolas122019.csv", 
                           ";", escape_double = FALSE, col_types = cols( 
                             dom_criacao = col_date(format = "%d/%m/%Y"), 
                             dt_autoriza = col_date(format = "%d/%m/%Y"), 
                             dt_criacao = col_date(format = "%d/%m/%Y"), 
                             dt_extintao = col_date(format = "%d/%m/%Y"), 
                             dt_ini_conv = col_date(format = "%d/%m/%Y"), 
                             dt_ini_func = col_date(format = "%d/%m/%Y"), 
                             latitude = col_number(), longitude = col_number()), 
                           trim_ws = TRUE)

#Corrigir algumas erros nas variáveis de geolocalização:

base_escolas[base_escolas$latitude>(-5),]$latitude<-10*base_escolas[base_escolas$latitude>(-5),]$latitude
  base_escolas[base_escolas$latitude<(-100),]$latitude<-base_escolas[base_escolas$latitude<(-100),]$latitude/10
base_escolas[base_escolas$longitude>(-5),]$longitude<-10*base_escolas[base_escolas$longitude>(-5),]$longitude
  base_escolas[base_escolas$longitude<(-100),]$longitude<-base_escolas[base_escolas$longitude<(-100),]$longitude/10

#Selecionar apenas escolas da rede pública de administração direta:
  
base_escolas_mun<-base_escolas[base_escolas$rede=="DIR"&!is.na(base_escolas$rede),]

    # unique(base_escolas_mun$tipoesc)
    # [1] "EMEF"      "EMEI"      "CEU EMEF"  "CEU EMEI"  "CEMEI"     "CIEJA"     "CEI DIRET" "CEU CEI"   "EMEBS"     "CCI/CIPS" 
    # [11] "EMEFM

#Criar variáveis com os anos de criação e de início de funcionamento das escolas:

base_escolas_mun$dom_ano<-year(base_escolas_mun$dom_criacao)

base_escolas_mun$ini_func_ano<-year(base_escolas_mun$dt_ini_func)

base_escolas_mun$dif_ano<-base_escolas_mun$ini_func_ano-base_escolas_mun$dom_ano

  saveRDS(base_escolas_mun,file = "base_escolas_mun.rds")
  

####################Relacionar base de matrículas à base de seções e criar variáveis:

  base_escolas_mun<-readRDS("base_escolas_mun.rds")
  
#Abrir bases de votos:

  res_lv_sec_t1<-readRDS("res_lv_sec_t1.rds")
  res_lv_sec_t2<-readRDS("res_lv_sec_t2.rds")
  
  res_lv<-bind_rows(res_lv_sec_t1[,1:10],res_lv_sec_t2[,1:10])
    res_lv$NR_TURNO<-NULL
    res_lv<-unique(res_lv)
  

#Definir parâmetros:

  #Anos de fim de legislatura/eleições:
  anos_pref<-c(2004,2008,2012,2016)
  
  #Raio de distância ao redor do local de votação (em Km):
  k<-1.25
  n_dist<-"1.25Km"
  
  
#Variável: Escolas públicas de administração direta que começaram a funcionar

res_lv[,paste("esc_ini_fun",n_dist,sep = "_")]<-0

for (g in 1:4) {
  
  temp<-base_escolas_mun[base_escolas_mun$ini_func_ano%in%seq(anos_pref[g]-3,anos_pref[g])&
                           base_escolas_mun$dif_ano>=0,]
  
  temp<-temp[!is.na(temp$latitude),]
  
  dist<-matrix(NA,nrow(res_lv[res_lv$NR_ANO==anos_pref[g],]),nrow(temp))  
  
  for (i in 1:nrow(temp)) {
    
    dist[,i]<-1*(spDistsN1(as.matrix(res_lv[res_lv$NR_ANO==anos_pref[g],c("lon","lat")]),as.numeric(temp[i,c("longitude","latitude")]),longlat = T)<k)
    
    
  }
  
  dist<-rowSums(dist)
  
  res_lv[res_lv$NR_ANO==anos_pref[g],paste("esc_ini_fun",n_dist,sep = "_")]<-dist
  
}

res_lv[,paste("I_esc_ini_fun",n_dist,sep = "_")]<-1*(res_lv[,paste("esc_ini_fun",n_dist,sep = "_")]>0)


#Variável: Escolas públicas de administração direta criadas pelo incumbente que começaram a funcionar

res_lv[,paste("esc_ini_fun_incumb",n_dist,sep = "_")]<-0

for (g in 1:4) {
  
  temp<-base_escolas_mun[base_escolas_mun$ini_func_ano%in%seq(anos_pref[g]-3,anos_pref[g])&
                          base_escolas_mun$dom_ano%in%seq(anos_pref[g]-3,anos_pref[g])&
                           base_escolas_mun$dif_ano>=0,]
  
  temp<-temp[!is.na(temp$latitude),]
  
  dist<-matrix(NA,nrow(res_lv[res_lv$NR_ANO==anos_pref[g],]),nrow(temp))  
  
  for (i in 1:nrow(temp)) {
    
    dist[,i]<-1*(spDistsN1(as.matrix(res_lv[res_lv$NR_ANO==anos_pref[g],c("lon","lat")]),as.numeric(temp[i,c("longitude","latitude")]),longlat = T)<k)
    
    
  }
  
  dist<-rowSums(dist)
  
  res_lv[res_lv$NR_ANO==anos_pref[g],paste("esc_ini_fun_incumb",n_dist,sep = "_")]<-dist
  
}

res_lv[,paste("I_esc_ini_fun_incumb",n_dist,sep = "_")]<-1*(res_lv[,paste("esc_ini_fun_incumb",n_dist,sep = "_")]>0)


#Variável: Escolas públicas de administração direta criadas pelo incumbente que não começaram a funcionar

res_lv[,paste("esc_nao_fun_incumb",n_dist,sep = "_")]<-0

for (g in 1:4) {
  
  temp<-base_escolas_mun[!base_escolas_mun$ini_func_ano%in%seq(anos_pref[g]-3,anos_pref[g])&
                          base_escolas_mun$dom_ano%in%seq(anos_pref[g]-3,anos_pref[g]),]
  
  temp<-temp[!is.na(temp$latitude),]
  
  dist<-matrix(NA,nrow(res_lv[res_lv$NR_ANO==anos_pref[g],]),nrow(temp))  
  
  for (i in 1:nrow(temp)) {
    
    dist[,i]<-1*(spDistsN1(as.matrix(res_lv[res_lv$NR_ANO==anos_pref[g],c("lon","lat")]),as.numeric(temp[i,c("longitude","latitude")]),longlat = T)<k)
    
    
  }
  
  dist<-rowSums(dist)
  
  res_lv[res_lv$NR_ANO==anos_pref[g],paste("esc_nao_fun_incumb",n_dist,sep = "_")]<-dist
  
}

res_lv[,paste("I_esc_nao_fun_incumb",n_dist,sep = "_")]<-1*(res_lv[,paste("esc_nao_fun_incumb",n_dist,sep = "_")]>0)


#Variável: Equipamentos escolares de administração direta não criados pelo incumbente que começaram a funcionar em sua legislatura

res_lv[,paste("esc_ini_fun_nao_incumb",n_dist,sep = "_")]<-0

for (g in 1:4) {
  
  temp<-base_escolas_mun[base_escolas_mun$ini_func_ano%in%seq(anos_pref[g]-3,anos_pref[g])&
                          !base_escolas_mun$dom_ano%in%seq(anos_pref[g]-3,anos_pref[g]),]
 
  temp<-temp[!is.na(temp$latitude),]
  
   
  dist<-matrix(NA,nrow(res_lv[res_lv$NR_ANO==anos_pref[g],]),nrow(temp))  
  
  for (i in 1:nrow(temp)) {
    
    dist[,i]<-1*(spDistsN1(as.matrix(res_lv[res_lv$NR_ANO==anos_pref[g],c("lon","lat")]),as.numeric(temp[i,c("longitude","latitude")]),longlat = T)<k)
    
    
  }
  
  dist<-rowSums(dist)
  
  res_lv[res_lv$NR_ANO==anos_pref[g],paste("esc_ini_fun_nao_incumb",n_dist,sep = "_")]<-dist
  
}

res_lv[,paste("I_esc_ini_fun_nao_incumb",n_dist,sep = "_")]<-1*(res_lv[,paste("esc_ini_fun_nao_incumb",n_dist,sep = "_")]>0)


#Variável: Numero de Escolas que começaram a funcionar em legilaturas passadas

res_lv[,paste("esc_ini_fun_lag",n_dist,sep = "_")]<-0

for (g in 1:4) {
  
  temp<-base_escolas_mun[base_escolas_mun$ini_func_ano<(anos_pref[g]-3)&
                          base_escolas_mun$dom_ano<(anos_pref[g]-3),]
  
  temp<-temp[!is.na(temp$ini_func_ano),]
  
  dist<-matrix(NA,nrow(res_lv[res_lv$NR_ANO==anos_pref[g],]),nrow(temp))  
  
  for (i in 1:nrow(temp)) {
    
    dist[,i]<-1*(spDistsN1(as.matrix(res_lv[res_lv$NR_ANO==anos_pref[g],c("lon","lat")]),as.numeric(temp[i,c("longitude","latitude")]),longlat = T)<k)
    
    
  }
  
  dist<-rowSums(dist)
  
  res_lv[res_lv$NR_ANO==anos_pref[g],paste("esc_ini_fun_lag",n_dist,sep = "_")]<-dist
  
}


res_lv[,paste("I_esc_ini_fun_lag",n_dist,sep = "_")]<-1*(res_lv[,paste("esc_ini_fun_lag",n_dist,sep = "_")]>0)


#Variável: Escolas criadas em legislaturas anteriores que não começaram a funcionar até o mandato do incumbente

res_lv[,paste("esc_nao_fun_lag",n_dist,sep = "_")]<-0

for (g in 1:4) {
  
  temp<-base_escolas_mun[base_escolas_mun$ini_func_ano>=(anos_pref[g]-3)|is.na( base_escolas_mun$ini_func_ano),]
  temp<-temp[temp$dom_ano<(anos_pref[g]-3)|is.na( temp$dom_ano),]
  temp<-temp[!is.na(temp$latitude),]
  
  dist<-matrix(NA,nrow(res_lv[res_lv$NR_ANO==anos_pref[g],]),nrow(temp))  
  
  for (i in 1:nrow(temp)) {
    
    dist[,i]<-1*(spDistsN1(as.matrix(res_lv[res_lv$NR_ANO==anos_pref[g],c("lon","lat")]),as.numeric(temp[i,c("longitude","latitude")]),longlat = T)<k)
    
    
  }
  
  dist<-rowSums(dist)
  
  res_lv[res_lv$NR_ANO==anos_pref[g],paste("esc_nao_fun_lag",n_dist,sep = "_")]<-dist
  
}


res_lv[,paste("I_esc_nao_fun_lag",n_dist,sep = "_")]<-1*(res_lv[,paste("esc_nao_fun_lag",n_dist,sep = "_")]>0)


#Variável:Escolas criadas pelo incumbente que começaram a funcionar no ano da eleição

res_lv[,paste("esc_ini_fun_incumb_a4",n_dist,sep = "_")]<-0

for (g in 1:4) {
  
  temp<-base_escolas_mun[base_escolas_mun$ini_func_ano==anos_pref[g]&
                          base_escolas_mun$dom_ano%in%seq(anos_pref[g]-3,anos_pref[g])&
                           base_escolas_mun$dif_ano>=0,]
  temp<-temp[!is.na(temp$ini_func_ano),]

  dist<-matrix(NA,nrow(res_lv[res_lv$NR_ANO==anos_pref[g],]),nrow(temp))  
  
  for (i in 1:nrow(temp)) {
    
    dist[,i]<-1*(spDistsN1(as.matrix(res_lv[res_lv$NR_ANO==anos_pref[g],c("lon","lat")]),as.numeric(temp[i,c("longitude","latitude")]),longlat = T)<k)
    
    
  }
  
  dist<-rowSums(dist)
  
  res_lv[res_lv$NR_ANO==anos_pref[g],paste("esc_ini_fun_incumb_a4",n_dist,sep = "_")]<-dist
  
}

res_lv[,paste("I_esc_ini_fun_incumb_a4",n_dist,sep = "_")]<-1*(res_lv[,paste("esc_ini_fun_incumb_a4",n_dist,sep = "_")]>0)


#Variável:Escolas que começaram a funcionar no ano da eleição

res_lv[,paste("esc_ini_fun_a4",n_dist,sep = "_")]<-0

for (g in 1:4) {
  
  temp<-base_escolas_mun[base_escolas_mun$ini_func_ano==anos_pref[g]&
                           base_escolas_mun$dif_ano>=0,]
  temp<-temp[!is.na(temp$ini_func_ano),]
  
  dist<-matrix(NA,nrow(res_lv[res_lv$NR_ANO==anos_pref[g],]),nrow(temp))  
  
  for (i in 1:nrow(temp)) {
    
    dist[,i]<-1*(spDistsN1(as.matrix(res_lv[res_lv$NR_ANO==anos_pref[g],c("lon","lat")]),as.numeric(temp[i,c("longitude","latitude")]),longlat = T)<k)
    
    
  }
  
  dist<-rowSums(dist)
  
  res_lv[res_lv$NR_ANO==anos_pref[g],paste("esc_ini_fun_a4",n_dist,sep = "_")]<-dist
  
}

res_lv[,paste("I_esc_ini_fun_a4",n_dist,sep = "_")]<-1*(res_lv[,paste("esc_ini_fun_a4",n_dist,sep = "_")]>0)


#Variável:Escolas criadas pelo incumbente no ano da eleição

res_lv[,paste("esc_dom_incumb_a4",n_dist,sep = "_")]<-0

for (g in 1:4) {
  
  temp<-base_escolas_mun[base_escolas_mun$dom_ano==anos_pref[g]&
                           base_escolas_mun$dif_ano>=0,]
  temp<-temp[!is.na(temp$dom_ano),]
  
  dist<-matrix(NA,nrow(res_lv[res_lv$NR_ANO==anos_pref[g],]),nrow(temp))  
  
  for (i in 1:nrow(temp)) {
    
    dist[,i]<-1*(spDistsN1(as.matrix(res_lv[res_lv$NR_ANO==anos_pref[g],c("lon","lat")]),as.numeric(temp[i,c("longitude","latitude")]),longlat = T)<k)
    
    
  }
  
  dist<-rowSums(dist)
  
  res_lv[res_lv$NR_ANO==anos_pref[g],paste("esc_dom_incumb_a4",n_dist,sep = "_")]<-dist
  
}

res_lv[,paste("I_esc_dom_incumb_a4",n_dist,sep = "_")]<-1*(res_lv[,paste("esc_dom_incumb_a4",n_dist,sep = "_")]>0)


#Unir base de de votos com variáveis de criadas:

res_lv_sec_t1<-left_join(res_lv_sec_t1,res_lv)
res_lv_sec_t2<-left_join(res_lv_sec_t2,res_lv)


#Salvar bases:

saveRDS(res_lv_sec_t1,file = paste("res_lv_",n_dist,"_t1",".rds",sep = ""))
saveRDS(res_lv_sec_t2,file = paste("res_lv_",n_dist,"_t2",".rds",sep = ""))

  