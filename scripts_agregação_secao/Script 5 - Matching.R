################
### Matching ###
################

library(MatchIt)
library(dplyr)

rm(list = ls())

#Definir parâmetro de distância e turno:
k<-1
n_dist<-"1Km"
n_censo<-"1000m"
t<-1

#Abrir base:

res_lv_sec<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))

#Dados do Censo 2000 (utilizados paras as eleições de 2004 e 2008):

censo2000<-readRDS(paste("res_lv_2000_",n_censo,".rds",sep = ""))

  censo2000$rend<-censo2000$Var02/censo2000$Var01 #Renda domiciliar média
  censo2000$esc<-censo2000$Var09/censo2000$Var08 #Escolaridade média dos responsáveis
  censo2000$i0_9<-(censo2000$V1448+censo2000$V1449)/censo2000$V1330 #Percentual de indivíduos de 9 anos ou menos 
  censo2000$i10_19<-(censo2000$V1450+censo2000$V1451)/censo2000$V1330 #Percentual de indivíduos de 10 a 19 anos


#Dados do Censo 2010 (utilizados paras as eleições de 2012 e 2016):

censo2010<-readRDS(paste("res_lv_2010_",n_censo,".rds",sep = ""))

  censo2010$rend<-censo2010$bas_V005_/censo2010$bas_V001 #Renda domiciliar média
  censo2010$esc<-censo2010$res2_V093/censo2010$res2_V001 #Percentual de responsáveis alfabetizados
  censo2010$i0_9<-(censo2010$p13_V022+censo2010$p13_V036+censo2010$p13_V037+censo2010$p13_V038+
                     censo2010$p13_V039+censo2010$p13_V040+censo2010$p13_V041+censo2010$p13_V042+
                     censo2010$p13_V043)/(censo2010$p13_V001+censo2010$p13_V002) #Percentual de indivíduos de 9 anos ou menos
  censo2010$i10_19<-(censo2010$p13_V044+censo2010$p13_V045+censo2010$p13_V046+censo2010$p13_V047+
                       censo2010$p13_V048+censo2010$p13_V049+censo2010$p13_V050+censo2010$p13_V051+
                       censo2010$p13_V052+censo2010$p13_V053)/(censo2010$p13_V001+censo2010$p13_V002) #Percentual de indivíduos de 10 a 19 anos

  
#Matching com tratamento de abertura de escolas pelo incumbente em cada Legislatura:
  
  #Seleção de variáveis relevantes:
  
  var<-c("NR_ANO","NR_SECAO","NR_ZONA","NR_LOCVOT","QTD_VOTOS_P","QTD_VOTOS_P_lag","rend","esc","i0_9","i10_19",paste(c("I_esc_ini_fun_","esc_ini_fun_lag_","esc_nao_fun_lag_"),n_dist,sep = ""))
  
  temp1<-left_join(res_lv_sec[res_lv_sec$NR_ANO%in%c(2004,2008),],censo2000)[,var]
  temp2<-left_join(res_lv_sec[res_lv_sec$NR_ANO%in%c(2012,2016),],censo2010)[,var]
  
  
  #Matching usando propesity score para vizinhos mais próximos (distância logit) com reposição e sem calibre:

  #2004 e 2008 (matching exato para zona eleitoral das seções):
  
  lv_match04_1<-matchit(I_esc_ini_fun_1Km ~ QTD_VOTOS_P_lag + esc_ini_fun_lag_1Km + esc_nao_fun_lag_1Km + rend + esc + i0_9 + i10_19,
                  exact=c("NR_ZONA"),
                  data = temp1[!is.na(temp1$QTD_VOTOS_P_lag)&temp1$NR_ANO==2004,],
                  method = "nearest", distance = "logit",discard = "control", m.order="largest", replace=T)

  lv_match08_1<-matchit(I_esc_ini_fun_1Km ~ QTD_VOTOS_P_lag + esc_ini_fun_lag_1Km + esc_nao_fun_lag_1Km + rend + esc + i0_9 + i10_19,
                      exact=c("NR_ZONA"),
                      data = temp1[!is.na(temp1$QTD_VOTOS_P_lag)&temp1$NR_ANO==2008,],
                      method = "nearest", distance = "logit",discard = "control", m.order="largest", replace=T)
  
  #2012 e 2016 (matching exato para o ano da eleição e a zona eleitoral das seções):
  
  lv_match12_1<-matchit(I_esc_ini_fun_1Km ~ QTD_VOTOS_P_lag + esc_ini_fun_lag_1Km + esc_nao_fun_lag_1Km + rend + esc + i0_9 + i10_19,
                   exact=c("NR_ZONA"),
                   data = temp2[!is.na(temp2$QTD_VOTOS_P_lag)&temp2$NR_ANO==2012,],
                   method = "nearest", distance = "logit",discard = "control", m.order="largest", replace=T)
  
  lv_match16_1<-matchit(I_esc_ini_fun_1Km ~ QTD_VOTOS_P_lag + esc_ini_fun_lag_1Km + esc_nao_fun_lag_1Km + rend + esc + i0_9 + i10_19,
                      exact=c("NR_ZONA"),
                      data = temp2[!is.na(temp2$QTD_VOTOS_P_lag)&temp2$NR_ANO==2016,],
                      method = "nearest", distance = "logit",discard = "control", m.order="largest", replace=T)

  #Consolidar base pareada e salvar:

  b04<-match.data(lv_match04_1)
    b04$NR_ANO<-2004
  b08<-match.data(lv_match08_1)
    b08$NR_ANO<-2008
  b12<-match.data(lv_match12_1)
    b12$NR_ANO<-2012
  b16<-match.data(lv_match16_1)
    b16$NR_ANO<-2016
  
  
  lv_match_data_1<-bind_rows(b04,b08,b12,b16)

  lv_match_data_1$COD<-paste(lv_match_data_1$NR_SECAO,lv_match_data_1$NR_ZONA,sep = "_")
  lv_match_data_1$COD2<-paste(lv_match_data_1$NR_LOCVOT,lv_match_data_1$NR_ZONA,sep = "_")

  saveRDS(lv_match_data_1,paste("lv_match_data_1_",n_dist,".rds",sep = ""))

  
#Matching com tratamento de abertura de escolas criadas pelo incumbente em cada legislatura:
  
  #Seleção de variáveis relevantes:
  
  var<-c("NR_ANO","NR_SECAO","NR_ZONA","NR_LOCVOT","QTD_VOTOS_P","QTD_VOTOS_P_lag","rend","esc","i0_9","i10_19",paste(c("I_esc_ini_fun_incumb_","esc_ini_fun_lag_","esc_nao_fun_lag_"),n_dist,sep = ""))
  
  temp1<-left_join(res_lv_sec[res_lv_sec$NR_ANO%in%c(2004,2008),],censo2000)[,var]
  temp2<-left_join(res_lv_sec[res_lv_sec$NR_ANO%in%c(2012,2016),],censo2010)[,var]
  
  
  #Matching usando propesity score para vizinhos mais próximos (distância logit) com reposição e sem calibre:
  
  #2004 e 2008 (matching exato para zona eleitoral das seções):
  
  lv_match04_2<-matchit(I_esc_ini_fun_incumb_1Km ~ QTD_VOTOS_P_lag + esc_ini_fun_lag_1Km + esc_nao_fun_lag_1Km + rend + esc + i0_9 + i10_19,
                      exact=c("NR_ZONA"),
                      data = temp1[!is.na(temp1$QTD_VOTOS_P_lag)&temp1$NR_ANO==2004,],
                      method = "nearest", distance = "logit",discard = "control", m.order="largest", replace=T)
  
  lv_match08_2<-matchit(I_esc_ini_fun_incumb_1Km ~ QTD_VOTOS_P_lag + esc_ini_fun_lag_1Km + esc_nao_fun_lag_1Km + rend + esc + i0_9 + i10_19,
                      exact=c("NR_ZONA"),
                      data = temp1[!is.na(temp1$QTD_VOTOS_P_lag)&temp1$NR_ANO==2008,],
                      method = "nearest", distance = "logit",discard = "control", m.order="largest", replace=T)
  
  #2012 e 2016 (matching exato para o ano da eleição e a zona eleitoral das seções):
  
  lv_match12_2<-matchit(I_esc_ini_fun_incumb_1Km ~ QTD_VOTOS_P_lag + esc_ini_fun_lag_1Km + esc_nao_fun_lag_1Km + rend + esc + i0_9 + i10_19,
                      exact=c("NR_ZONA"),
                      data = temp2[!is.na(temp2$QTD_VOTOS_P_lag)&temp2$NR_ANO==2012,],
                      method = "nearest", distance = "logit",discard = "control", m.order="largest", replace=T)
  
  lv_match16_2<-matchit(I_esc_ini_fun_incumb_1Km ~ QTD_VOTOS_P_lag + esc_ini_fun_lag_1Km + esc_nao_fun_lag_1Km + rend + esc + i0_9 + i10_19,
                      exact=c("NR_ZONA"),
                      data = temp2[!is.na(temp2$QTD_VOTOS_P_lag)&temp2$NR_ANO==2016,],
                      method = "nearest", distance = "logit",discard = "control", m.order="largest", replace=T)
  
  #Consolidar base pareada e salvar:
  
  b04<-match.data(lv_match04_2)
  b04$NR_ANO<-2004
  b08<-match.data(lv_match08_2)
  b08$NR_ANO<-2008
  b12<-match.data(lv_match12_2)
  b12$NR_ANO<-2012
  b16<-match.data(lv_match16_2)
  b16$NR_ANO<-2016
  
  
  lv_match_data_2<-bind_rows(b04,b08,b12,b16)
  
  lv_match_data_2$COD<-paste(lv_match_data_2$NR_SECAO,lv_match_data_2$NR_ZONA,sep = "_")
  lv_match_data_2$COD2<-paste(lv_match_data_2$NR_LOCVOT,lv_match_data_2$NR_ZONA,sep = "_")
  
  saveRDS(lv_match_data_2,paste("lv_match_data_2_",n_dist,".rds",sep = ""))
  

#Matching com tratamento de abertura de escolas no ano da eleição em cada legislatura:
  
  #Seleção de variáveis relevantes:
  
  var<-c("NR_ANO","NR_SECAO","NR_ZONA","NR_LOCVOT","QTD_VOTOS_P","QTD_VOTOS_P_lag","rend","esc","i0_9","i10_19",paste(c("I_esc_ini_fun_a4_","esc_ini_fun_lag_","esc_nao_fun_lag_"),n_dist,sep = ""))
  
  temp1<-left_join(res_lv_sec[res_lv_sec$NR_ANO%in%c(2004,2008),],censo2000)[,var]
  temp2<-left_join(res_lv_sec[res_lv_sec$NR_ANO%in%c(2012,2016),],censo2010)[,var]
  
  
  #Matching usando propesity score para vizinhos mais próximos (distância logit) com reposição e sem calibre:
  
  #2004 e 2008 (matching exato para zona eleitoral das seções):
  
  lv_match04_3<-matchit(I_esc_ini_fun_a4_1Km ~ QTD_VOTOS_P_lag + esc_ini_fun_lag_1Km + esc_nao_fun_lag_1Km + rend + esc + i0_9 + i10_19,
                        exact=c("NR_ZONA"),
                        data = temp1[!is.na(temp1$QTD_VOTOS_P_lag)&temp1$NR_ANO==2004,],
                        method = "nearest", distance = "logit",discard = "control", m.order="largest", replace=T)
  
  lv_match08_3<-matchit(I_esc_ini_fun_a4_1Km ~ QTD_VOTOS_P_lag + esc_ini_fun_lag_1Km + esc_nao_fun_lag_1Km + rend + esc + i0_9 + i10_19,
                        exact=c("NR_ZONA"),
                        data = temp1[!is.na(temp1$QTD_VOTOS_P_lag)&temp1$NR_ANO==2008,],
                        method = "nearest", distance = "logit",discard = "control", m.order="largest", replace=T)
  
  #2012 e 2016 (matching exato para o ano da eleição e a zona eleitoral das seções):
  
  lv_match12_3<-matchit(I_esc_ini_fun_a4_1Km ~ QTD_VOTOS_P_lag + esc_ini_fun_lag_1Km + esc_nao_fun_lag_1Km + rend + esc + i0_9 + i10_19,
                        exact=c("NR_ZONA"),
                        data = temp2[!is.na(temp2$QTD_VOTOS_P_lag)&temp2$NR_ANO==2012,],
                        method = "nearest", distance = "logit",discard = "control", m.order="largest", replace=T)
  
  lv_match16_3<-matchit(I_esc_ini_fun_a4_1Km ~ QTD_VOTOS_P_lag + esc_ini_fun_lag_1Km + esc_nao_fun_lag_1Km + rend + esc + i0_9 + i10_19,
                        exact=c("NR_ZONA"),
                        data = temp2[!is.na(temp2$QTD_VOTOS_P_lag)&temp2$NR_ANO==2016,],
                        method = "nearest", distance = "logit",discard = "control", m.order="largest", replace=T)
  
  #Consolidar base pareada e salvar:
  
  b04<-match.data(lv_match04_3)
  b04$NR_ANO<-2004
  b08<-match.data(lv_match08_3)
  b08$NR_ANO<-2008
  b12<-match.data(lv_match12_3)
  b12$NR_ANO<-2012
  b16<-match.data(lv_match16_3)
  b16$NR_ANO<-2016
  
  
  lv_match_data_3<-bind_rows(b04,b08,b12,b16)
  
  lv_match_data_3$COD<-paste(lv_match_data_3$NR_SECAO,lv_match_data_3$NR_ZONA,sep = "_")
  lv_match_data_3$COD2<-paste(lv_match_data_3$NR_LOCVOT,lv_match_data_3$NR_ZONA,sep = "_")
  
  saveRDS(lv_match_data_3,paste("lv_match_data_3_",n_dist,".rds",sep = ""))  
  
  
#Salvar ambiente:
  
  rm(list = ls(pattern = "censo"))
  rm(list = ls(pattern = "temp"))
  
  
  save.image(paste("lv_match_data_",n_dist,".RData",sep = ""))
  


  