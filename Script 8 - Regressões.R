##################
### Regressões ###
##################

rm(list = ls())

library(lfe)
library(stargazer)
library(dplyr)

############
##1º Turno##
############

######################
## Amostra Completa ##
######################

base<-readRDS("res_lv_1Km_t1.rds")
  
  base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
  base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000
  
 
#Tratamento 1: Abertura de escolas pelo incumbentes  
   
reg1<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
             
             #+ QTD_VOTOS_P_lag
             
             #+ esc_ini_fun_lag_1Km
             
             #+ esc_nao_fun_lag_1Km
             
             + QTD_VOTOS_LV
             
             + factor(NR_ANO) |COD2| 0 |COD2, data = base)  
  
reg2<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
             
             + QTD_VOTOS_P_lag
             
             #+ esc_ini_fun_lag_1Km
             
             #+ esc_nao_fun_lag_1Km
             
             + QTD_VOTOS_LV
             
             + factor(NR_ANO) |COD2| 0 |COD2, data = base) 
  
reg3<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
             
             + QTD_VOTOS_P_lag
             
             + esc_ini_fun_lag_1Km
             
             + esc_nao_fun_lag_1Km
             
             + QTD_VOTOS_LV
             
             + factor(NR_ANO) |COD2 | 0 |COD2, data = base)  

stargazer(reg1,reg2,reg3,
          type = "html",out="reg_1_1Km.html",
          title = "Regressões  (1º Turno) - Base Irrestrita com Efeito Fixo de LV",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

#Tratamento 2: Abertura de Escolas criadas apenas pelo incumbente 

reg4<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

reg5<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base) 

reg6<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

stargazer(reg4,reg5,reg6,
          type = "html",out="reg_2_1Km.html",
          title = "Regressões  (1º Turno) - Base Irrestrita com Efeito Fixo de LV",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura e Criação de Esc. (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

#Tratamento 3: Abertura de Escolaspelo incumbente no ano da eleição

reg7<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

reg8<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base) 

reg9<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

stargazer(reg7,reg8,reg9,
          type = "html",out="reg_3_1Km.html",
          title = "Regressões  (1º Turno) - Base Irrestrita com Efeito Fixo de LV",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. - Eleição (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))


#######################################
## Amostra Completa sem Efeitos Fixos##
#######################################

base<-readRDS("res_lv_1Km_t1.rds")

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000


#Tratamento 1: Abertura de escolas pelo incumbentes  

reg1<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

reg2<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base) 

reg3<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

stargazer(reg1,reg2,reg3,
          type = "html",out="reg_1#_1Km.html",
          title = "Regressões  (1º Turno) - Base Irrestrita - MQO Agrupado",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

#Tratamento 2: Abertura de Escolas criadas apenas pelo incumbente 

reg4<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

reg5<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base) 

reg6<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

stargazer(reg4,reg5,reg6,
          type = "html",out="reg_2#_1Km.html",
          title = "Regressões  (1º Turno) - Base Irrestrita - MQO Agrupado",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura e Criação de Esc. (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

#Tratamento 3: Abertura de Escolaspelo incumbente no ano da eleição

reg7<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

reg8<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base) 

reg9<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

stargazer(reg7,reg8,reg9,
          type = "html",out="reg_3#_1Km.html",
          title = "Regressões  (1º Turno) - Base Irrestrita - MQO Agrupado",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. - Eleição (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))


############################################################################################################################
## Amostra restrita 1 (seções em locais de votação em que se abriu ou se criou alguma escola durante o período analisado) ##
############################################################################################################################

#Tratamento 1: Abertura de escolas pelo incumbentes  

base<-readRDS("restr_1_1_1Km.RDS")

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000


reg1<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

reg2<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base) 

reg3<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

stargazer(reg1,reg2,reg3,
          type = "html",out="reg_1_r1_1Km.html",
          title = "Regressões  (1º Turno) - Base Restrita 1  - MQO Agrupado",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

#Tratamento 2: Abertura de Escolas criadas apenas pelo incumbente 

base<-readRDS("restr_1_2_1Km.RDS")

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000


reg4<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

reg5<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base) 

reg6<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

stargazer(reg4,reg5,reg6,
          type = "html",out="reg_2_r1_1Km.html",
          title = "Regressões  (1º Turno) - Base Restrita 1  - MQO Agrupado",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura e Criação de Esc. (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

#Tratamento 3: Abertura de Escolas pelo incumbente no ano da eleição

base<-readRDS("restr_1_3_1Km.RDS")

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000


reg7<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

reg8<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base) 

reg9<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

stargazer(reg7,reg8,reg9,
          type = "html",out="reg_3_r1_1Km.html",
          title = "Regressões  (1º Turno) - Base Restrita 1  - MQO Agrupado",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. - Eleição (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

####################################################################################################################################################
## Amostra restrita 2 (pares de locais de votação tradados/controles próximos e menor diferença de desempenho do incumbente na eleição anterior) ###
####################################################################################################################################################


#Tratamento 1: Abertura de escolas pelo incumbentes  

base<-readRDS("restr_2_1Km.RDS")

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")

base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000


reg1<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

reg2<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base) 

reg3<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

stargazer(reg1,reg2,reg3,
          type = "html",out="reg_1_r2_1Km.html",
          title = "Regressões  (1º Turno) - Base Restrita 2 - MQO Agrupado",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))



#Tratamento 2: Abertura de Escolas criadas apenas pelo incumbente 

base<-readRDS("restr_3_1Km.RDS")

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000


reg4<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV 
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

reg5<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV 
           
           + factor(NR_ANO) |0| 0 |COD2, data = base) 

reg6<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

stargazer(reg4,reg5,reg6,
          type = "html",out="reg_2_r2_1Km.html",
          title = "Regressões  (1º Turno) - Base Restrita 2 - MQO Agrupado",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura e Criação de Esc. (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))


#Tratamento 3: Abertura de Escolas pelo incumbente no ano da eleição

base<-readRDS("restr_4_1Km.rds")

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000


reg7<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

reg8<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base) 

reg9<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base)  

stargazer(reg7,reg8,reg9,
          type = "html",out="reg_3_r2_1Km.html",
          title = "Regressões  (1º Turno) - Base Restrita 2 - MQO Agrupado",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. - Eleição (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

###################################################################################
## Amostra restrita 3 (pares de seções pareadas com métodos de propensity score) ##
###################################################################################

res_lv_sec<-readRDS("res_lv_1Km_t1.rds")


#Tratamento 1: Abertura de escolas pelo incumbentes (com controles censitários)

base<-readRDS("lv_match_data_1_1Km.rds")
base<-left_join(base,res_lv_sec)

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000


reg1<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) 
           
           + rend
           
           + i10_19
           
           + i0_9
           
           |0| 0 |COD2, data = base, weights = base$weights)  

reg2<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) 
           
           + rend
           
           + i10_19
           
           + i0_9
           
           |0| 0 |COD2, data = base, weights = base$weights) 

reg3<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) 
           
           + rend
           
           + i10_19
           
           + i0_9
           
           |0| 0 |COD2, data = base, weights = base$weights)  

stargazer(reg1,reg2,reg3,
          type = "html",out="reg_1_r3$_1Km.html",
          title = "Regressões  (1º Turno) - Base Restrita 3 - MQO Agrupado",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))


#Tratamento 1: Abertura de escolas pelo incumbentes  

base<-readRDS("lv_match_data_1_1Km.rds")
base<-left_join(base,res_lv_sec)

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000

reg1<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) 
           |0| 0 |COD2, data = base, weights = base$weights)  

reg2<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) 
           |0| 0 |COD2, data = base, weights = base$weights) 

reg3<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
          
           + factor(NR_ANO) 
           |0| 0 |COD2, data = base, weights = base$weights)  

stargazer(reg1,reg2,reg3,
          type = "html",out="reg_1_r3_1Km.html",
          title = "Regressões  (1º Turno) - Base Restrita 3 - MQO Agrupado",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))



#Tratamento 2: Abertura de Escolas criadas apenas pelo incumbente

base<-readRDS("lv_match_data_2_1Km.rds")
base<-left_join(base,res_lv_sec)

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000



reg4<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base, weights = base$weights)  

reg5<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base, weights = base$weights) 

reg6<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base, weights = base$weights)  

stargazer(reg4,reg5,reg6,
          type = "html",out="reg_2_r3_1Km.html",
          title = "Regressões  (1º Turno) - Base Restrita 3 - MQO Agrupado",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura e Criação de Esc. (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))


#Tratamento 2: (com controles censitários)

base<-readRDS("lv_match_data_2_1Km.rds")
base<-left_join(base,res_lv_sec)

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000


reg1<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) 
           
           + rend
           
           + i10_19
           
           + i0_9
           
           |0| 0 |COD2, data = base, weights = base$weights)  

reg2<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) 
           
           + rend
           
           + i10_19
           
           + i0_9
           
           |0| 0 |COD2, data = base, weights = base$weights) 

reg3<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) 
           
           + rend
           
           + i10_19
           
           + i0_9
           
           |0| 0 |COD2, data = base, weights = base$weights)  

stargazer(reg1,reg2,reg3,
          type = "html",out="reg_2_r3$_1Km.html",
          title = "Regressões  (1º Turno) - Base Restrita 3 - MQO Agrupado",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura e Criação de Esc. (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))



#Tratamento 3: Abertura de Escolas pelo incumbente no ano da eleição

base<-readRDS("lv_match_data_3_1Km.rds")
base<-left_join(base,res_lv_sec)

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000


reg7<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base, weights = base$weights)  

reg8<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0|0 |COD2, data = base, weights = base$weights) 

reg9<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |0| 0 |COD2, data = base, weights = base$weights)  

stargazer(reg7,reg8,reg9,
          type = "html",out="reg_3_r3_1Km.html",
          title = "Regressões  (1º Turno) - Base Restrita 3 - MQO Agrupado",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. - Eleição (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

#Tratamento 3: (com controles censitários)

base<-readRDS("lv_match_data_3_1Km.rds")
base<-left_join(base,res_lv_sec)

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000


reg1<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) 
           
           + rend
           
           + i10_19
           
           + i0_9
           
           |0| 0 |COD2, data = base, weights = base$weights)  

reg2<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) 
           
           + rend
           
           + i10_19
           
           + i0_9
           
           |0| 0 |COD2, data = base, weights = base$weights) 

reg3<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) 
           
           + rend
           
           + i10_19
           
           + i0_9
           
           |0| 0 |COD2, data = base, weights = base$weights)  

stargazer(reg1,reg2,reg3,
          type = "html",out="reg_3_r3$_1Km.html",
          title = "Regressões  (1º Turno) - Base Restrita 3 - MQO Agrupado",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. - Eleição (1Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

#################################################################
## Variações no raio considerado  ao redor do Local de Votação ##
#################################################################

###########
# 1.25 Km #
###########

base<-readRDS("res_lv_1.25Km_t1.rds")

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000

#Tratamento 1: Abertura de escolas pelo incumbentes  

reg1<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1.25Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1.25Km
           
           #+ esc_nao_fun_lag_1.25Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

reg2<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1.25Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1.25Km
           
           #+ esc_nao_fun_lag_1.25Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base) 

reg3<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1.25Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1.25Km
           
           + esc_nao_fun_lag_1.25Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

stargazer(reg1,reg2,reg3,
          type = "html",out="reg_1_1.25Km.html",
          title = "Regressões  (1º Turno) - Base Irrestrita com Efeito Fixo de Seção e LV",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. (1.25Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

#Tratamento 2: Abertura de Escolas criadas apenas pelo incumbente 

reg4<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1.25Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1.25Km
           
           #+ esc_nao_fun_lag_1.25Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

reg5<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1.25Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1.25Km
           
           #+ esc_nao_fun_lag_1.25Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base) 

reg6<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1.25Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1.25Km
           
           + esc_nao_fun_lag_1.25Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

stargazer(reg4,reg5,reg6,
          type = "html",out="reg_2_1.25Km.html",
          title = "Regressões  (1º Turno) - Base Irrestrita com Efeito Fixo de Seção e LV",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura e Criação de Esc. (1.25Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

#Tratamento 3: Abertura de Escolaspelo incumbente no ano da eleição

reg7<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1.25Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1.25Km
           
           #+ esc_nao_fun_lag_1.25Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

reg8<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1.25Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1.25Km
           
           #+ esc_nao_fun_lag_1.25Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base) 

reg9<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1.25Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1.25Km
           
           + esc_nao_fun_lag_1.25Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

stargazer(reg7,reg8,reg9,
          type = "html",out="reg_3_1.25Km.html",
          title = "Regressões  (1º Turno) - Base Irrestrita com Efeito Fixo de Seção e LV",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. - Eleição (1.25Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

#########
# 1.5Km #
#########

base<-readRDS("res_lv_1.5Km_t1.rds")

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000

#Tratamento 1: Abertura de escolas pelo incumbentes  

reg1<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1.5Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1.5Km
           
           #+ esc_nao_fun_lag_1.5Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

reg2<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1.5Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1.5Km
           
           #+ esc_nao_fun_lag_1.5Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base) 

reg3<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_1.5Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1.5Km
           
           + esc_nao_fun_lag_1.5Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

stargazer(reg1,reg2,reg3,
          type = "html",out="reg_1_1.5Km.html",
          title = "Regressões  (1º Turno) - Base Irrestrita com Efeito Fixo de Seção e LV",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. (1.5Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

#Tratamento 2: Abertura de Escolas criadas apenas pelo incumbente 

reg4<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1.5Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1.5Km
           
           #+ esc_nao_fun_lag_1.5Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

reg5<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1.5Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1.5Km
           
           #+ esc_nao_fun_lag_1.5Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base) 

reg6<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_1.5Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1.5Km
           
           + esc_nao_fun_lag_1.5Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

stargazer(reg4,reg5,reg6,
          type = "html",out="reg_2_1.5Km.html",
          title = "Regressões  (1º Turno) - Base Irrestrita com Efeito Fixo de Seção e LV",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura e Criação de Esc. (1.5Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

#Tratamento 3: Abertura de Escolaspelo incumbente no ano da eleição

reg7<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1.5Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1.5Km
           
           #+ esc_nao_fun_lag_1.5Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

reg8<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1.5Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1.5Km
           
           #+ esc_nao_fun_lag_1.5Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base) 

reg9<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_1.5Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1.5Km
           
           + esc_nao_fun_lag_1.5Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

stargazer(reg7,reg8,reg9,
          type = "html",out="reg_3_1.5Km.html",
          title = "Regressões  (1º Turno) - Base Irrestrita com Efeito Fixo de Seção e LV",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. - Eleição (1.5Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))


###########
# 0.75 Km #
###########

base<-readRDS("res_lv_0.75Km_t1.rds")

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000

#Tratamento 1: Abertura de escolas pelo incumbentes  

reg1<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_0.75Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_0.75Km
           
           #+ esc_nao_fun_lag_0.75Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

reg2<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_0.75Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_0.75Km
           
           #+ esc_nao_fun_lag_0.75Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base) 

reg3<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_0.75Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_0.75Km
           
           + esc_nao_fun_lag_0.75Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

stargazer(reg1,reg2,reg3,
          type = "html",out="reg_1_0.75Km.html",
          title = "Regressões  (1º Turno) - Base Irrestrita com Efeito Fixo de Seção e LV",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. (0.75Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

#Tratamento 2: Abertura de Escolas criadas apenas pelo incumbente 

reg4<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_0.75Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_0.75Km
           
           #+ esc_nao_fun_lag_0.75Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

reg5<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_0.75Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_0.75Km
           
           #+ esc_nao_fun_lag_0.75Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base) 

reg6<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_incumb_0.75Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_0.75Km
           
           + esc_nao_fun_lag_0.75Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

stargazer(reg4,reg5,reg6,
          type = "html",out="reg_2_0.75Km.html",
          title = "Regressões  (1º Turno) - Base Irrestrita com Efeito Fixo de Seção e LV",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura e Criação de Esc. (0.75Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

#Tratamento 3: Abertura de Escolaspelo incumbente no ano da eleição

reg7<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_0.75Km
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_0.75Km
           
           #+ esc_nao_fun_lag_0.75Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

reg8<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_0.75Km
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_0.75Km
           
           #+ esc_nao_fun_lag_0.75Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base) 

reg9<-felm(QTD_VOTOS_P ~ I_esc_ini_fun_a4_0.75Km
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_0.75Km
           
           + esc_nao_fun_lag_0.75Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

stargazer(reg7,reg8,reg9,
          type = "html",out="reg_3_0.75Km.html",
          title = "Regressões  (1º Turno) - Base Irrestrita com Efeito Fixo de Seção e LV",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Abertura de Esc. - Eleição (0.75Km)",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))



######################
# Tratamento Placebo #
######################

base<-readRDS("res_lv_1Km_t1.rds")

base$COD2<-paste(base$NR_ZONA,base$NR_LOCVOT,sep = "_")
base$QTD_VOTOS_LV<-base$QTD_VOTOS_LV/1000

  sum(base$I_esc_ini_fun_1Km) #14879
  
  teste<-sample(1:nrow(base),14879,replace = T)
  
  teste<-base[teste,]
    teste$I_placebo<-1
  
  temp<-anti_join(base,teste)
    temp$I_placebo<-0
  
  base<-bind_rows(teste,temp)



reg1<-felm(QTD_VOTOS_P ~ I_placebo
           
           #+ QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2| 0 |COD2, data = base)  

reg2<-felm(QTD_VOTOS_P ~ I_placebo
           
           + QTD_VOTOS_P_lag
           
           #+ esc_ini_fun_lag_1Km
           
           #+ esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2 | 0 |COD2, data = base) 

reg3<-felm(QTD_VOTOS_P ~ I_placebo
           
           + QTD_VOTOS_P_lag
           
           + esc_ini_fun_lag_1Km
           
           + esc_nao_fun_lag_1Km
           
           + QTD_VOTOS_LV
           
           + factor(NR_ANO) |COD2 | 0 |COD2, data = base)  

stargazer(reg1,reg2,reg3,
          type = "html",out="reg_pla_1Km.html",
          title = "Regressões  (1º Turno) - Tratamento placebo com Efeito Fixo de Seção e LV",
          dep.var.labels = c("% votos no candidato incumbente"),
          covariate.labels = c("D: Placebo",
                               "% votos no incumbente (t-1)",
                               "Estoque de Esc. Abertas",
                               "Estoque de Esc. Criadas (ñ abertas)",
                               "N.º de Eleitores no LV",
                               "D: 2008",
                               "D: 2012",
                               "D: 2016"),notes = c("Erros-padrões robustos e clusterizados no nível de LV"))

