##################################
#### Estatísticas Descritivas ####
##################################

library(dplyr)
library(Rmisc)

rm(list = ls())

  #Definir parâmetro de distância e turno:
  
  k<-1
  n_dist<-"1Km"
  n_censo<-"1000m"
  t<-1
  
  #Abrir Bases:
  
  res_lv_sec<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))
    res_lv_sec$COD<-paste(res_lv_sec$NR_ZONA,res_lv_sec$NR_SECAO,sep = "_")
    res_lv_sec$COD2<-paste(res_lv_sec$NR_ZONA,res_lv_sec$NR_LOCVOT,sep = "_")
  
  base_escolas_mun <- readRDS("~/Projetos_R/MONO_proj/base_escolas_mun.rds")
  
  res_lv<-res_lv_sec[,1:10]
  res_lv$NR_TURNO<-NULL
  res_lv$NR_SECAO<-NULL
  res_lv<-unique(res_lv)

############################### 
##1. Base Escolas Municipais ##
###############################  
  
    #Tipos de escolas: 
    unique(base_escolas_mun$tipoesc)
    # [1] "EMEF"      "EMEI"      "CEU EMEF"  "CEU EMEI"  "CEMEI"     "CIEJA"     "CEI DIRET" "CEU CEI"   "EMEBS"     "CCI/CIPS" 
    # [11] "EMEFM" 
    
    #Total:1516
    nrow(base_escolas_mun)
    
    #1. Total aberto por ano desde 2001:
    
    temp1<-data.frame(table(base_escolas_mun[base_escolas_mun$dom_ano%in%2001:2016,]$dom_ano))
    colnames(temp1)<-c("Ano","esc_dom")
    
    temp2<-data.frame(table(base_escolas_mun[base_escolas_mun$ini_func_ano%in%2001:2016,]$ini_func_ano))
    colnames(temp2)<-c("Ano","esc_in_fin")
    
    ed_1_1<-left_join(temp1,temp2)
    
    write.table(ed_1_1,file = "ed_1_1.csv",sep = ";",row.names = F)
    
    #2. Relação entre criação e abertura pelos incumbentes:
    
    base_escolas_mun$leg_dom<-0
    base_escolas_mun[base_escolas_mun$dom_ano%in%2001:2004,]$leg_dom<-2004
    base_escolas_mun[base_escolas_mun$dom_ano%in%2005:2008,]$leg_dom<-2008
    base_escolas_mun[base_escolas_mun$dom_ano%in%2009:2012,]$leg_dom<-2012
    base_escolas_mun[base_escolas_mun$dom_ano%in%2013:2016,]$leg_dom<-2016
    base_escolas_mun$leg_ini<-0
    base_escolas_mun[base_escolas_mun$ini_func_ano%in%2001:2004,]$leg_ini<-2004
    base_escolas_mun[base_escolas_mun$ini_func_ano%in%2005:2008,]$leg_ini<-2008
    base_escolas_mun[base_escolas_mun$ini_func_ano%in%2009:2012,]$leg_ini<-2012
    base_escolas_mun[base_escolas_mun$ini_func_ano%in%2013:2016,]$leg_ini<-2016  
    
    temp<-base_escolas_mun[!base_escolas_mun$leg_ini==0,]  
    temp$I<-1*temp$leg_dom==temp$leg_ini
    
    ed_1_2<-aggregate(temp$I,by=list(temp$leg_ini),FUN=mean) #Percentual due escolas abertas em cada legislatura que foram criadas pelo incumbente
    
    write.table(ed_1_2,file = "ed_1_1.csv",sep = ";",row.names = F)
    
    #3. Tempo médio entre criação e abertura para escolas criadas a partir de 2001:
    
    temp1<-aggregate(base_escolas_mun[base_escolas_mun$dom_ano>2000&base_escolas_mun$dif_ano>0,]$dif_ano,
                     by=list(base_escolas_mun[base_escolas_mun$dom_ano>2000&base_escolas_mun$dif_ano>0,]$leg_dom),FUN=function(x){mean(x,na.rm = T)})
    
    ed_1_3<-temp1[!temp1$Group.1==0,]
    
    write.table(ed_1_3,file = "ed_1_1.csv",sep = ";",row.names = F)
    
######################## 
##2. Dados Eleitorais ##
########################
  
    #1.Votação do incumbente, número de seções e locais de votação:
    
    temp1<-aggregate(res_lv_sec[,c("QTD_VOTOS_T","QTD_VOTOS")],by=list(res_lv_sec$NR_ANO),FUN=sum)
    temp1$incumb_p<-temp1$QTD_VOTOS/temp1$QTD_VOTOS_T
    
    temp2<-aggregate(rep(1,nrow(res_lv_sec)),by=list(res_lv_sec$NR_ANO),FUN=sum)
    colnames(temp2)[2]<-"n_secao" 
    
    temp3<-aggregate(rep(1,nrow(res_lv)),by=list(res_lv$NR_ANO),FUN=sum)
    colnames(temp3)[2]<-"n_locvot" 
    
    temp1<-left_join(temp1,temp2)
    ed_2_1<-left_join(temp1,temp3)
    
    write.table(ed_2_1,file = "ed_2_1.csv",sep = ";",row.names = F)
    
    #2.Seções repetidas:
    
    temp4<-aggregate(rep(1,nrow(res_lv_sec)),by=list(res_lv_sec$COD),FUN=sum)
    ed_2_2<-data.frame(table(temp4$x),as.numeric(table(temp4$x))/nrow(temp4))
    
    write.table(ed_2_2,file = "ed_2_2.csv",sep = ";",row.names = F)
    
#############################    
##3.Variáveis de interesse ##
#############################    
  
    cor.test(base$I_esc_ini_fun_incumb_1Km ,base$I_esc_ini_fun_1Km)
    cor.test(base$I_esc_ini_fun_a4_1Km ,base$I_esc_ini_fun_1Km)
    cor.test(base$I_esc_ini_fun_incumb_1Km ,base$I_esc_ini_fun_a4_1Km)
    
    var1<-paste("I_esc_ini_fun",n_dist,sep = "_")
    var2<-paste("I_esc_ini_fun_incumb",n_dist,sep = "_")
    
    ##################
    #Amostra Completa#
    ##################
    
    res_lv_sec<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))
    
      #1 e 2.Diferenças de médias simples:
      
      temp1<-summarySE(res_lv_sec,"QTD_VOTOS_P",c(var1),conf.interval = 0.95)
      
      temp2<-summarySE(res_lv_sec,"QTD_VOTOS_P_lag",c(var1),conf.interval = 0.95,na.rm = T)
      
      temp3<-summarySE(res_lv_sec,paste("esc_ini_fun_lag",n_dist,sep = "_"),c(var1),conf.interval = 0.95)
      
      temp4<-summarySE(res_lv_sec,paste("esc_nao_fun_lag",n_dist,sep = "_"),c(var1),conf.interval = 0.95)
      
      ed_3_1<-bind_rows(temp1,temp2,temp3,temp4)
      
        write.table(ed_3_1,file = "ed_3_1.csv",sep = ";",row.names = F)
      
      temp1<-summarySE(res_lv_sec,"QTD_VOTOS_P",c(var2),conf.interval = 0.95)
      
      temp2<-summarySE(res_lv_sec,"QTD_VOTOS_P_lag",c(var2),conf.interval = 0.95,na.rm = T)
      
      temp3<-summarySE(res_lv_sec,paste("esc_ini_fun_lag",n_dist,sep = "_"),c(var2),conf.interval = 0.95)
      
      temp4<-summarySE(res_lv_sec,paste("esc_nao_fun_lag",n_dist,sep = "_"),c(var2),conf.interval = 0.95)
      
      ed_3_2<-bind_rows(temp1,temp2,temp3,temp4)
      
        write.table(ed_3_2,file = "ed_3_2.csv",sep = ";",row.names = F)
      
      
      #3 e 4.Diferença de médias por ano:
      
      ed_3_3<-summarySE(res_lv_sec,"QTD_VOTOS_P",c("NR_ANO",var1),conf.interval = 0.95)
      
        write.table(ed_3_3,file = "ed_3_3.csv",sep = ";",row.names = F)
      
      ed_3_4<-summarySE(res_lv_sec,"QTD_VOTOS_P",c("NR_ANO",var2),conf.interval = 0.95)
      
        write.table(ed_3_4,file = "ed_3_4.csv",sep = ";",row.names = F)

    ####################  
    #Amostra restrita 1#
    ####################  
      
      res_lv_sec<-readRDS(paste("restr_1_",n_dist,".RDS",sep = ""))
      
      #1 e 2.Diferenças de médias simples:
      
      temp1<-summarySE(res_lv_sec,"QTD_VOTOS_P",c(var1),conf.interval = 0.95)
      
      temp2<-summarySE(res_lv_sec,"QTD_VOTOS_P_lag",c(var1),conf.interval = 0.95,na.rm = T)
      
      temp3<-summarySE(res_lv_sec,paste("esc_ini_fun_lag",n_dist,sep = "_"),c(var1),conf.interval = 0.95)
      
      temp4<-summarySE(res_lv_sec,paste("esc_nao_fun_lag",n_dist,sep = "_"),c(var1),conf.interval = 0.95)
      
      ed_3_1_r1<-bind_rows(temp1,temp2,temp3,temp4)
      
        write.table(ed_3_1_r1,file = "ed_3_1_r1.csv",sep = ";",row.names = F)
      
      temp1<-summarySE(res_lv_sec,"QTD_VOTOS_P",c(var2),conf.interval = 0.95)
      
      temp2<-summarySE(res_lv_sec,"QTD_VOTOS_P_lag",c(var2),conf.interval = 0.95,na.rm = T)
      
      temp3<-summarySE(res_lv_sec,paste("esc_ini_fun_lag",n_dist,sep = "_"),c(var2),conf.interval = 0.95)
      
      temp4<-summarySE(res_lv_sec,paste("esc_nao_fun_lag",n_dist,sep = "_"),c(var2),conf.interval = 0.95)
      
      ed_3_2_r1<-bind_rows(temp1,temp2,temp3,temp4)
      
        write.table(ed_3_2_r1,file = "ed_3_2_r1.csv",sep = ";",row.names = F)
      
      
      #3 e 4.Diferença de médias por ano:
      
      ed_3_3_r1<-summarySE(res_lv_sec,"QTD_VOTOS_P",c("NR_ANO",var1),conf.interval = 0.95)
      
        write.table(ed_3_3_r1,file = "ed_3_3_r1.csv",sep = ";",row.names = F)
      
      ed_3_4_r1<-summarySE(res_lv_sec,"QTD_VOTOS_P",c("NR_ANO",var2),conf.interval = 0.95)
      
        write.table(ed_3_4_r1,file = "ed_3_4_r1.csv",sep = ";",row.names = F)
      
    ####################
    #Amostra restrita 2#
    ####################
      
      res_lv_sec<-readRDS(paste("restr_2_",n_dist,".RDS",sep = ""))
      res_lv_sec_<-readRDS(paste("restr_3_",n_dist,".RDS",sep = ""))
      
      #1 e 2.Diferenças de médias simples:
      
      temp1<-summarySE(res_lv_sec,"QTD_VOTOS_P",c(var1),conf.interval = 0.95)
      
      temp2<-summarySE(res_lv_sec,"QTD_VOTOS_P_lag",c(var1),conf.interval = 0.95,na.rm = T)
      
      temp3<-summarySE(res_lv_sec,paste("esc_ini_fun_lag",n_dist,sep = "_"),c(var1),conf.interval = 0.95)
      
      temp4<-summarySE(res_lv_sec,paste("esc_nao_fun_lag",n_dist,sep = "_"),c(var1),conf.interval = 0.95)
      
      ed_3_1_r2<-bind_rows(temp1,temp2,temp3,temp4)
      
      write.table(ed_3_1_r2,file = "ed_3_1_r2.csv",sep = ";",row.names = F)
      
      temp1<-summarySE(res_lv_sec_,"QTD_VOTOS_P",c(var2),conf.interval = 0.95)
      
      temp2<-summarySE(res_lv_sec_,"QTD_VOTOS_P_lag",c(var2),conf.interval = 0.95,na.rm = T)
      
      temp3<-summarySE(res_lv_sec_,paste("esc_ini_fun_lag",n_dist,sep = "_"),c(var2),conf.interval = 0.95)
      
      temp4<-summarySE(res_lv_sec_,paste("esc_nao_fun_lag",n_dist,sep = "_"),c(var2),conf.interval = 0.95)
      
      ed_3_2_r2<-bind_rows(temp1,temp2,temp3,temp4)
      
      write.table(ed_3_2_r2,file = "ed_3_2_r2.csv",sep = ";",row.names = F)
      
      #3 e 4.Diferença de médias por ano:
      
      ed_3_3_r2<-summarySE(res_lv_sec,"QTD_VOTOS_P",c("NR_ANO",var1),conf.interval = 0.95)
      
      write.table(ed_3_3_r2,file = "ed_3_3_r2.csv",sep = ";",row.names = F)
      
      ed_3_4_r2<-summarySE(res_lv_sec_,"QTD_VOTOS_P",c("NR_ANO",var2),conf.interval = 0.95)
      
      write.table(ed_3_4_r2,file = "ed_3_4_r2.csv",sep = ";",row.names = F)
      
    ####################
    #Amostra restrita 3#
    ####################
      
      res_lv_sec<-readRDS(paste("lv_match_data_1_",n_dist,".RDS",sep = ""))
      res_lv_sec_<-readRDS(paste("lv_match_data_2_",n_dist,".RDS",sep = ""))
      
      base<-readRDS("res_lv_1Km_t1.rds")
      res_lv_sec<-left_join(res_lv_sec,base)
      res_lv_sec_<-left_join(res_lv_sec_,base)
      
      
      #1 e 2.Diferenças de médias simples:
      
      temp1<-summarySE(res_lv_sec,"QTD_VOTOS_P",c(var1),conf.interval = 0.95)
      
      temp2<-summarySE(res_lv_sec,"QTD_VOTOS_P_lag",c(var1),conf.interval = 0.95,na.rm = T)
      
      temp3<-summarySE(res_lv_sec,paste("esc_ini_fun_lag",n_dist,sep = "_"),c(var1),conf.interval = 0.95)
      
      temp4<-summarySE(res_lv_sec,paste("esc_nao_fun_lag",n_dist,sep = "_"),c(var1),conf.interval = 0.95)
      
      ed_3_1_r3<-bind_rows(temp1,temp2,temp3,temp4)
      
      write.table(ed_3_1_r3,file = "ed_3_1_r3.csv",sep = ";",row.names = F)
      
      temp1<-summarySE(res_lv_sec_,"QTD_VOTOS_P",c(var2),conf.interval = 0.95)
      
      temp2<-summarySE(res_lv_sec_,"QTD_VOTOS_P_lag",c(var2),conf.interval = 0.95,na.rm = T)
      
      temp3<-summarySE(res_lv_sec_,paste("esc_ini_fun_lag",n_dist,sep = "_"),c(var2),conf.interval = 0.95)
      
      temp4<-summarySE(res_lv_sec_,paste("esc_nao_fun_lag",n_dist,sep = "_"),c(var2),conf.interval = 0.95)
      
      ed_3_2_r3<-bind_rows(temp1,temp2,temp3,temp4)
      
      write.table(ed_3_2_r3,file = "ed_3_2_r3.csv",sep = ";",row.names = F)
      
      #3 e 4.Diferença de médias por ano:
      
      ed_3_3_r3<-summarySE(res_lv_sec,"QTD_VOTOS_P",c("NR_ANO",var1),conf.interval = 0.95)
      
      write.table(ed_3_3_r3,file = "ed_3_3_r3.csv",sep = ";",row.names = F)
      
      ed_3_4_r3<-summarySE(res_lv_sec_,"QTD_VOTOS_P",c("NR_ANO",var2),conf.interval = 0.95)
      
      write.table(ed_3_4_r3,file = "ed_3_4_r3.csv",sep = ";",row.names = F)
      
      
############################################################################   
##4, 5 e 6. Estatísticas descritivas das variáveis dos Censos de 2000 e 2010 ##
############################################################################      
    
    var1<-paste("I_esc_ini_fun",n_dist,sep = "_")
    var2<-paste("I_esc_ini_fun_incumb",n_dist,sep = "_")  

################
## Censo 2000 ##
################
    
    ################## 
    #Amostra Completa#
    ##################
      
    res_lv_sec<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))
      
    censo2000<-readRDS(paste("res_lv_2000_",n_censo,".rds",sep = ""))
    
    censo2000$rend<-censo2000$Var02/censo2000$Var01 #Renda domiciliar média
    censo2000$esc<-censo2000$Var09/censo2000$Var08 #Escolaridade média dos responsáveis
    censo2000$i0_9<-(censo2000$V1448+censo2000$V1449)/censo2000$V1330 #Percentual de indivíduos de 9 anos ou menos 
    censo2000$i10_19<-(censo2000$V1450+censo2000$V1451)/censo2000$V1330 #Percentual de indivíduos de 10 a 19 anos
    
    res_lv_sec<-left_join(res_lv_sec,censo2000)
    
    #1 e 2.Diferenças de médias simples:
    

    temp1<-summarySE(res_lv_sec,"rend",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp2<-summarySE(res_lv_sec,"esc",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp3<-summarySE(res_lv_sec,"i0_9",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp4<-summarySE(res_lv_sec,"i10_19",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    ed_4_1<-bind_rows(temp1,temp2,temp3,temp4)
    
      write.table(ed_4_1,file = "ed_4_1.csv",sep = ";",row.names = F)
    

    temp1<-summarySE(res_lv_sec,"rend",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp2<-summarySE(res_lv_sec,"esc",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp3<-summarySE(res_lv_sec,"i0_9",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp4<-summarySE(res_lv_sec,"i10_19",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    ed_4_2<-bind_rows(temp1,temp2,temp3,temp4)
    
      write.table(ed_4_2,file = "ed_4_2.csv",sep = ";",row.names = F)
    
    ####################
    #Amostra Restrita 1#
    ####################
    
    res_lv_sec<-readRDS(paste("restr_1_",n_dist,".RDS",sep = ""))
    
    res_lv_sec<-left_join(res_lv_sec,censo2000)
    
    #1 e 2.Diferenças de médias simples:
    

    temp1<-summarySE(res_lv_sec,"rend",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp2<-summarySE(res_lv_sec,"esc",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp3<-summarySE(res_lv_sec,"i0_9",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp4<-summarySE(res_lv_sec,"i10_19",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    ed_4_1_r1<-bind_rows(temp1,temp2,temp3,temp4)
    
    write.table(ed_4_1_r1,file = "ed_4_1_r1.csv",sep = ";",row.names = F)
    

    temp1<-summarySE(res_lv_sec,"rend",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp2<-summarySE(res_lv_sec,"esc",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp3<-summarySE(res_lv_sec,"i0_9",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp4<-summarySE(res_lv_sec,"i10_19",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    ed_4_2_r1<-bind_rows(temp1,temp2,temp3,temp4)
    
    write.table(ed_4_2_r1,file = "ed_4_2_r1.csv",sep = ";",row.names = F)
    
    ####################
    #Amostra Restrita 2#
    ####################
    
    res_lv_sec<-readRDS(paste("restr_2_",n_dist,".RDS",sep = ""))
    res_lv_sec_<-readRDS(paste("restr_3_",n_dist,".RDS",sep = ""))
    
    res_lv_sec<-left_join(res_lv_sec,censo2000)
    res_lv_sec_<-left_join(res_lv_sec_,censo2000)
    
    
    #1 e 2.Diferenças de médias simples:
    

    temp1<-summarySE(res_lv_sec,"rend",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp2<-summarySE(res_lv_sec,"esc",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp3<-summarySE(res_lv_sec,"i0_9",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp4<-summarySE(res_lv_sec,"i10_19",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    ed_4_1_r2<-bind_rows(temp1,temp2,temp3,temp4)
    
    write.table(ed_4_1_r2,file = "ed_4_1_r2.csv",sep = ";",row.names = F)
    

    temp1<-summarySE(res_lv_sec_,"rend",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp2<-summarySE(res_lv_sec_,"esc",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp3<-summarySE(res_lv_sec_,"i0_9",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp4<-summarySE(res_lv_sec_,"i10_19",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    ed_4_2_r2<-bind_rows(temp1,temp2,temp3,temp4)
    
    write.table(ed_4_2_r2,file = "ed_4_2_r2.csv",sep = ";",row.names = F)
    
################
## Censo 2010 ##
################
    
    ##################
    #Amostra Completa#
    ##################
    
    res_lv_sec<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))
    
    censo2010<-readRDS(paste("res_lv_2010_",n_censo,".rds",sep = ""))
    
    
    censo2010$rend<-censo2010$bas_V005/censo2010$N_sc #Renda domiciliar média (aproximação)
    censo2010$esc<-censo2010$res2_V093/censo2010$res2_V001 #Percentual de responsáveis alfabetizados
    censo2010$i0_9<-(censo2010$p13_V022+censo2010$p13_V036+censo2010$p13_V037+censo2010$p13_V038+
                       censo2010$p13_V039+censo2010$p13_V040+censo2010$p13_V041+censo2010$p13_V042+
                       censo2010$p13_V043)/(censo2010$p13_V001+censo2010$p13_V002) #Percentual de indivíduos de 9 anos ou menos
    censo2010$i10_19<-(censo2010$p13_V044+censo2010$p13_V045+censo2010$p13_V046+censo2010$p13_V047+
                         censo2010$p13_V048+censo2010$p13_V049+censo2010$p13_V050+censo2010$p13_V051+
                         censo2010$p13_V052+censo2010$p13_V053)/(censo2010$p13_V001+censo2010$p13_V002) #Percentual de indivíduos de 10 a 19 anos
    
    
    res_lv_sec<-left_join(res_lv_sec,censo2010)
    
    #1 e 2.Diferenças de médias simples:
    

    temp1<-summarySE(res_lv_sec,"rend",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp2<-summarySE(res_lv_sec,"esc",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp3<-summarySE(res_lv_sec,"i0_9",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp4<-summarySE(res_lv_sec,"i10_19",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    ed_5_1<-bind_rows(temp1,temp2,temp3,temp4)
    
    write.table(ed_5_1,file = "ed_5_1.csv",sep = ";",row.names = F)
    

    temp1<-summarySE(res_lv_sec,"rend",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp2<-summarySE(res_lv_sec,"esc",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp3<-summarySE(res_lv_sec,"i0_9",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp4<-summarySE(res_lv_sec,"i10_19",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    ed_5_2<-bind_rows(temp1,temp2,temp3,temp4)
    
    #write.table(ed_5_2,file = "ed_5_2.csv",sep = ";",row.names = F)
  
    ####################    
    #Amostra Restrita 1#
    ####################  
    
    res_lv_sec<-readRDS(paste("restr_1_",n_dist,".RDS",sep = ""))
    
    res_lv_sec<-left_join(res_lv_sec,censo2010)
    
    #1 e 2.Diferenças de médias simples:
    

    temp1<-summarySE(res_lv_sec,"rend",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp2<-summarySE(res_lv_sec,"esc",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp3<-summarySE(res_lv_sec,"i0_9",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp4<-summarySE(res_lv_sec,"i10_19",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    ed_5_1_r1<-bind_rows(temp1,temp2,temp3,temp4)
    
    #write.table(ed_5_1_r1,file = "ed_5_1_r1.csv",sep = ";",row.names = F)
    

    temp1<-summarySE(res_lv_sec,"rend",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp2<-summarySE(res_lv_sec,"esc",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp3<-summarySE(res_lv_sec,"i0_9",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp4<-summarySE(res_lv_sec,"i10_19",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    ed_5_2_r1<-bind_rows(temp1,temp2,temp3,temp4)
    
    #write.table(ed_5_2_r1,file = "ed_5_2_r1.csv",sep = ";",row.names = F)
    
    ####################  
    #Amostra Restrita 2#
    ####################  
    
    res_lv_sec<-readRDS(paste("restr_2_",n_dist,".RDS",sep = ""))
    res_lv_sec_<-readRDS(paste("restr_3_",n_dist,".RDS",sep = ""))
    
    res_lv_sec<-left_join(res_lv_sec,censo2010)
    res_lv_sec_<-left_join(res_lv_sec_,censo2010)
    
    
    #1 e 2.Diferenças de médias simples:
    

    temp1<-summarySE(res_lv_sec,"rend",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp2<-summarySE(res_lv_sec,"esc",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp3<-summarySE(res_lv_sec,"i0_9",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp4<-summarySE(res_lv_sec,"i10_19",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    ed_5_1_r2<-bind_rows(temp1,temp2,temp3,temp4)
    
    #write.table(ed_5_1_r2,file = "ed_5_1_r2.csv",sep = ";",row.names = F)
    

    temp1<-summarySE(res_lv_sec_,"rend",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp2<-summarySE(res_lv_sec_,"esc",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp3<-summarySE(res_lv_sec_,"i0_9",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp4<-summarySE(res_lv_sec_,"i10_19",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    ed_5_2_r2<-bind_rows(temp1,temp2,temp3,temp4)
    
    #write.table(ed_5_2_r2,file = "ed_5_2_r2.csv",sep = ";",row.names = F)
    
####################
#Amostra Restrita 3#
####################
    
    base<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))
    
    res_lv_sec<-readRDS(paste("lv_match_data_1_",n_dist,".RDS",sep = ""))
    
    res_lv_sec<-left_join(res_lv_sec,base)
    
    res_lv_sec_<-readRDS(paste("lv_match_data_2_",n_dist,".RDS",sep = ""))
    
    res_lv_sec_<-left_join(res_lv_sec_,base)
    
    res_lv_sec$rend<-res_lv_sec$rend*res_lv_sec$weights
    res_lv_sec$esc<-res_lv_sec$esc*res_lv_sec$weights
    res_lv_sec$i0_9<-res_lv_sec$i0_9*res_lv_sec$weights
    res_lv_sec$i10_19<-res_lv_sec$i10_19*res_lv_sec$weights
    
    res_lv_sec_$rend<-res_lv_sec_$rend*res_lv_sec_$weights
    res_lv_sec_$esc<-res_lv_sec_$esc*res_lv_sec_$weights
    res_lv_sec_$i0_9<-res_lv_sec_$i0_9*res_lv_sec_$weights
    res_lv_sec_$i10_19<-res_lv_sec_$i10_19*res_lv_sec_$weights
    
    #1 e 2.Diferenças de médias simples:
    

    temp1<-summarySE(res_lv_sec,"rend",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp2<-summarySE(res_lv_sec,"esc",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp3<-summarySE(res_lv_sec,"i0_9",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    temp4<-summarySE(res_lv_sec,"i10_19",c("NR_ANO",var1),conf.interval = 0.95,na.rm = T)
    
    ed_6_1_r3<-bind_rows(temp1,temp2,temp3,temp4)
    
    #write.table(ed_6_1_r3,file = "ed_6_1_r3.csv",sep = ";",row.names = F)
    

    temp1<-summarySE(res_lv_sec_,"rend",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp2<-summarySE(res_lv_sec_,"esc",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp3<-summarySE(res_lv_sec_,"i0_9",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    temp4<-summarySE(res_lv_sec_,"i10_19",c("NR_ANO",var2),conf.interval = 0.95,na.rm = T)
    
    ed_6_2_r3<-bind_rows(temp1,temp2,temp3,temp4)
    
    #write.table(ed_6_2_r3,file = "ed_6_2_r3.csv",sep = ";",row.names = F)
    
#######################################################
#Comparação das seções que não consideradas na análise#
#######################################################
  
    var1<-paste("I_esc_ini_fun",n_dist,sep = "_")
    var2<-paste("I_esc_ini_fun_incumb",n_dist,sep = "_")  
    
    
  #Seções sem variável de votação do incumbente na eleição anterior:
    
    res_lv_sec<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))
    
    res_lv_sec$I<-1*is.na(res_lv_sec$QTD_VOTOS_lag)
    
    
    #1 e 2.Diferenças de médias simples:
    
    temp0<-summarySE(res_lv_sec,c(var1),'I',conf.interval = 0.95)
    
    temp1<-summarySE(res_lv_sec,c(var2),'I',conf.interval = 0.95)

    temp3<-summarySE(res_lv_sec,paste("esc_ini_fun_lag",n_dist,sep = "_"),'I',conf.interval = 0.95)
    
    temp4<-summarySE(res_lv_sec,paste("esc_nao_fun_lag",n_dist,sep = "_"),'I',conf.interval = 0.95)
    
    ed_7_1<-bind_rows(temp0,temp1,temp3,temp4)
    
    write.table(ed_7_1,file = "ed_7_1.csv",sep = ";",row.names = F)
    
    
  #Seções que aparecem em apenas 1 período:
    
    res_lv_sec<-readRDS(paste("res_lv_",n_dist,"_t",t,".rds",sep = ""))
      res_lv_sec$COD<-paste(res_lv_sec$NR_ZONA,res_lv_sec$NR_SECAO,sep = "_")
    
    temp4<-aggregate(rep(1,nrow(res_lv_sec)),by=list(res_lv_sec$COD),FUN=sum)
    
    res_lv_sec$I<-1*res_lv_sec$COD%in%temp4[temp4$x<2,]$Group.1
    
    
    #1 e 2.Diferenças de médias simples:
    
    temp0<-summarySE(res_lv_sec,c(var1),'I',conf.interval = 0.95)
    
    temp1<-summarySE(res_lv_sec,c(var2),'I',conf.interval = 0.95)
    
    temp3<-summarySE(res_lv_sec,paste("esc_ini_fun_lag",n_dist,sep = "_"),'I',conf.interval = 0.95)
    
    temp4<-summarySE(res_lv_sec,paste("esc_nao_fun_lag",n_dist,sep = "_"),'I',conf.interval = 0.95)
    
    ed_7_2<-bind_rows(temp0,temp1,temp3,temp4)
    
    write.table(ed_7_2,file = "ed_7_1.csv",sep = ";",row.names = F)

    