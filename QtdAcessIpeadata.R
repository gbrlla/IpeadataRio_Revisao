# Codigo para pegar sempre o primeiro dia do mes
datas <- seq(as.Date("2000-01-01"), 
             Sys.Date() - as.numeric(substr(Sys.Date(), 9, 10)) + 1, 
             by = "1 month")

# Sys.Date() - informa a data atual


# ------ Abrindo conexao
con <- RODBC::odbcConnect(dsn = "ipeadata", uid = "", pwd = "")

for (i in 2:length(datas)) {
  
  print(datas[i - 1])
  
  txt <- paste0("SELECT p.PERNOME_P,
                      qtd.SERBASE,
                m.TEMNOME_P,
                n.FNTNOME_P,
                qtd.SERCODIGOTROLL,
                qtd.SERNOME_P,
                qtd.QtdConsultas,
                qtd.Posicao
                FROM (SELECT s.PERID, 
                s.SERID,
                s.SERCODIGOTROLL,
                s.SERNOME_P,
                s.FNTID,
                s.SERTIPO,
                s.CATID,
                CASE  
                WHEN s.SERTIPO = 'N' THEN 'Macroeconômico' 
                WHEN s.SERTIPO = 'R' and s.CATID = 1 THEN 'Regional' 
                WHEN s.SERTIPO = 'R' and s.CATID = 2 THEN 'Social' 
                ELSE 'ERRO!' END AS SERBASE,
                s.TEMID,
                COUNT(*) QtdConsultas,
                RANK() OVER (PARTITION BY s.PERID ORDER BY COUNT(*) DESC) Posicao
                FROM dbo.logconsultaNet con
                INNER JOIN dbo.SERIES s ON con.SERID = s.SERID
                WHERE con.lgchora BETWEEN '", datas[i - 1], 
              "' AND '", datas[i] - 1, " 23:59:59.997'
              GROUP BY s.PERID,
              s.SERID,
              s.SERCODIGOTROLL,
              s.SERNOME_P,
              s.FNTID,
              s.SERTIPO,
              s.TEMID,
              s.CATID) qtd
              INNER JOIN dbo.PERIODICIDADES p ON qtd.PERID = p.PERID
              INNER JOIN dbo.TEMA m ON qtd.TEMID = m.TEMID
              INNER JOIN dbo.FONTES n ON qtd.FNTID = n.FNTID
              WHERE qtd.Posicao >= 1
              ORDER BY p.PERID, qtd.Posicao")
  
  txt <- gsub("\n", " ", txt)
  
  # ------ Mae
  acessos <- 
    RODBC::sqlQuery(con, txt)
  excluir <- c(1:4, 6)
  acessos <- acessos[,-excluir]
  
#Trocar para o left
mae_acesso <- right_join(acessos, mae, by="SERCODIGOTROLL")
write.csv2(mae_acesso, paste0("QtdAcessIpeadata/QtdAcessIpeadatamae_", substr(datas[i - 1], 1, 7), ".csv"))
  
}

test<-list()
for (i in 1:229){
  aux<-read.csv2(file[[i]],header=TRUE)
  test[[length(test)+1]]<-aux
}

names(test)<-nomes

teste<-list()
for (i in 1:lenght(test)){
  aux<-data.frame(SERCODIGOTROLL=test[[i]][6],paste("Qtd_Acesso",nomes[[i]],sep="_")=test[[i]][8],paste("Posicao",nomes[[i]],sep="_")=test[[i]][9])
  teste[[lenght(teste)+1]]<-aux
}

tes<-do.call(cbind,teste)

final<-left_join(mae,tes,by=c())

# ------ Fechando conexao
RODBC::odbcClose(con)



acessosBanco <- 
acessos %>% 
separate(col = 'SERCODIGOTROLL', into = 'BANCO', sep = "_") %>%
group_by(BANCO) %>% 
summarise(QtdConsultasTotal = sum(QtdConsultas), n = n()) %>% 
mutate(QtdConsultasMedia = QtdConsultasTotal/n) %>% 
arrange(desc(QtdConsultasTotal))

