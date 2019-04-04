# -------- Lista de pacotes
pacotes <- c('RODBC', 'dplyr', 'magrittr', 'tidyr', 'knitr', 'kableExtra')

# -------- Carregando pacotes
for (i in 1:length(pacotes))
{
  pck <- names(installed.packages()[, 1]) == pacotes[i]
  if (length(names(installed.packages()[, 1])[pck]) == 0) {
    install.packages(pacotes[i], repos = 'http://cran.fiocruz.br/')
  }
  suppressPackageStartupMessages(library(pacotes[i],character.only = TRUE)) 
}
rm(pacotes, i, pck)


# -------- Abrindo conexão com a base em SQL
con <- RODBC::odbcConnect(dsn = "ipeadata", uid = "", pwd = "")

# -------- Solicitando Mae
mae <- 
  RODBC::sqlQuery(con, paste0("SELECT ", 
                              "dbo.SERIES.SERCODIGOTROLL, ",
                              "dbo.SERIES.SERSTATUS, ", 
                              "dbo.SERIES.FNTID, ", 
                              "dbo.SERIES.TEMID, ", 
                              "dbo.SERIES.SERNOME_P, ", 
                              "dbo.SERIES.SERTIPO, ",
                              "dbo.SERIES.SERDESCRICAO_P, ", 
                              "dbo.SERIES.SERLIBERADA, ", 
                              "dbo.SERIES.SERPRIVATIVA, ", 
                              "dbo.SERIES.SERPRAZOATUALIZACAO, ",
                              "dbo.SERIES.SERINDICADORIPEA, ",
                              "dbo.SERIES.SERHISTORICA, ",
                              "dbo.SERIES.SERHISTORICO, ",
                              "dbo.SERIES.SERINDGER, ", 
                              "dbo.SERIES.CATID ",
                              "FROM dbo.SERIES;")) %>% 
  as_tibble() %>% 
  mutate(SERCODIGOTROLL2 = SERCODIGOTROLL) %>% 
  separate(col = 'SERCODIGOTROLL2', into = 'BANCO', sep = "_") %>%
  mutate(COND_EXIB = if_else(SERLIBERADA == 0, "Oculta", if_else(SERPRIVATIVA == 1, "Intranet", "Internet"))) %>%
  mutate(GRANDE_TEMA = if_else(SERTIPO == "N", "Macro", if_else(SERTIPO == "R" & CATID == 1, "Regional", "Social"))) 

# ------ Excluindo colunas desnecessárias: SERTIPO e CATID
excluir<-c(6,15)
mae<-mae[,-excluir]

# ------ Padronizando
mae$SERCODIGOTROLL <- as.character(mae$SERCODIGOTROLL)
mae$SERLIBERADA <- as.factor(mae$SERLIBERADA)
mae$SERPRIVATIVA <- as.factor(mae$SERPRIVATIVA)
mae$BANCO <- as.factor(mae$BANCO)


# -------- Status
mae$SERSTATUS <- factor(mae$SERSTATUS,
                        levels = c("A", "I"),
                        labels = c("Ativa", "Inativa"))

# -------- Liberada
mae$SERLIBERADA <- factor(mae$SERLIBERADA,
                          levels = c("0", "1"),
                          labels = c("Oculta", "Não oculta"))

# -------- Privativa
mae$SERPRIVATIVA <- factor(mae$SERPRIVATIVA,
                           levels = c("0", "1"),
                           labels = c("Internet", "Intranet"))

# -------- Tema
mae$TEMID <- factor(mae$TEMID, 
                    levels = c("28", "23", "25", "10", "7", "5",
                               "2", "8", "81", "24", "37", "38",        
                               "11", "29", "18", "12", "19", "6",
                               "39", "32", "31", "15", "4", "40",
                               "3", "27", "14", "9",	"1", "16",
                               "30",	"13",	"41",	"20",	"17",	"33",	
                               "26",	"54",	"55",	"63",	"56",	"57",
                               "58", "59",	"60",	"80",	"79",	"78"), 
                    labels = c("Agropecuária", "Assistência social", "Avaliação do governo",
                               "Balanço de pagamentos", "Câmbio", "Comércio exterior", "Consumo e vendas",                                    "Contas nacionais", "Contas Regionais", "Correção monetária", "Demografia",
                               "Desenvolvimento humano", "Economia internacional",                                    "Educação", "Eleições", "Emprego", "Estoque de capital", "Finanças públicas", "Financeiras",
                               "Geográfico", "Habitação", "Indicadores sociais", "Juros", "Mercado de trabalho", "Moeda e crédito",
                               "Percepção e expectativa", "População", "Preços", "Produção", "Projeções", "Renda", "Salário e renda",
                               "Saúde", "Segurança Pública", "Sinopse macroeconômica", "Transporte", "Vendas", "Deputado Estadual",
                               "Deputado Federal", "Eleitorado", "Governador", "Prefeito", "Presidente", "Senador", "Vereador",
                               "IDHm1991", "IDHm2000", "IDHm2010"))

# -------- Fontes
mae$FNTID <- factor(mae$FNTID, 
                    levels = c("2092078006", "323394154", "378", "547", "384",
                               "36083653", "407", "1347352618", "1700650372",
                               "748520328", "424", "243107374", "98296583",
                               "1842281591", "239155352", "1438016766", "544",
                               "1842744596", "463809858", "381", "1184389689",
                               "1611149327", "236096736", "1333431519", "364",
                               "542350462", "533314505", "510", "390", "239155349",
                               "1333430858", "182606746", "1650971488", "1678619981",
                               "35633143", "1678619626", "545", "405", "1678619735",
                               "98295021", "516", "1904049245", "603335807", "138574035",
                               "2005645694", "378762457", "1357659634", "131968735",
                               "2000043053", "98298095", "1678619927", "2088612456",
                               "542", "548", "1333433217", "2092078007", "1333432004",
                               "306982476", "474103043", "1333080354", "419", "1333084734",
                               "1678619506", "488", "1333089242", "433", "227539092", 
                               "539", "1333430856", "162243178", "1552828063", "428", "497",
                               "231410416", "36083703", "365", "420", "391", "1333360735",
                               "385", "1145113178", "1333430559", "1678619636", "1198638800",
                               "290546545", "1552504139", "268118390", "477", "1333075672",
                               "1574094045", "1671208031", "417", "517", "75492174",
                               "344632916", "1333335497", "1333430842", "371280626", 
                               "1333352062", "394", "380", "389840948", "741191201", "478", 
                               "432", "409", "2005653650", "464200173", "239156299", 
                               "442618574", "1333430857", "464158387", "1711891468",
                               "1678619923","1678619084", "475", "383", "406", "1678619651",
                               "366", "1678620089", "81923816", "372278781", "1678620049",
                               "386", "382218618"), 
                    labels = c("CRU", "Anac", "BNDES/SE", "Procon-SP", "Economist", "Abracal",
                               "IPEA", "IBGE/PNAD Contínua", "RFFSA", "Assembleias", 
                               "ACSP/IEGV", "MTE-Caged", "MPS", "ONS", 
                               "FGV/Conj. Econ. - IGP antigo", "Nemesis", "TSE", "Anatel",
                               "Federal Reserve Board", "Conab/IE", "IBGE/SCN 2010 Anual",
                               "Datasus", "FGV/Conj. Econômica Inativa", "Bacen Outras/SGS",
                               "Abia", "IBGE/IPP", "KOF", "Fundap/Diesp", "Fiesp", 
                               "FGV/Conj. Econ. - IGP", "Bacen/Not. Imp./Moeda", "IBMEC",
                               "EIA", "IBGE/PME", "IBGE/Coagro", "IBGE/PIM-DG", "FNDE",
                               "IBS/ES", "IBGE/PIM-PF", "Fiemg", "Abras", "IBGE/RTSP",
                               "Nasdaq", "Firjan", "IBGE/SCN 2000 Trim.", "Antaq/AEP",
                               "FENABRAVE", "Seade/PED", "IPUMS", "FMI/IFS", "IBGE/PME antiga",
                               "MME", "Valor Econômico", "JP Morgan", "Bacen Outras/BPE",
                               "Caixa Econômica Federal", "Bacen Outras/ISP", "ANTT/AETT",
                               "MDA", "Bacen/BP (BPM6)", "SNIC", "Bacen/Dív. Ext.",
                               "IBGE/PAM", "Anfavea", "Bacen/PII", "Outras fontes", 
                               "IEA", "IBGE", "Bacen/Boletim/Intern.", "Dieese", "OCDE",
                               "FGV/Conj. Econômica", "Min. Fazenda/Cotec", "MTE/Caged", "Anda",
                               "Abinee", "Sobeet", "Fipe", "Bacen/Boletim/Bpantigo",
                               "Eletrobras", "MDIC/SECEX Países", "Bacen/Not. Imp./Set. Ext.",
                               "IBGE/Pimes", "IBGE/SCN 2010 Trim.", "MDS", "Serasa", 
                               "Min. Fazenda/STN", "Min. Fazenda/SRF", "Bacen/BP (BPM5)",
                               "BCB", "IPEA/AEB", "Seab-PR", "Fecomercio SP", "Minagincom", 
                               "OUTFONTE", "Bacen/Boletim/Ativ. Ec.", 
                               "Bacen/Not. Imp./F. Púb.", "MTE/Outras", "Bacen/Boletim/BP",
                               "Funcex", "CNI", "BM&FBovespa", "Funcex/Séries CNAE 1.0",
                               "Min. Fazenda-STN", "ANP", "MDIC/SECEX", "IBGE/SCN 2000 Anual",
                               "Banco Mundial/GEM", "FGV/Conj. Econ.", "Anbima",
                               "Bacen/Boletim/M. Finan.", "Banco Mundial/WDI", "BLS",
                               "IBGE/PMC", "IBGE/SCN Consolidado", "Min. Fazenda/Cotepe",
                               "Dieese1", "IBS/IE", "IBGE/PIM-PF antiga", "ABPO", "IBGE/SNIPC",
                               "IBGE/PPM", "CJF", "IBGE/Pop", "Eletros", 
                               "Funcex/Séries antigas"))

# -------- Fechando conexao
RODBC::odbcClose(con)


# -------- Analisando dados
glimpse(mae)





# -------- Salvando mae didático
write.csv2(mae, "mae_didatico.csv")


