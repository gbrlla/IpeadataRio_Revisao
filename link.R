planilha<-dplyr::inner_join(coment,link,by="codigo")
setwd("C:\\Users\\b248968182\\Desktop\\SCN10")
write.csv(planilha, file = "scn10.csv")