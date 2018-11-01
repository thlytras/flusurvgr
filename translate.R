library(readxl)
tr <- rbind(
  as.data.frame(read_excel("ui_translations.xls")),
  as.data.frame(read_excel("content_translations.xls")),
  as.data.frame(read_excel("current_translations.xls")))


rownames(tr) <- tr$ID
tr$ID <- NULL


save(tr, file="data_translations.RData")
