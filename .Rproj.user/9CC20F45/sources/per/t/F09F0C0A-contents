#update data
getwd()

psdzip <- "psd_alldata_csv"
urlPath <- "https://apps.fas.usda.gov/psdonline/downloads/"
xlsFile <- paste0("psd_alldata.csv")
zipFile <- paste0(psdzip,".zip")
download.file(paste0(urlPath,zipFile),zipFile)
unzip(zipFile)
unlink(zipFile)

