library(httr)
library(jsonlite)

download_file_location <- "C:\\temp\\"

base_url <- 'https://power.larc.nasa.gov/cgi-bin/v1/DataAccess.py?request=execute&'
identifiers <- 'identifier=Regional&'
parameters <- 'parameters=RH2M&'#,FROST_DAYS,RH2M,SG_DAY_HOUR_AVG,SI_EF_MAX_TILTED_SURFACE&'
start_date <- 'startDate=20190701&'
end_date <- 'endDate=20190731&'
community <- 'userCommunity=SSE&'
temp <- 'tempAverage=DAILY&'
out <- 'outputList=JSON&'
region <- 'bbox=-31.0,152.0,-30.0,153.0&'
user <- 'user=anonymous'

final_url <- paste0(base_url,identifiers,parameters,start_date,end_date,community,temp,out,region,user)
final_url

a <- GET(url = final_url)
b <- rawToChar(a$content)
c <- fromJSON(b)

json_location <- c[["outputs"]][["json"]]
download.file(json_location,paste0("json_data.json"))


library("MODIS")
library(rgdal)
library(RCurl)

getProduct()
getProduct("MOD09GQ")
# 152.691, -30.99, 152.693, -30.98
boundary <- raster(xmn = 152.691, xmx = 152.693, ymn = -30.99, ymx = -30.98)
tile <- getTile(boundary)
tile
hTile <- tile@tileH
vTile <- tile@tileV
a <- getHdf("MOD09GQ", begin = "2019.08.01", end = "2019.08.02", tileH = 31, tileV = 12, checkIntegrity = FALSE)




