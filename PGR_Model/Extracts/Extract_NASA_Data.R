library(slga)
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


aoi <- c(152.95, -27.55, 153.07, -27.45)
?get_slga_data
bne_surface_clay <- slga:::get_slga_data(url = soil_url)

getAnywhere(make_soils_url)
soil_url <- slga:::make_soils_url(product = 'NAT', attribute = 'CLY', component = 'CLO', depth = 1, aoi = aoi)