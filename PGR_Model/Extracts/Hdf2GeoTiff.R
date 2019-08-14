library("raster")
library("rgeos")
library("maptools")
library("maps")

# Thanks to https://shekeine.github.io/modis/2014/08/27/HDF_to_multi-band_geotiff

hdf2mband <- function(in.dir, prm.path, out.dir, nbands){
  
  INLIST <- list.files(path=in.dir, full.names=T)
  INNAMES<- list.files(path=in.dir, full.names=F)
  
  #Manufacure output names with .tif extension
  OUTNAMES <- INNAMES
  for (i in 1:length(OUTNAMES)){OUTNAMES[i] <- gsub(pattern=".hdf", x=OUTNAMES[i], replacement=".tif")}
  
  for (i in 1:length(INLIST)){
    
    #Call mrt to subset and reproject from sinusoidal to geographic CRS
    system(
      command=paste(paste("cd ", out.dir, "&&", sep=""),
                    
                    #Write hdf file to text file
                    paste("echo ", INLIST[i], " > ", out.dir, "/mosaicinput.txt","&&", sep=""),
                    
                    #Run mrt mosaic and write output to hdf file
                    paste("mrtmosaic -i ", out.dir, "/mosaicinput.txt -o ", out.dir, 
                          "/mosaic_tmp.hdf","&&", sep=""),
                    
                    #Call resample 
                    paste("resample -p ", prm.path, " -i ", out.dir, "/mosaic_tmp.hdf -o ", 
                          out.dir, "/", OUTNAMES[i], "&&",sep=""),
                    
                    paste("exit 0"), sep=""), intern=T
    )
    print(paste("Done processing:", INNAMES[i]))
  }
  
  #Clean up working files
  file.remove(paste(out.dir,"/mosaic_tmp.hdf", sep=""))
  file.remove(paste(out.dir,"/mosaicinput.txt", sep=""))
  file.remove(paste(out.dir,"/resample.log", sep=""))
  
  #Aggregate single bands to multiband rasters
  band.paths <- list.files(path=out.dir, full.names=T)
  
  repeat{
    
    stack.i <- stack()
    
    for (i in 1:nbands){stack.i <- addLayer(stack.i, i=read.ras(x=band.paths[i]))}
    
    writeRaster(x=stack.i, filename=paste(out.dir, "/", 
                                          OUTNAMES[1], sep=""),  format="GTiff", overwrite=TRUE)
    file.remove(band.paths[1:nbands])
    
    #Truncate done jobs
    band.paths <- band.paths[-(1:nbands)]
    OUTNAMES <- OUTNAMES[-1]
    
    if(length(band.paths)==0)
      break
  }
}