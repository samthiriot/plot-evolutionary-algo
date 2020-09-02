
contatenate_files <- function (directoryWithResultsName) {

    pop <- NULL
    i <- 1
    while (TRUE) {
    	# TODO check creation time of the file
    	filename <- paste(directoryWithResultsName,"/population",i,".csv", sep="");
    	if (!file.exists(filename)) {
    		break
    	}
    	#print(filename)
    	
    	popraw <- read.csv(header = T, file=filename) # col.names=colnames, colClasses=coltypes, 
    	#print(head(popraw))
    
    	pop <- if (is.null(pop)) popraw else rbind(pop, popraw)
    		
    	i <- i + 1
    }
    
    pop
}
