LoadRequiredPackages <- function(packageList){
    for(package in packageList){
        error <- F
        tryCatch({if(!(package %in% installed.packages()[,'Package'])){
            chooseCRANmirror(ind = 39)
            install.packages(package)}
        },
        error=function(e){
            error <<- T
            cat(sprintf("Error installing package: %s", e))
            cat(sprintf("Skipping package %s", package))
        })
        if (error) next
        
        suppressMessages(require(package,character.only = T))
    }
}