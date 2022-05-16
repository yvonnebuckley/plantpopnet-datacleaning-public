## Caroline Mckeon 15/04/19
## Opperating systems differences fix

## this script determinds the opperating system the complier is being run on
## and if necessary, changes system local allowing macs to run scripts written on windows systems

## create function returning the opperating system the complied cleaner is being run on
get_os <- function() {
  if (.Platform$OS.type == "windows") { 
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac" 
  } else if (.Platform$OS.type == "unix") { 
    "unix"
  } else {
    stop("Unknown OS")
  }
}

x <- get_os()

## if run on mac, set system locale to deal with windows encoding 
if (x == "mac") Sys.setlocale('LC_ALL','C')