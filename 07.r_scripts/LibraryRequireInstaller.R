
##########################################################
#                                                        #
#                  Library Installer                     #
#                                                        #
##########################################################


libraryRequireInstall = function(packageName, ...)
{
  if(!require(packageName, character.only = TRUE)) 
    warning(paste("*** The package: '", packageName, "' was not installed ***", sep=""))
}
