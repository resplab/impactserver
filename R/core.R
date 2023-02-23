



#' @export
GetVersionNote <- function()
{
  "2023.02.22 (User Producer)"
}


globalVars <- new.env()
globalVars$createUserWhenNotFound<-T



Connect <- function(db, fileSystem=T)
{
  require(storr)
  require(DBI)
  if(!fileSystem)
  {
    con <- dbConnect(RPostgres::Postgres(),
                      host = 'localhost', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                      port = 5432, # or any other port specified by your DBA
                      user = 'postgres',
                      password = 'postgres')
    globalVars$con <- con
    st <- storr_dbi(paste0(db,"Data"), paste0(db,"Keys"), con=con)
  }
  else
  {
    basePath <- unname(ifelse(Sys.info()["sysname"]=="Linux", "/var/lib/impactserver/storr/" ,"C:/Users/msafavi/test/storr/settings/"))
    path <- paste0(basePath,db)
    st <- storr_rds(path=paste0("c:\\IMPACTDB\\",db))
  }
}


#' @export
Disconnect <- function()
{
  tryCatch(
    {con <- globalVars$con; DBI::dbDisconnect(con)},
    error=function(cond){},
    warning=function(cond){}
    )
}



#Internal for unviersally formatted datetime
#' @export
timeStamp <- function()
{
  as.character(Sys.time())
}




#' @export
Set <- function(val1,val2) {
  st <- Connect("settings")
  st$set(val1,val2)
  Disconnect()
}


#' @export
Get <- function(val1) {
  st <- Connect("settings")
  st$get(val1)
  Disconnect()
}




#' @export
SaveSettings <- function(user,settingVars) {
  st <- Connect("settings")
  st$set(paste0("user.",user),settingVars)
  Disconnect()
  return(T)
}



#' @export
LoadSettings <- function(user) {
  st <- Connect("settings")
  out <- NULL
  if(st$exists(paste0("user.",user)))
  {
    out <- st$get(paste0("user.",user))
  }
  else
  {
    out <- NULL
  }

  Disconnect()
  out
}



#' @export
FlushSettings <- function()
{
  require(storr)
  st <- Connect("settings")
  st$destroy()
}









