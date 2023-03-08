



#' @export
GetVersionNote <- function()
{
  "2023.03.08 (Melting Snow)"
}


globalVars <- new.env()
globalVars$createUserWhenNotFound<-T
globalVars$caller <- ""


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
    st <- storr_rds(path=paste0("c:\\IMPACTDB\\",db))
  }
}





Disconnect <- function()
{
  tryCatch(
    {con <- globalVars$con; DBI::dbDisconnect(con)},
    error=function(cond){},
    warning=function(cond){}
    )
}



#Internal for universally formatted datetime
timeStamp <- function()
{
  as.character(Sys.time())
}




#' @export
Gateway <- function(accessKey, userName, func, parms)
{
  if(accessKey %in% c("0123456789"))
  {
    globalVars$caller <- userName
    res <- do.call(func, parms)
    try({AddLog(source=userName, event=func, logData=list(inData=parms, outData=res))}, silent = T)
    return(res)
  }
  else
  {
    return(NULL)
  }
}

