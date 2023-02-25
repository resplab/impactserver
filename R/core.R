



#' @export
GetVersionNote <- function()
{
  "2023.02.23 (Gate Keeper)"
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
    try(AddLog(source=userName, event=func, logData=parms) , silent = T)
    return(do.call(func, parms))
  }
  else
  {
    return(NULL)
  }
}

