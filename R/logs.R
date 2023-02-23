templateLog <- data.frame(
  dt=timeStamp(),
  event="",
  data="[]"
)


#' @export
GetTemplateLog <- function()
{
  templateLog
}




#' @export
GetLogs <- function()
{
  st <- Connect("logs")

  if(st$exists("logs"))
  {
    df <- st$get("logs")
  }
  else
  {
    df <- templateLog[-1,]
  }

  Disconnect()

  df
}






#' @export
AddLog <- function(dt=timeStamp(), source, event, logData=list())
{
  success <- F

  st <- Connect("logs")

  if(st$exists("logs"))
  {
    df <- st$get("logs")
  }
  else
  {
    df <- GetTemplateLog()[-1,]
  }

  index <- dim(df)[1]+1

  df[index,'dt']<-dt
  df[index,'source']<-source
  df[index,'event']<-event

  if(length(logData)>0)
  {
    require(jsonlite)
    df[index,'logData']<-toJSON(logData)
  }
  else
  {
    df[index,'logData']<-"[]"
  }


  st$set("logs",df)

  Disconnect()

  T
}
