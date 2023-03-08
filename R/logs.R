templateLog <- data.frame(
  dt=timeStamp(),
  event="",
  logData="[]"
)


GetTemplateLog <- function()
{
  templateLog
}




GetLogs <- function()
{
  try({y <- read.table(file="c:\\IMPACTserver\\impactserver.log", sep = "|"); return(as.data.frame(y))},silent = T)

  return(data.frame())
}






AddLog <- function(dt=timeStamp(), source, event, logData=list())
{
  x <- data.frame(dt=dt, source=source, event=event)

  if(length(logData)>0)
  {
    require(jsonlite)
    x$logData<-toJSON(logData)
  }
  else
  {
    x$logData<-"[]"
  }

  write.table(x, file="c:\\IMPACTserver\\impactserver.log", append = T, row.names = F, col.names = F, sep = "|")

  T
}



FlushLogs <- function()
{
  try({file.remove("c:\\IMPACTserver\\impactserver.log")},silent = T)
}
