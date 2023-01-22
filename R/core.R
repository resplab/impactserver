# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#Internal for unviersally formatted datetime
#' @export
GetDBPath <- function(db)
{
  basePath <- unname(ifelse(Sys.info()["sysname"]=="Linux", "/var/lib/impactserver/storr/" ,"C:/Users/msafavi/test/storr/settings/"))
  paste0(basePath,db)
}




timeStamp <- function()
{
  as.character(Sys.time())
}




#' @export
Set <- function(val1,val2) {
  require(storr)
  st <- storr::storr_rds(path=GetDBPath("settings"))
  st$set(val1,val2)
}


#' @export
Get <- function(val1) {
  require(storr)
  st <- storr::storr_rds(path=GetDBPath("settings"))
  st$get(val1)
}




#' @export
SaveSettings <- function(user,settingVars) {
  require(storr)
  st <- storr::storr_rds(path=GetDBPath("settings"))
  st$set(paste0("user.",user),settingVars)
  return(0)
}



#' @export
LoadSettings <- function(user) {
  require(storr)
  #require(jsonlite)
  st <- storr::storr_rds(path=GetDBPath("settings"))
  out <- NULL
  if(st$exists(paste0("user.",user)))
  {
    out <- st$get(paste0("user.",user))
  }
  else
  {
    out <- NULL
  }

  out
}



#' @export
FlushSettings <- function()
{
  require(storr)
  st <- storr::storr_rds(path=GetDBPath("settings"))
  st$destroy()
}











templatePatient <- data.frame( dtAdded=timeStamp(),
                               dtActed=timeStamp(),
                               dtScheduled=timeStamp(),
                               source="", #CRC, PFT, Assistant. The only one that is ignored in the update
                               status="", #Custom to be coded by study personnel as needed
                               phn="1234567890",
                               firstName="Mohsen",
                               lastName="Sadatsafavi",
                               dob="1976-04-28",
                               whitelisted=1,
                               pftAvailable=0,
                               seen=0,
                               assistantUsed=0,
                               approachable=0,
                               physician="",
                               machine="",
                               acceptData=""
                               )





#' @export
GetPatients <- function()
{
  require(storr)
  st <- storr::storr_rds(path=GetDBPath("patients"))

  if(st$exists("patients"))
  {
    df <- st$get("patients")
  }
  else
  {
    df <- templatePatient[-1,]
  }

  df
}



# Patchy adding is accepted. DOES NOT CHECK for duplicates
#' @export
AddPatient <- function(patient)
{
  success <- F

  require(storr)
  st <- storr::storr_rds(path=GetDBPath("patients"))

  if(st$exists("patients"))
  {
    df <- st$get("patients")
  }
  else
  {
    df <- GetTemplatePatient()[-1,]
  }
  index <- dim(df)[1]+1

  phn <- patient$phn

  if(length(which(df[,'phn']==phn))==0)
  {
    patient$dtAdded <- patient$dtActed <- timeStamp()
    for(element in names(patient))
    {
      if(length(patient[[element]])>0)
      {
        df[index,element] <- patient[[element]]
      }
    }
    success = T
  }

  st$set("patients",df)

  success
}



#Key is PHN. Throws error if patient is not there.
#Patchy updating is acceptable. ONE PATIENT AT A TIME
#Does not create new patient
#' @export
UpdatePatient <- function(patient)
{
  success <- F
  require(storr)
  st <- storr::storr_rds(path=GetDBPath("patients"))
  if(!st$exists("patients"))
  {
    return(F)
  }

  df <- st$get("patients")

  for(i in 1:dim(df)[1])
  {
    if(df[i,'phn']==patient$phn)
    {
      success <- T
      for(element in names(patient))
      {
        if(element!="source") df[i, element] <- patient[[element]]
      }
      df[i,'dtActed'] <- timeStamp() #Cant cheat and update this manually so the last item to override
    }
    st$set("patients",df)
  }

  success
}




#' @export
AddUpdatePatient <- function(patient)
{
  pt <- FindPatient(list(phn=patient$phn))
  if(length(pt)>0)
  {
    UpdatePatient(patient)
  }
  else
  {
    AddPatient(patient)
  }
}








#' @export
GetTemplatePatient <- function()
{
  templatePatient
}



#' Matches by EVERY thing. If you want by PHN submit a patient with only PHN
#' @export
FindPatient <- function(patient, tolerance=0)
{
  require(storr)
  st <- storr::storr_rds(path=GetDBPath("patients"))
  if(!st$exists("patients"))
  {
    return((list()))
  }

  df <- st$get("patients")

  if(dim(df)[1]==0)
  {
    return((list()))
  }

  out <- NULL
  for(i in 1:dim(df)[1])
  {
    success <- T
    for(element in names(patient))
    {
      if(!is.na(patient[[element]]))
      {
        if(is.na(df[i,element]))
        {
          success <- F
        }
        else
        {
          if(patient[[element]]!=df[i,element])
          {
            success <- F
          }
        }
      }
    }
    if(success)
    {
      df[i,'seen'] <- 1
      df[i,'dtActed'] <- timeStamp()
      st$set("patients", df)
      return(df[i,])
    }
  }

  return(list())
}









#' @export
UpdatePHN <- function(oldPhn, newPhn)
{
  success <- F
  require(storr)
  st <- storr::storr_rds(path=GetDBPath("patients"))
  if(!st$exists("patients"))
  {
    return(F)
  }

  df <- st$get("patients")
  index <- which(df$phn==oldPhn)
  if(length(index)>0)
  {
    index2 <- which(df$phn==newPhn)
    if(length(index2)==0)
    {
      df[index,'phn'] <- newPhn
      df[index,'dtActed'] <- timeStamp()
      st$set("patients",df)
      success = T
    }
  }

  success
}



#' @export
DeletePatient <- function(phn)
{
  success <- F
  require(storr)
  st <- storr::storr_rds(path=GetDBPath("patients"))
  if(!st$exists("patients"))
  {
    return(F)
  }

  df <- st$get("patients")
  index <- which(df$phn==phn)
  if(length(index)>0)
  {
    df <- df[-index,]
    st$set("patients",df)
    success = T
  }

  success
}




#' @export
FlushPatients <- function()
{
  require(storr)
  st <- storr::storr_rds(path=GetDBPath("patients"))
  st$set("patients",GetTemplatePatient()[-1,])
}



