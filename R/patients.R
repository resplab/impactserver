



templatePatient <- data.frame( dtAdded=timeStamp(),
                               dtActed=timeStamp(),
                               dtScheduled=timeStamp(),
                               source="", #CRC, PFT, Assistant. The only one that is ignored in the update
                               status="", #Custom to be coded by study personnel as needed
                               code="1234567890",
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
                               data="{}"
)




#' @export
GetTemplatePatient <- function()
{
  templatePatient
}


#' @export
GetPatients <- function()
{
  st <- Connect("patients")

  if(st$exists("patients"))
  {
    df <- st$get("patients")
  }
  else
  {
    df <- templatePatient[-1,]
  }

  Disconnect()

  df
}



# Patchy adding is accepted. DOES NOT CHECK for duplicates
#' @export
AddPatient <- function(patient)
{
  success <- F

  if(length(patient$code)==0) return(F)

  st <- Connect("patients")

  if(st$exists("patients"))
  {
    df <- st$get("patients")
  }
  else
  {
    df <- GetTemplatePatient()[-1,]
  }
  index <- dim(df)[1]+1

  code <- patient$code

  if(length(which(df[,'code']==code))==0)
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

  Disconnect()

  success
}



#Key is Code. Throws error if patient is not there.
#Patchy updating is acceptable. ONE PATIENT AT A TIME
#Does not create new patient
#' @export
UpdatePatient <- function(patient)
{
  success <- F
  st <- Connect("patients")

  if(!st$exists("patients"))
  {
    return(F)
  }

  df <- st$get("patients")

  for(i in 1:dim(df)[1])
  {
    if(df[i,'code']==patient$code)
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

  Disconnect()

  success
}




#' @export
UpdateAddPatient <- function(patient)
{
  pt <- FindPatient(list(code=patient$code))
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
GetPatient <- function(code, tolerance=0)
{
  st <- Connect("patients")

  if(!st$exists("patients"))
  {
    return((list()))
  }

  df <- st$get("patients")

  if(dim(df)[1]==0)
  {
    return((list()))
  }

  i <- which(df$code==code)
  df[i,'seen'] <- 1
  df[i,'dtActed'] <- timeStamp()
  st$set("patients", df)

  Disconnect()

  return(df[i,])
}






#' Matches by EVERY thing. If you want by Code submit a patient with only Code
#' @export
FindPatient <- function(patient, tolerance=0)
{
  st <- Connect("patients")

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

  Disconnect()

  return(list())
}









#' @export
UpdateCode <- function(oldCode, newCode)
{
  success <- F

  st <- Connect("patients")
  if(!st$exists("patients"))
  {
    return(F)
  }

  df <- st$get("patients")
  index <- which(df$code==oldCode)
  if(length(index)>0)
  {
    index2 <- which(df$code==newCode)
    if(length(index2)==0)
    {
      df[index,'code'] <- newCode
      df[index,'dtActed'] <- timeStamp()
      st$set("patients",df)
      success = T
    }
  }

  Disconnect()

  success
}



#' @export
DeletePatient <- function(code)
{
  success <- F

  st <- Connect("patients")

  if(!st$exists("patients"))
  {
    return(F)
  }

  df <- st$get("patients")
  index <- which(df$code==code)
  if(length(index)>0)
  {
    df <- df[-index,]
    st$set("patients",df)
    success = T
  }

  Disconnect()

  success
}




#' @export
FlushPatients <- function()
{
  st <- Connect("patients")
  st$set("patients",GetTemplatePatient()[-1,])
  Disconnect()
}
