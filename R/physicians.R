

templatePhysician <- data.frame(dtAdded=timeStamp(),
                               dtActed=timeStamp(), #Last datetime they logged in
                               dtInteracted=timeStamp(),
                               source="", #Manager or Assistant. The only filed that is ignored in the update
                               status="", #Custom to be coded by study personnel as needed
                               userName="dsin",
                               firstName="Don",
                               lastName="Sin",
                               intervention="C", #C, T
                               seen=0,
                               assistantUsed=0,
                               machine="", #Last machine they signed on
                               settingVars=""
)




#' @export
GetTemplatePhysician <- function()
{
  templatePhysician
}


#' @export
GetPhysicians <- function()
{
  st <- Connect("physicians")

  if(st$exists("physicians"))
  {
    df <- st$get("physicians")
  }
  else
  {
    df <- templatePhysician[-1,]
  }

  Disconnect()

  df
}



# Patchy adding is accepted. DOES NOT CHECK for duplicates
#' @export
AddPhysician <- function(physician)
{
  success <- F

  st <- Connect("physicians")

  if(st$exists("physicians"))
  {
    df <- st$get("physicians")
  }
  else
  {
    df <- GetTemplatePhysician()[-1,]
  }
  index <- dim(df)[1]+1

   userName <- physician$userName

  if(length(which(df[,'userName']==userName))==0)
  {
    physician$dtAdded <- physician$dtActed <- timeStamp()
    for(element in names(physician))
    {
      if(length(physician[[element]])>0)
      {
        df[index,element] <- physician[[element]]
      }
    }
    success = T
  }

  st$set("physicians",df)

  Disconnect()

  success
}



#Key is userName. Throws error if physician is not there.
#Patchy updating is acceptable. ONE RECORD AT A TIME
#Does not create new physicain
#' @export
UpdatePhysician <- function(physician)
{
  success <- F
  st <- Connect("physicians")

  if(!st$exists("physicians"))
  {
    return(F)
  }

  df <- st$get("physicians")

  for(i in 1:dim(df)[1])
  {
    if(df[i,'userName']==physician$userName)
    {
      success <- T
      for(element in names(physician))
      {
        if(element!="source") df[i, element] <- physician[[element]]
      }
      df[i,'dtActed'] <- timeStamp() #Cant cheat and update this manually so the last item to override
    }
    st$set("physicians",df)
  }

  Disconnect()

  success
}




#' @export
AddUpdatePhysician <- function(physician)
{
  pt <- FindPhysician(list(userName=physician$userName))
  if(length(pt)>0)
  {
    UpdatePhysician(physician)
  }
  else
  {
    AddPhysician(physician)
  }
}











#' Matches by EVERY thing. If you want by userName submit a physicain with only userName
#' @export
FindPhysician <- function(physician, tolerance=0)
{
  st <- Connect("physician")

  if(!st$exists("physician"))
  {
    return((list()))
  }

  df <- st$get("physician")

  if(dim(df)[1]==0)
  {
    return((list()))
  }

  out <- NULL
  for(i in 1:dim(df)[1])
  {
    success <- T
    for(element in names(physician))
    {
      if(!is.na(physician[[element]]))
      {
        if(is.na(df[i,element]))
        {
          success <- F
        }
        else
        {
          if(physician[[element]]!=df[i,element])
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
      st$set("physicians", df)
      return(df[i,])
    }
  }

  Disconnect()

  return(list())
}









#' @export
UpdateUserName <- function(oldUserName, newUserName)
{
  success <- F

  st <- Connect("physicians")
  if(!st$exists("physicians"))
  {
    return(F)
  }

  df <- st$get("physicians")
  index <- which(df$userName==oldUserName)
  if(length(index)>0)
  {
    index2 <- which(df$userName==newUserName)
    if(length(index2)==0)
    {
      df[index,'userName'] <- newUserName
      df[index,'dtActed'] <- timeStamp()
      st$set("physicians",df)
      success = T
    }
  }

  Disconnect()

  success
}




#' @export
DeletePhysician <- function(userName)
{
  success <- F

  st <- Connect("physicians")

  if(!st$exists("physicians"))
  {
    return(F)
  }

  df <- st$get("physicians")
  index <- which(df$userName==userName)
  if(length(index)>0)
  {
    df <- df[-index,]
    st$set("physicians",df)
    success = T
  }

  Disconnect()

  success
}




#' @export
FlushPhysicians <- function()
{
  st <- Connect("physicians")
  st$set("physicians",GetTemplatePhysician()[-1,])
  Disconnect()
}

