

templateUser <- data.frame(
                               dtAdded=timeStamp(),
                               dtActed=timeStamp(), #Last datetime they logged in
                               dtInteracted="",
                               source="", #Manager or Assistant. The only filed that is ignored in the update
                               status="", #Custom to be coded by study personnel as needed
                               userName="johndoe",
                               firstName="John",
                               lastName="Doe",
                               contribution="Pilot",
                               settingVars="{}"
)




GetTemplateUser <- function()
{
  templateUser
}




GetUsers <- function()
{
  st <- Connect("users")

  if(st$exists("users"))
  {
    df <- st$get("users")
  }
  else
  {
    df <- templateUser[-1,]
  }

  Disconnect()

  df
}



# Patchy adding is accepted. DOES NOT CHECK for duplicates
AddUser <- function(user)
{
  success <- F

  st <- Connect("users")

  if(st$exists("users"))
  {
    df <- st$get("users")
  }
  else
  {
    df <- GetTemplateUser()[-1,]
  }
  index <- dim(df)[1]+1

   userName <- user$userName

  if(length(which(df[,'userName']==userName))==0)
  {
    user$dtAdded <- user$dtActed <- timeStamp()
    for(element in names(user))
    {
      if(length(user[[element]])>0)
      {
        df[index,element] <- user[[element]]
      }
    }
    success = T
  }

  st$set("users",df)

  Disconnect()

  success
}



#Key is userName. Throws error if user is not there.
#Patchy updating is acceptable. ONE RECORD AT A TIME
#Does not create new physicain
UpdateUser <- function(user)
{
  success <- F
  st <- Connect("users")

  if(!st$exists("users"))
  {
    return(F)
  }

  df <- st$get("users")

  for(i in 1:dim(df)[1])
  {
    if(df[i,'userName']==user$userName)
    {
      success <- T
      for(element in names(user))
      {
        if(element!="source") df[i, element] <- user[[element]]
      }
      df[i,'dtActed'] <- timeStamp() #Cant cheat and update this manually so the last item to override
    }
    st$set("users",df)
  }

  Disconnect()

  success
}




AddUpdateUser <- function(user)
{
  pt <- FindUser(list(userName=user$userName))
  if(length(pt)>0)
  {
    UpdateUser(user)
  }
  else
  {
    AddUser(user)
  }
}












GetUser <- function(userName, tolerance=0)
{
  st <- Connect("users")

  if(!st$exists("users"))
  {
    return((list()))
  }

  df <- st$get("users")


  if(dim(df)[1]==0)
  {
    res <- GetTemplateUser()[-1,]
  }
  else
  {
    res <- df[which(df$userName==userName),]
  }

  if(dim(res)[1]==0)
  {
    if(globalVars$createUserWhenNotFound)
    {
      newUser <- GetTemplateUser()
      newUser$userName <- userName
      newUser$firstName <- "Unknown"
      newUser$lastName <- "Unknown"
      newUser$contribution <- "default"
      newUser$source="Server"
      AddUser(newUser)
      res <- newUser
    }
  }

  #try(AddLog(source="Server", event="FindUser", logData=list(userName=userName)), silent=T)

  return(res)
}









UpdateUserName <- function(oldUserName, newUserName)
{
  success <- F

  st <- Connect("users")
  if(!st$exists("users"))
  {
    return(F)
  }

  df <- st$get("users")
  index <- which(df$userName==oldUserName)
  if(length(index)>0)
  {
    index2 <- which(df$userName==newUserName)
    if(length(index2)==0)
    {
      df[index,'userName'] <- newUserName
      df[index,'dtActed'] <- timeStamp()
      st$set("users",df)
      success = T
    }
  }

  Disconnect()

  success
}




DeleteUser <- function(userName)
{
  success <- F

  st <- Connect("users")

  if(!st$exists("users"))
  {
    return(F)
  }

  df <- st$get("users")
  index <- which(df$userName==userName)
  if(length(index)>0)
  {
    df <- df[-index,]
    st$set("users",df)
    success = T
  }

  Disconnect()

  success
}




FlushUsers <- function()
{
  st <- Connect("users")
  st$set("users",GetTemplateUser()[-1,])
  Disconnect()
}

