library(shiny)
library(DT)
library(storr)
library(impactserver)




render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = 'none', server = server, editable = editable, options=list("pageLength" = 100), ...)
}




shinyApp(
  ui = fluidPage(
    mainPanel(
      headerPanel("IMPACT study coordinator centre"),
      tabsetPanel(type = "tabs",
                  tabPanel("Clinic day", selectInput("clinicDayDateSelector","Which day",c(Sys.Date()-1,Sys.Date(),Sys.Date()+1)),  DTOutput('clinicDay'), actionButton("clinicDay_refresh_btn","Refresh")),
                  tabPanel("Whitelist", DTOutput('whitelist'), actionButton("add_patient_btn", "New patient")),
                  tabPanel("ACCEPT calculator", tags$a(href="https://impactstudy.vch.ca/acceptcalc/?accesskey=123456", "Open in new tab", target="newwin"), tags$iframe(src="https://impactstudy.vch.ca/acceptcalc/?accesskey=123456", width="100%", height="1000")),
                  tabPanel("Physicians",DTOutput('physicians'), actionButton("add_physician_btn", "New physician"), actionButton("physicians_refresh_btn","Refresh")),
                  tabPanel("Logs", actionButton("logs_refresh_btn","Refresh"), DTOutput('logs'), actionButton("flush_logs_btn", "Delete logs"))
      )),print(paste("Server version:",GetVersionNote()))
  ),


  server = function(input, output, session)
  {
    GetData <- function()
    {
      whitelistVars <- c('dtScheduled', 'code','firstName','lastName', 'physician','whitelisted')

      clinicDay <- impactserver:::GetPatients()
      whitelist <- clinicDay[which(clinicDay$whitelisted==T | clinicDay$whitelisted==1),whitelistVars]

      physicians <- impactserver:::GetUsers()

      list(clinicDay=clinicDay, whitelist=whitelist, physicians=physicians)
    }

    GetLogs <- function()
    {
      out <- impactserver:::GetLogs()
      if(dim(out)[1]>0)
      {
        out <- out[dim(out)[1]:1,]
      }

      out
    }


    tmp <- GetData()
    dfClinicDay <- tmp$clinicDay
    dfWhitelist <- tmp$whitelist
    dfPhysicians <- tmp$physicians

    options(DT.options = list(pageLength = 5))

    # server-side processing
    output$whitelist = render_dt(dfWhitelist, 'cell')
    output$clinicDay = render_dt(dfClinicDay, 'cell')
    output$physicians = render_dt(dfPhysicians, 'cell')
    output$logs = render_dt(GetLogs(), 'none')

    # edit a single cell
    observeEvent(input$clinicDay_cell_edit, {
      info = input$clinicDay_cell_edit
      str(info)  # check what info looks like (a data frame of 3 columns)
      n_changes <- dim(info)[1]
      tmp <- GetData()
      dfClinicDay <- tmp$clinicDay
      for(i in 1:n_changes)
      {
        code <- dfClinicDay[info$row[i],'code']
        cell_name <- colnames(dfClinicDay)[info$col[i]]
        cell_value <- info$value
        if(cell_name=="code")
        {
          if(nchar(cell_value)==0)
          {
            impactserver:::DeletePatient(code)
          }
          {
            impactserver:::UpdateCode(code,cell_value)
          }
        }
        else
        {
          pt <- data.frame(code=code)
          pt[cell_name] <- cell_value
          impactserver:::UpdatePatient(pt)
        }
      }

      tmp <- GetData()
      dfClinicDay <- tmp$clinicDay
      dfWhitelist <- tmp$whitelist
      output$clinicDay = render_dt(dfClinicDay, 'cell')
      output$whitelist = render_dt(dfWhitelist, 'cell')
    })

    #whitelistProxy = dataTableProxy('whiteList')
    observeEvent(input$add_patient_btn, {
      impactserver:::AddPatient(data.frame(code="0123456789", source="CRC", dtScheduled=as.character(Sys.Date()+1), whitelisted=T))
      tmp <- GetData()
      dfWhitelist <- tmp$whitelist
      output$whitelist = render_dt(dfWhitelist, 'cell')
      #replaceData(whitelistProxy, dfWhitelist, resetPaging = TRUE)
    })

    observeEvent(input$whitelist_cell_edit, {
      info = input$whitelist_cell_edit
      str(info)  # check what info looks like (a data frame of 3 columns)
      n_changes <- dim(info)[1]
      tmp <- GetData()
      dfWhitelist <- tmp$whitelist
      for(i in 1:n_changes)
      {
        code <- dfWhitelist[info$row[i],'code']
        cell_name <- colnames(dfWhitelist)[info$col[i]]
        cell_value <- info$value
        if(cell_name=="code")
        {
          if(nchar(cell_value)==0)
          {
            impactserver:::DeletePatient(code)
          }
          {
            impactserver:::UpdateCode(code,cell_value)
          }
        }
        else
        {
          pt <- data.frame(code=code)
          pt[cell_name] <- cell_value
          impactserver:::UpdatePatient(pt)
        }
      }
      tmp <- GetData()
      dfClinicDay <- tmp$clinicDay
      dfWhitelist <- tmp$whitelist
      output$clinicDay = render_dt(dfClinicDay, 'cell')
      output$whitelist = render_dt(dfWhitelist, 'cell')
    })


    observeEvent(input$clinicDay_refresh_btn, {
      tmp <- GetData()
      dfClinicDay <- tmp$clinicDay
      dfWhitelist <- tmp$whitelist
      output$clinicDay = render_dt(dfClinicDay, 'cell')
      output$whitelist = render_dt(dfWhitelist, 'cell')
    })


    ##Physicians
    observeEvent(input$add_physician_btn, {
      impactserver:::AddUser(data.frame(userName="janedoe", source="Manager"))
      tmp <- GetData()
      dfPhysicians <- tmp$physicians
      output$physicians = render_dt(dfPhysicians, 'cell')
    })


    observeEvent(input$physicians_cell_edit,
                 {
                   info = input$physicians_cell_edit
                   str(info)  # check what info looks like (a data frame of 3 columns)
                   n_changes <- dim(info)[1]
                   tmp <- GetData()
                   dfPhysicians <- tmp$physicians
                   for(i in 1:n_changes)
                   {
                     userName <- dfPhysicians[info$row[i],'userName']
                     cell_name <- colnames(dfPhysicians)[info$col[i]]
                     cell_value <- info$value
                     if(cell_name=="userName")
                     {
                       if(nchar(cell_value)==0)
                       {
                         impactserver:::DeleteUser(userName)
                       }
                       {
                         impactserver:::UpdateUserName(userName,cell_value)
                       }
                     }
                     else
                     {
                       dc <- data.frame(userName=userName)
                       dc[cell_name] <- cell_value
                       impactserver:::UpdateUser(dc)
                     }
                   }
                   tmp <- GetData()
                   dfPhysicians <- tmp$physicians
                   output$physicians = render_dt(dfPhysicians, 'cell')
                 })

    observeEvent(input$physicians_refresh_btn, {
      tmp <- GetData()
      dfPhysicians <- tmp$physicians
      output$physicians = render_dt(dfPhysicians, 'cell')
    })

    #Logs
    observeEvent(input$flush_logs_btn, {
      impactserver:::FlushLogs()
      output$logs = render_dt(GetLogs(), 'none')
    })

    observeEvent(input$logs_refresh_btn, {
      output$logs = render_dt(GetLogs(), 'none')
    })

  }
)





