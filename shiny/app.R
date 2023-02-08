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
                  tabPanel("Clinic day", selectInput("clinicDayDateSelector","Which day",c(Sys.Date()-1,Sys.Date(),Sys.Date()+1)),  DTOutput('clinicDay'), actionButton("refresh_btn","Refresh")),
                  tabPanel("Whitelist", DTOutput('whitelist'), actionButton("add_patient_btn", "New patient")),
                  tabPanel("ACCEPT calculator",h1("Todo")),
                  tabPanel("Physicians",DTOutput('physicians'), actionButton("add_physician_btn", "New physician"))
      )),
  ),


  server = function(input, output, session)
  {
    GetData <- function()
    {
      whitelistVars <- c('dtScheduled', 'code','firstName','lastName','whitelisted')

      clinicDay <- GetPatients()
      whitelist <- clinicDay[which(clinicDay$whitelisted==T),whitelistVars]

      physicians <- GetUsers()

      list(clinicDay=clinicDay, whitelist=whitelist, physicians=physicians)
    }


    tmp <- GetData()
    dfClinicDay <- tmp$clinicDay
    dfWhitelist <- tmp$whitelist
    dfPhysicians <- tmp$physicians

    options(DT.options = list(pageLength = 5))

    # server-side processing
    output$whitelist = render_dt(dfWhitelist, 'cell')
    output$clinicDay = render_dt(dfClinicDay, 'cell')
    output$physicians <- render_dt(tmp$physicians, 'cell')

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
            DeletePatient(code)
          }
          {
            UpdateCode(code,cell_value)
          }
        }
        else
        {
          pt <- data.frame(code=code)
          pt[cell_name] <- cell_value
          UpdatePatient(pt)
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
      AddPatient(data.frame(code="0123456789", source="CRC", dtScheduled=as.character(Sys.Date()+1), whitelisted=T))
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
            DeletePatient(code)
          }
          {
            UpdateCode(code,cell_value)
          }
        }
        else
        {
          pt <- data.frame(code=code)
          pt[cell_name] <- cell_value
          UpdatePatient(pt)
        }
      }
      tmp <- GetData()
      dfClinicDay <- tmp$clinicDay
      dfWhitelist <- tmp$whitelist
      output$clinicDay = render_dt(dfClinicDay, 'cell')
      output$whitelist = render_dt(dfWhitelist, 'cell')
    })


    observeEvent(input$refresh_btn, {
      tmp <- GetData()
      dfClinicDay <- tmp$clinicDay
      dfWhitelist <- tmp$whitelist
      output$clinicDay = render_dt(dfClinicDay, 'cell')
      output$whitelist = render_dt(dfWhitelist, 'cell')
    })


##Physicians
    observeEvent(input$add_physician_btn, {
      AddUser(data.frame(userName="janedoe", source="Manager"))
      tmp <- GetData()
      dfPhysicians <- tmp$physicians
      output$physicians = render_dt(dfPhysicians, 'cell')
    })


    observeEvent(input$physicians_cell_edit, {
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
            DeletePhysician(userName)
          }
          {
            UpdateUserName(userName,cell_value)
          }
        }
        else
        {
          dc <- data.frame(userName=userName)
          dc[cell_name] <- cell_value
          UpdateUser(dc)
        }
      }
      tmp <- GetData()
      dfPhysicians <- tmp$physicians
      output$physicains = render_dt(dfPhysicians, 'cell')
    })



  }
)





