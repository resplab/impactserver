

library(shiny)
library(DT)
library(storr)





render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = 'none', server = server, editable = editable, ...)
}





shinyApp(
  ui = fluidPage(
    mainPanel(
      headerPanel("IMPACT study coordinator centre"),
      tabsetPanel(type = "tabs",
                  tabPanel("Clinic day", DTOutput('clinicDay'), actionButton("refresh_btn","Refresh")),
                  tabPanel("Whitelist", DTOutput('whitelist'), actionButton("add_btn", "New row")),
                  tabPanel("ACCEPT calculator",h1("Todo"))
      )),
  ),

  server = function(input, output, session)
  {
    require(ImpactServer)
    GetData <- function()
    {
      whitelistVars <- c('phn','firstName','lastName','whitelisted')

      clinicDay <- GetPatients()
      whitelist <- clinicDay[which(clinicDay$whitelisted==T),whitelistVars]

      list(clinicDay=clinicDay, whitelist=whitelist)
    }


    tmp <- GetData()
    dfClinicDay <- tmp$clinicDay
    dfWhitelist <- tmp$whitelist

    options(DT.options = list(pageLength = 5))

    # server-side processing
    output$whitelist = render_dt(dfWhitelist, 'cell')
    output$clinicDay = render_dt(dfClinicDay, 'cell')

    # edit a single cell
    observeEvent(input$clinicDay_cell_edit, {
      info = input$clinicDay_cell_edit
      str(info)  # check what info looks like (a data frame of 3 columns)
      n_changes <- dim(info)[1]
      tmp <- GetData()
      dfClinicDay <- tmp$clinicDay
      for(i in 1:n_changes)
      {
        phn <- dfClinicDay[info$row[i],'phn']
        cell_name <- colnames(dfClinicDay)[info$col[i]]
        cell_value <- info$value
        if(cell_name=="phn")
        {
          if(nchar(cell_value)==0)
          {
            DeletePatient(phn)
          }
          {
            UpdatePHN(phn,cell_value)
          }
        }
        else
        {
          pt <- data.frame(phn=phn)
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
    observeEvent(input$add_btn, {
      AddPatient(data.frame(phn="123456789", source="CRC",  whitelisted=T))
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
        phn <- dfWhitelist[info$row[i],'phn']
        cell_name <- colnames(dfWhitelist)[info$col[i]]
        cell_value <- info$value
        if(cell_name=="phn")
        {
          if(nchar(cell_value)==0)
          {
            DeletePatient(phn)
          }
          {
            UpdatePHN(phn,cell_value)
          }
        }
        else
        {
          pt <- data.frame(phn=phn)
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
  }
)



