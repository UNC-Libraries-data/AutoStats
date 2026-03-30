
# Created by Lorin Bruckner for UNC Chapel Hill Libraries (lorin.bruckner@unc.edu)

#Before pushing to github:
#export(appdir = ".", destdir = "docs")

library(shiny)
library(bslib)
library(tidyverse)
library(shinycssloaders)
library(shinyjs)
library(lubridate)

#Workaround for Chromium issue that prevents file downloads
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Track version number
vn <- "v3.0"


# UI Tabs ----

# LibCal Appointments
ui_libcal <- div(class = "main",
                 fluidRow(
                   column(12,
                          h3("Get LibCal Appointment Stats"),
                          actionLink("helpLink", "Help", icon = icon("circle-question"))                          )
                   ),
                 
                 fluidRow(
                   class = "nameRow",
                   column(4,
                          textInput("lcFirst", "First Name")
                          ),
                   column(4,
                          textInput("lcLast", "Last Name")
                          )
                   ),
                 
                 fluidRow(
                   class = "lcButtonRow",
                   column(4,
                          fileInput("libcalUp",
                                    label = "Upload LibCal File",
                                    accept = ".csv")
                          ),
                   column(4,
                          actionButton("submit_libcal", "Submit")
                          )
                   ),
                 fluidRow(
                   column(12,
                          textOutput("lcErrorMsg")
                          )
                   ),
                 withSpinner(type = 7, size = .8,
                             fluidRow(id = "lcSpinnerRow")
                             )
                 )

# E-mail Consultations
ui_emails <- div(class = "main",
                 fluidRow(
                   column(12,
                          h3("Get E-Mail Consultation Stats"),
                          actionLink("helpLink", "Help", icon = icon("circle-question"))
                          )
                 ),
                 
                 fluidRow(
                   class = "nameRow",
                   column(4,
                          textInput("emFirst", "First Name")
                          ),
                   column(4,
                          textInput("emLast", "Last Name")
                          )
                 ),
                 fluidRow(
                   class = "emButtonRow",
                   column(4,
                          fileInput("emailUp", 
                                    label = "Upload E-Mail File",
                                    accept = ".csv")
                          ),
                   column(4,
                          actionButton("submit_email", "Submit")
                          ),
                   
                   fluidRow(
                     column(12,
                            textOutput("emErrorMsg")
                     )
                   ),
                   withSpinner(type = 7, size = .8,
                               fluidRow(id = "emSpinnerRow")
                   )
                 )
                )

# Instruction Requests
ui_instreq <- div(class = "main",
                  fluidRow(
                    column(12,
                           h3("Get Instruction Request Stats"),
                           actionLink("helpLink", "Help", icon = icon("circle-question"))
                           )
                  ),
                  
                  fluidRow(
                    class = "nameRow",
                    column(4,
                           textInput("irFirst", "First Name")
                           ),
                    column(4,
                           textInput("irLast", "Last Name")
                           )
                  ),
                  
                  fluidRow(
                    class = "irButtonRow",
                    column(4,
                           fileInput("instreqUp",
                                     label = "Upload All Instruction Request Files",
                                     accept = ".txt",
                                     multiple = TRUE)
                           ),
                    column(4,
                           actionButton("submit_instreq", "Submit")
                           )
                  ),
                  
                  fluidRow(
                    column(12,
                           textOutput("irErrorMsg")
                    ),
                    withSpinner(type = 7, size = .8,
                                fluidRow(id = "irSpinnerRow")
                  )
                 )
              )

# Help page
ui_help <- div(class = "main",
               h3("How to Use Autostats"),
               tags$ul(class = "helpTOC",
                 tags$li(a("How to generate and upload LibCal stats", href = "#libcal")),
                 tags$li(a("How to generate and upload e-mail stats", href = "#email")),
                 tags$li(a("How to generate and upload instruction request stats", href = "#instreq"))
               ),
               h4(id = "libcal",
                  "How to generate and upload LibCal stats"),
               div(class = "alert",
                   h6(icon("triangle-exclamation"), class = "alert-title", "WARNING"),
                   p(class="alert-content",
                     "AutoStats cannot read any LibCal records with a date prior to September, 2025. Files with records prior to that date will generate an error."
                   )
                   ),
               h5("Download a CSV file from LibCal."),
               tags$ol(class = "steps",
                       tags$li("In LibApps, use the dropdown menu to navigate to", strong("LibCal.")),
                       tags$li("Click on the", strong("Appointments"), "tab and select", strong("Booking Explorer.")),
                       tags$li("Use the", strong("Date Range"), "picker to select the dates of the appointments you want to download. Click the ", strong("Go"), "button."),
                       tags$li("After clicking 'Go', you will then be able to click the", strong("Export"), "button. You will be prompted to save a CSV file to your computer.")
               ),
               h5("Upload the LibCal CSV file to AutoStats."),
               tags$ol(class = "steps",
                       tags$li("In Autostats, navigate to ", strong("LibCal Appointments.")),
                       tags$li("Fill out the form and upload the CSV file you downloaded from Booking Explorer. Click ", strong("Submit.")),
                       tags$li("If the correct file has been submitted, a ", strong("Download"), "button will appear. Click on it to download a file for LibInsight.")
               ),
               h5("Upload the AutoStats CSV file to LibInsight."),
               tags$ol(class = "steps",
                       tags$li("In LibApps, use the dropdown menu to navigate to", strong("LibInsight.")),
                       tags$li("Select the ", strong("Digital Research Services "), "dataset."),
                       tags$li("From the top menu, select ", strong("Record Data.")),
                       tags$li("Click the button labeled ", strong("Upload File.")),
                       tags$li("Scroll to the bottom and click on the ", strong("File Upload"), "panel."),
                       tags$li("Click on the ", strong("Choose File"), "button, and select the file you downloaded from AutoStats. You do not need to make any other changes to the form."),
                       tags$li("Click ", strong("Upload data."))
               ),
               hr(),
               h4(id = "email",
                  "How to generate and upload e-mail stats"),
               div(class = "alert",
                   h6(icon("triangle-exclamation"), class = "alert-title", "WARNING"),
                   p(class="alert-content",
                     "AutoStats cannot read ", strong("Topic(s) of Consultation"), "from e-mail consultations. That field will be left blank. If you wish to add a topic such as 'Generative AI', it is reccommended you edit the record in LibInsight after uploading."
                   )
               ),
               h5("Collect e-mail consultations in Outlook."),
               tags$ol(class = "steps",
                       tags$li("In Outlook, create a new folder under", strong("Sent Items "), "and label it", strong("Consults.")),
                       tags$li("Move or copy all consult emails into this folder.")
               ),
               h5("Import Outlook e-mails in Excel."),
               tags$ol(class = "steps",
                       tags$li("Open a blank spreadsheet in Excel."),
                       tags$li("Click on the ", strong("Data "), "menu at the top of Excel."),
                       tags$li("In the Data ribbon, click on ", strong("Get Data, "), "and select", strong("From Other Sources > From Microsoft Exchange.")),
                       tags$li("In the pop-up window, enter your e-mail address."),
                       tags$li("If you are prompted to sign in, click on", strong("Microsoft account "), "then ", strong("Sign in. "), "Select your ", strong("AD.UNC.EDU" ), "account, and press", strong("Continue.")),
                       tags$li("Click ", strong("Connect.")),
                       tags$li("In the Navigator, on the left side, select ", strong("Mail.")),
                       tags$li("At the bottom, click on ", strong("Transform Data.")),
                       tags$li("In the Power Query Editor, find the ", strong("Folder Path "), "column, and click on the small button with a downward arrow."),
                       tags$li("In pop-up menu, at the bottom right, click on the ", strong("Load more "), "link."),
                       tags$li("Deselect ", strong("(Select All). ")),
                       tags$li("Scroll through the list and put a check next to ", strong("\\Sent Items\\Consults\\."), "Click on ", strong("OK.")),
                       tags$li("In the ribbon at the top, click on ", strong("Close & Load."), "Select ", strong("Close & Load.")),
                       tags$li("Save the spreadsheet as a ", strong("CSV file."))
               ),
               h5("Upload the E-mail CSV file to AutoStats."),
               tags$ol(class = "steps",
                       tags$li("In Autostats, navigate to ", strong("E-mails.")),
                       tags$li("Fill out the form and upload the CSV file you saved in Excel. Click ", strong("Submit.")),
                       tags$li("If the correct file has been submitted, a ", strong("Download"), "button will appear. Click on it to download a file for LibInsight.")
               ),
               h5("Upload the AutoStats CSV file to LibInsight."),
               tags$ol(class = "steps",
                       tags$li("In LibApps, use the dropdown menu to navigate to", strong("LibInsight.")),
                       tags$li("Select the ", strong("Digital Research Services "), "dataset."),
                       tags$li("From the top menu, select ", strong("Record Data.")),
                       tags$li("Click the button labeled ", strong("Upload File.")),
                       tags$li("Scroll to the bottom and click on the ", strong("File Upload"), "panel."),
                       tags$li("Click on the ", strong("Choose File"), "button, and select the file you downloaded from AutoStats. You do not need to make any other changes to the form."),
                       tags$li("Click ", strong("Upload data."))
               ),
               hr(),
               h4(id = "instreq",
                  "How to generate and upload instruction request stats"),
               div(class = "alert",
                   h6(icon("triangle-exclamation"), class = "alert-title", "WARNING"),
                   p(class="alert-content",
                     "AutoStats cannot determine what an instruction session is about. The fields, ", strong("Was this continuing education?, Was this instruction about University History?")," and ", strong("Was this instruction focused on AI / Generative AI?"), "will be left blank. If you wish to add this information, it is reccommended you edit the record in LibInsight after uploading."
                   )
               ),
               h5("Create text files from instruction request forms."),
               tags$ol(class = "steps",
                       tags$li("Highlight the contents of an instruction request form, then copy and paste them into a ", strong("plain text file"), "using a text editor. Don't worry about any blank lines or extra spaces."),
                       tags$li("Locate the ", strong("Preferred Dates"), "in the text, and ", strong("delete any dates during which you did NOT provide instruction. "), "There should only be one date present in the file."),
                       tags$li("Save as a ", strong("TXT file.")),
                       tags$li("Repeat this for each instruction request you want to record, saving them all as separate TXT files in a single folder.")
               ),
               h5("Upload the text files to AutoStats."),
               tags$ol(class = "steps",
                       tags$li("In Autostats, navigate to ", strong("Instruction Requests")),
                       tags$li("Fill out the form and upload ALL of the text files, using SHIFT and CTRL keys to select multiple files at a time. Click ", strong("Submit.")),
                       tags$li("If the correct files have been submitted, a ", strong("Download"), "button will appear. Click on it to download a file for LibInsight.")
               ),
               h5("Upload the AutoStats CSV file to LibInsight."),
               tags$ol(class = "steps",
                       tags$li("In LibApps, use the dropdown menu to navigate to", strong("LibInsight.")),
                       tags$li("Select the ", strong("Instruction "), "dataset."),
                       tags$li("From the top menu, select ", strong("Record Data.")),
                       tags$li("Click the button labeled ", strong("Upload File.")),
                       tags$li("Scroll to the bottom and click on the ", strong("File Upload"), "panel."),
                       tags$li("Click on the ", strong("Choose File"), "button, and select the file you downloaded from AutoStats. You do not need to make any other changes to the form."),
                       tags$li("Click ", strong("Upload data."))
               )
               )

# UI ----
ui <- fluidPage(
  
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  title = paste("AutoStats", vn),
  
  div(class = "title", 
      titlePanel(title = "AutoStats", windowTitle = "AutoStats 3.0"),
      div(class = "version", vn)
      ),
  
  # navigation bar
  navset_pill_list( 
    id = "navBar",
    nav_item("What type of stats do you want to generate?"),
    nav_panel(icon = icon("calendar"), "LibCal Appointments", ui_libcal), 
    nav_panel(icon = icon("envelope"), "E-mails", ui_emails), 
    nav_panel(icon = icon("person-chalkboard"), "Instruction Requests", ui_instreq),
    nav_spacer(),
    nav_panel(value = "helpTab", icon = icon("circle-question"), "Help", ui_help)
  ) 
)

# Server ----
server <- function(input, output) {
  
  #Track whether download buttons are present
  libcal_dlShown <- reactiveVal(FALSE)
  email_dlShown <- reactiveVal(FALSE)
  instreq_dlShown <- reactiveVal(FALSE)

  # Help links
  observeEvent(input$helpLink, {
    nav_select("navBar", selected = "helpTab")
  })

# LibCal ----
  
  # Make the download buttons appear after the submit buttons are clicked
  # (while pausing for spinner)
  observeEvent(input$submit_libcal, {
    
    # Track whether errors are present in the file
    libcal_noError <- reactiveVal(TRUE)
    
    # Check for problems with the uploaded file
    tryCatch({
      read_csv(input$libcalUp$datapath) |> 
        select(Date, `What is the topic of your consultation?`, `What can I help you with during our consult? Please provide as much information as possible so I can prepare.`)
    }, 
    error = function(e) {
      libcal_noError(FALSE)
    })
    
    # Show error messages for missing input
    output$lcErrorMsg <- renderText({
      validate(
        need(input$lcFirst, "Please enter a first name."),
        need(input$lcLast, "Please enter a last name."),
        need(libcal_noError(), "There is a problem with the upload. Please make sure you've chosen a valid LibCal CSV file.")
      )
    })

    # Only show the download button if it isn't there already 
    # and all necessary information is included
    req(input$lcFirst)
    req(input$lcLast)
    req(input$libcalUp)
    req(libcal_noError())
    req(!libcal_dlShown())

    showSpinner("lcSpinnerRow")
    Sys.sleep(3)
    hideSpinner("lcSpinnerRow")
    
    insertUI(
      selector = ".lcButtonRow",
      where = "afterEnd",
      ui = fluidRow(
        class = "lcEndbuttons",
        column(12,
               p(strong("Click on the buttons below to download a file for LibInsight or start over.")),
               downloadButton("libcalDown", label = "Download"),
               actionButton("lcReset", "Start Over", icon = icon("arrow-rotate-left"))
               )
        )
      )
    
    libcal_dlShown(TRUE) 

    })
  
  # Transformation of LibCal file into LibInsights file
  libcalDown <- reactive({
    
    req(input$libcalUp)
    
    libCaldf <- read_csv(input$libcalUp$datapath) |> 
      filter(Status != "Cancelled") |> 
      select(Date, `Start Time`, `What is the topic of your consultation?`, `What can I help you with during our consult? Please provide as much information as possible so I can prepare.`) |> 
      mutate(`Start Date` = paste(Date, `Start Time`)) |>
      mutate(`Start Date` = format(as_datetime(`Start Date`), "%Y-%m-%d %H:%M")) |> 
      select(-c(Date, `Start Time`)) |> 
      rename(`Topic(s) of Consultation:` = `What is the topic of your consultation?`) |> 
      mutate(`Topic(s) of Consultation:` = str_replace_all(`Topic(s) of Consultation:`,",", ";")) |> 
      mutate(`Topic(s) of Consultation:` = str_replace_all(`Topic(s) of Consultation:`,"N/A", "")) |>
      rename(`Additional Information` = `What can I help you with during our consult? Please provide as much information as possible so I can prepare.`) |> 
      mutate(`Internal Notes` = paste("Created with AutoStats", vn)) |> 
      mutate(`Entered By` = paste(input$lcLast, input$lcFirst, sep = ", ")) |> 
      mutate(`What type of question is this?` = "Research/Reference")|> 
      mutate(`Who answered this question (Name(s))` = paste(input$lcLast, input$lcFirst)) |> 
      mutate(`Who in DRS answered this question?` = "Staff")
    
    print(libCaldf)
    libCaldf
    
  })
  
  libcalDownName <- paste0("AutoStats-LibCal-", Sys.Date(), ".csv") 
  
  # Provide the LibInsights file
  output$libcalDown <- downloadHandler(
    filename = libcalDownName,
    content = function(file) {
      write_csv(libcalDown(), file, na = "")
    }
  )
  
  # Reset the form when starting over
  observeEvent(input$lcReset, {
    reset("lcFirst")
    reset("lcLast")
    reset("libcalUp")
    removeUI(selector = ".lcEndbuttons")
    libcal_dlShown(FALSE)
  })

# Emails ----
  
  # Make the download buttons appear after the submit buttons are clicked
  # (while pausing for spinner)
  observeEvent(input$submit_email, {
    
    # Track whether errors are present in the file
    email_noError <- reactiveVal(TRUE)
    
    # Check for problems with the uploaded file
    tryCatch({
      read_csv(input$emailUp$datapath) |> 
        select(Subject, DateTimeSent)
    }, 
    error = function(e) {
      email_noError(FALSE)
    })
    
    # Show error messages for missing input
    output$emErrorMsg <- renderText({
      validate(
        need(input$emFirst, "Please enter a first name."),
        need(input$emLast, "Please enter a last name."),
        need(email_noError(), "There is a problem with the upload. Please make sure you've chosen a valid CSV file for e-mails.")
      )
    })
    
    # Only show the download button if it isn't there already 
    # and all necessary information is included
    req(input$emFirst)
    req(input$emLast)
    req(input$emailUp)
    req(email_noError())
    req(!email_dlShown())
    
    showSpinner("emSpinnerRow")
    Sys.sleep(3)
    hideSpinner("emSpinnerRow")
    
    insertUI(
      selector = ".emButtonRow",
      where = "afterEnd",
      ui = fluidRow(
        class = "emEndbuttons",
        column(12,
               p(strong("Click on the buttons below to download a file for LibInsight or start over.")),
               downloadButton("emailDown", label = "Download"),
               actionButton("emReset", "Start Over", icon = icon("arrow-rotate-left"))
        )
      )
    )
    
    email_dlShown(TRUE) 
    
  })
  
  # Transformation of Email file into LibInsights file
  emailDown <- reactive({
    
    req(input$emailUp)
    
    emaildf <- read_csv(input$emailUp$datapath) |> 
      select(Subject, DateTimeSent) |> 
      mutate(`Start Date` = format(mdy_hm(DateTimeSent), "%Y-%m-%d %H:%M")) |> 
      rename(`Additional Information` = Subject) |> 
      select(`Start Date`, `Additional Information`) |>
      mutate(`Topic(s) of Consultation:` = "") |>
      mutate(`Internal Notes` = paste("Created with AutoStats", vn, "(e-mail consultation)")) |>
      mutate(`Entered By` = paste(input$emLast, input$emFirst, sep = ", ")) |>
      mutate(`What type of question is this?` = "Research/Reference") |>
      mutate(`Who answered this question (Name(s))` = paste(input$emLast, input$emFirst)) |>
      mutate(`Who in DRS answered this question?` = "Staff")
    
    emaildf
    
  })
  
  emailDownName <- paste0("AutoStats-Email-", Sys.Date(), ".csv")
  
  # Provide the LibInsights file
  output$emailDown <- downloadHandler(
    filename = emailDownName,
    content = function(file) {
      write_csv(emailDown(), file, na = "")
    }
  )
  
  # Reset the form when starting over
  observeEvent(input$emReset, {
    reset("emFirst")
    reset("emLast")
    reset("emailUp")
    removeUI(selector = ".emEndbuttons")
    email_dlShown(FALSE)
  })
  
# Instruction Requests ----
  
  # Make the download buttons appear after the submit buttons are clicked
  # (while pausing for spinner)
  observeEvent(input$submit_instreq, {
    
    # Track whether errors are present in the files
    instreq_noError <- reactiveVal(TRUE)
    
    # Check for problems with the uploaded files
    tryCatch({
      files <- input$instreqUp
      for(i in 1:nrow(files)){
        text <- read_file(files$datapath[i])
        check <- str_detect(text, "Would you like this instruction taught in-person\\?")
        if (check == FALSE) {
          instreq_noError(FALSE)
          print("failed check")
        }
      }
    }, 
    error = function(e) {
      instreq_noError(FALSE)
      print("failed trycatch")
      print(e$message)
    })
    
    # Show error messages for missing input
    output$irErrorMsg <- renderText({
      validate(
        need(input$irFirst, "Please enter a first name."),
        need(input$irLast, "Please enter a last name."),
        need(instreq_noError(), "There is a problem with the upload. Please make sure you've chosen valid text files for instruction requests.")
      )
    })
    
    # Only show the download button if it isn't there already 
    # and all necessary information is included
    req(input$irFirst)
    req(input$irLast)
    req(input$instreqUp)
    req(instreq_noError())
    req(!instreq_dlShown())
    
    showSpinner("irSpinnerRow")
    Sys.sleep(3)
    hideSpinner("irSpinnerRow")
    
    insertUI(
      selector = ".irButtonRow",
      where = "afterEnd",
      ui = fluidRow(
        class = "irEndbuttons",
        column(12,
               p(strong("Click on the buttons below to download a file for LibInsight or start over.")),
               downloadButton("instreqDown", label = "Download"),
               actionButton("irReset", "Start Over", icon = icon("arrow-rotate-left"))
        )
      )
    )
    
    instreq_dlShown(TRUE)
    
  })
  
  # Transformation of instruction request files into LibInsights file
  instreqDown <- reactive({
    
    req(input$instreqUp)
    
    rawText <- input$instreqUp
    forms <- list()
    
    # create a dataframe of text files
    # remove blank lines and extra spaces
    for(i in 1:nrow(rawText)){
      text <- read_file(rawText$datapath[i]) |> 
        str_replace_all("[\\\r\\\n]", " ") |> 
        str_replace_all("\\s+", " ")
      forms[i] <- text
    }
    
    # create outgoing dataframe
    instreqdf <- rawText  |>
      mutate(form = forms) |>
      select(form) |>
      mutate(date = str_extract(form, "[0-9]{2}[/][0-9]{2}[/][0-9]{4}")) |>
      mutate(time = str_extract(form, "[0-9]{2}[:][0-9]{2}[\\s][a-z]{2}")) |>
      mutate(datetime = paste(date, time)) |>
      mutate(datetime = parse_date_time(datetime, "%m-%d-%Y %H:%M %p")) |>
      mutate(`Instruction Date` = format(datetime, "%Y/%m/%d %H:%M")) |>
      mutate(`Internal Notes` = paste("Created with AutoStats", vn, "(instruction request)")) |>
      mutate(course = str_extract(form, "(?<=Number ).+(?= Course)")) |>
      mutate(`Course Department` = str_extract(course, "[a-zA-Z ]+")) |>  
      mutate(`Service Point/Library Department` = paste(input$irLast, input$irFirst)) |>
      mutate(`Instruction Type` = "Course Related (include dept/number)") |>
      mutate(`Course Instructor` = str_extract(form, "(?<=Instructor Name ).+(?= Instructor E)")) |>
      mutate(`Course Instructor Email` = str_extract(form, "(?<=Instructor Email ).+(?= Course D)")) |> 
      mutate(`Duration in minutes` = str_extract(form, "(?<=Class Length )\\d+")) |> 
      mutate(inperson = str_extract(form, "(?<=Would you like this instruction taught in-person\\? )Yes|No")) |> 
      mutate(`Instruction Location`= ifelse(inperson == "Yes", 
                                            "On campus but not in a library", 
                                            "Remote")) |> 
      mutate(`Instruction Type` = "Course Related (include dept/number)") |>
      mutate(`Number of participants` = str_extract(form, "(?<=Number of Students )\\d+")) |> 
      mutate(`Staff Department(s)` = "DRS") |> 
      mutate(`Staff Name(s)` = paste(input$emFirst, input$emLast)) |>
      mutate(`Staff Type` = "Library Staff") |>
      # The field below is populated based on the course numbering conventions listed here: https://catalog.unc.edu/courses/course-numbering/
      mutate(coursenum = as.numeric(str_extract(course, "\\d+"))) |> 
      mutate(`Type of participants`= case_when(
        coursenum <= 399 ~ "UNC Undergraduate",
        coursenum >= 400 & coursenum <= 699 ~ "UNC Graduate Students; UNC Undergraduate",
        coursenum >= 700 ~ "UNC Graduate Students")) |> 
      mutate(`Was this continuing education?` = "") |> 
      mutate(`Was this instruction about University History?` = "") |>
      mutate(`Was this instruction focused on AI / Generative AI?` = "") |> 
      select(-c(form, date, time, datetime, course, inperson, coursenum))
    
    instreqdf
    
  })
  
  instreqDownName <- paste0("AutoStats-Instruction-Requests-", Sys.Date(), ".csv")
  
  # Provide the LibInsights file
  output$instreqDown <- downloadHandler(
    filename = instreqDownName,
    content = function(file) {
      write_csv(instreqDown(), file, na = "")
    }
  )
  
  # Reset the form when starting over
  observeEvent(input$irReset, {
    reset("irFirst")
    reset("irLast")
    reset("instreqUp")
    removeUI(selector = ".irEndbuttons")
    email_dlShown(FALSE)
  })
  
}

shinyApp(ui = ui, server = server)