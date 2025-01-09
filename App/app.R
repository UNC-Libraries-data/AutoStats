#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Created by Lorin Bruckner for UNC Chapel Hill Libraries (lorin.bruckner@unc.edu)

# scrolling
# record "other patron type"

library(shiny)
library(shinyjs)
library(tidyverse)
library(sortable)
library(lubridate)


# ui
ui <- fluidPage(
  
  # CSS and js ----
  tags$head(
    
    tags$style(HTML("
                    body {background-color: #13294B; font-size: 16px}
                    #title {text-align: center; margin: 40px 0; color: #fff;}
                    .autoStats {font-size: 60px; font-weight: 700; color: #3B9BDB; text-shadow: 4px 4px 4px #000;}
                    #first {text-align: center; color: #fff; background-color: #13294B; border: 0px}
                    .header {font-size: 30px; margin-bottom: 40px;}
                    .action-button, .shiny-download-link {padding: 20px; border-radius: 10px; margin: 10px 10px 10px 0; text-transform: uppercase; font-weight:700;}
                    .action-button:focus, .action-button:hover, .shiny-download-link:focus, .shiny-download-link:hover {background-color:#3B9BDB; border-color:#3B9BDB;}
                    .shiny-download-link {padding: 20px 40px;}
                    .stepRowConditional {min-width: 850px; background-color: #d3e8f7; margin: 0px 10% 30px 10%; padding: 60px 40px; border-radius: 20px; border: 2px solid #fff;}
                    .stepNum {margin-right: 40px;}
                    .num {font-size: 80px; font-weight: 700; color: #3B9BDB; text-shadow: 2px 2px 2px #666666; text-align: right;}
                    .stepTitle {font-size: 20px; font-weight: 700; text-align: right;}
                    .stepContent img {border: #cccccc 1px solid; margin-bottom: 40px; max-width:600px}
                    .stepContent p {max-width: 700px;}
                    .stepContent p a {font-weight: 700; color: #23527c; text-decoration: underline;}
                    .note {color: #cd6505; font-style: italic; font-weight: 700;}
                    .nameField {display: inline-block;}
                    .form-group {display: inline;}
                    .radioButtons {margin: 20px 0px}
                    .fileUpload {max-width: 300px}
                    .shiny-output-error-validation {color: #A80604; font-style: italic; font-weight: 700;}
                    .rank-list-container {background-color: #ffffff !important;}
                    .rank-list-title {font-weight:700;}
                    .dragbox {float: left; max-width: 300px}
                    .whitebox {background-color: #ffffff; border: 1px solid #ddd; padding: 10px; margin: 5px;}
                    .submit {clear:both; margin-top: 20px;}
                    .stepFour {height: 250px; line-height: 200px;}
                    .download {vertical-align: middle; line-height: normal; display: inline-block;}
                    @keyframes yellowfade {from {background: #ffffb3;} to {background: #d3e8f7;}}
                    #appt4 {animation-name: yellowfade; animation-duration: 5s;}
                    #emappt4 {animation-name: yellowfade; animation-duration: 5s;}
                    #inst5 {animation-name: yellowfade; animation-duration: 5s;}
                    "))
  ),
  
  # use javascript
  useShinyjs(),
  
  # Title ----
  fluidRow(
    column(12,
           div(id = "title",
               p(class = "autoStats", HTML("AutoStats")),
               p(HTML("<em>Created by Lorin Bruckner for the University of North Carolina Chapel Hill Libraries</em>"))
           )
    )
  ),
  
  
  # Choose Appointments, Emails or Instruction
  hidden(conditionalPanel(class = "stepRowConditional", id = "first",
                   condition = "",
                   fluidRow(
                     column(12,
                            div(
                              p(class = "header", "I want to..."),
                              actionButton("appts", "Upload LibCal Appointments"),
                              actionButton("emails", "Upload Email Consultations"),
                              actionButton("instruction", "Upload Instruction Sessions")
                            )
                     )
                   )
  )),
  
  # Appointments Step 1 ----
  hidden(conditionalPanel(class = "stepRowConditional", id = "appt1",
                   condition = "",
                   fluidRow(class = "stepRow",
                            column(3,
                                   div(class = "stepNum",
                                       p(class = "num", 1),
                                       p(class = "stepTitle", "Download appointments from LibCal.")
                                   )
                            ),
                            column(9,
                                   div(class = "stepContent",
                                       p(HTML("Log into LibCal and click the <strong>Appointments</strong> button. Select the <strong>Booking Explorer</strong> tab.")),
                                       img(src = "images/step1a.jpg"),
                                       p(HTML("Change the date range to include only the appointments you want to upload to LibInsight. You can use Custom Range to select specific dates. Click <strong>GO</strong>.")),
                                       img(src = "images/step1b.jpg"),
                                       p(HTML("Click on the <strong>Export</strong> button and download the CSV file.")),
                                       img(src = "images/step1c.jpg"),
                                       p(class = "note", HTML("NOTE: The CSV file will include cancelled appointments, but AutoStats will remove them and they won't be uploaded to LibInsight."))
                                   )
                            )
                   )
  )),
  
  # Appointments Step 2 ----
  hidden(conditionalPanel(class = "stepRowConditional", id = "appt2",
                   condition = "",
                   fluidRow(class = "stepRow",
                            column(3,
                                   div(class = "stepNum",
                                       p(class = "num", 2),
                                       p(class = "stepTitle", "Fill out the form.")
                                   )
                            ),
                            column(9,
                                   div(class = "stepContent",
                                       p(HTML("Fill out the form below and upload the CSV file you retrieved from LibCal.")),
                                       
                                       # Inputs for Name(s)
                                       div(class = "nameField", textInput("fName", "First Name")),
                                       div(class = "nameField", textInput("lName", "Last Name")),
                                       
                                       # Inputs for Who in DRS answered this question?
                                       div(class = "radioButtons", 
                                           radioButtons("empType", "I am a...", 
                                                        c("Staff Member" = "Staff", 
                                                          "Student Constultant" = "Student Consultant"))),
                                       
                                       # Upload appointments file
                                       div(class = "fileUpload",
                                           fileInput("rawAppt", 
                                                     "Upload LibCal CSV file", 
                                                     multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                                       )
                                   )
                            )
                   )
  )),
  
  # Appointments Step 3 ----
  hidden(conditionalPanel(class = "stepRowConditional", id = "appt3",
                   condition = "",
                   fluidRow(
                     column(3,
                            div(class = "stepNum",
                                p(class = "num", 3),
                                p(class = "stepTitle", "Drag the fields to match.")
                            )
                     ),
                     column(9,
                            div(class = "stepContent",
                                
                                div(id="step3anchor", ""),
                                
                                # drag and drop fields - display only once file is uploaded 
                                div( class = "bucket-list-container default-sortable",
                                     
                                     p(HTML("Look at the list of appointment questions you use in LibCal below. Which ones match up with the questions on LibInsight? Drag the appropriate appointment questions to their matching LibInsight fields. When you're done, click <strong>Submit</strong>.")),
                                     p(class = "note", "NOTE: If you leave these fields blank, they will also be blank on your LibInsight records."),
                                     
                                     div(class = "default-sortable bucket-list bucket-list-horizontal",
                                         
                                         
                                         # appointment questions box
                                         div(class = "dragbox", uiOutput("apptQs", style="flex:1 0 200px;")),
                                         
                                         # LibInsight fields
                                         uiOutput("libInFields", style="flex:1 0 200px;"),
                                         
                                         #submit button
                                         div(class = "submit",
                                             uiOutput("butSubmit")
                                         )
                                     )
                                     
                                )
                                
                            )
                     )
                   )
  )),
  
  # Appointments Step 4 ----
  hidden(conditionalPanel(class = "stepRowConditional", id = "appt4",
                   condition = "",
                   div(id="step4anchor"),
                   fluidRow(
                     column(3,
                            div(class = "stepNum",
                                p(class= "num", 4),
                                p(class = "stepTitle", "Save the AutoStats File.")
                            )
                     ),
                     column(9,
                            div(class = "stepContent stepFour",
                                div(class = "download",
                                    p(HTML("<strong>Click on the button below to download an AutoStats file.</strong>")),
                                    downloadButton("dlFile", "Download")
                                )
                                
                            )
                     )
                   )
  )),
  
  # Appointments Step 5 ----
  hidden(conditionalPanel(class = "stepRowConditional", id = "appt5",
                   condition = "",
                   fluidRow(
                     column(3,
                            div(class = "stepNum",
                                p(class = "num", 5),
                                p(class = "stepTitle", "Upload the AutoStats File to LibInsight.")
                            )
                     ),
                     column(9,
                            div(class = "stepContent",
                                p(HTML("Log into LibInsights. Under Shortcuts, select a dataset from the <strong>Record Data to</strong> menu.")),
                                img(src = "images/step5a.jpg"),
                                p(HTML("Click on the <strong>Record Data button</strong>, and press <strong>Upload File</strong>.")),
                                img(src = "images/step5b.jpg"),
                                p(HTML("Scroll down to the bottom of the page. Click on <strong>Choose File</strong> and select your AutoStats file. Do not make any other selections. Click the <strong>Upload Data</strong> button.")),
                                img(src = "images/step5c.jpg"),
                                p(HTML("Your stats have now been recorded!"))
                            )
                     )
                   ),
                   fluidRow(
                     actionButton("restart", "Start Over")
                   )
  )),
  
  # Emails Step 1 ----
  hidden(conditionalPanel(class = "stepRowConditional", id = "emappt1",
                   condition = "",
                   fluidRow(class = "stepRow",
                            column(3,
                                   div(class = "stepNum",
                                       p(class = "num", 1),
                                       p(class = "stepTitle", "Set up View in Outlook.")
                                   )
                            ),
                            column(9,
                                   div(class = "stepContent",
                                       p(HTML("<strong>Store copies of email consultations in an Outlook folder.</strong> When you're ready to upload your email consults, navigate to that folder. <a href = 'https://smallbusiness.chron.com/emails-between-certain-dates-outlook-79638.html', target = '_blank'>Follow these instructions to find emails within a certain date range.</a>")),
                                       img(src = "images/em_step1a.png"),
                                       p(HTML("Click on <strong>View > Current View > View Settings</strong>.")),
                                       img(src = "images/em_step1b.png"),
                                       p(HTML("In the pop-up window, click <strong>Format Columns...</strong>. In the next window, select <strong>Recieved</strong>. Select the second format option from the list. It should display both the date and time, such as <strong>'8/24/2021 11:23 AM'</strong>. Click <strong>OK</strong> in both windows. ")),
                                       img(src = "images/em_step1c.png")
                                   )
                            )
                   )
  )),
  
  # Emails Step 2 ----
  hidden(conditionalPanel(class = "stepRowConditional", id = "emappt2",
                   condition = "",
                   fluidRow(class = "stepRow",
                            column(3,
                                   div(class = "stepNum",
                                       p(class = "num", 2),
                                       p(class = "stepTitle", "Copy emails from Outlook to Excel.")
                                   )
                            ),
                            column(9,
                                   div(class = "stepContent",
                                       p(HTML("Use the <strong>shift key</strong> to highlight all of the emails you want to upload and press <strong>CTRL+C</strong> to copy them.")),
                                       img(src = "images/em_step2a.png"),
                                       p(HTML("Open a new workbook in Excel. Click on the first cell and press <strong>CTRL+V</strong> to paste your records into the spreadsheet.")),
                                       img(src = "images/em_step2b.png"),
                                       p(HTML("Save the file as <strong>CSV UTF-8</strong>.")),
                                       img(src = "images/em_step2c.png")
                                   )
                            )
                   )
  )),
  
  # Emails Step 3 ----
  hidden(conditionalPanel(class = "stepRowConditional", id = "emappt3",
                   condition = "",
                   fluidRow(class = "stepRow",
                            id="step3anchor-emails",
                            column(3,
                                   div(class = "stepNum",
                                       p(class = "num", 3),
                                       p(class = "stepTitle", "Fill out the form.")
                                   )
                            ),
                            column(9,
                                   div(class = "stepContent",
                                       p(HTML("Fill out the form below and upload the CSV file you saved in Excel.")),
                                       
                                       # Inputs for Name(s)
                                       div(class = "nameField", textInput("emfName", "First Name")),
                                       div(class = "nameField", textInput("emlName", "Last Name")),
                                       
                                       # Inputs for Who in DRS answered this question?
                                       div(class = "radioButtons", 
                                           radioButtons("em_empType", "I am a...", 
                                                        c("Staff Member" = "Staff", 
                                                          "Student Constultant" = "Student Consultant"))),
                                       
                                       # Upload appointments file
                                       div(class = "fileUpload",
                                           fileInput("rawEmails", 
                                                     "Upload CSV file", 
                                                     multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                                       ),
                                       
                                       conditionalPanel(condition = "output.gotEmails", div(uiOutput("emVal")))
                                       
                                   )
                            )
                   )
  )),
  
  # Emails Step 4 ----
  hidden(conditionalPanel(class = "stepRowConditional",
                   id = "emappt4",
                   condition = "",
                   div(id="step4anchor-emails", ""),
                   fluidRow(
                     column(3,
                            div(class = "stepNum",
                                p(class= "num", 4),
                                p(class = "stepTitle", "Save the AutoStats File.")
                            )
                     ),
                     column(9,
                            div(class = "stepContent stepFour",
                                div(class = "download",
                                    p(HTML("<strong>Click on the button below to download an AutoStats file.</strong>")),
                                    downloadButton("dlEmailFile", "Download")
                                )
                                
                            )
                     )
                   )
  )),
  
  # Emails Step 5 ----
  hidden(conditionalPanel(class = "stepRowConditional", id = "emappt5",
                   condition = "",
                   fluidRow(
                     column(3,
                            div(class = "stepNum",
                                p(class = "num", 5),
                                p(class = "stepTitle", "Upload the AutoStats File to LibInsight.")
                            )
                     ),
                     column(9,
                            div(class = "stepContent",
                                p(HTML("Log into LibInsights. Under Shortcuts, select a dataset from the <strong>Record Data to</strong> menu.")),
                                img(src = "images/step5a.jpg"),
                                p(HTML("Click on the <strong>Record Data button</strong>, and press <strong>Upload File</strong>.")),
                                img(src = "images/step5b.jpg"),
                                p(HTML("Scroll down to the bottom of the page. Click on <strong>Choose File</strong> and select your AutoStats file. Do not make any other selections. Click the <strong>Upload Data</strong> button.")),
                                img(src = "images/step5c.jpg"),
                                p(HTML("Your stats have now been recorded!"))
                            )
                     )
                   ),
                   fluidRow(
                     actionButton("emRestart", "Start Over")
                   )
  )),

  # Instruction Step 1 ----
  hidden(conditionalPanel(class = "stepRowConditional", id = "inst1",
                          condition = "",
                          fluidRow(class = "stepRow",
                                   column(3,
                                          div(class = "stepNum",
                                              p(class = "num", 1),
                                              p(class = "stepTitle", "Copy instruction request form.")
                                          )
                                   ),
                                   column(9,
                                          div(class = "stepContent",
                                              p(HTML("<strong>Highlight all of the text</strong> in the instruction request form in your email.")),
                                              p(HTML("<strong>Right-click</strong> and select <strong>copy</strong>.")),
                                              img(src = "images/inst_step1.png")
                                          )
                                   )
                          )
  )),
  # Instruction Step 2 ----
  hidden(conditionalPanel(class = "stepRowConditional", id = "inst2",
                          condition = "",
                          fluidRow(class = "stepRow",
                                   column(3,
                                          div(class = "stepNum",
                                              p(class = "num", 2),
                                              p(class = "stepTitle", "Paste as plain text.")
                                          )
                                   ),
                                   column(9,
                                          div(class = "stepContent",
                                              p(HTML("<strong>Open a text editor</strong> and <strong>paste the text</strong> into a new file.")),
                                              p(HTML("Do not inlcude formatting. It should look similar to the screenshot below:")),
                                              img(src = "images/inst_step2.png")
                                          )
                                   )
                          )
  )),
  # Instruction Step 3 ----
  hidden(conditionalPanel(class = "stepRowConditional", id = "inst3",
                          condition = "",
                          fluidRow(class = "stepRow",
                                   column(3,
                                          div(class = "stepNum",
                                              p(class = "num", 3),
                                              p(class = "stepTitle", "Save the text file.")
                                          )
                                   ),
                                   column(9,
                                          div(class = "stepContent",
                                              p(HTML("<strong>Save</strong> as a <strong>.txt</strong> file.")),
                                              p(HTML("The <strong>file name</strong> should include the <strong>date</strong> that the instruction session <strong>actually occured</strong>.")),
                                              p(HTML("<strong>You must use the following naming convention: instruction_mm-dd-yyyy.</strong> See the screenshot below:")),
                                              img(src = "images/inst_step3.png")
                                          )
                                   )
                          )
  )),
  
  # Instruction Step 4 ----
  hidden(conditionalPanel(class = "stepRowConditional", id = "inst4",
                          condition = "",
                          fluidRow(class = "stepRow",
                                   id="step4anchor-inst",
                                   column(3,
                                          div(class = "stepNum",
                                              p(class = "num", 4),
                                              p(class = "stepTitle", "Upload text files.")
                                          )
                                   ),
                                   column(9,
                                          div(class = "stepContent",
                                              p(HTML("Fill out the form below and upload <strong>ALL</strong> of the text files.")),
                                              p(class = "note", "NOTE: To select multiple files at once, hold down the shift key while clicking."),
                                              
                                              # Inputs for Name(s)
                                              div(class = "nameField", textInput("instfName", "First Name")),
                                              div(class = "nameField", textInput("instlName", "Last Name")),
                                              
                                              # Inputs for Who in DRS answered this question?
                                              div(class = "radioButtons", 
                                                  radioButtons("inst_empType", "I am a...", 
                                                               c("Staff Member" = "Library Staff", 
                                                                 "Student Consultant" = "Student Assistant"))),
                                              
                                              # Upload appointments file
                                              div(class = "fileUpload",
                                                  fileInput("rawText", 
                                                            "Upload all text files", 
                                                            multiple = TRUE, accept = ".txt")
                                              ),
                                              
                                              conditionalPanel(condition = "output.gotText", div(uiOutput("instVal")))
                                              
                                          )
                                   )
                          )
  )),
  
  # Instruction Step 5 ----
  hidden(conditionalPanel(class = "stepRowConditional",
                          id = "inst5",
                          condition = "",
                          div(id="step5anchor-inst", ""),
                          fluidRow(
                            column(3,
                                   div(class = "stepNum",
                                       p(class= "num", 5),
                                       p(class = "stepTitle", "Save the AutoStats File.")
                                   )
                            ),
                            column(9,
                                   div(class = "stepContent stepFour",
                                       div(class = "download",
                                           p(HTML("<strong>Click on the button below to download an AutoStats file.</strong>")),
                                           downloadButton("dlInstFile", "Download")
                                       )
                                       
                                   )
                            )
                          )
  )),
  
  # Instruction Step 6 ----
  hidden(conditionalPanel(class = "stepRowConditional", id = "inst6",
                          condition = "",
                          fluidRow(
                            column(3,
                                   div(class = "stepNum",
                                       p(class = "num", 6),
                                       p(class = "stepTitle", "Upload the AutoStats File to LibInsight.")
                                   )
                            ),
                            column(9,
                                   div(class = "stepContent",
                                       p(HTML("Log into LibInsights. Under Shortcuts, select a dataset from the <strong>Record Data to</strong> menu.")),
                                       img(src = "images/step5a.jpg"),
                                       p(HTML("Click on the <strong>Record Data button</strong>, and press <strong>Upload File</strong>.")),
                                       img(src = "images/step5b.jpg"),
                                       p(HTML("Scroll down to the bottom of the page. Click on <strong>Choose File</strong> and select your AutoStats file. Do not make any other selections. Click the <strong>Upload Data</strong> button.")),
                                       img(src = "images/step5c.jpg"),
                                       p(HTML("Your stats have now been recorded!"))
                                   )
                            )
                          ),
                          fluidRow(
                            actionButton("instRestart", "Start Over")
                          )
  ))

)




# server ----
server <- function(input, output, session) {
  
  ###### HOME PAGE -----

  #Show the right steps at the right time
  showElement("first")
  
  observeEvent(input$appts, {
    hideElement("first")
    showElement("appt1")
    showElement("appt2")
  })
  
  observeEvent(input$emails, {
    hideElement("first")
    showElement("emappt1")
    showElement("emappt2")
    showElement("emappt3")
  })
  
  observeEvent(input$instruction, {
    hideElement("first")
    showElement("inst1")
    showElement("inst2")
    showElement("inst3")
    showElement("inst4")
  })

  ###### LIBCAL APPOINTMENTS ------  
  
  #Function for renaming status fields. X is the uploaded libcal spreadsheet.
  #Returns a dataframe with new names.
  renStat <- function(x){
    
    lCols <- tolower(names(x))
    statCols <- which(str_detect(lCols, "status"))
    statN <- length(statCols)
    sc1 <- statCols[1]
    
    names(x)[sc1] <- "Appt Status"
    
    if(statN > 1) { 
      sc2 <- statCols[2]
      names(x)[sc2] <- "Status"
    }
    
    return(x)
  }
  
  # check if file is uploaded
  output$gotFile <- reactive({
    return(!is.null(input$rawAppt))
  })
  outputOptions(output, "gotFile", suspendWhenHidden = FALSE)
  
  # jump to step 3 after a file is uploaded
  observeEvent(input$rawAppt, {
    showElement("appt3")
    delay(1000, runjs("document.getElementById('step3anchor').scrollIntoView({behavior: 'smooth'});"))
  })
  
  # check if dragging is finished
  output$doneDragging <- reactive({
    return(names(dfOutFile()[1]) == "Start Date")
  })
  outputOptions(output, "doneDragging", suspendWhenHidden = FALSE)
  
  # jump to step 4 after dragging is done
  observeEvent(input$send, {
    showElement("appt4")
    showElement("appt5")
    delay(1000, runjs("document.getElementById('step4anchor').scrollIntoView({behavior: 'smooth'});"))
  })
  
  # make sure CSV file has the right columns
  validateCSV <- function(fPath, blank = FALSE) {
    
    csvNames <- names(read_csv(fPath))
    
    if (str_sub(tolower(fPath), -3) == "csv") {
      if (csvNames[1] != "Booking ID" &
          csvNames[2] != "With" &
          csvNames[3] != "Name") {
        if(blank == FALSE) {        
          "!!ERROR ENCOUNTERED!! The file you uploaded doesn't have the correct columns. Please scroll up and upload a different file."
        }
      }
    }
  }
  
  # get appointment questions from file
  questList <- reactive({
    dfInFile <- renStat(read_csv(input$rawAppt$datapath))
    questStart <- which(names(dfInFile) == "Internal Notes") + 1
    questNames <- names(dfInFile[questStart:length(dfInFile)])
    return(questNames)
  })
  
  # generate appointment questions list
  output$apptQs <- renderUI({
    
    # make sure we have a name and the file is correct
    validate(
      need(input$fName, "Please enter your first name in Step 2."),
      need(input$lName, "Please enter your last name in Step 2."),
      need(str_sub(tolower(input$rawAppt$datapath), -3) == "csv", "The file you uploaded isn't a CSV file. Please upload a different file in step 2."),
      validateCSV(input$rawAppt$datapath)
    )
    
    labels <- questList()
    
    rank_list(
      text = "Your Appointment Questions",
      labels = labels,
      input_id = "inQuest",
      options = sortable_options(group = "questions", sort = FALSE)
    )
    
  })
  
  # generate libInsight fields
  output$libInFields <- renderUI({
    
    # make sure we have a name and the file is correct
    validate(
      need(input$fName, ""),
      need(input$lName, ""),
      need(str_sub(tolower(input$rawAppt$datapath), -3) == "csv", ""),
      validateCSV(input$rawAppt$datapath, blank = TRUE)
    )
    
    div(class = "dragbox whitebox", 
        p(HTML("<strong>LibInsight Questions</strong>")),
        div (
          # Patron Type field box
          rank_list(
            text = "Patron Type",
            labels = list(),
            input_id = "patronType",
            options = sortable_options(group = list(name = "questions", 
                                                    put = htmlwidgets::JS("function(to) { return to.el.children.length < 1; }")
            )
            )
          ),
          
          # Department field box
          rank_list(
            text = "Department",
            labels = list(),
            input_id = "dept",
            options = sortable_options(group = list(name = "questions", 
                                                    put = htmlwidgets::JS("function(to) { return to.el.children.length < 1; }")
            )
            )
          ),
          
          # Additional Information (optional) field box
          rank_list(
            text = "Additional Information (optional)",
            labels = list(),
            input_id = "addInfo",
            options = sortable_options(group = list(name = "questions", 
                                                    put = htmlwidgets::JS("function(to) { return to.el.children.length < 1; }")
            )
            )
          )
          
        )
    )
  })
  
  # submit button
  output$butSubmit <- renderUI({
    
    # make sure we have a name and the file is correct
    validate(
      need(input$fName, ""),
      need(input$lName, ""),
      need(str_sub(tolower(input$rawAppt$datapath), -3) == "csv", ""),
      validateCSV(input$rawAppt$datapath, blank = TRUE)
    )
    
    actionButton("send", "Submit")
  })
  
  dfOutFile <- eventReactive(input$send, {
    
    # read the csv
    dfInFile <- renStat(read_csv(input$rawAppt$datapath))
    
    # get user's full name
    name <- reactive({
      fullname <- paste(input$fName, input$lName)
      return(fullname)
    })
    
    # remove double spaces in column names because the sortable package
    # automatically removes them from any input fields
    names(dfInFile) <- str_replace(names(dfInFile), "  ", " ")
    
    # create outgoing dataframe
    dfOutFile <- dfInFile %>% 
      filter(`Appt Status` != "Cancelled") %>% 
      mutate(`Date` = parse_date_time(Date, orders = c("mdy","ymd"), tz = "America/New_York")) %>% 
      mutate(`Start Date` = paste(Date,`Start Time`)) %>% 
      mutate(`Internal Notes` = paste("AutoStats upload; Booking ID:", `Booking ID`)) %>% 
      mutate(`Entered By` = "Hub, Research") %>% 
      mutate(`More Details` = "") %>% 
      mutate(`Name(s)` = name()) %>%
      mutate(`Other Details` = "") %>% 
      mutate(`Other Referral:` = "") %>% 
      mutate(`Referred to:` = "") %>%
      mutate(`Was this a non-desk question?` = "Yes") %>%
      mutate(`Was this an email question?` = "") %>%
      mutate(`Was this remote?` = "Yes") %>%
      mutate(`What type of question is this?` = "Research/Reference") %>%
      mutate(`Who in DRS answered this question?` = input$empType) %>% 
      select(`Start Date`, `Internal Notes`, `Entered By`, `Additional Information (optional)` = input$addInfo,
             `Department` = input$dept, `More Details`, `Name(s)`, `Other Details`, `Other Referral:`, `Referred to:`, 
             `Was this a non-desk question?`, `Was this an email question?`, `Was this remote?`, 
             `What type of question is this?`, `Who in DRS answered this question?`, `Patron Type` = input$patronType)
    
    # handle curly quotes
    dfOutFile$`Additional Information (optional)` <- str_replace_all(dfOutFile$`Additional Information (optional)`, 
                                                                     c("[“”]" = "\"", "[‘’]" = "'"))
    
    return(dfOutFile)
    
  })
  
  output$dlFile <- downloadHandler(
    filename = paste("AutoStats-", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      write_csv(dfOutFile(), file, na = "")
    }
  )
  
  ###### EMAIL CONSULTS -----
  
  observeEvent(input$rawEmails, {
    
    # stop leaving step 3 while validating
    runjs("document.getElementById('step3anchor-emails').scrollIntoView();")
    
    # validate CSV
    output$emVal <- renderUI({
      
      # make sure we have a name and the file is correct
      validate(
        need(input$emfName, "Please enter your first name."),
        need(input$emlName, "Please enter your last name."),
        need(str_sub(tolower(input$rawEmails$datapath), -3) == "csv", "The file you uploaded isn't a CSV file. Please upload a different file."),
        validateEmailCSV(input$rawEmails$datapath)
      )
      
      # proceed to step 4
      output$validated <- reactive({
        return(TRUE)
      })
      
      showElement("emappt4")
      showElement("emappt5")
      delay(500, runjs("document.getElementById('step4anchor-emails').scrollIntoView({behavior: 'smooth'});"))
      
    })
  })
  
  # make sure CSV file has the right columns
  validateEmailCSV <- function(fPath) {
    
    emfile <- read_csv(fPath)

    if (str_sub(tolower(fPath), -3) == "csv") {
      if (!("Received" %in% names(emfile))) {
        "The file you uploaded doesn't have the correct columns. Please upload a different file."
      } else if (FALSE %in% str_detect(emfile$Received, "/") | 
                 FALSE %in% str_detect(emfile$Received, ":"))
      {
        "The file you uploaded doesn't have the correct date format. Please follow step 1 to obtain the correct date information from Outlook."
      }
      
    }
  }
  
  output$dlEmailFile <- downloadHandler(
    
    filename = paste("AutoStatsEmails-", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      
      # read the csv
      dfInFile <- read_csv(input$rawEmails$datapath, 
                           name_repair = ~ make.names(., unique = TRUE))
      
      #get user's full name
      name <- reactive({
        fullname <- paste(input$emfName, input$emlName)
        return(fullname)
      })
      
      # create outgoing dataframe
      dfOutFileEm <- dfInFile %>%
        separate(Received, into = c("sepdate", "septime"), sep = " ") %>%
        separate(sepdate, into = c("m", "d", "y"), sep = "/" ) %>% 
        unite(`newdate`, c("y", "m", "d"), sep = "-") %>%
        unite(`Start Date`, c("newdate", "septime"), sep = " ") %>% 
        mutate(`Internal Notes` = "AutoStats upload; Email consultation") %>% 
        mutate(`Entered By` = "Hub, Research") %>%
        mutate(`Additional Information (optional)` = "") %>%
        mutate(`Department` = "") %>%
        mutate(`More Details` = "") %>% 
        mutate(`Name(s)` = name()) %>%
        mutate(`Other Details` = "") %>% 
        mutate(`Other Referral:` = "") %>% 
        mutate(`Referred to:` = "") %>%
        mutate(`Was this a non-desk question?` = "Yes") %>%
        mutate(`Was this an email question?` = "Yes") %>%
        mutate(`Was this remote?` = "Yes") %>%
        mutate(`What type of question is this?` = "Research/Reference") %>%
        mutate(`Who in DRS answered this question?` = input$em_empType) %>%
        mutate(`Patron Type` = "") %>% 
        select(`Start Date`, `Internal Notes`, `Entered By`, `Additional Information (optional)`, 
               `Department`, `More Details`, `Name(s)`, `Other Details`, `Other Referral:`, `Referred to:`, 
               `Was this a non-desk question?`, `Was this an email question?`, `Was this remote?`, 
               `What type of question is this?`, `Who in DRS answered this question?`, `Patron Type`)
      
      write_csv(dfOutFileEm, file, na = "")
      
    }
  )
  
  ###### INSTRUCTION SESSIONS -----
  
  observeEvent(input$rawText, {
    
    # stop leaving step 4 while validating
    runjs("document.getElementById('step4anchor-inst').scrollIntoView();")
    
    # validate TXT
    output$instVal <- renderUI({

      # make sure we have a name and the files are correct
      validate(
        need(input$instfName, "Please enter your first name."),
        need(input$instlName, "Please enter your last name."),
        validateInstTXT(input$rawText)
      )

      # proceed to step 5
      output$validated <- reactive({
        return(TRUE)
      })

      showElement("inst5")
      showElement("inst6")
      delay(500, runjs("document.getElementById('step5anchor-inst').scrollIntoView({behavior: 'smooth'});"))

    })
  })
  
  # make sure files are TXT and have the right content
  validateInstTXT <- function(x) {

    for(i in 1:nrow(x)){
      
      fName <- x$name[i]
      content <- read_file(x$datapath[i])

      if (str_sub(tolower(fName), -3) != "txt") {
        return(paste(fName, "isn't a TXT file. Please upload a different file."))
      } else if (str_sub(content, 1, 13) != "Type of Class") {
        return(paste(fName, "doesn't appear to be an instruction request. Please upload a different file."))
      } else if (!str_detect(fName, "instruction_\\d{2}-\\d{2}-\\d{4}.txt")) {
        return(paste(fName, "doesn't follow required naming conventions for instruction session uploads. Please check Step 3 and rename the file."))
      }
      
    }
    
  }
  
  output$dlInstFile <- downloadHandler(
    
    filename = paste("AutoStatsInstruction-", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      
      # create a dataframe from the text files
      rawText <- input$rawText
      forms <- vector()
      
      for(i in 1:nrow(input$rawText)){
        text <- read_file(input$rawText$datapath[i])
        forms <- c(forms, text)
      }
      
      #get user's full name
      name <- reactive({
        fullname <- paste(input$instfName, input$instlName)
        return(fullname)
      })
      
      #get user's account name
      acct <- reactive({
        revname <- paste(input$instlName, input$instfName, sep = ", ")
        return(revname)
      })
      
      # create outgoing dataframe
      dfOutFileInst <- rawText %>%
        mutate(form = forms) %>% 
        mutate(rawDate = str_sub(name, 13, 22)) %>% 
        mutate(rawTime = str_extract(form, "\\d\\d:\\d\\d\\s.+")) %>%
        mutate(rawDateTime = paste(rawDate, rawTime)) %>% 
        mutate(parsedDateTime = parse_date_time(rawDateTime, "%m-%d-%Y %H:%M %p")) %>% 
        mutate(`Instruction Date` = format(parsedDateTime, "%Y/%m/%d %H:%M")) %>%
        mutate(`Internal Notes` = "AutoStats upload; Instruction session") %>% 
        mutate(`Service Point/Library Department` = acct()) %>%
        mutate(course = str_extract(form, "(Course Department and Number\\s+)(.+)", 2)) %>%
        mutate(courseNum = str_extract(course, "\\d+")) %>% 
        mutate(`Course Department` = str_extract(course, "[a-zA-Z ]+")) %>% 
        mutate(`Course Instructor` = str_extract(form, "(Instructor Name\\s+)(.+)", 2)) %>% 
        mutate(`Course Instructor Email` = str_extract(form, "(Instructor Email\\s+)(.+)", 2)) %>% 
        mutate(`Duration in minutes` = str_extract(form, "(Class Length\\s+)(\\d+)", 2)) %>%
        mutate(inPerson = str_extract(form, "(taught in-person\\?\\s+)(.+)", 2)) %>%
        mutate(`Instruction Location`= ifelse(
          inPerson == "Yes", "On campus but not in a library", "Remote")) %>%
        mutate(`Instruction Type` = "Course Related (include dept/number)") %>%
        mutate(`Is this continuing education?` = "") %>%
        mutate(`Number of participants` = str_extract(form, "(Number of Students\\s+)(\\d+)", 2)) %>%
        mutate(`Staff Department(s)` = "DRS") %>%
        mutate(`Staff Name(s)` = name()) %>%
        mutate(`Staff Type` = input$inst_empType) %>%
        #This field is populated based on course numbering conventions (https://catalog.unc.edu/courses/course-numbering/)
        mutate(`Type of participants`= case_when(
          courseNum <= 399 ~ "UNC Undergraduate",
          courseNum >= 400 & courseNum <= 699 ~ "UNC Graduate Students; UNC Undergraduate",
          courseNum >= 700 ~ "UNC Graduate Students",)) %>% 
        select(`Instruction Date`, `Internal Notes`, `Service Point/Library Department`, 
               `Course Department`, `Course Instructor`, `Course Instructor Email`, 
               `Duration in minutes`, `Instruction Location`, `Instruction Type`, 
               `Is this continuing education?`, `Number of participants`, 
               `Staff Department(s)`, `Staff Name(s)`, `Staff Type`, 
               `Type of participants`)
      
      write_csv(dfOutFileInst, file, na = "")
      
    }
  )
  ###### RESTART SESSION ----
  
  # Restart buttons
  observeEvent(input$restart, {
    
    # Turn off all steps except the first
    hideElement("appt1")
    hideElement("appt2")
    hideElement("appt3")
    hideElement("appt4")
    hideElement("appt5")
    showElement("first")
    
    # reset inputs
    reset("fName")
    reset("lName")
    reset("rawAppt")
    reset("inQuest")
    reset("patronType")
    reset("dept")
    reset("addInfo")
    
  })
  
  observeEvent(input$emRestart, {
    
    # Turn off all steps except the first
    hideElement("emappt1")
    hideElement("emappt2")
    hideElement("emappt3")
    hideElement("emappt4")
    hideElement("emappt5")
    showElement("first")
    
    # reset inputs
    reset("emfName")
    reset("emlName")
    reset("rawEmails")
    
  })

  observeEvent(input$instRestart, {
    
    # Turn off all steps except the first
    hideElement("inst1")
    hideElement("inst2")
    hideElement("inst3")
    hideElement("inst4")
    hideElement("inst5")
    hideElement("inst6")
    showElement("first")
    
    # reset inputs
    reset("instfName")
    reset("instlName")
    reset("rawText")
  })
  
  
  # Close tab
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

