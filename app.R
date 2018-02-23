library(shiny)
library(readr)
library(tools)
library(rdrop2)


ui <- fluidPage(
   titlePanel("Abstract ReviewR"),

   # Sidebar with email entry
   sidebarLayout(
      sidebarPanel(
         textInput("email","Please enter your email."),
         passwordInput("key","Please enter provided access key."),
         actionButton("emailconf", "Confirm Email and Access Key"),
         mainPanel(h3("Instructions:"), 
                   h6("Hello Esteemed Meta-analysis reviewer!

                      We are performing a meta-analysis of studies exploring the connection between high fat diet and the microbiome. We would like to include studies where distal gut microbiota was analyzed via sequencing and where a principal aim of the study was to analyze the influence of a high calorie diet on distal gut microbiota (this does not have to be the only aim of the study). A challenge for such studies is that often one study may cover more than one topic or the wording authors use in describing their research can be varied (i.e. high fat diet can be referred to as \"western\" or \"ketogenic\" and some studies may use sugar instead of fats). Unbiased abstract review can greatly help deal with this challenge.
                      
                      You have been selected because of your expertise in such work :). You are reviewing approximately 70 studies out of a collection of 427. A second reviewer will also review the same studies you are looking at. Please look at the following titles and abstracts and determine whether you feel they should be included in a meta-analysis exploring the connection between changes in the distal gut microbiota and conditions that cause diet-induced obesity. An ideal study would fit the following criteria:"),
                      
                      h6("1) Performed sequencing on distal gut microbiota"),
                      h6("2) Is restricted to a rodent model (humanized mice are OK!)"),
                      h6("3) Is primary data (i.e. not a review or a meta-analysis)"),
                      h6("4) Has high-fat diet and control group"),
    
                      h6("Homogeneity will greatly help our meta-analysis. Please attempt to exclude studies that primarily seemed to study other disease states unless you feel they would directly connect to diet-induced obesity.
                      You can include or exclude studies however you would like. You get one opportunity to create a selection for a given study. You can use the above criteria or any other criteria you would like that helps achieve the goal of this meta-analysis.
                      You will be rewarded with positive emotions for your help.")
                   )
      ),
      mainPanel(
         h3("Title"),
          textOutput("title"),
         h3("Authors"),
          textOutput("authors"),
         h3("Journal"),
          textOutput("journal"),
         h3("Abstract"),
          textOutput("abstract"),
         hr(),
         h3("Submit recommendation below. Please note this is final and can not be changed."),
         actionButton("include","Include in Study"),
         actionButton("exclude", "Exclude in Study"),
         hr(),
         textOutput("progress")
      )
   )
)


users<-read_tsv("ReviewerIDs.txt") #not included for anonymity
papers<-read_csv("AssignedReviewers.csv")
validated=FALSE
progress=0
total=0


server <- function(input, output) {

variables=reactiveValues(validated=FALSE,
                         progress=0,
                         total=0,
                         user="",
                         key="",
                         reviewerID="",
                         revlist=data.frame())

observeEvent(input$emailconf, {
  variables$user<-input$email
  variables$key<-input$key

    if(sum(variables$user %in% users$Email)==1){
      if(users[which(users$Email==variables$user),]$Key==variables$key){
        variables$validated=TRUE
        variables$reviewerID=users[which(users$Email==variables$user),]$ReviewerID
        
        if(drop_exists(paste0(variables$reviewerID,"_decisions.txt"))){
          drop_get(paste0(variables$reviewerID,"_decisions.txt"), overwrite = TRUE)
          variables$revlist<-read_tsv(paste0(variables$reviewerID,"_decisions.txt"))
          variables$progress=sum(!is.na(variables$revlist$Decision))+1
        } else {
        variables$revlist<-subset(papers, ReviewerA==variables$reviewerID | ReviewerB==variables$reviewerID)
        variables$revlist$Decision<-NA
        variables$revlist$TimeStamp<-NA
        variables$progress=1
        }

        variables$total=nrow(variables$revlist)
        
      output$title<-renderText({toTitleCase(variables$revlist[variables$progress,]$sorttitle)})
      output$authors<-renderText({variables$revlist[variables$progress,]$authors})
      output$journal<-renderText({variables$revlist[variables$progress,]$fulljournalname})
      output$abstract<-renderText({variables$revlist[variables$progress,]$Abstract})

      }
    } else {
      variables$validated=FALSE
    }
    output$progress<-renderText({paste0(variables$user," Validated=", variables$validated, " Progress: ", variables$progress, " of ", variables$total)})
    })

observeEvent(input$include, {
  if(variables$validated==TRUE){

    variables$revlist[variables$progress,]$Decision<-"Include"
    variables$revlist[variables$progress,]$TimeStamp<-date()
    write_tsv(variables$revlist, paste0(variables$reviewerID,"_decisions.txt"))
    drop_upload(paste0(variables$reviewerID,"_decisions.txt"))

    variables$progress=variables$progress+1
    
    if(variables$progress==(nrow(variables$revlist)+1)){
      showNotification("Oh hey there, did you realize that you are already done? Please close your browser.")
    }
    
    output$title<-renderText({toTitleCase(variables$revlist[variables$progress,]$sorttitle)})
    output$authors<-renderText({variables$revlist[variables$progress,]$authors})
    output$journal<-renderText({variables$revlist[variables$progress,]$fulljournalname})
    output$abstract<-renderText({variables$revlist[variables$progress,]$Abstract})
    output$progress<-renderText({paste0(variables$user, " Progress: ", variables$progress, " of ", variables$total)})

  }
})

observeEvent(input$exclude, {
  if(variables$validated==TRUE){

    #sync result
    variables$revlist[variables$progress,]$Decision<-"Exclude"
    variables$revlist[variables$progress,]$TimeStamp<-date()
    write_tsv(variables$revlist, paste0(variables$reviewerID,"_decisions.txt"))
    drop_upload(paste0(variables$reviewerID,"_decisions.txt"))

    variables$progress=variables$progress+1

    if(variables$progress==(nrow(variables$revlist)+1)){
      showNotification("Oh hey there, did you realize that you are already done? Please close your browser.")
    }
    
    output$title<-renderText({toTitleCase(variables$revlist[variables$progress,]$sorttitle)})
    output$authors<-renderText({variables$revlist[variables$progress,]$authors})
    output$journal<-renderText({variables$revlist[variables$progress,]$fulljournalname})
    output$abstract<-renderText({variables$revlist[variables$progress,]$Abstract})
    output$progress<-renderText({paste0(variables$user, " Progress: ", variables$progress, " of ", variables$total)})

  }
})


}

# Run the application
shinyApp(ui = ui, server = server)

