library(shiny)
library(DT)
library(rsconnect)

#Second Iteration

rsconnect::writeManifest()

#Loading datafiles and creating some shortcuts to make things easier in the output code. 
df <-read.csv("E.csv")
df2 <-read.csv("F13.csv")
df3<-read.csv("F14.csv")
df4 <-read.csv("F1.csv")
df5<-read.csv("F3F4.csv")
df6<-read.csv("F12.csv")

EXCL_ILL <-"ILL"
EXCL_PER <-c("Serial", "ILL")
INC_PER <-"Serial"
INC_VIDEO <-c("DVD", "DVD Nonfiction", "DVD-R", "High Demand DVD-R", "High Demand DVD and Blurays", "Media", "Video")
INC_AUDIO <-c("Audiobook", "High Demand Audiobook","Music")
INC_OTHER <-c("Electronic Device", "Equipment", "Kit", "Realia", "Software")
PHYS <-c("Audiobook", "Book", "Book Club Kit", "DVD", "DVD Nonfiction","DVD-R", "Government Document", "High Demand Audiobooks", "High Demand Books", "High Demand DVD and Blurays", "High Demand DVD-R", "ILL", "Media", "Music", "Noncirculating","Serial","Video")
OTHER <-c("Electronic Device", "Equipment", "Kit", "Realia","Software")

#Creating Page layout and headings. 
ui <-fluidPage(
  titlePanel ("SPARK Annual Reports Data (Test)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("org", "Select your Library System:", choices = unique(df$Parent.Org)),
      selectInput("libs", "Select your Library or Libraries",choices=NULL,multiple=TRUE)
    ),
    mainPanel(
      h3("E1. Number of Items in the library's catalog as of December 31, 2024"),
      dataTableOutput("totalitems"),
      h3("E2. Number of Print Materials (Excluding Magazines/Periodicals"),
      dataTableOutput("excludingper"),
      h3("E3. Number of Current Print Periodical Titles (Subscriptions"),
      dataTableOutput("per"),
      h3("E4. Number of Audio - Physical Units"),
      dataTableOutput("audio"),
      h3("E5. Number of Video - Physical Units"),
      dataTableOutput("video"),
      h3("E6. Number of Other Physical Materials"),
      dataTableOutput("other"),
      h3("F1.Registered users as of December 31, 2024." ),
      dataTableOutput("users"),
      h3("F3. Circulation of physical books, physical audio units, physical video unites, and physical periodicals/serials"),
      dataTableOutput("circ"),
      h3("F4. Circulation of Other Physical Items (wi-fi hotspots, tools, boardgames, etc."),
      dataTableOutput("circother"),
      h3("F12. Circulation of Children's physical materials during the reporting year - included with the Total Circulation of Physical Materials data element."),
      dataTableOutput("circjuv"),
      h3("F13. Interlibrary loan items provided to other libraries - SPARK libraries via Resource Sharing" ),
      dataTableOutput("ILLto"),
      h3("F14. Interlibrary loan items received from other libraries - SPARK libraries via Resource Sharing"),
      dataTableOutput("ILLfrom")
      
    )
  )
)

server <-function(input, output, session){

#Populate selected libraries

  observeEvent(input$org, {
    libs <- unique(df[df$Parent.Org == input$org, "Owning.Library"])
    libs <- sort(libs)
    updateSelectInput(session, "libs", choices = libs)
  })

#Create base filtered data based on org and library selection. 
  filtered_base <-reactive({
    req(input$org, input$libs)
    df[df$Parent.Org == input$org & df$Owning.Library %in% input$libs, ]
  })

#Adding data for F13 using Owning.Libary and Circulating.Library as matchpoints
  filtered_f13 <- reactive({
    req(filtered_base())
    df2[df2$Circulating.Library %in% filtered_base()$Owning.Library &
          df2$Circulating.Library!=df2$Destination.Library, ]
  })
#Adding data for F14 using Destination.Library and Owning.Library as matchpoints. 
  filtered_f14 <- reactive({
    req(filtered_base())
    df3[df3$Destination.Library %in% filtered_base()$Owning.Library &
          df3$Destination.Library!=df3$Circulating.Sending.Library.Name, ]
  })

#Adding data for F1 using Name and Owning.Libary as matchpoints
  filtered_f1 <-reactive({
    req(filtered_base())
    df4[df4$Name %in% filtered_base()$Owning.Library,]
  })

#Adding data for F3 and F4 using Owning Library and Check Out Library Name as matchpoints. 
  filtered_f3f4 <-reactive({
    req(filtered_base())
    df5[df5$Check.Out.Library.Name %in% filtered_base()$Owning.Library,]
  })

#Adding data for F12 using Owning Library and Name as matchpoints. 
  filtered_f12<-reactive ({
    req(filtered_base())
    df6[df6$Name %in% filtered_base()$Owning.Library,]
  })

#Output E1: Total items (excluding circ modifier ILL)
  output$totalitems <-renderDataTable({
  fb <-filtered_base()
  req(nrow(fb)>0)
  fb_filtered <-fb[!fb$Circulation.Modifier %in% EXCL_ILL,]
  data<-aggregate(Count.of.Items ~ Owning.Library + Circulating.Library, data=fb_filtered, FUN=sum)
  grand_total <-data.frame(
    Owning.Library="Grand Total",
    Circulating.Library="",
    Count.of.Items=sum(data$Count.of.Items, na.rm=TRUE)
  )
  rbind(data,grand_total)
  }, rownames=FALSE)

#Output E2: Total items excluding periodicals (excluding circ modifiers ILL and Serial)
  output$excludingper <-renderDataTable({
    fb <-filtered_base()
    req(nrow(fb)>0)
    fb_filtered <-fb[!fb$Circulation.Modifier %in% EXCL_PER,]
    data<-aggregate(Count.of.Items ~ Owning.Library + Circulating.Library, data=fb_filtered, FUN=sum)
    grand_total <-data.frame(
      Owning.Library="Grand Total",
      Circulating.Library="",
      Count.of.Items=sum(data$Count.of.Items, na.rm=TRUE)
  )
  rbind(data,grand_total)
}, rownames=FALSE)

#Output E3: Total periodical (only circ modifier Serial)
  output$per <-renderDataTable({
    fb <-filtered_base()
    req(nrow(fb)>0)
    fb_filtered <-fb[fb$Circulation.Modifier %in% INC_PER,]
    data<-aggregate(Count.of.Items ~ Owning.Library + Circulating.Library, data=fb_filtered, FUN=sum)
    grand_total <-data.frame(
      Owning.Library="Grand Total",
      Circulating.Library="",
      Count.of.Items=sum(data$Count.of.Items, na.rm=TRUE)
    )
    rbind(data,grand_total)
  }, rownames=FALSE)

#Output E4: Total audio (including circ. modifiers Audiobook, High Demand Audiobook, Music)
  output$audio <-renderDataTable({
    fb <-filtered_base()
    req(nrow(fb)>0)
    fb_filtered <-fb[fb$Circulation.Modifier %in% INC_AUDIO,]
    data<-aggregate(Count.of.Items ~ Owning.Library + Circulating.Library, data=fb_filtered, FUN=sum)
    grand_total <-data.frame(
      Owning.Library="Grand Total",
      Circulating.Library="",
      Count.of.Items=sum(data$Count.of.Items, na.rm=TRUE)
    )
    rbind(data,grand_total)
  }, rownames=FALSE)
  
#Output E5: Total video (including circ modifiers DVD, DVD Nonfiction, DVD-R, High Demand DVD-R, High Demand DVD and Blurays, Media, Video)
  output$video <-renderDataTable({
    fb <-filtered_base()
    req(nrow(fb)>0)
    fb_filtered <-fb[fb$Circulation.Modifier %in% INC_VIDEO,]
    data<-aggregate(Count.of.Items ~ Owning.Library + Circulating.Library, data=fb_filtered, FUN=sum)
    grand_total <-data.frame(
      Owning.Library="Grand Total",
      Circulating.Library="",
      Count.of.Items=sum(data$Count.of.Items, na.rm=TRUE)
    )
    rbind(data,grand_total)
  }, rownames=FALSE)
  
#Output E6: Total other (including circ modifiers Electronic Device, Equipment, Kit, Realia, Software)
  output$other<-renderDataTable({
    fb <-filtered_base()
    req(nrow(fb)>0)
    fb_filtered <-fb[fb$Circulation.Modifier %in% INC_OTHER,]
    data<-aggregate(Count.of.Items ~ Owning.Library + Circulating.Library, data=fb_filtered, FUN=sum)
    grand_total <-data.frame(
      Owning.Library="Grand Total",
      Circulating.Library="",
      Count.of.Items=sum(data$Count.of.Items, na.rm=TRUE)
    )
    rbind(data,grand_total)
  }, rownames=FALSE)

#Output F1: Total Patrons
  
  output$users <-renderDataTable({
    fb<-filtered_f1()
    req(nrow(fb)>0)
    data<-fb[fb$Name %in% input$libs,]
    grand_total<-data.frame(
      Name ="Grand Total",
      User.ID =sum(data$User.ID, na.rm=TRUE)
    )
    rbind(data,grand_total)
  }, rownames=FALSE)

#Output F3 : Circulation of Physical Units. 
  output$circ <-renderDataTable({
  fb<-filtered_f3f4()
  req(nrow(fb)>0)
  fb_filtered<-fb[fb$Circulation.Modifier %in% PHYS,]
  data <-aggregate(Circulation ~ Check.Out.Library.Name, data =fb_filtered, FUN=sum)
  grand_total <-data.frame(
    Check.Out.Library.Name ="Grand Total",
    Circulation = sum(data$Circulation, na.rm=TRUE)
  )
  rbind(data,grand_total)
}, rownames=FALSE)

#Output F4: Circulation of Non-Physical Units.
  output$circother <-renderDataTable({
    fb<-filtered_f3f4()
    req(nrow(fb)>0)
    fb_filtered<-fb[fb$Circulation.Modifier %in% OTHER,]
    data <-aggregate(Circulation ~ Check.Out.Library.Name, data =fb_filtered, FUN=sum)
    grand_total <-data.frame(
      Check.Out.Library.Name ="Grand Total",
      Circulation = sum(data$Circulation, na.rm=TRUE)
    )
    rbind(data,grand_total)
  }, rownames=FALSE)

#Output F12: Juv Circulation
  output$circjuv <-renderDataTable({
    fb<-filtered_f12()
    req(nrow(fb)>0)
    data <-aggregate(Circ.ID ~ Name, data =fb, FUN=sum)
    grand_total <-data.frame(
      Name ="Grand Total",
      Circ.ID = sum(data$Circulation, na.rm=TRUE)
    )
    rbind(data,grand_total)
  }, rownames=FALSE)

#Output F13: ILL items provided to other libraries - SPARK libraries via Resource Sharing. 
  output$ILLto <-renderDataTable({
    fb<-filtered_f13()
    req(nrow(fb)>0)
    data <-aggregate(Transit.ID ~ Circulating.Library, data=fb, FUN=sum)
    grand_total <-data.frame(
      Circulating.Library="Grand Total",
      Transit.ID=sum(data$Transit.ID, na.rm=TRUE)
  )
  rbind(data,grand_total)
}, rownames=FALSE)

#Output F14: ILL items received from other libraries - SPARK libraries via Resource Sharing.
  output$ILLfrom <-renderDataTable({
    fb<-filtered_f14()
    req(nrow(fb)>0)
    data<-aggregate(Transit.ID ~Destination.Library, data=fb, FUN=sum)
    grand_total<-data.frame(
      Destination.Library="Grand Total",
      Transit.ID = sum(data$Transit.ID, na.rm=TRUE)
  )
  rbind(data,grand_total)
}, rownames=FALSE)

}

shinyApp(ui=ui, server=server)


