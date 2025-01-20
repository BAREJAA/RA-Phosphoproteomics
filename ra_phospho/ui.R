header <- dashboardHeader(title = "RA Phosphoproteomics",
                          tags$li(class = "dropdown",
                                  tags$img(height = "50px", alt="Duke Logo", src="duke_logo.png")
                          ) # https://stackoverflow.com/questions/35961970/how-to-add-a-company-logo-to-shinydashboard-header-not-mainpanel-or-mainheader
                  
                          )

sidebar <- dashboardSidebar(
  uiOutput("sidebarpanel")
)

body <- dashboardBody(
  shinyjs::useShinyjs(),
  uiOutput("body")
        )

dashboardPage(header, sidebar, body)



