# Define UI
shinyUI(
  fluidPage(
    theme = shinythemes::shinytheme('cerulean'),
    dashboardPage(
        skin = 'blue',
        dashboardHeader(title = '#healthcare'),
        dashboardSidebar(
          sidebarMenu(
            actionLink(inputId='ab1', label="Glenna Dunn", 
                          icon = icon("github"), 
                          onclick ="window.open('https://github.com/gcdunn', '_blank')"),
            br(),
            br(),
            menuItem('Healthcare Outcomes', tabName = 'outcomes', icon = icon('notes-medical')),
            menuItem('Insurance Coverage', tabName = 'insurance', icon = icon('medkit')),
            menuItem('Twitter Analysis', tabName = 'twitter', icon = icon('twitter')),
            menuItem('Data Sources', tabName = 'data', icon = icon('database'))
            
          )),
        dashboardBody(
          tabItems(
            tabItem(tabName = 'outcomes',
              title = '', status = 'primary', solidHeader = TRUE, width=3,
              h3('State-level analysis of healthcare outcomes'),
              fluidRow(
                box(
                  title = 'State:',
                  selectInput('state', label = NULL,
                    choices = medicaid$State,
                    selected = 'Tennessee')
                ), # close box
                box(
                  title = 'Year:',
                  sliderInput('year', label = NULL,
                    min = min(years),
                    max = max(years),
                    value = 2016, sep = "")
                ), # close box
                infoBoxOutput("medicaid_box",width=5)
          ), #close fluidRow
          fluidRow(
            box(
              title = "Un-insurance rates", status = "primary", solidHeader = FALSE,width=40,
              plotOutput("insurance_map", height = 500, width = 800)
            ) # close box
          ) # close fluidBox
        ), #close tabItem
        tabItem(tabName = 'insurance',
                title = '', status = 'primary', solidHeader = TRUE, width=3,
                h3('Breakdown of insurance coverage'),
                fluidRow(
                  box(
                    title = 'State:',
                    selectInput('state2', label = NULL,
                                choices = medicaid$State,
                                selected = 'Tennessee')
                  ), # close box
                  box(
                    title = 'Year:',
                    sliderInput('year2', label = NULL,
                                min = min(years),
                                max = max(years),
                                value = 2016, sep = "")
                  ) # close box
                ), #close fluidRow
                fluidRow(
                  box(
                    title = "Types of Insurance", status = "primary", solidHeader = FALSE,width=40,
                    plotOutput("insurance_pie", height = 500, width = 800)
                  ) # close box
                ) # close fluidBox
        ), #close tabItem
        tabItem(tabName = 'data',
                title = 'Data', status = 'primary', solidHeader = TRUE, width=3,
                h3('Data Sources'),
                br(),
                cdc <- a('CDC WONDER', href= "https://wonder.cdc.gov/", target = "_blank"),
                br(),
                bls <- a('Kaiser Family Foundation', href= "https://www.kff.org/", target = "_blank"),
                br(),
                twitter <- a('Healthcare Twitter Analysis', href= "http://healthcare-twitter-analysis.com.s3-website-us-west-1.amazonaws.com/", target = "_blank")
                
        ) #close tabItem
      ) #close tabItems
    ) #close dashboardBody
  ) #close dashboardPage
  ) #close fluidPage
) #close shinyUI
