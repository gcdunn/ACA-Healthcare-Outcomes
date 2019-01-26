# Define UI
shinyUI(
  fillPage(
    theme = shinythemes::shinytheme('yeti'),
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
            menuItem('Closing the Gap', tabName = 'main', icon = icon('arrows-alt-h')),
            menuItem('Medicaid Coverage', tabName = 'insurance', icon = icon('medkit')),
            menuItem('After ACA: 2015-2017', tabName = 'access', icon = icon('notes-medical')),
            menuItem('Twitter Analysis', tabName = 'twitter', icon = icon('twitter')),
            menuItem('Data Sources', tabName = 'data', icon = icon('database'))
          
          )
        ),
        dashboardBody(
          tabItems(
            tabItem(tabName = 'main',
                    title = 'Data', status = 'primary', solidHeader = TRUE, width=3,
                    h2('Overview'),
                    h4('One of the major goals of The Affordable Care Act was to close the 
                       coverage gap through Medicaid expansion.  A Supreme Court decision in 
                       2012 made expansion optional at the state level.  Fourteen states that 
                       had no previous existing Medicaid expansion program (e.g. a waiver) began
                       Mediciad expansion in January 2014.  Sixteen states sis not expand Medicaid
                       through 2017.  The remaining states had either an existing waiver program 
                       or adopted Medicaid expansion later.  The purpose this project is to explore
                       the differences between the first two groups: states that did or did not 
                       expand Medicaid in 2014.'),
                    img(src = "medicaidgap.png")
            ), #close tabItem
            tabItem(tabName = 'access',
              title = '', status = 'primary', solidHeader = TRUE, width=3,
              h2('After ACA: Do states with Medicaid expansion offer improved healthcare access?'),
              fluidRow(
                box(
                  title = 'Select a choice:',
                  width=4,
                  selectInput('metric', label = NULL,
                    choices = unique(all_access$Data),
                    selected = 'Percent of women without a healthcare provider')
                  
                ), # close box
                valueBoxOutput("stat_box1",width=4),
                valueBoxOutput("stat_box2",width=4)
                #)
          ), #close fluidRow
          fluidRow(
            box(
              title = "2015-2017 Healthcare access", status = "primary", solidHeader = FALSE,width=40,
              plotOutput("medicaid_plot", height=600)
            ) # close box
          ) # close fluidBox
        ), #close tabItem
        tabItem(tabName = 'insurance',
                title = '', status = 'primary', solidHeader = TRUE, width=3,
                h2('Medicaid coverage at the state level'),
                fluidRow(
                  box(
                    title = 'State:',
                    width=4,
                    selectInput('state', label = NULL,
                                choices = medicaid$State,
                                selected = 'Kentucky')
                  ), # close box
                  
                  valueBoxOutput("medicaid_box",width=4),
                  valueBoxOutput("change_box",width=4)
                  
                ), #close fluidRow
                fluidRow(
                  box(
                    title = "Change in mean Medicaid coverage before and after Medicaid expansion: 2011-2013 vs. 2015-2017",status = "primary", solidHeader = FALSE,width=40,
                    plotOutput("medicaid_lollipop", height=600)
                  ) # close box
                ) # close fluidBox
        ), #close tabItem
        tabItem(tabName = 'twitter',
                title = '', status = 'primary', solidHeader = TRUE, width=3,
                h2('How do people feel about healthcare right now?'),
                fluidRow(
                    
                    column(width=6,wordcloud2Output("word_cloud",height="500px")),
                
                    column(width=6,box( title = "Sentiment analysis of tweets referencing healthcare",width=40,
                    plotOutput("sentiment_graph"),height="500px")
                    )
                ), #close fluidRow
                h4('Tweets about healthcare intersect with politics, jobs, and current events.  There are more than 5,000
                   words in this data set.')
        ), #close tabItem
        tabItem(tabName = 'data',
                title = 'Data', status = 'primary', solidHeader = TRUE, width=3,
                h2('Data Sources'),
                br(),
                kff <- a('Kaiser Family Foundation', href= "https://www.kff.org/", target = "_blank"),
                br(),
                cdc <- a('CDC WONDER', href= "https://wonder.cdc.gov/", target = "_blank"),
                br(),
                hhs <- a('Department of Health and Human Services', href= "https://aspe.hhs.gov/", target = "_blank"),
                br(),
                kc <- a('Kids Count', href= "https://datacenter.kidscount.org/", target = "_blank"),
                br(),
                twitter <- a('Twitter', href= "http://www.twitter.com/", target = "_blank")
                
        ) #close tabItem
      ) #close tabItems
    ) #close dashboardBody
  ) #close dashboardPage
  ) #close fluidPage
) #close shinyUI
