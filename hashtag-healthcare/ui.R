# Define UI
shinyUI(
  fluidPage(
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
            menuItem('Medicaid Coverage', tabName = 'insurance', icon = icon('medkit')),
            menuItem('After ACA: 2015-2017', tabName = 'access', icon = icon('notes-medical')),
            menuItem('Twitter Analysis', tabName = 'twitter', icon = icon('twitter')),
            menuItem('Data Sources', tabName = 'data', icon = icon('database'))
          
          )
        ),
        dashboardBody(
          tabItems(
            tabItem(tabName = 'access',
              title = '', status = 'primary', solidHeader = TRUE, width=3,
              h3('After ACA: Do states with Medicaid expansion offer improved healthcare access?'),
              fluidRow(
                box(
                  title = 'Select a choice:',
                  selectInput('metric', label = NULL,
                    choices = unique(all_access$Data),
                    selected = 'Percent of women without a healthcare provider')
                ), # close box
                valueBoxOutput("stat_box1",width=6),
                valueBoxOutput("stat_box2",width=6)
                #)
          ), #close fluidRow
          fluidRow(
            box(
              title = "2015-2017 Healthcare access", status = "primary", solidHeader = FALSE,width=40,
              plotOutput("medicaid_plot", height=500)
            ) # close box
          ) # close fluidBox
        ), #close tabItem
        tabItem(tabName = 'insurance',
                title = '', status = 'primary', solidHeader = TRUE, width=3,
                h3('Medicaid coverage at the state level'),
                fluidRow(
                  box(
                    title = 'State:',
                    selectInput('state', label = NULL,
                                choices = medicaid$State,
                                selected = 'Kentucky')
                  ), # close box
                  
                  valueBoxOutput("medicaid_box",width=6),
                  valueBoxOutput("change_box",width=6)
                  
                ), #close fluidRow
                fluidRow(
                  box(
                    title = "Change in mean Medicaid coverage before and after Medicaid expansion: 2011-2013 vs. 2014-2016", status = "primary", solidHeader = FALSE,width=40,
                    plotOutput("medicaid_lollipop", height=500)
                  ) # close box
                ) # close fluidBox
        ), #close tabItem
        tabItem(tabName = 'twitter',
                title = '', status = 'primary', solidHeader = TRUE, width=3,
                h3('How do people feel about healthcare right now?'),
                fluidRow(
                    
                    column(width=6,wordcloud2Output("word_cloud",height="500px")),
                
                    column(width=6,box( title = "Sentiment analysis of tweets referencing healthcare",width=40,
                    plotOutput("sentiment_graph"),height="500px")
                    )
                ), #close fluidRow
                h4('Tweets about healthcare intersect with politics, jobs, and current events.')
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
