
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(gt)
library(ggthemes)
library(DT)
library(scales)
library(reshape2)

# read in data

highest_growth <- read_rds("highest_growth.rds")
largest <- read_rds("largest.rds")
display35 <- read_rds("display35.rds")


# Define UI for application that draws a histogram

ui <- navbarPage("Measuring Growth in CA Elementary School Districts",
   
  
   # different tabs
   tabPanel("Explore Elementary School Performance by District",
            
            fluidPage(
               
               # Application title
               
               titlePanel("Elementary School Growth"),
               
               
               mainPanel(
                  
                  # selectInput("sdistrict", 
                  #             "Please select a CA school district to view.",
                  #             display35$District),
                  # br(),
                  # br(),
                  # 
                  # 
                  "This table measures growth of student performance on tests administered while entering grade 3 and exiting grade 5 according to grade level averages across the US. Use the toggles to sort by different metrics.",
                  br(),
                  br(),
                  DTOutput("35_comparisonTable")

                  # br(),
                  # br(),
                  # br(),
                  # br(),
                  # br(),
                  # br(),
                  
                  
                  
                  # plotOutput("35_bigPlotTable")
                  
                  # br()
               )
            )
    ),
   # 
   # tabPanel("Explore College Readiness by School District",
   #          
   #          fluidPage(
   #            
   #            # Application title
   #            
   #            titlePanel("School District Stats"),
   #            
   #            
   #            mainPanel(
   #              
   #              selectInput("sdistrict", 
   #                          "Please select a CA school district to view.",
   #                          select_eff$District),
   #              br(),
   #              br(),
   # 
   # 
   #              DTOutput("comparisonTable"),
   # 
   #              br(),
   #              br(),
   #              br(),
   #              br(),
   #              br(),
   #              br(),
   # 
   # 
   # 
   #              plotOutput("bigPlotTable")
   # 
   #              # br()
   #            )
   #          )
   # ),
   # 
   # tabPanel("Most Efficient",
   #          fluidPage(
   #            titlePanel("10 Most Efficient School Districts"),
   #            mainPanel(
   #              DTOutput("effTable")
   #            )
   #          )
   #          
   # ),
   # tabPanel("Spending and Performance",
   #          fluidPage(
   #            titlePanel("Spending & Performance: Not Always Correlated"),
   #            mainPanel(
   #              "In both graphs shown below (ranked in order of top spending and top performance respectively), there are clearly districts that underperform given their per pupil
   #              spend, as well as overperform given their spend.",
   #          
   #              br(),
   #              br(),
   #              br(),
   #              plotOutput("spendPlot"),
   #              br(),
   #              br(),
   #              br(),
   #              plotOutput("perfPlot")
   #            )
   #            
   #          )
   #  ),
   # 
   # tabPanel("Deep Dive - Most Efficient Schools",
   #          fluidPage(
   #            titlePanel("Top 3 Schools - Other School Characteristics"),
   #            mainPanel(
   #              "Literature on school efficiency done by Melvin and Sharma suggests that efficient schools can be defined by the ratio of students/administrators.
   #              This provides an alternative definition than the spending efficiency that I am exploring by examining spending and performance outcomes. However, I 
   #              wanted to see if these two measures of efficiency are correlated at all. Melvin and Sharma poses additional correlative factors for efficiency such as
   #              percent of low income students and percent of teachers with advanced degrees, which are among the factors examined here.",
   #              br(),
   #              br(),
   #              selectInput("col", 
   #                          "Please select a characteristic to view.",
   #                          c("Teacher Salary", "Admin", "Teacher Education", "Teacher Experience", "Free & Reduced Lunch")),
   #              br(),
   #              br(),
   #              
   #              plotOutput("deepDive")
   #              # br(),
   #              # br(),
   #              # br(),
   #              # plotOutput("perfPlot")
   #            )
   #            
   #          )
   # ),
              
  
  
  tabPanel("About",
           fluidPage(
             titlePanel("About This Project"),
             mainPanel(
               "This project looks to analyze school district performance for students Grades 3-5 based on grade level growth. Given that growth from grades 3-5 has large
               snowball effects on growth in later years (i.e. falling behind by grade 3 makes it much harder to catch up in grades 6-8, I thought this would be a salient
               measure of growth within different school districts. I've included enrollment to account for school districts that have to manage the complexity of scale, 
               rewarding school districts that have educated large number of students. Future iterations will include data on percent of students on free and reduced lunch, 
               as well as performance along race, to further look at whether the growth is happening among all students, or whether a district is lagging behind in educating
               certain groups of individuals. Data comes from 3rd graders that entered from the years of 2006-2010 and comes from the Stanford Education Data Archive. ",
               
               br(),
               br(),
               
               "This project was created by Jefferey Huang (jeffereyhuang[at]gmail.com). The code can be found at https://github.com/jeffereyhuang/ca-school-district-efficiency."
            
             )
           )
           
  )
              
              
)



# Define server logic required to draw a graphs
server <- function(input, output) {
   
   # output$spendPlot <- renderPlot({
   #    expense %>% mutate(dname = fct_reorder(dname, d_spend)) %>% 
   #    
   #    ggplot(aes(x=dname,y=act_comp_rank)) +
   #    geom_bar(stat="identity") + 
   #    coord_flip() +
   #    theme_economist() +
   #    ylim(0,100) +
   #    labs(title="Top 10 Highest Spending Districts - ACT Performance", y="Percentile Rank for ACT Scores",x=NULL)
   # })
   # 
   output$`35_comparisonTable` <- renderDT({
      display35 %>% 
         datatable(rownames=FALSE,
         class="display",
         options=list(dom="t")) %>% 
      formatRound(c(2:4), 2)
   })
   
}
   # 
   # output$comparisonTable <- renderDT({
   #   eff %>% filter(District %in% c(input$sdistrict, "Average Values")) %>% 
   #   select(District, `Per Pupil Spend`, `Avg. SAT Composite Score`, `Avg. ACT Composite Score`, `Avg. AP Score`) %>% 
   #   datatable(rownames=FALSE,
   #             class="display",
   #             options=list(dom="t")) %>%
   #     formatRound(c(3), 0) %>% 
   #     formatRound(c(4:5), 1) %>% 
   #     formatCurrency("Per Pupil Spend", "$", digits = 0)
   #     
   #   
   # })
   # 
   # output$bigPlotTable <- renderPlot({
   #   eff_corr <- eff %>% select(enrollment, District, admin_ratio_per100, mast_per, doc_per) %>% 
   #     mutate(admin_ratio_per100 = 10* admin_ratio_per100,
   #            mast_per = 100 * mast_per,
   #            doc_per = 100 * doc_per) %>% 
   #     rename(`Enrollment`=enrollment,
   #            `Admin to Student Ratio (per 1,000)` = admin_ratio_per100,
   #            `Percent of Teachers with Masters'` = mast_per,
   #            `Percent of Teachers with Doctorates'` = doc_per
   #            )
   # 
   #   eff_corr %>% filter(District %in% c(input$sdistrict, "Average Values")) %>% 
   #   melt(id.vars='District') %>% 
   #     
   #   ggplot(aes(x=District,y=value, fill=District), show.legend=FALSE) +
   #     geom_bar(stat="identity", width = 0.5) +
   #     facet_wrap(~variable, scales="free") +
   #     theme_economist() +
   #     theme(legend.position="none") + 
   #     labs(title="Efficiency Correlates",x=NULL, y=NULL)
   # })
   # 
   # output$perfPlot <- renderPlot({
   #   comp %>% mutate(dname = fct_reorder(dname, act_avg_comp)) %>% 
   #                   
   #   ggplot(aes(x=dname, y=expense_rank)) +
   #     geom_bar(stat="identity") +
   #     coord_flip() + 
   #     theme_economist() +
   #     ylim(0,100) +
   #     labs(title="Per Pupil Spending of Top 10 Highest Performing Districts", y="Percentile Rank in Spending", x=NULL)
   #   
   # })
   # 
   # output$effTable <- renderDT({
   #   top10 %>% select(District, "Composite Percentile Rank", "Participation Percentile Rank", "Spend Percentile Rank", "Efficiency Index") %>% 
   #    datatable(
   #              rownames=FALSE,
   #              class="display",
   #              options=list(dom="t")) %>%
   #              formatRound(c(1:8), 1)
   # }) 
   # 
   # output$deepDive <- renderPlot({
   #   if(input$col == "Teacher Salary") {
   #     ggplot(eff_tsal, aes(x=variable,y=value, fill=District)) +
   #       geom_bar(stat="identity", position="dodge") + 
   #       theme_economist() +
   #       scale_y_continuous(labels = dollar, breaks = c(30000, 60000, 90000, 120000)) +
   #       labs(title="Teacher Salaries", y="Salary",x=NULL) + 
   #       theme(legend.title=element_blank(), legend.position = "bottom")
   #   }
   #   else if(input$col == "Admin") {
   #     ggplot(eff_ratio, aes(x=variable,y=value, fill=District)) +
   #       geom_bar(stat="identity", position="dodge") + 
   #       theme_economist() +
   #       scale_y_continuous(breaks = c(0, 3, 6, 9, 12)) +
   #       labs(title="Administrative Ratios - Students and Teachers", y="Admin",x=NULL) +
   #       theme(legend.title=element_blank(), legend.position = "bottom")
   #     
   #   }
   #   else if(input$col == "Teacher Experience") {
   #     ggplot(eff_addtl, aes(x=variable,y=value, fill=District)) +
   #       geom_bar(stat="identity", position="dodge") + 
   #       theme_economist() +
   #       labs(title="Teacher Experience", y="Years",x=NULL) +
   #       theme(legend.title=element_blank(), legend.position = "bottom")
   #     
   #   }
   #   else if(input$col == "Teacher Education") {
   #     eff
   #     ggplot(eff_adv, aes(x=variable,y=value, fill=District)) +
   #       geom_bar(stat="identity", position="dodge") + 
   #       theme_economist() +
   #       scale_y_continuous(labels=percent) +
   #       labs(title="Teacher Education", y="Percent",x=NULL) +
   #       theme(legend.title=element_blank(), legend.position = "bottom")
   #     
   #   }
   #   else if(input$col == "Free & Reduced Lunch") {
   #     ggplot(eff_frpm, aes(x=variable,y=value, fill=District)) +
   #       geom_bar(stat="identity", position="dodge") + 
   #       theme_economist() +
   #       scale_y_continuous(labels= percent) +
   #       labs(title="Percent of Students on Free and Reduced Lunch", y="Percent",x=NULL) +
   #       theme(legend.title=element_blank(), legend.position = "bottom")
   #     
   #   }
   # })

# Run the application 
shinyApp(ui = ui, server = server)

