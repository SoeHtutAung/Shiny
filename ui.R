# create UI
ui <- page_navbar(
  title = "Humanitarian dashboard by Soe",
  bg = "#2D89C8",
  inverse = TRUE,
  ## page 1
  nav_panel(
    title = "Regional situation",
    layout_sidebar(sidebar = sidebar1,
                   mainPanel(
                     cd5, # scatterplot
                     layout_column_wrap(
                       width = 1/2, height = 600, cd2, cd3), # maps
                     cd1, # table
                     layout_column_wrap(
                       width = "250px", fill = FALSE, vb1, vb2),# value boxes
                     cd4, # selected parameters
                     # link to ocha page
                     a("Go to OCHA site", class = "btn btn-primary btn-md", 
                       href = "https://www.unocha.org/myanmar")
                   )
    )
  ),
  ## page 2
  nav_panel(
    title = "District situation",
    layout_sidebar(sidebar = sidebar2,
                   mainPanel(pg2_cd1
                   ))
  ),
  ## page 3
  nav_panel(
    title = "Township situation",
    leafletOutput("map4", height = "100%", width = "100%"),
    sidebar3
  ),
  ## navigation link
  nav_spacer(),
  nav_menu(
    title = HTML(paste0(shiny::icon("circle-question"), " Reach me")),
    align = "right",
    nav_item(tags$a(shiny::icon("linkedin"),"LinkedIn", href = "https://www.linkedin.com/in/soe-h-aung")),
    nav_item(tags$a(shiny::icon("github"), "GitHub", href = "https://github.com/SoeHtutAung"))
  )
)
