require('shiny')

shinyUI(pageWithSidebar(
  
  headerPanel("Database Schema Visualization"),
  
  ## NETWORK 
  sidebarPanel(
    conditionalPanel(
      condition="input.mytab=='network'",
      
      sliderInput("vlabcex", 
                  "Vertex label size:", 
                  value = 1,
                  min = .05, 
                  max = 5,
                  step = 0.05),
      
      sliderInput("sizeNodesSlider", 
                  "Vertex size", 
                  value = 10,
                  min = 0.1, 
                  max = 30,
                  step = 0.1),
      
      sliderInput("ewidth", 
                  "Edge width:", 
                  value = 1,
                  min = 1, 
                  max = 10,
                  step = 1),
      
      selectInput("lay2", "Choose a layout", 
                  choices = c('circle', 'fruchterman.reingold')),
      
      selectInput("vcolor", "Color vertices by:", 
                  choices = c('# of keys', '# of connections', 'strength of keys')),
      
      checkboxInput(inputId='sizeNodes', label='Size vertices by connections', value=T),
      
      checkboxInput(inputId='curved', label='Curve edges', value=F),
      
      checkboxInput(inputId='islands', label='Remove unconnected Tables', value=T),
      
      radioButtons(inputId='subEdges', label='Select Edges:', choices=c('all', 'some')),
      
        conditionalPanel("input.subEdges=='some'",
                         checkboxInput(inputId='delE', label='Delete un-selected edges', value=F),
                         checkboxGroupInput(inputId='edgev', label='Variables', choices=sort(unique(E(g)$name)))
                       )
      
      
      ),
    
    ## STRENGTH TABLE
    conditionalPanel(
      condition="input.mytab=='strength'",
        selectInput(inputId='key', label='Choose a key', choices=sort(unique(E(g)$name))),
      sliderInput("keylab", 
                  "Variable labels", 
                  value = 1,
                  min = .1, 
                  max = 10,
                  step = .1)
        
    ),
    
    ## KEY-TABLE HEATMAP
    conditionalPanel(
      condition="input.mytab=='keyTab'",
      sliderInput("colLabCex", 
                  "Table Label Size", 
                  value = 1,
                  min = .1, 
                  max = 10,
                  step = .1),
      sliderInput("rowLabCex", 
                  "Key Label Size", 
                  value = 1,
                  min = .1, 
                  max = 10,
                  step = .1),
      sliderInput("marginSize", 
                  "Margin Size", 
                  value = 15,
                  min = 0, 
                  max = 100,
                  step = 1),
      sliderInput("test", 
                  "width size", 
                  value = 800,
                  min = 0, 
                  max = 5000,
                  step = 50)
    ),
    
    ## TABLE VIEW 
      conditionalPanel(
        condition="input.mytab=='adjlist'",
        
        selectInput("tab", "Choose a Table", 
                    choices = c('all', unique(V(g)$name))),
        
        selectInput("edgev", "Choose a Variable", 
                    choices = c('all', unique(E(g)$name)))
      )
  ),
  
  
  
  ## PANELS TO SHOW
  mainPanel(
    tabsetPanel(id='mytab',
      tabPanel('Network', value='network', plotOutput('circle', height='150%')),
      tabPanel('Key Strength', value='strength', plotOutput('strengthPlot', height='150%')),
      tabPanel('Key-Table Matrix', value='keyTab', plotOutput('keyTabMat', height='150%')),
      tabPanel('Adjacency List', value='adjlist', tableOutput('adjlist'))
    )
  )
  
))


