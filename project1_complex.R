# Project 1: Complex Numbers & Arithmetic - Pirate Treasure Map
# Setup once: install.packages("shiny")   |   Run: click "Run App" in RStudio
library(shiny)

ui <- fluidPage(
  titlePanel("Pirate Treasure Map (Complex Numbers)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("east", "Steps East", 2),
      numericInput("north", "Steps North (after turning left)", 3),
      numericInput("tre", "Treasure X (real part)", 3),
      numericInput("tim", "Treasure Y (imaginary part)", 4)
    ),
    mainPanel(plotOutput("cplot"), verbatimTextOutput("ctext"))
  )
)

server <- function(input, output, session) {
  pirate <- reactive({
    dir <- 1 + 0i
    pos <- 0 + 0i
    pos <- pos + input$east * dir
    dir <- dir * 1i                      # turn left 90 degrees
    list(mid = pos, pos = pos + input$north * dir)
  })
  treasure <- reactive(complex(real = input$tre, imaginary = input$tim))

  output$cplot <- renderPlot({
    p <- pirate(); t <- treasure()
    lim <- max(abs(c(Re(p$pos), Im(p$pos), Re(t), Im(t))), 1) + 1
    plot(0, 0, type = "n", xlim = c(-1, lim), ylim = c(-1, lim),
         xlab = "Real", ylab = "Imaginary", main = "Complex plane map")
    grid()
    arrows(0, 0, Re(p$mid), Im(p$mid), col = "blue", lwd = 2)
    arrows(Re(p$mid), Im(p$mid), Re(p$pos), Im(p$pos), col = "blue", lwd = 2)
    points(Re(t), Im(t), pch = 8, col = "red", cex = 2.5)
    text(Re(t), Im(t), "Treasure", pos = 3, col = "red")
  })

  output$ctext <- renderText({
    p <- pirate()$pos; t <- treasure(); g <- t - p
    paste0("Pirate at: ", format(p), "\n",
           "Gap to treasure: ", format(g), "\n",
           "Distance left: ", round(Mod(g), 2), "\n",
           "Direction (degrees): ", round(Arg(g) * 180 / pi, 1), "\n",
           "Conjugate of treasure: ", format(Conj(t)), "\n",
           "Treasure x Pirate: ", format(t * p), "\n",
           "Treasure / Pirate: ", if (p == 0) "undefined" else format(t / p))
  })
}

shinyApp(ui, server)
