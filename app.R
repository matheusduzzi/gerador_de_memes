library(magick)
library(shiny)


ui <- fluidPage(
    titlePanel("Gerador de Memes"),
    
    sidebarLayout(
        
        sidebarPanel(
            
            fileInput("upload", "Upload new image", accept = c('image/png', 'image/jpeg')),
            textInput("text_cima", label = h3("Texto de cima")),
            hr(),
            textInput("text_baixo", label = h3("Texto de baixo")),
            verbatimTextOutput("value"),
            hr(),
            textInput("size", "Tamanho", value = "500x500!"),
            sliderInput("rotation", "Rotação", 0, 360, 0),
            sliderInput("blur", "Borrão", 0, 20, 0),
            sliderInput("implode", "Deformação", -1, 1, 0, step = 0.01),
            
            checkboxGroupInput("effects", "Efeitos",
                               choices = list("edge", "charcoal", "negate", "flip", "flop")),
            downloadButton('downloadImage', 'Faça download do meme')
            
        ),
        mainPanel(
            imageOutput("img")
        )
    )
)

server <- function(input, output, session) {
    
    
    imageLoc <- reactiveVal("https://raw.githubusercontent.com/matheusduzzi/gerador_de_memes/master/Bem%20vindo.png")
    imageVal <- reactive({
        image_convert(image_read(imageLoc()), "jpeg")
    })
    
    observeEvent(input$upload, {
        if (length(input$upload$datapath)) {
            imageLoc(input$upload$datapath)
        }
        updateCheckboxGroupInput(session, "effects", selected = "")
    })
    
    observe({
        info <- image_info(imageVal())
        updateTextInput(session, "size", value = paste0(info$width, "x", info$height, "!"))
    })
    
    updatedImageLoc <- reactive({
        image <- imageVal()
        
        if("edge" %in% input$effects)
            image <- image_edge(image)
        
        if("charcoal" %in% input$effects)
            image <- image_charcoal(image)
        
        if("negate" %in% input$effects)
            image <- image_negate(image)    
        
        if("flip" %in% input$effects)
            image <- image_flip(image)
        
        if("flop" %in% input$effects)
            image <- image_flop(image)
        
        tmpfile <- image %>%
            image_annotate(input$text_baixo, size = 50, gravity = "southwest",
                           font = "monospace",location = "+100+100", color = "white") %>% 
            image_annotate(input$text_cima, size = 50,gravity = "northwest",
                           font = "monospace",location = "+100+100", color = "white") %>% 
            image_resize(input$size) %>%
            image_implode(input$implode) %>%
            image_blur(input$blur, input$blur) %>%
            image_rotate(input$rotation) %>%
            image_write(tempfile(fileext='jpg'), format = 'jpg')
        
        tmpfile
    })
    
    output$img <- renderImage({
        
        list(src = updatedImageLoc(), contentType = "image/jpeg")
    }, 
    deleteFile = FALSE
    )
    
    output$downloadImage <- downloadHandler(
        filename = "meu_meme.jpeg",
        contentType = "image/jpeg",
        content = function(file) {
            file.copy(updatedImageLoc(), file)
        }
    )  
}

shinyApp(ui, server)

