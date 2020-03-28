library(pdftools)
library(magick)
library(tesseract)
library(dplyr)
library(shiny)
library(shinymaterial)
# library(shinydashboard)
library(shinycssloaders)

# rsconnect::deployApp(appName = "Orca")

ui <- material_page(title = "Orca",
                    tags$style(HTML("#pdftext {height:650px; overflow-y:scroll}")),
                    nav_bar_color = "black",
                    material_row(
                        material_column(width = 6, offset = 1,
                                        fileInput("uploadedpdf",
                                                  label="Upload PDF Here...",
                                                  accept = c(".pdf"),
                                                  multiple = FALSE)),
                        # material_column(width = 1,
                        #                 br(),
                        #                 actionButton("readbtn",
                        #                              "Read >")),
                        material_column(width = 5),
                    ),
                    
                    material_row(
                        # column(6, htmlOutput('pdfviewer'))
                        material_column(width = 6,
                                        htmlOutput('pdfviewer')),
                        # imageOutput("pdfimage")),
                        # column(6, textOutput("pdftext")),
                        material_column(width = 6,
                                        htmlOutput("pdftext") %>% withSpinner(color = "#000000"))
                        #%>% withSpinner(color="#0dc5c1")),
                    )
)


server <- function(input, output, session) {
    
    eng <- tesseract("eng")
    # path <- "ocrscan.pdf"
    
    # output$pdftext <- renderText({
    #   pdfpath <- input$uploadedpdf$datapath
    #   
    #   pdfpath %>%
    #     pdftools::pdf_convert(dpi = 600, filenames = paste0(pdfpath, "_img.png")) %>% 
    #     tesseract::ocr(engine = eng)
    # })
    
    
    output$pdftext <- renderText({
        validate(
            need(input$uploadedpdf$datapath != "", "Upload your PDF and the OCR will show the result here!")
        )
        pdfpath <- input$uploadedpdf$datapath
        
        pages <- pdfpath %>%
            pdftools::pdf_convert(dpi = 600)  #%>% 
        # tesseract::ocr(engine = eng, HOCR = TRUE)
        outputhtml <- lapply(pages, function(x){tesseract::ocr(x, engine = eng, HOCR = T)}) %>%
            paste(collapse = "<br>")
        
        return(outputhtml)
    })
    
    
    output$pdfviewer <- renderText({
        validate(
            need(input$uploadedpdf$datapath != "", "")
        )
        file.copy(input$uploadedpdf$datapath, "www/temppdf.pdf", overwrite = TRUE)
        
        # pdffileloc <- input$uploadedpdf$datapath
        # pdffileloc <- paste0("file:///",
        #                      gsub("\\\\", "/",
        #                           input$uploadedpdf$datapath))
        # pdffileloc <- gsub("c:", "file///C:", pdffileloc)
        
        outputhtml <- paste('<iframe style="height:650px; width:100%" src="',
                            # input$uploadedpdf$datapath,
                            # pdffileloc,
                            "temppdf.pdf",
                            '"></iframe>',
                            sep = "")
        
        return(outputhtml)
    })
    
}

shinyApp(ui, server)