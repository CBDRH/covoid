

library(shiny)
library(visNetwork)

### UI
ui <- fluidPage(

titlePanel("Minimal working example of visNetwork issue"),
helpText("Desired functionality is for the user to click on a node or edge to open a dialogue box and set the related parameter"),
strong("Issue:"),
p("The dialogue box does not launch when the user clicks on the same node/edge consecutively, or on a node adjacent to the last clicked edge."),
strong("Troubleshooting | Current Node and Edge values:"),
htmlOutput("troubleshoot"),
visNetworkOutput("network")

)


### Server
server <- function(input, output) {


    #########################
    ### Define visNetwork ###
    #########################

    output$network <- renderVisNetwork({

        # Define nodes, labeled 1 to 3
        nodes <- data.frame(id = 1:3, label = c(1, 2, 3))

        # Define edges, labelled A and B
        edges <- data.frame(from = c(1, 2), to = c(2, 3), id = c("A", "B"), label = c("A","B"))

        # Create visNetwork
        visNetwork(nodes, edges) %>%
            visEvents(

                # Assign clicked node to input$node_id
                selectNode = "function(data) {
                    Shiny.onInputChange('node_id', data.nodes);
                    ;}",

                # Assign clicked edge to input$edge_id
                selectEdge = "function(data) {
                    Shiny.onInputChange('edge_id', data.edges);
                    ;}"
            )
    })


    ####################################################
    ### Set parameters by clicking on a network node ###
    ####################################################

    # values$setNode and values$setEdge are placeholders for the clicked node and edge
    values <- reactiveValues(setNode = 0, setEdge = 'Z')

    # They start out as NULL so need to replace that with zero
    observe(values$setNode <- ifelse(is.null(input$node_id), 0, input$node_id))
    observe(values$setEdge <- ifelse(is.null(input$edge_id), 'Z', input$edge_id))

    # They are reverted to 0 and z respectively every time a dialogue box is closed
    observeEvent(input$modal_done,{
        values$setNode <- 0
        values$setEdge <- 'Z'
        removeModal()
    })

    # Open modal dialogue when node or edge is clicked
    observe(
        ## Nodes

        # A.
        if (values$setNode==1) { isolate(
            showModal(modalDialog(
                sliderInput("a_num", label="Set value for Node 1", min=0, max=1, value=.5),
                easyClose = FALSE,
                footer = actionButton("modal_done", label = "Done") ))
        )}

        # B.
        else if (values$setNode==2) { isolate(
            showModal(modalDialog(
                sliderInput("b_num", label="Set value for Node 2", min=0, max=1, value=.5),
                easyClose = FALSE,
                footer = actionButton("modal_done", label = "Done") ))
        )}

        # C.
        else if (values$setNode==3) { isolate(
            showModal(modalDialog(
                sliderInput("c_num", label="Set value for Node 3", min=0, max=1, value=.5),
                easyClose = FALSE,
                footer = actionButton("modal_done", label = "Done") ))
        )}

        # A->B.
        else if (values$setEdge=="A") { isolate(
            showModal(modalDialog(
                sliderInput("ab_num", label="Set value for Edge A", min=0, max=1, value=.5),
                easyClose = FALSE,
                footer = actionButton("modal_done", label = "Done") ))
        )}

        # C->D.
        else if (values$setEdge=="B") { isolate(
            showModal(modalDialog(
                sliderInput("ab_num", label="Set value for Edge B", min=0, max=1, value=.5),
                easyClose = FALSE,
                footer = actionButton("modal_done", label = "Done") ))
        )}

    )



    ### Print current node and edges to screen to help troubleshoot
    output$troubleshoot <- renderText({
        HTML(
            paste(
            "Last clicked node:", input$node_id, br(),
            "Last clicked edge:", input$edge_id, br(),
            "Current value of node placeholder:", values$setNode, br(),
            "Current value of edge placeholder:", values$setEdge, br()
        )
        )
    })


}

# Run the application
shinyApp(ui = ui, server = server)
