library(shiny)
library(plotly)
library(DT)

ds <- read.csv(file="../concatenated.csv", header=T) #, nrow=1000
ds$ID <- row.names(ds) #seq.int(nrow(ds))

ds_iteration_max <- max(ds$evolution.generation)

ds_cols <- colnames(ds)
ds_cols <- ds_cols[1:length(ds_cols)-1]

ds_vars <- as.list(seq(1,length(ds_cols)))
names(ds_vars) <- ds_cols

# identify numeric columns
ds_numeric_cols <- colnames(ds[,sapply(ds, is.numeric)])
ds_numeric_cols <- sort(ds_numeric_cols[1:length(ds_numeric_cols)-1])
ds_numeric_vars <- as.list(seq(1,length(ds_numeric_cols)))
names(ds_numeric_vars) <- ds_numeric_cols
# (without the iteration column)
ds_numeric_vars <- ds_numeric_vars[2:length(ds_numeric_vars)]
# append the "nothing" choice
ds_numeric_cols <- append(ds_numeric_vars, list("[no color]"=1),0)

# ideas
# https://deanattali.com/blog/advanced-shiny-tips/


# Define UI for app that draws a histogram ----
ui <- fluidPage(
	
	# App title ----
	titlePanel("Evolution"),

	sidebarLayout(
		position = "right",
		sidebarPanel(
			selectInput(
				"colorvar", 
				label = "Color", 
				choices = ds_numeric_cols, 
				selected = 1
			),
			conditionalPanel(
				"input.colorvar!='1'",
				selectInput(
						"colorscale", 
						"scale",
						# from https://plotly.com/r/reference/#heatmap-colorscale
						choices = list("YlGnBu","YlOrRd","Bluered",
								"RdBu","Picnic","Rainbow",
								"Portland","Jet","Hot","Blackbody","Earth",
								"Electric","Viridis","Cividis",
								"Greys","Greens","Reds","Blues"
								),
						selected = "Portland"
						)
			),
			h3("Scatter Plot"),
			checkboxInput("drawScatter", label = "draw X,Y scatter plot", value = TRUE),
			conditionalPanel(
				"input.drawScatter",
				selectInput(
						"x", 
						"X",
						choices = ds_vars[2:length(ds_vars)],
						selected = length(ds_cols)-1
						),
				checkboxInput("xlog", label = "logarithmic", value = FALSE),
				selectInput(
						"y", 
						"Y",
						choices = ds_vars[2:length(ds_vars)],
						selected = length(ds_cols)
						),
				checkboxInput("ylog", label = "logarithmic", value = FALSE)
			),
			h3("Parallel Plot"),
			checkboxInput("drawParallel", label = "draw parallel plot", value = FALSE),
			h3("Scatter Plot Matrix"),
			checkboxInput("drawSplom", label = "draw scatter plot matrix", value = FALSE),
			h3("Data Table"),
			checkboxInput("drawTable", label = "show data table", value = FALSE)
		),
		
		# Main panel for displaying outputs ----
		mainPanel(
			fluidRow(
				column(10,offset = 2,
					sliderInput(
						"sliderIteration", 
						label = "Iteration", 
						min = 1, 
						max = ds_iteration_max, 
						value = ds_iteration_max/5,
						animate = animationOptions(interval=200, loop=F),
						step=ds_iteration_max/1000,
						width='100%'
						)
				),
				column(10,offset = 2,
					textOutput(outputId = "infoIteration")
				)
			),
			conditionalPanel(
				"input.drawScatter",
				plotlyOutput(outputId = "scatterPlot", height='600px')
			),
			conditionalPanel(
				"input.drawParallel",
				plotlyOutput(outputId = "parallelPlot", height='600px')
			),
			conditionalPanel(
				"input.drawSplom",
				plotlyOutput(outputId = "splomPlot", height='600px')
			),
			conditionalPanel(
				"input.drawTable",
				DTOutput(outputId = "datatable")
			)
		)
	)
	
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {

	varx <- reactive({
		ds_cols[as.integer(input$x)]
	})
	vary <- reactive({
		ds_cols[as.integer(input$y)]
	})
	iteration <- reactive({
		as.integer(input$sliderIteration)
	})
	relevant_ds <- reactive({
		ds[ds$evolution.generation==iteration(),]
	})
	
	min_x <- reactive({
		min(ds[,as.integer(input$x)])
	})
	max_x <- reactive({
		max(ds[,as.integer(input$x)])
	})
	
	min_y <- reactive({
		min(ds[,as.integer(input$y)])
	})
	max_y <- reactive({
		max(ds[,as.integer(input$y)])
	})
	
	color_idx <- reactive({
		as.integer(input$colorvar)
	})
	var_color <- reactive({
		color_idx <- color_idx()
		if (color_idx==1) { 
			print("no color!")
			NULL
		} else {
			print("color")
			print(ds_numeric_cols[color_idx])
			ds_numeric_cols[color_idx]
		}
	})
	

	color_scale <- reactive({
		input$colorscale
	})

	selected_keys_without_scatter <- reactive({
		event.data <- event_data(event = "plotly_selected", source = "plotSplom")

		if (is.null(event.data)) {
			if (is.null(input$datatable_rows_selected)) {
				NULL
			} else {
				as.list(as.integer(input$datatable_rows_selected)-1)
			}
		} else {
			which(relevant_ds()$ID %in% as.integer(event.data$key))-1
		}	
	})

	selected_keys_without_splom <- reactive({
		event.data <- event_data(event = "plotly_selected", source = "plotScatter")

		if (is.null(event.data)) {
			if (is.null(input$datatable_rows_selected)) {
				NULL
			} else {
				as.list(as.integer(input$datatable_rows_selected)-1)
			}
		} else {
			which(relevant_ds()$ID %in% as.integer(event.data$key))-1
		}	
	})

	selected_keys_without_table <- reactive({
		event.data <- event_data(event = "plotly_selected", source = "plotScatter")

		if (is.null(event.data)) {		
			event.data <- event_data(event = "plotly_selected", source = "plotSplom")
		} 

		if (is.null(event.data)) {		
			NULL
		} else {
			which(relevant_ds()$ID %in% as.integer(event.data$key))-1
		}	
	})

	tooltips <- reactive({
		d <- relevant_ds()
		d <- d[,3:ncol(d)-1]
		d_cols <- colnames(d)
			apply(d, 1,
				function(line) {
					paste(
					as.vector(
						sapply(
							d_cols, 
							function(k) { 
								paste(k, line[k], sep=":")
							}
						)
					),
					collapse="\n"
					)
				}
				)
		
	})
	
	output$scatterPlot <- renderPlotly({

		if (input$drawScatter) {

			print(paste("iteration", iteration()))
			
			relevant_ds <- relevant_ds()
			
			var_color <- var_color()
			marker_opts <- if (is.null(var_color)) {
				# no color
				list(
					size=7
					)
			} else {
				list(
					size=7, 
					color=relevant_ds[,color_idx()],
					showscale=T,
					colorscale=color_scale()
					)
			}
			
			# define range of variables so they do not change over iterations 
			# (only if numeric!)
			range_x <- if (is.numeric(relevant_ds[,varx()])) {
				margin <- (max_x() - min_x())/100
				c(min_x()-margin,max_x()+margin)
			} else {
				NULL
			}
			range_y <- if (is.numeric(relevant_ds[,vary()])) {
				margin <- (max_y() - min_y())/100
				c(min_y()-margin,max_y()+margin)
			} else {
				NULL
			}
			

			xaxis_opt = list(title=varx(), range=range_x)
			yaxis_opt = list(title=vary(), range=range_y)

			# apply log scales
			if (input$xlog) { 
				xaxis_opt["type"] <- "log"
				xaxis_opt[["range"]] <- NULL
			}
			if (input$ylog) { 
				yaxis_opt["type"] <- "log"
				#yaxis_opt[["range"]] <- c(log(min_y())-margin,log(max_y()))
				yaxis_opt[["range"]] <- NULL
			}

			sp <- selected_keys_without_scatter()
			selectedpoints <- NULL 
			if (!is.null(sp)) { 
				selectedpoints <- sp
				marker_opts["opacity"] <- 0.5
			}
			
			relevant_ds %>%
			plot_ly(
				x=relevant_ds[,varx()], 
				y=relevant_ds[,vary()], 
				type="scatter", 
				mode="markers", 
				marker=marker_opts,
				key=relevant_ds$ID,
				source="plotScatter",
				hoverinfo = "text",
				text=tooltips(),
				selectedpoints=selectedpoints,
				selected=attrs_selected(
					marker = list(
						size=8, 
						color="red",
						opacity = 1
						)
					)
			) %>% layout( 
				xaxis=xaxis_opt, 
				yaxis=yaxis_opt
			) 
			
			
		} else {
			NULL
		}
	})

	output$infoIteration <- renderText({
		paste(nrow(relevant_ds()), "points in the Pareto front at iteration", iteration())
	})
	 
	output$parallelPlot <- renderPlotly({

		if (input$drawParallel) {
			
			relevant_ds <- relevant_ds()

			dimensions <- as.list(
				lapply(
						ds_cols[2:length(ds_cols)], 
						function(v) { 

							if (is.numeric(relevant_ds[,v])) { 
								list(label=v, values=relevant_ds[,v])
							} else {
								vv <- sort(unique(relevant_ds[,v]))
								print("non numeric")
								
								cvals <- seq(1,length(vv)) - length(vv)/2
								vals <- as.list(cvals)
								names(vals) <- vv
								
								mappedvalues <- unlist(sapply(relevant_ds[,v], function(k) { vals[k] }), use.names=F)
								
								list(
									label=v, 
									tickvals=seq(1,length(vv)) - length(vv)/2, 
									ticktext=vv,
									values=mappedvalues
									)
							}
						}
					)
				)

			var_color <- var_color()
			line_opts <- if (is.null(var_color)) {
				# no color
				list()
			} else {
				list(
					color=relevant_ds[,color_idx()],
					showscale = TRUE,
					colorscale=color_scale()
					)
			}

			relevant_ds %>% 
			plot_ly(
				type='parcoords',
				dimensions=dimensions,
				line = line_opts,
				key=relevant_ds$ID,
				source="plotParallel"
				)
		
		} else NULL
	})
	
	output$splomPlot <- renderPlotly({

		if (input$drawSplom) {
			
			relevant_ds <- relevant_ds()

			dimensions <- as.list(
				lapply(
						ds_cols[2:length(ds_cols)], 
						function(v) { 
							#if (is.numeric(relevant_ds[,v])) { 
								list(label=v, values=relevant_ds[,v])
							# } else {
								# vv <- sort(unique(relevant_ds[,v]))
								# print("non numeric")
								
								# cvals <- seq(1,length(vv)) - length(vv)/2
								# vals <- as.list(cvals)
								# names(vals) <- vv
								
								# mappedvalues <- unlist(sapply(relevant_ds[,v], function(k) { vals[k] }), use.names=F)
								
								# list(
									# label=v, 
									# tickvals=seq(1,length(vv)) - length(vv)/2, 
									# ticktext=vv,
									# values=mappedvalues
									# )
							# }
						}
					)
				)
				
			var_color <- var_color()
			marker_opts <- if (is.null(var_color)) {
				# no color
				list(
				  # color = as.integer(df$class),
				  # colorscale = pl_colorscale,
				  size = 7,
				  line = list(
					width = 1,
					color = 'rgb(230,230,230)'
				  )
				)
			} else {
				# with color!
				list(
				  color = relevant_ds[,color_idx()],
				  colorscale=color_scale(),
				  size = 7,
				  line = list(
					width = 1,
					color = 'rgb(230,230,230)'
				  ),
				  showscale=T
				)
			}
			
			axis = list(
				showline=FALSE,
				zeroline=FALSE,
				gridcolor='#ffff',
				ticklen=4)

			
			sp <- selected_keys_without_splom()	
			selectedpoints <- NULL 
			if (!is.null(sp)) { 
				selectedpoints <- sp
				marker_opts["opacity"] <- 0.3
			}


			plot_ly(
				relevant_ds,
				key=relevant_ds$ID,
				source="plotSplom",
				selectedpoints=selectedpoints
			) %>% add_trace(
				type="splom",
				dimensions=dimensions,
				hoverinfo = "text",
				text=tooltips(),
				marker = marker_opts,
				selected=attrs_selected(
					marker = list(
						size=8, 
						color="red",
						opacity = 1.0
						)
					)
			) %>% layout(
				#title='SPLOM',
				hovermode='closest',
				dragmode= 'select',
				plot_bgcolor='rgba(240,240,240, 0.95)',
				xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
				yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
				xaxis2=axis,
				xaxis3=axis,
				xaxis4=axis,
				yaxis2=axis,
				yaxis3=axis,
				yaxis4=axis
			) %>% style(
				diagonal = list(visible = F),
				showlowerhalf = T,
				showupperhalf = F
			)
			
				
		} else NULL
	})

	output$datatable <- renderDT({

		if (input$drawTable) {

			relevant_ds <- relevant_ds()
			
			selected_keys <- selected_keys_without_table()
			selected <- if (is.null(selected_keys)) {
				NULL
			} else {
				selected_keys + 1
			}

			datatable(
			      	relevant_ds[,3:ncol(relevant_ds)-1],
				selection = list(mode='multiple', selected = selected),
				#server = FALSE,
				options = list(
				  	pageLength = 20,
					lengthChange = FALSE
					),
				rownames = FALSE
			)
		} else {
			NULL
		}
	})


}

shinyApp(ui = ui, server = server)

