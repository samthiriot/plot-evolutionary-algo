library(shiny)
library(ggplot2)
library(plotly)


ds <- read.csv(file="../concatenated.csv", header=T) #, nrow=1000

ds_iteration_max <- max(ds$evolution.generation)

ds_cols <- colnames(ds)

ds_vars <- as.list(seq(1,length(ds_cols)))
names(ds_vars) <- ds_cols

# identify numeric columns
ds_numeric_cols <- sort(colnames(ds[,sapply(ds, is.numeric)]))
ds_numeric_vars <- as.list(seq(1,length(ds_numeric_cols)))
names(ds_numeric_vars) <- ds_numeric_cols
# (without the iteration column)
ds_numeric_vars <- ds_numeric_vars[2:length(ds_numeric_vars)]
# append the "nothing" choice
ds_numeric_cols <- append(ds_numeric_vars, list("[no color]"=1),0)

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
								#"Viridis","Blackbody","Bluered","Blues","Jet",
								#"Magma","Plasma","Dense"
								#"Aggrnyl","Agsunset","Blugrn","Bluyl","Brwnyl","Plotly3"
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
			checkboxInput("drawSplom", label = "draw scatter plot matrix", value = FALSE)
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
					textOutput(outputId = "infoIteration"),
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

	tooltips <- reactive({
		d <- relevant_ds()
		d <- d[,2:ncol(d)]
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
				print(xaxis_opt["range"])
				margin <- (max_x() - min_x())/100
				xaxis_opt["range"] <- c(log(min_x()-margin),log(max_x()+margin))
			}
			if (input$ylog) { 
				yaxis_opt["type"] <- "log"
				margin <- (max_y() - min_y())/100
				yaxis_opt["range"] <- c(log(min_y()-margin),log(max_y()+margin))
			}

			plot_ly(
				data=relevant_ds, 
				x=relevant_ds[,varx()], 
				y=relevant_ds[,vary()], 
				type="scatter", 
				mode="markers", 
				marker=marker_opts,
				hoverinfo = "text",
				text=tooltips()
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
			plot_ly(
				data=relevant_ds, 
				type='parcoords',
				dimensions=dimensions,
				line = line_opts
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
				
			relevant_ds %>% plot_ly(
			) %>% add_trace(
				type="splom",
				dimensions=dimensions,
				hoverinfo = "text",
				text=tooltips(),
				marker = marker_opts
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

}

shinyApp(ui = ui, server = server)

