
library(shiny)
library(plotly)
library(DT)

# first list the files available around
list_csv_files <- function() {
	# first read the list of available files here and there
	files <- c(
		list.files(path=".", pattern = "\\.csv$", ignore.case=T, include.dirs=F, full.names=T),
		list.files(path="..", pattern = "\\.csv$", ignore.case=T, include.dirs=F, full.names=T)
	)
	if (!length(files)) {
		stop(paste("no CSV file found in current directory", getwd(), "; please copy a CSV file in the working directory, or specific the file to read using \nlibrary(tools) \n options(plot.evolution.file=file_path_as_absolute(\"myfile.csv\"))"))
	}	
	# read the info from files
	fi <- file.info(files)
	fi_maxdate <- apply(fi, 1, function (d) { if (d["mtime"]>d["ctime"]) d["mtime"] else d["ctime"]  })
	# store into a dataframe
	available_files <- data.frame(files, fi_maxdate, fi$size)
	colnames(available_files) <- c("file","date","size")
	available_files <- available_files[rev(order(available_files$date)),]
}

default_displayed_file <- if (!is.null(getOption("plot.evolution.file"))) {
	# use the file provided by the user
	inf <- file.info(getOption("plot.evolution.file"))
	list(
		file=getOption("plot.evolution.file"),
		date=if (inf$ctime > inf$mtime) inf$ctime else inf$mtime,
		size=inf$size	
	)
} else {
	print("detecting CSV files in the current directory...")
	files <- list_csv_files()
	print(paste(nrow(files), "available files"))
	print(files)
	files[1,]
}

default_displayed_filename <- as.character(default_displayed_file$file)
print(paste("displaying by default",default_displayed_filename))

# read the default file 
default_ds <- read.csv(file=default_displayed_filename, header=T) #, nrow=1000
default_ds$ID <- row.names(default_ds) 

# ideas
# https://deanattali.com/blog/advanced-shiny-tips/


# Define UI for app that draws a histogram ----
ui <- function(request) {fluidPage(
	
	# App title ----
	uiOutput("titlePanel"),
	
	sidebarLayout(
		position = "right",
		sidebarPanel(
			width = 3,
			fileInput(
				"uploadedFile", "upload CSV", 
				multiple = FALSE, 
				accept =  c(
				  "text/csv",
				  "text/comma-separated-values,text/plain",
				  ".csv"), 
				width = NULL),
			uiOutput("inputColorVar"),
			conditionalPanel(
				"input.colorvar!='1'",
				selectInput(	"colorscale", 
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
			h3("scatter plot"),
			checkboxInput("drawScatter", label = "draw X,Y scatter plot", value = TRUE),
			conditionalPanel(
				"input.drawScatter",
				uiOutput("inputScatterX"),
				checkboxInput("xlog", label = "logarithmic", value = FALSE),
				uiOutput("inputScatterY"),
				checkboxInput("ylog", label = "logarithmic", value = FALSE),
				checkboxInput("rangeWhole", label = "range of axis for all iterations", value = TRUE)
			),
			h3("scatter plot matrix"),
			checkboxInput("drawSplom", label = "draw scatter plot matrix", value = FALSE),
			conditionalPanel(
				"input.drawSplom",
				uiOutput("inputVariablesSplom")
			),
			h3("parallel plot"),
			checkboxInput("drawParallel", label = "draw parallel plot", value = FALSE),
			conditionalPanel(
				"input.drawParallel",
				uiOutput("inputVariablesParPlot")
			),
			h3("data table"),
			checkboxInput("drawTable", label = "show data table", value = FALSE),
			conditionalPanel(
				"input.drawTable",
				uiOutput("inputVariablesTable")
			),
		        bookmarkButton()
		),
		
		# Main panel for displaying outputs ----
		mainPanel(
			width = 9,
			fluidRow(
				column(9,offset = 2,
					uiOutput("inputSliderIteration")
				),
				column(9,offset = 2,
					uiOutput(outputId = "infoFile")
				),
				column(9,offset = 2,
					textOutput(outputId = "infoIteration")
				)			
			),
			conditionalPanel(
				"input.drawScatter",
				plotlyOutput(outputId = "scatterPlot", height='550px'),
				hr()
			),			
			conditionalPanel(
				"input.drawSplom",
				plotlyOutput(outputId = "splomPlot", height='600px'),
				hr()
			),
			conditionalPanel(
				"input.drawParallel",
				plotlyOutput(outputId = "parallelPlot", height='550px'),
				hr()
			),
			conditionalPanel(
				"input.drawTable",
				DTOutput(outputId = "datatable"),
				hr()
			)
		)
	),
	uiOutput(outputId = "bottomInfo")
	
)}


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

	# excluse from bookmark the useless values
	setBookmarkExclude(c("plotly_relayout-plotScatter", "plotly_hover-plotScatter", "plotly_afterplot-plotScatter",
				"plotly_afterplot-plotParallel",
				"plotly_afterplot-plotSplom",
				"datatable_rows_current","datatable_cells_selected","datatable_cell_clicked","datatable_rows_selected","datatable_rows_all","datatable_columns_selected","datatable_search","datatable_state",
				".clientValue-default-plotlyCrosstalkOpts"))

	# access the data
	# ... returns either the default dataset, or reads the new dataset from the file provided by the user
	ds <- reactive({
		inFile <- input$uploadedFile
		if (!is.null(inFile)) {
			# TODO trycatch
			print("NEW FILE!!!")
			print(inFile)
			ds <- read.csv(file=inFile$datapath, header=T) #, nrow=1000
			ds$ID <- row.names(ds) 
			ds
		} else {
			default_ds
		}
	})		
	ds_cols <- reactive({
		cols <- colnames(ds())
		cols <- cols[1:length(cols)-1]
		cols
	}) 
	ds_vars <- reactive({
		vars <- as.list(seq(1,length(ds_cols())))
		names(vars) <- ds_cols()
		vars
	})
	ds_numeric_cols <- reactive({
		# identify numeric columns
		ds <- ds()
		ds_numeric_cols <- colnames(ds[,sapply(ds, is.numeric)])
		ds_numeric_cols <- ds_numeric_cols[1:length(ds_numeric_cols)]
		# append the "nothing" choice
		ds_numeric_cols <- append(ds_numeric_cols[2:length(ds_numeric_cols)], list("[no color]"=1),0)

		ds_numeric_cols
	})
	ds_iteration_max <- reactive({
		max(ds()$evolution.generation)
	})
	displayed_file <- reactive({
		inFile <- input$uploadedFile
		if (!is.null(inFile)) {
			print("NEW FILE!!!")
			print(inFile)
			inf <- file.info(inFile$datapath)
			list(
				file=inFile$name,
				date=inf$mtime, # anyway we lost track of the original datetime after the upload :-/ 
				size=inf$size	
			)

		} else {
			default_displayed_file
		}
	})
	display_file_origin <- reactive({
		inFile <- input$uploadedFile
		if (!is.null(inFile)) {
			"uploaded"
		} else {
			"created"
		}
	})
	displayed_file_basename <- reactive({
		basename(as.character(displayed_file()$file))
	})

	# access user parameters
	varx <- reactive({
		ds_cols()[as.integer(input$x)]
	})
	vary <- reactive({
		ds_cols()[as.integer(input$y)]
	})
	iteration <- reactive({
		as.integer(input$sliderIteration)
	})
	relevant_ds <- reactive({
		ds <- ds()
		ds[ds$evolution.generation==iteration(),]
	})
	
	min_x <- reactive({
		min(ds()[,as.integer(input$x)])
	})
	max_x <- reactive({
		max(ds()[,as.integer(input$x)])
	})
	
	min_y <- reactive({
		min(ds()[,as.integer(input$y)])
	})
	max_y <- reactive({
		max(ds()[,as.integer(input$y)])
	})
	
	var_color <- reactive({
		if (is.null(input$colorvar) || input$colorvar=="1") { 
			#print("no color!")
			NULL
		} else {
			#print("color")
			#print(input$colorvar)
			input$colorvar
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
			as.list(which(relevant_ds()$ID %in% as.integer(event.data$key))-1)
		}	
	})

	selected_keys_without_splom <- reactive({
		event.data <- event_data(event = "plotly_selected", source = "plotScatter")
	
		#if (is.null(event.data)) {
		#	event.data <- event_data(event = "plotly_brushing", source = "plotScatter")
		#}
		if (is.null(event.data)) {
			if (is.null(input$datatable_rows_selected)) {
				NULL
			} else {
				as.list(as.integer(input$datatable_rows_selected)-1)
			}
		} else {
			as.list(which(relevant_ds()$ID %in% as.integer(event.data$key))-1)
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

	# all the input UI widgets rendered server side because they rely on data
	output$inputColorVar <- renderUI({
		selectInput(	"colorvar", 
			label = "color", 
			choices = ds_numeric_cols(), 
			selected = 1
			)
	})

	output$inputScatterX <- renderUI({
		ds_vars <- ds_vars()
		selectInput(	"x", "X",
				choices = ds_vars[2:length(ds_vars)],
				selected = length(ds_cols())-1
				)
	})

	output$inputScatterY <- renderUI({
		ds_vars <- ds_vars()
		selectInput(	"y", "Y",
				choices = ds_vars[2:length(ds_vars)],
				selected = length(ds_cols())
				)
	})
	output$inputSliderIteration <- renderUI({	
		ds_iteration_max <- ds_iteration_max()
		sliderInput(
			"sliderIteration", 
			label = "Iteration", 
			min = 1, 
			max = ds_iteration_max, 
			value = ds_iteration_max/5,
			animate = animationOptions(interval=150, loop=F),
			step=ds_iteration_max/1000,
			width='100%'
			)		
	})
	
	output$scatterPlot <- renderPlotly({

		if (input$drawScatter) {

			#print(paste("iteration", iteration()))
			
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
					color=relevant_ds[,var_color],
					showscale=T,
					colorscale=color_scale()
					)
			}
			
			# define range of variables so they do not change over iterations 
			# (only if numeric!)
			range_x <- if (input$rangeWhole && is.numeric(relevant_ds[,varx()])) {
				margin <- (max_x() - min_x())/100
				c(min_x()-margin,max_x()+margin)
			} else {
				NULL
			}
			range_y <- if (input$rangeWhole && is.numeric(relevant_ds[,vary()])) {
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
			
			
		} else NULL
	})

	output$infoIteration <- renderText({
		l <- length(selected_keys_without_table())
		selectedtxt <- if (l > 1) paste("(", l, "points selected",")") else if (l == 1) "( 1 point selected )" else "" 
		paste(nrow(relevant_ds()), "points in the Pareto front at iteration", iteration(), selectedtxt)
	})
	 
	output$parallelPlot <- renderPlotly({

		if (input$drawParallel) {
			
			relevant_ds <- relevant_ds()

			validate(need(length(input$parPlotVariables)>=2, message="at least 2 variables should be select for a parallel plot diagram"))

			dimensions <- as.list(
				lapply(
					input$parPlotVariables,
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
					color=relevant_ds[,var_color],
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

			validate(need(length(input$splomVariables)>2, message="at least 3 variables should be select for a SPLOM"))

			dimensions <- as.list(
				lapply(
					input$splomVariables, 
					function(v) { 
						list(label=v, values=relevant_ds[,v])
					}
					)
				)
				
			var_color <- var_color()
			marker_opts <- if (is.null(var_color)) {
				# no color
				list(
					size = 6,
					line = list(
						width = 1,
						color = 'rgb(230,230,230)'
					)
				)
			} else {
				# with color!
				list(
					color = relevant_ds[,var_color],
					colorscale=color_scale(),
					size = 6,
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
					opacity = 0.5,
					marker = list(
						size=7, 
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

			validate(need(length(input$tableVariables)>2, message="please select at least two columns to visualize"))

			relevant_ds <- relevant_ds()
			
			selected_keys <- selected_keys_without_table()
			selected <- if (is.null(selected_keys)) {
				NULL
			} else {
				selected_keys + 1
			}

			datatable(
			      	relevant_ds[,input$tableVariables], #relevant_ds[,3:ncol(relevant_ds)-1],
				selection = list(mode='multiple', selected = selected),
				#server = FALSE,
				options = list(
				  	pageLength = 20,
					lengthChange = FALSE
					),
				rownames = FALSE
			)
		} else NULL
	})



	output$infoFile <- renderUI({
		displayed_file <- displayed_file()
		size_mb <- as.integer(displayed_file$size/1024/1024)
		size <- if (size_mb <= 0) {
			paste(as.integer(displayed_file$size/1024), "Kb")
		} else {
			paste(size_mb, "Mb")			
		}

		tagList("displaying file", code(displayed_file_basename()), 
			display_file_origin(), "on", as.character(displayed_file$date), 
			"(", size, ")")
	})


	output$inputVariablesSplom <- renderUI({
		ds_cols <- ds_cols()
		selectInput(
			"splomVariables", label = "show variables", 
			choices = as.list(ds_cols[2:length(ds_cols)]),
			selected = as.list(ds_cols[max(2, length(ds_cols)-4):length(ds_cols)]), # select by default up to 4 last columns
			multiple = T)
	})

	output$inputVariablesParPlot <- renderUI({
		ds_cols <- ds_cols()
		selectInput(
			"parPlotVariables", label = "show variables", 
			choices = as.list(ds_cols[2:length(ds_cols)]),
			selected = as.list(ds_cols[max(2, length(ds_cols)-8):length(ds_cols)]), # select by default up to 4 last columns
			multiple = T)
	})

	output$inputVariablesTable <- renderUI({
		ds_cols <- ds_cols()
		selectInput(
			"tableVariables", label = "show columns", 
			choices = as.list(ds_cols[2:length(ds_cols)]),
			selected = as.list(ds_cols[max(2, length(ds_cols)-12):length(ds_cols)]), # select by default up to 4 last columns
			multiple = T)
	})


	output$bottomInfo <- renderUI({
		tagList("LGPL-2.1 License. Update, track bug, contribute: ", 
			a("github.com/samthiriot/plot-evolutionary-algo",href="https://github.com/samthiriot/plot-evolutionary-algo",target="_blank"))
	})

	output$titlePanel <- renderUI({
		titlePanel(displayed_file_basename())
	})


}

options(shiny.maxRequestSize = 100*1024^2)
shinyApp(ui = ui, server = server, enableBookmarking = "url")

