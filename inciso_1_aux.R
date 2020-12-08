# Function to make graphs
# Arguments: df(dataframe), variable(variable of interest, must be introduced in the form of df$variable), colour (colour of the graph), title, subtitle and size (size of the line).

make_graph <- function (df = source_of_graphs2, variable, colour = "deepskyblue4", title, subtitle = "", size = 1){
  grafico <- ggplot(df, aes(x=time, y = variable))
  grafico <- grafico + geom_line(color = colour, size = size)
  grafico <- grafico + labs(x = " ", y =" ", title = title, subtitle = subtitle)
  grafico <- grafico + theme_classic()
  grafico <- grafico + theme(axis.ticks = element_blank())
  grafico
}