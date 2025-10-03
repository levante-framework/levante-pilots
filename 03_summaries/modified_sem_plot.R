cplot_happy_arrows <- function(x, y, ...){
  df_nodes <- x$nodes
  df_edges <- x$edges
  if("show" %in% names(df_nodes)){
    df_nodes <- df_nodes[df_nodes$show, , drop = FALSE]
    if(!all(df_edges$from %in% df_nodes$name & df_edges$to %in% df_nodes$name)){
      message("Some edges involve nodes with argument 'show = FALSE'. These were dropped.")
      df_edges <- df_edges[df_edges$from %in% df_nodes$name & df_edges$to %in% df_nodes$name, , drop = FALSE]
    }
  }

  if("show" %in% names(df_edges)){
    df_edges <- df_edges[df_edges$show, , drop = FALSE]
  }
  if(nrow(df_nodes) == 0){
    stop("No nodes left to plot.")
  }
  rect_width <- x$rect_width
  rect_height <- x$rect_height
  ellipses_width <- x$ellipses_width
  ellipses_height <- x$ellipses_height
  variance_width <- x$variance_width
  variance_height <- x$variance_height
  arrow_angle <- x$arrow_angle
  arrow_length <- x$arrow_length
  var_arrow_angle <- x$var_arrow_angle
  var_arrow_length <- x$var_arrow_length
  spacing_x <- x$spacing_x
  spacing_y <- x$spacing_y
  -----------------------
  numeric_cols <- c("curvature")
  df_edges[numeric_cols[which(numeric_cols %in% names(df_edges))]] <- lapply(df_edges[numeric_cols[which(numeric_cols %in% names(df_edges))]], as.numeric)

  if(nrow(df_edges)){
    connect_points <- .connect_points(df_nodes, df_edges)

    df_edges <- cbind(df_edges, connect_points)

    df_edges <- cbind(df_edges, setNames(data.frame(t(apply(connect_points, 1, function(x){(x[1:2]+x[3:4])/2}))), c("text_x

