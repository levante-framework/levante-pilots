
plot_happy_arrows <- function(x, y, ...){
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
  text_size <- x$text_size
  fix_coord <- x$fix_coord

  # Check dfs ---------------------------------------------------------------
  numeric_cols <- c("curvature")
  df_edges[numeric_cols[which(numeric_cols %in% names(df_edges))]] <- lapply(df_edges[numeric_cols[which(numeric_cols %in% names(df_edges))]], as.numeric)

  if(nrow(df_edges)){
    connect_points <- .connect_points(df_nodes, df_edges)

    df_edges <- cbind(df_edges, connect_points)

    df_edges <- cbind(df_edges, setNames(data.frame(t(apply(connect_points, 1, function(x){(x[1:2]+x[3:4])/2}))), c("text_x", "text_y")))
  }


  # Make plot ---------------------------------------------------------------

  p <- ggplot(NULL)

  if(any(df_edges$from == df_edges$to)){
    p <- .plot_variances(p, df = df_edges[df_edges$from == df_edges$to, ], text_size = text_size, height = variance_height, width=variance_width,
                         arrow_angle=var_arrow_angle, arrow_length=var_arrow_length)
  }
  if(any(!df_edges$from == df_edges$to)){
    p <- .plot_edges(p, df_edges[!df_edges$from == df_edges$to, ], text_size = text_size, arrow_angle= arrow_angle, arrow_length=arrow_length)
  }

  p <- .plot_nodes(p, df = df_nodes, text_size = text_size, ellipses_width = ellipses_width, ellipses_height = ellipses_height)

  if("level" %in% names(df_nodes) & "level" %in% names(df_edges)){
    if("group" %in% names(df_nodes) & "group" %in% names(df_edges)){
      p <- p + facet_grid(level~group, scales = "free")
    } else {
      p <- p + facet_wrap(~level, scales = "free")
    }
  } else {
    if("group" %in% names(df_nodes) & "group" %in% names(df_edges)){
      p <- p + facet_wrap(~group, scales = "free")
    } else {

    }
  }
  if(fix_coord){
    if(!("level" %in% names(df_nodes) | "level" %in% names(df_edges) | "group" %in% names(df_nodes) | "group" %in% names(df_edges))){
      p <- p + coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
    } else {
      p <- p+ theme(aspect.ratio = 1)
    }
  }
  p + theme(axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(), legend.position = "none",
            panel.background = element_blank(), panel.border = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.background = element_blank())

}

#' @importFrom stats na.omit
.connect_points <- function(df_nodes, df_edges){
  node_id <- do.call(paste0, df_nodes[, na.omit(match(c("name", "group", "level"), names(df_nodes))), drop = FALSE])

  edge_id <- do.call(paste0, df_edges[, na.omit(match(c("from", "group", "level"), names(df_edges))), drop = FALSE])

  loc <- list(
    c("node_xmin", "y"),
    c("node_xmax", "y"),
    c("x", "node_ymax"),
    c("x", "node_ymin"))[match(df_edges$connect_from, c("left", "right", "top", "bottom"))]
  from_loc <- t(mapply(function(x,y){unlist(df_nodes[node_id == x, y])}, x = edge_id, y = loc, USE.NAMES = FALSE))
  edge_id <- do.call(paste0, df_edges[, na.omit(match(c("to", "group", "level"), names(df_edges))), drop = FALSE])
  loc <- list(
    c("node_xmin", "y"),
    c("node_xmax", "y"),
    c("x", "node_ymax"),
    c("x", "node_ymin"))[match(df_edges$connect_to, c("left", "right", "top", "bottom"))]
  to_loc <- t(mapply(function(x,y){unlist(df_nodes[node_id == x, y])}, x = edge_id, y = loc, USE.NAMES = FALSE))

  setNames(data.frame(cbind(from_loc, to_loc)), c("edge_xmin", "edge_ymin", "edge_xmax", "edge_ymax"))
}

matrix_to_nodes <- function(nodes, shape){
  nodes_long <- setNames(as.data.frame.table(nodes), c("y", "x", "label"))
  nodes_long[1:2] <- lapply(nodes_long[1:2], as.numeric)
  nodes_long$y <- (max(nodes_long$y)+1)-nodes_long$y
  nodes_long$label <- as.character(nodes_long$label)
  nodes_long$shape <- as.vector(shape)
  nodes_long <- nodes_long[!nodes_long$label == "", ]
  nodes_long
}

globalVariables(c("name", "..count.."))

#' @title Extract nodes from a SEM model object
#' @description Attempts to extract nodes from a SEM model object, where nodes
#' are defined as observed or latent variables.
#' @param x A model object of class \code{mplusObject} or \code{lavaan}.
#' @param label Either a character, indicating which column to use for node
#' labels, or an expression. See Details.
#' Defaults to \code{paste(name, est_sig, sep = "\n"},
#' which gives the node name followed by the estimated value with
#' significance asterisks.
#' @param ... Additional parameters passed to \code{\link{table_results}}. For
#' example, users can pass the \code{digits} argument to control the number of
#' digits in the node label, or pass the \code{columns} argument to retain
#' auxiliary columns in the \code{tidy_nodes data.frame} for further processing
#' (see Examples).
#' @details The function \code{get_nodes} identifies all dependent and
#' independent variables in the model as nodes. If a mean structure / intercepts
#' are included in the model, the output of \code{table_results} for those
#' means / intercepts is used to label the nodes.
#'
#' ## Custom labels
#'
#' One way to create custom node labels is by passing an expression to
#' \code{label}, as in the default value of the argument. When an expression is
#' passed to \code{label}, it is evaluated in the context of a \code{data.frame}
#' containing the results
#' of a call to \code{\link{table_results}} on the \code{x} argument, with an
#' additional column labeled \code{name}, which contains the node names.
#'
#' Another way to create custom labels is by requesting auxiliary variables
#' using the \code{columns} argument (which is passed to
#' \code{\link{table_results}}), and then using these columns to construct a new
#' label. See examples.
#' @return An object of class 'tidy_nodes'
#' @examples
#' # Standard use extracts node names and shape
#' # (rect for observed, oval for latent)
#' library(lavaan)
#' res <- sem("dist ~ speed", cars)
#' get_nodes(res)
#'
#' # To label nodes with mean values, include meanstructure in the model
#' # Note that it is possible to pass the argument 'digits' to table_results
#' # through '...'
#' res <- sem("dist ~ speed", cars, meanstructure = TRUE)
#' get_nodes(res, digits = 3)
#'
#' # Pass an expression to the 'label' argument for custom labels
#' get_nodes(res, label = paste0(name, " ", est_sig, "\n", confint))
#'
#' # Pass the argument 'columns' to table_results through '...' to retain
#' # auxiliary columns for further processing
#' nod <- get_nodes(res, columns = c("est_sig", "confint"))
#' nod
#' nod <- within(nod, {label <- paste0(name, " ", est_sig, "\n", confint)})
#' nod
#' @rdname get_nodes
#' @keywords tidy_graph
#' @export
get_nodes <- function(x, label = paste2(name, est_sig, sep = "\n"), ...){
  UseMethod("get_nodes", x)
}

# # @method get_nodes mplus.model
# # @export
# get_nodes.mplus.model <- function(x, label = "est_sig", ...){
#   Args <- as.list(match.call()[-1])
#   Args$x <- table_results(x, columns = NULL)
#   do.call(get_nodes, Args)
# }

# @method get_nodes character
# @export
# get_nodes.character <- function(x, label = NULL, ...){
#   dots <- list(...)
#   cl <- match.call()
#   cl["columns"] <- list(NULL)
#   cl[[1L]] <- str2lang("tidySEM::table_results")
#   cl$x <- eval.parent(cl)
#   if("columns" %in% names(dots)){
#     cl["columns"] <- dots["columns"]
#   }
#   cl[[1L]] <- str2lang("tidySEM::get_nodes")
#   eval.parent(cl)
# }



#' @method get_nodes lavaan
#' @export
#' @importFrom lavaan parameterTable lavInspect
get_nodes.lavaan <- function(x, label = paste2(name, est_sig, sep = "\n"), ...){
  dots <- list(...)
  cl <- match.call()
  cl["columns"] <- list(NULL)
  cl[[1L]] <- str2lang("tidySEM::table_results")
  cl$x <- eval.parent(cl)
  # if(!fit@Options[["meanstructure"]]){
  #   message("Use the argument 'meanstructure = TRUE' in the lavaan call to obtain information about node means.")
  # }
  if("columns" %in% names(dots)){
    cl["columns"] <- dots["columns"]
  }
  cl[[1L]] <- str2lang("tidySEM::get_nodes")
  eval.parent(cl)
}

#' @method get_nodes default
#' @export
get_nodes.default <- function(x, label = paste2(name, est_sig, sep = "\n"), ...){
  dots <- list(...)
  cl <- match.call()
  cl["columns"] <- list(NULL)
  cl[[1L]] <- str2lang("tidySEM::table_results")
  cl$x <- tryCatch(eval.parent(cl), error = function(e){
    stop("Could not extract nodes from object.")
  })
  if("columns" %in% names(dots)){
    cl["columns"] <- dots["columns"]
  }
  cl[[1L]] <- str2lang("tidySEM::get_nodes")
  eval.parent(cl)
}

#' @method get_nodes mplusObject
#' @export
get_nodes.mplusObject <- get_nodes.lavaan


#' @method get_nodes MxModel
#' @export
get_nodes.MxModel <- get_nodes.lavaan


#' @method get_nodes mplus.object
#' @export
get_nodes.mplus.object <- get_nodes.lavaan

#' @method get_nodes mplus.model
#' @export
get_nodes.mplus.model <- get_nodes.lavaan

#' @method get_nodes tidy_results
#' @export
get_nodes.tidy_results <- function(x, label = paste2(name, est_sig, sep = "\n"), label_name = TRUE, ...){
  dots <- list(...)
  cl <- match.call()
  cl[[1L]] <- str2lang("tidySEM:::get_nodes.tidy_results")
  if("group" %in% names(x)){
    x_list <- lapply(unique(x$group), function(i){
      cl$x <- x[x$group == i, -which(names(x) == "group")]
      tmp <- eval(cl)
      tmp$group <- i
      tmp
    })
    return(bind_list(x_list))
  }
  if("level" %in% names(x)){
    x_list <- lapply(unique(x$level), function(i){
      cl$x <- x[x$level == i, -which(names(x) == "level")]
      tmp <- eval(cl)
      tmp$name <- paste0(tmp$name, ".", i)
      tmp$level <- i
      tmp
    })
    return(bind_list(x_list))
  }
  if(!all(c("lhs", "rhs", "op") %in% names(x))){
    if(!"label" %in% names(x)){
      stop("Could not extract edges; re-run with table_results(columns = NULL).")
    }
    addcols <- .labeltolhsrhsop(x$label)
    x <- cbind(x, addcols[, names(addcols)[!names(addcols) %in% names(x)], drop = FALSE])
  }
  latent <- unique(x$lhs[x$op == "=~"])
  obs <- unique(c(x$lhs[x$op %in% c("~~", "~", "~1")], x$rhs[x$op %in% c("~")]))
  nodes <- unique(c(latent, obs))
  nodes <- data.frame(name = unique(nodes), shape = c("rect", "oval")[(unique(nodes) %in% latent)+1], stringsAsFactors = FALSE)
  #nodes$label <- nodes$name

  # Make a data.frame based on the information about node means
  node_df <- x[x$op == "~1", ]
  if("label" %in% names(node_df)){
    names(node_df)[names(node_df) == "label"] <- "label_results"
  }
  # Keep only rows corresponding to the identified nodes
  node_rows <- match(nodes$name, node_df$lhs)
  if(any(!is.na(node_rows))){
    node_df$name <- node_df$lhs
    nodes <- merge(nodes, node_df, by = "name", all.x = TRUE)
  }
  # If the user asked for a label
  # Check if the label is an expression. If it is, substitute it
  if (inherits(suppressWarnings(try(label, silent = TRUE)), "try-error")) {
    label <- substitute(label)
  }

  if(is.character(label)){
    if(!is.null(nodes[[label]])){
      nodes[["label"]] <- nodes[[label]]
    } else {
      nodes[["label"]] <- nodes[["name"]]
    }
  } else {
    if(!is.null(label)){
      nodes[["label"]] <- tryCatch(eval(label, envir = nodes), error = function(e){
        nodes[["name"]]
      })
    } else {
      nodes[["label"]] <- ""
    }
  }

  # Retain all requested columns
  if(is.null(dots[["columns"]])){
    keep_cols <- unique(c(c("name", "shape", "label"), names(node_df)[names(node_df) %in% names(nodes)]))
  } else {
    keep_cols <- c(c("name", "shape", "label"), dots[["columns"]])
  }

  nodes <- nodes[, keep_cols, drop = FALSE]
  class(nodes) <- c("tidy_nodes", class(nodes))
  nodes
}

#' @title Extract edges from a SEM model object
#' @description Attempts to extract edges from a SEM model object, where edges
#' are defined as regression paths and covariances between variables (nodes).
#' @param x A model object of class \code{mplusObject} or \code{lavaan}.
#' @param label Either a character, indicating which column to use for edge
#' labels, or an expression. See Details.
#' Defaults to \code{"est_sig"},
#' which labels edges with the estimated value with significance asterisks,
#' as obtained from \code{\link{table_results}}. See Details and examples for
#' more information.
#' @param ... Additional parameters passed to \code{\link{table_results}}. For
#' example, users can pass the \code{digits} argument to control the number of
#' digits in the edge label, or pass the \code{columns} argument to retain
#' auxiliary columns in the \code{tidy_edges data.frame} for further processing
#' (see Examples).
#' @details The function \code{get_edges} identifies all regression paths,
#' latent variable definitions, and covariances in the model as edges.
#' The output of \code{table_results} for those
#' paths is used to label the edges.
#'
#' ## Custom labels
#'
#' One way to create custom edge labels is by passing an expression to
#' \code{label}. When an expression is
#' passed to \code{label}, it is evaluated in the context of a \code{data.frame}
#' containing the results
#' of a call to \code{\link{table_results}} on the \code{x} argument.
#'
#' Another way to create custom labels is by requesting auxiliary variables
#' using the \code{columns} argument (which is passed to
#' \code{\link{table_results}}), and then using these columns to construct a new
#' label. See examples.
#' @return An object of class 'tidy_edges'
#' @examples
#' # Standard use
#' library(lavaan)
#' res <- sem("dist ~ speed", cars)
#' get_edges(res)
#'
#' # Pass an expression to the 'label' argument for custom labels
#' get_edges(res, label = paste(est_sig, confint))
#'
#' # Pass the argument 'columns' to table_results through '...' to retain
#' # auxiliary columns for further processing
#' edg <- get_edges(res, columns = c("est_sig", "confint"))
#' edg
#' edg <- within(edg, {label <- paste(est_sig, confint)})
#' edg
#' @rdname get_edges
#' @keywords tidy_graph
#' @export
get_edges <- function(x, label = "est_sig", ...){
  UseMethod("get_edges", x)
}

#' @method get_edges default
#' @export
get_edges.default <- function(x, label = paste2(name, est_sig, sep = "\n"), ...){
  dots <- list(...)
  cl <- match.call()
  cl["columns"] <- list(NULL)
  cl[[1L]] <- str2lang("tidySEM::table_results")
  cl$x <- tryCatch(eval.parent(cl), error = function(e){
    stop("Could not extract edges from object.")
  })
  if("columns" %in% names(dots)){
    cl["columns"] <- dots["columns"]
  }
  cl[[1L]] <- str2lang("tidySEM:::get_edges.tidy_results")
  eval.parent(cl)
}

# @method get_edges character
# @export
# get_edges.character <- function(x, label = NULL, ...){
#   dots <- list(...)
#   cl <- match.call()
#   cl["columns"] <- list(NULL)
#   cl[[1L]] <- str2lang("tidySEM::table_results")
#   cl$x <- eval.parent(cl)
#   if("columns" %in% names(dots)){
#     cl["columns"] <- dots["columns"]
#   }
#   cl[[1L]] <- quote(get_edges)
#   eval.parent(cl)
# }

#' @method get_edges lavaan
#' @export
get_edges.lavaan <- function(x, label = "est_sig", ...){
  dots <- list(...)
  cl <- match.call()
  #cl[["label"]] <- force(label)
  cl[[1L]] <- str2lang("tidySEM::table_results")
  cl["columns"] <- list(NULL)
  cl$x <- eval.parent(cl)
  if("columns" %in% names(dots)){
    cl["columns"] <- dots["columns"]
  }
  cl[[1L]] <- str2lang("tidySEM:::get_edges.tidy_results")
  eval.parent(cl)
}

#' @method get_edges mplusObject
#' @export
get_edges.mplusObject <- get_edges.lavaan

#' @method get_edges MxModel
#' @export
get_edges.MxModel <- get_edges.lavaan

#' @method get_edges mplus.object
#' @export
get_edges.mplus.object <- get_edges.lavaan

#' @method get_edges mplus.model
#' @export
get_edges.mplus.model <- get_edges.lavaan

#' @method get_edges tidy_results
#' @export
get_edges.tidy_results <- function(x, label = "est_sig", ...){
  dots <- list(...)
  cl <- match.call()
  if(!is.null(attr(x, "user_specified"))){
    x$show <- attr(x, "user_specified")
  }
  if("group" %in% names(x)){
    x_list <- lapply(na.omit(unique(x$group)), function(i){
      cl$x <- x[x$group == i, -which(names(x) == "group")]
      tmp <- eval.parent(cl)
      tmp$group <- i
      tmp
    })
    return(bind_list(x_list))
  }
  if("level" %in% names(x)){
    x_list <- lapply(na.omit(unique(x$level)), function(i){
      cl$x <- x[x$level == i, -which(names(x) == "level")]
      tmp <- eval.parent(cl)
      tmp$from <- paste0(tmp$from, ".", i)
      tmp$to <- paste0(tmp$to, ".", i)
      tmp$level <- i
      tmp
    })
    return(bind_list(x_list))
  }
  if(!all(c("lhs", "rhs", "op") %in% names(x))){
    if(!"label" %in% names(x)){
      stop("Could not extract edges; re-run with table_results(columns = NULL).")
    }
    addcols <- .labeltolhsrhsop(x$label)
    x <- cbind(x, addcols[, names(addcols)[!names(addcols) %in% names(x)], drop = FALSE])
  }
  if("label" %in% names(x)){
    names(x)[names(x) == "label"] <- "label_results"
  }
  x <- x[x$op %in% c("~", "~~", "=~"), ]
  keep_cols <- names(x)
  x$from <- x$lhs
  x$to <- x$rhs
  x$arrow <- NA
  x$arrow[x$op == "~~"] <- "none" # Covariances
  x$arrow[x$op == "~~" & x$lhs == x$rhs] <- "both" # Variances
  x$arrow[x$op == "=~"] <- "last"
  x$arrow[x$op == "~"] <- "last"
  x$from[x$op == "~"] <- x$rhs[x$op == "~"]
  x$to[x$op == "~"] <- x$lhs[x$op == "~"]

  # If the user asked for a label
  # Check if the label is an expression. If it is, substitute it
  if (inherits(suppressWarnings(try(label, silent = TRUE)), "try-error")) {
    label <- substitute(label)
  }

  if(!is.null(dots[["columns"]])){
    keep_cols <- dots[["columns"]]
  }

  if(is.character(label)){
    if(!is.null(x[[label]])){
      x[["label"]] <- x[[label]]
      keep_cols <- c("label", keep_cols)
    }
  } else {
    if(!is.null(label)){
      x[["label"]] <- tryCatch(eval(label, envir = x), error = function(e){
        message("Could not construct label in get_edges().")
      })
      keep_cols <- c("label", keep_cols)
    }
  }



  tmp <- x #data.frame(as.list(x)[ c("from", "to", "arrow", keep_cols) ], check.names=FALSE)
  tmp$curvature <- tmp$connect_to <- tmp$connect_from <- NA
  tmp$curvature[x$op == "~~" & !x$lhs == x$rhs] <- 60
  tmp$linetype <- 1
  tmp$linetype[x$op == "~~" & !x$lhs == x$rhs] <- 2
  keep_cols <- unique(c("from", "to", "arrow", "label", "connect_from", "connect_to", "curvature", "linetype", keep_cols))
  keep_cols <- keep_cols[keep_cols %in% names(tmp)]
  tmp <- tmp[, keep_cols, drop = FALSE]
  class(tmp) <- c("tidy_edges", class(tmp))
  attr(tmp, "which_label") <- label
  row.names(tmp) <- NULL
  tmp
}

.labeltolhsrhsop <- function(lbl){
  op <- gsub("^.+?\\.(BY|ON|WITH)\\..+$", "\\1", lbl)
  op <- gsub("\\..*$", "", op)
  repl <- c("BY" = "=~", "WITH" = "~~", "Variances" = "~~", "Means" = "~1", "ON" = "~")
  op <- repl[op]
  lhs <- gsub("^.+\\b(.+?)\\.(BY|WITH|ON)\\..*$", "\\1", lbl)
  lhs <- gsub("^(Variances|Means)\\.", "", lhs)
  rhs <- gsub("^.+\\.(BY|WITH|ON)\\.(.+?)\\b.*$", "\\2", lbl)
  rhs <- gsub("^(Variances|Means)\\.", "", rhs)
  return(data.frame(lhs = lhs, op = op, rhs = rhs))
}

.euclidean_distance <- function(p, q){
  sqrt(sum((p - q)^2))
}

.manhattan_distance <- function(p, q){
  sum(abs(p-q))
}

match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  myfor <- evalq(formals(), parent.frame(1))

  for(i in setdiff(names(myfor), names(call)))
    call[i] <- list( myfor[[i]] )


  match.call(sys.function(sys.parent()), call)
}

.plot_variances <- function(p, df, text_size, width, height, arrow_angle, arrow_length, ...) {
  npoints <- 20
  xlabel <- xcenter <- df$edge_xmin
  ylabel <- ycenter <- df$edge_ymin
  
  offset <- rep(1.5, length(xcenter))
  
  ycenter[df$connect_from == "top"] <-
    ycenter[df$connect_from == "top"] + height/2
  ylabel[df$connect_from == "top"] <-
    ylabel[df$connect_from == "top"] + height
  ycenter[df$connect_from == "bottom"] <-
    ycenter[df$connect_from == "bottom"] - height/2
  ylabel[df$connect_from == "bottom"] <-
    ylabel[df$connect_from == "bottom"] - height
  offset[df$connect_from == "bottom"] <- .5
  xcenter[df$connect_from == "left"] <-
    xcenter[df$connect_from == "left"] - width/2
  xlabel[df$connect_from == "left"] <-
    xlabel[df$connect_from == "left"] - width
  offset[df$connect_from == "left"] <- 0
  xcenter[df$connect_from == "right"] <-
    xcenter[df$connect_from == "right"] + width/2
  xlabel[df$connect_from == "right"] <-
    xlabel[df$connect_from == "right"] + width
  offset[df$connect_from == "right"] <- 1
  df_label <- data.frame(df,
                         x = xlabel,
                         y = ylabel,
                         label = df$label)

  df_ellipse <-
    data.frame(do.call(rbind, lapply(1:nrow(df), function(this_var) {
      point_seq <-
        seq((offset[[this_var]] * pi +.4), (2 + offset[[this_var]]) * pi -.4, length.out = npoints) %% (2 *
                                                                                                  pi)
      matrix(
        c(
          xcenter[[this_var]] + width/2 * cos(point_seq),
          ycenter[[this_var]] + height/2 * sin(point_seq),
          rep(this_var, npoints)
        ),
        nrow = npoints,
        ncol = 3,
        dimnames = list(NULL, c("x", "y", "id"))
      )
    })))

  df$id <- 1:nrow(df)
  df_ellipse <- merge(df_ellipse, df, by = "id")

  p <- .plot_edges_internal(p, df_ellipse, arrow_angle=arrow_angle, arrow_length=arrow_length)
  .plot_label_internal(p, df_label, text_size)
}

.plot_nodes <- function(p, df, text_size, ellipses_width, ellipses_height){
  # Prepare aesthetics ------------------------------------------------------
  if(any(c("colour", "color") %in% names(df))){
    df[[which(names(df) %in% c("colour", "color"))]] <- as.character(df[[which(names(df) %in% c("colour", "color"))]])
  } else {
    df$colour <- "black"
  }

  if("fill" %in% names(df)){
    df$fill <- as.character(df$fill)
  } else {
    df$fill <- "white"
  }

  if(any(df$shape == "rect")){
    df_rect <- df[df$shape == "rect", ]
    Args <- c("linetype", "size", "colour", "color", "fill", "alpha", "linewidth")
    Args <- as.list(df_rect[which(names(df_rect) %in% Args)])
    Args <- c(list(
      data = df_rect,
      mapping = aes(xmin = .data[["node_xmin"]], xmax = .data[["node_xmax"]], ymin = .data[["node_ymin"]], ymax = .data[["node_ymax"]])),
      Args)
    p <- p + do.call(geom_rect, Args)
  }
  if(any(df$shape == "oval")){
    p <- .oval_node(p, df = df[df$shape == "oval", ], oval_width = ellipses_width, oval_height = ellipses_height, npoints = 360)
  }
  # Hier
  .plot_label_internal(p = p, df = df, text_size = text_size)
}

.determine_connections <- function(df_nodes, df_edges, angle){
  # Begin recursion
  if("group" %in% names(df_nodes)){
    x_list <- lapply(unique(df_nodes$group), function(i){
      .determine_connections(df_nodes = df_nodes[df_nodes$group == i, -which(names(df_nodes) == "group")],
                             df_edges = df_edges[df_edges$group == i, -which(names(df_edges) == "group")],
                             angle = angle)
    })
    return(bind_list(x_list))
  }
  if("level" %in% names(df_nodes)){
    x_list <- lapply(unique(df_nodes$level), function(i){
      .determine_connections(df_nodes = df_nodes[df_nodes$level == i, -which(names(df_nodes) == "level")],
                             df_edges = df_edges[df_edges$level == i, -which(names(df_edges) == "level")],
                             angle = angle)
    })
    return(bind_list(x_list))
  }
  # End recursion
  if(!is.null(angle)){
    .connect_using_angle(df_nodes = df_nodes, df_edges = df_edges, angle = angle)
  } else {
    .connect_nearest_neighbor(df_nodes = df_nodes, df_edges = df_edges, angle = angle)
  }
}

#' @importFrom RANN nn2
.connect_nearest_neighbor <- function(df_nodes, df_edges, angle){
  is_variance <- df_edges$from == df_edges$to
  out <- matrix(NA_character_, nrow = 2, ncol = nrow(df_edges))
  if(any(!is_variance)){
    connect_from <- apply(df_edges[!is_variance, ], 1, function(thisedge){
      points1 <- matrix(unlist(df_nodes[df_nodes$name == thisedge["from"], c("node_xmin", "x", "node_xmax", "x", "y", "node_ymin", "y", "node_ymax")]), ncol = 2)
      points2 <- matrix(unlist(df_nodes[df_nodes$name == thisedge["to"], c("node_xmin", "x", "node_xmax", "x", "y", "node_ymin", "y", "node_ymax")]), ncol = 2)
      indx1 <- RANN::nn2(points1, points2, k = 1, searchtype = "standard")
      indx2 <- RANN::nn2(points2, points1, k = 1, searchtype = "standard")
      c(c("left", "bottom", "right", "top")[indx1$nn.idx[which.min(indx1$nn.dists)]],
        c("left", "bottom", "right", "top")[indx2$nn.idx[which.min(indx2$nn.dists)]]
      )
    })
    out[, !is_variance] <- connect_from
  }
  if(any(is_variance)){
    graphmid <- c(median(df_nodes$x), median(df_nodes$y))

    varvariables <- df_edges$from[which(is_variance)]
    vars <- matrix(c(df_nodes$x[match(varvariables, df_nodes$name)], df_nodes$y[match(varvariables, df_nodes$name)]), ncol = 2)

    tmp <- vars - matrix(rep(graphmid, each = nrow(vars)), ncol = 2)
    angls <- (apply(tmp, 1, function(i){do.call(atan2, as.list(i))}) *180/pi) %% 360
    # Create list of preferred positions for each node
    position <- rep("top", length(varvariables))
    position[angls > 46 & angls < 135] <- "right"
    position[angls >= 135 & angls < 226] <- "bottom"
    position[angls >= 226 & angls < 315] <- "left"

    # Per node, check which positions are least busy and choose the preferred one
    for(thisvariable in varvariables){
      fromthisnode <- df_edges$from[!is_variance] == thisvariable
      tothisnode <- df_edges$to[!is_variance] == thisvariable
      if(any(fromthisnode | tothisnode)){

        connections <- table(c(c("bottom", "left", "right", "top"), connect_from[1, which(fromthisnode)], connect_from[2, which(tothisnode)])) - 1
        opposite <- c("top", "right", "left", "bottom")[which.max(connections)]
        if(connections[opposite] > 0){
          position[which(thisvariable == varvariables)] <- names(connections)[which.min(connections)]
        } else {
          position[which(thisvariable == varvariables)] <- opposite
        }

      }
    }
    out[, is_variance] <- matrix(rep(position, each = 2), nrow = 2)
  }

  cbind(t(out), df_edges$curvature)


}


.connect_using_angle <- function(df_nodes, df_edges, angle){
  connector_sides <-
    cbind(c("left", "right", "bottom", "top")[rep(1:4, each = 4)],
          c("left", "right", "bottom", "top")[rep(1:4, 4)])
  out <- matrix(nrow = nrow(df_edges), ncol = 2)


  # Connect nodes -----------------------------------------------------------
  curws <- df_edges$curvature
  # For columns
  same_column <- df_nodes$x[match(df_edges$from, df_nodes$name)] == df_nodes$x[match(df_edges$to, df_nodes$name)]
  xrange <- range(df_nodes$x)
  # Check if nodes vary along the x range
  if(diff(xrange) > 0){
    midpoint_df <- mean(xrange)
    left_half <- df_nodes$x[match(df_edges$from, df_nodes$name)] < midpoint_df

    curws[same_column & left_half & (!is.na(df_edges$curvature))] <- curws[same_column & left_half & (!is.na(df_edges$curvature))]*sign(df_nodes$y[match(df_edges$to, df_nodes$name)]-df_nodes$y[match(df_edges$from, df_nodes$name)])[same_column & left_half & (!is.na(df_edges$curvature))]

    out[same_column & left_half & (!is.na(df_edges$curvature) | df_edges$from == df_edges$to), ] <- c("left", "left")
    out[same_column & !left_half & (!is.na(df_edges$curvature) | df_edges$from == df_edges$to), ] <- c("right", "right")
  } else {
    #out[same_column & (!is.na(df_edges$curvature) | df_edges$from == df_edges$to), ] <- c("right", "right")
  }

  # For rows
  same_row <- df_nodes$y[match(df_edges$from, df_nodes$name)] == df_nodes$y[match(df_edges$to, df_nodes$name)]
  yrange <- range(df_nodes$y)
  if(diff(yrange) > 0){
    midpoint_df <- mean(yrange)
    top_half <- df_nodes$y[match(df_edges$from, df_nodes$name)] > midpoint_df

    curws[same_row & top_half & (!is.na(df_edges$curvature))] <- curws[same_row & top_half & (!is.na(df_edges$curvature))]*sign(df_nodes$x[match(df_edges$to, df_nodes$name)]-df_nodes$x[match(df_edges$from, df_nodes$name)])[same_row & top_half & (!is.na(df_edges$curvature))]

    out[same_row & !top_half & (!is.na(df_edges$curvature) | df_edges$from == df_edges$to), ] <- c("bottom", "bottom")
    out[same_row & top_half & (!is.na(df_edges$curvature) | df_edges$from == df_edges$to), ] <- c("top", "top")
  } else {
    #out[same_row & (!is.na(df_edges$curvature) | df_edges$from == df_edges$to), ] <- c("top", "top")
  }

  incomplete <- is.na(out[,1])
  df_edges <- df_edges[incomplete, ]
  if(any(incomplete)){
    if(!is.null(angle)){
      out[incomplete, ] <- t(mapply(function(from, to){
        if(angle > 180) angle <- 180
        if(angle < 0) angle <- 0
        fx <- df_nodes$x[df_nodes$name == from]
        tx <- df_nodes$x[df_nodes$name == to]
        fy <- df_nodes$y[df_nodes$name == from]
        ty <- df_nodes$y[df_nodes$name == to]

        if(!(fx == tx | fy == ty)){

          dx <- tx-fx
          dy <- ty-fy
          an <- (atan2(dy,dx)*(180/pi)) %% 360
          if(an > 90+.5*angle & an < 270-.5*angle){
            return(c("left", "right"))
          }
          if(an <= 90+(.5*angle) & an >= 90-(.5*angle)){
            return(c("top", "bottom"))
          }
          if(an <  90-.5*angle | an > 270+.5*angle){
            return(c("right", "left"))
          }
          return(c("bottom", "top"))
        } else {
          if(fx == tx){
            list(c("bottom", "top"), c("top", "bottom"))[[(((ty-fy)>0)+1)]]
          } else {
            list(c("left", "right"), c("right", "left"))[[(((tx-fx)>0)+1)]]
          }
        }

      }, from = df_edges$from, to = df_edges$to))
    } else {
      out[incomplete, ] <- t(mapply(function(from, to) {
        from_mat <-
          as.matrix(rbind(
            expand.grid(x = unlist(df_nodes[df_nodes$name == from, c("node_xmin", "node_xmax")]),
                        y = unlist(df_nodes[df_nodes$name == from, "y"])),
            expand.grid(x = unlist(df_nodes[df_nodes$name == from, "x"]),
                        y = unlist(df_nodes[df_nodes$name == from, c("node_ymin", "node_ymax")]))
          ))

        to_mat <-
          as.matrix(rbind(
            expand.grid(x = unlist(df_nodes[df_nodes$name == to, c("node_xmin", "node_xmax")]),
                        y = unlist(df_nodes[df_nodes$name == to, "y"])),
            expand.grid(x = unlist(df_nodes[df_nodes$name == to, "x"]),
                        y = unlist(df_nodes[df_nodes$name == to, c("node_ymin", "node_ymax")]))
          ))

        connector_sides[which.min(mapply(
          function(from, to) {
            do.call(.euclidean_distance, list(p = from_mat[from, ], q = to_mat[to, ]))
          },
          from = rep(1:4, each = 4),
          to = rep(1:4, 4)
        )), ]

      }, from = df_edges$from, to = df_edges$to))
    }
  }
  cbind(out, curws)
}

#' @importFrom stats dist
#' @importFrom ggplot2 scale_linetype_manual aes_string
#' @importFrom ggplot2 arrow
.plot_edges <- function(p, df, text_size = 5, npoints = 101,arrow_angle, arrow_length, ...) {
  df_edges <- data.frame(do.call(rbind, lapply(1:nrow(df), function(rownum){
    this_row <- df[rownum, ]
    if(is.na(this_row[["curvature"]])){
      label_loc <- ifelse(is.null(this_row[["label_location"]]), .5, this_row[["label_location"]])
      matrix(
        c(this_row[["edge_xmin"]], this_row[["edge_ymin"]], rownum,
          (1-label_loc) * matrix(c(this_row[["edge_xmin"]], this_row[["edge_ymin"]]), ncol = 2)+ label_loc*(matrix(c(this_row[["edge_xmax"]], this_row[["edge_ymax"]]), ncol = 2)), rownum,
          this_row[["edge_xmax"]], this_row[["edge_ymax"]], rownum),
        nrow = 3, byrow = TRUE, dimnames = list(NULL, c("x", "y", "id"))
      )
    } else {
      A = matrix(c(this_row[["edge_xmin"]], this_row[["edge_ymin"]]), nrow = 1)
      B = matrix(c(this_row[["edge_xmax"]], this_row[["edge_ymax"]]), nrow = 1)
      M <- matrix(c(mean(c(this_row[["edge_xmin"]], this_row[["edge_xmax"]])),
                    mean(c(this_row[["edge_ymin"]], this_row[["edge_ymax"]]))), nrow = 1)
      radius <- dist(rbind(A, B))
      AB <- B-A
      N <- matrix(c(AB[2], -AB[1]), nrow=1)
      C <- M + .5*(N * tan((this_row[["curvature"]]/180)*pi))
      radius <- dist(rbind(C, A))
      angles <- list(
        atan2(c(this_row[["edge_ymin"]], this_row[["edge_ymax"]]) - C[2], c(this_row[["edge_xmin"]], this_row[["edge_xmax"]]) - C[1]),
        atan2(c(this_row[["edge_ymin"]], this_row[["edge_ymax"]]) - C[2], c(this_row[["edge_xmin"]], this_row[["edge_xmax"]]) - C[1]) %% (2*pi)
      )
      angles <- angles[[which.min(sapply(angles, dist))]]
      point_seq <- seq(angles[1], angles[2],length.out = npoints)
      out <- matrix(
        c(C[1] + radius * cos(point_seq),
          C[2] + radius * sin(point_seq),
          rep(rownum, npoints)),
        nrow = npoints, ncol = 3, dimnames = list(NULL, c("x", "y", "id"))
      )
      # # Check if the curve should be flipped
      # midpoint <- out[floor(npoints/2), 1:2]
      # # replace this with node df
      # df_tmp <- df[, c("text_x", "text_y")]
      # names(df_tmp) <- c('x', 'y')
      # # Find the reflection of the midpoint
      # midpoint_reflect <- .reflect_points(points = matrix(midpoint, nrow = 1),
      #                                     start = unlist(this_row[c("edge_xmin", "edge_ymin")]),
      #                                     end = unlist(this_row[c("edge_xmax", "edge_ymax")]))
      # if(.flip_curve(otherpoints = df_tmp, candidatepoints = list(midpoint, midpoint_reflect))){
      #   out[, 1:2] <- .reflect_points(points = out[, 1:2],
      #                                 start = unlist(this_row[c("edge_xmin", "edge_ymin")]),
      #                                 end = unlist(this_row[c("edge_xmax", "edge_ymax")]))
      # }
      out
    }
  })))
  df$id <- 1:nrow(df)
  df_edges <- merge(df_edges, df, by = "id")
  # Prepare label df --------------------------------------------------------
  if(is.null(df[["label_location"]])){
    df$label_location <- .5
  }
  select_these <- middle_point <- table(df_edges$id)
  straightline <- middle_point == 3
  select_these[straightline] <- 2
  select_these[!straightline] <- ceiling(df$label_location[!straightline]*middle_point[!straightline])

  middle_point <- select_these+cumsum(c(0, middle_point[-length(middle_point)]))
  df_label <- df_edges[middle_point, ]
  df_label$label <- df$label
  df_label <- df_label[!(is.na(df_label$label) | df_label$label == ""), ]
  # # Prepare aesthetics ------------------------------------------------------
  if(!"linetype" %in% names(df_edges)){
    df_edges$linetype <- 1#2
    #df_edges$linetype[is.na(df_edges$curvature)] <- 1
  }
  if(!all(df_edges$arrow %in% c("first", "last", "both", "none"))) stop("Illicit 'arrow' value; acceptable values are c('first', 'last', 'both', 'none').")
  p <- .plot_edges_internal(p, df_edges, arrow_angle, arrow_length)
  # Add label and return ----------------------------------------------------
  .plot_label_internal(p, df_label, text_size)
}

#' @importFrom ggplot2 arrow
.plot_edges_internal <- function(p, df, arrow_angle=20, arrow_length=.7,...){
  # Prepare aesthetics ------------------------------------------------------
  if(!"linetype" %in% names(df)){
    df$linetype <- 1#2
    #df$linetype[is.na(df$curvature)] <- 1
  }
  if(any(df$arrow != "none")){
    df_path <- df[!df$arrow == "none", ]
    aes_args <- c("id", "arrow", "linetype", "size", "colour", "color", "alpha", "linewidth")
    aes_args <- df_path[!duplicated(df_path$id), which(names(df_path) %in% aes_args)]
    Args <- list(
      data = df_path[, c("x", "y", "id")],
      mapping = aes(x = .data[["x"]], y = .data[["y"]], group = .data[["id"]]),
      arrow = quote(ggplot2::arrow(angle = arrow_angle, length = unit(arrow_length, "inches"), ends = "last", type = "closed")))
    for(this_path in unique(df_path$id)){
      Args$data <- df_path[df_path$id == this_path, ]
      Args$arrow[[4]] <- force(aes_args$arrow[aes_args$id == this_path])
      argslist <- c(Args, as.list(aes_args[aes_args$id == this_path, -c(1, 2), drop = FALSE]))
      if(nchar(argslist$linetype) == 1){
        argslist$linetype <- as.numeric(argslist$linetype)
      }
      p <- p + do.call(geom_path, argslist)
    }
  }
  if(any(df$arrow == "none")){
    df_path <- df[df$arrow == "none", ]
    Args <- c("linetype", "linewidth", "size", "colour", "color", "alpha")
    Args <- as.list(df_path[which(names(df_path) %in% Args)])
    Args <- c(list(
      data = df_path,
      mapping = aes(x = .data[["x"]], y = .data[["y"]], group = .data[["id"]])),
      Args)
    p <- p + do.call(geom_path, Args)
  }
  p
}

.plot_label_internal <- function(p, df, text_size){
  retain_cols <- c("x", "y", "label", "group", "level")
  retain_cols <- retain_cols[which(retain_cols %in% names(df))]
  use_geom_text <- isTRUE("geom_text" %in% names(df))
  df <- df[, c(retain_cols, grep("^label_(fill|size|family|fontface|hjust|vjust|lineheight|colour|color|alpha)$", names(df), value = TRUE)), drop = FALSE]
  if(nrow(df) > 0){
    names(df) <- gsub("^label_", "", names(df))
    # Prepare aesthetics ------------------------------------------------------
    if(!"fill" %in% names(df)){
      df$fill = NA
    }
    if(!"size" %in% names(df)){
      df$size <- text_size
    }
    #browser()
    Args <- c("fill", "size", "family", "fontface", "hjust", "vjust", "lineheight", "colour","color",  "alpha", "geom_text")
    Args <- as.list(df[which(names(df) %in% Args)])
    Args <- c(list(
      data = df,
      mapping = aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
      label.size = NA
    ),
    Args)
    if(use_geom_text){
      Args$mapping <- aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]], customdata = .data[["label"]])
      Args[c("geom_text", "fill", "label.size")] <- NULL
      p <- p + do.call(geom_text, Args)
    } else {
      p <- p + do.call(geom_label, Args)
    }

  }
  p
}

reposition_variances <- function(df_edges){
  variances <- df_edges$from == df_edges$to & df_edges$arrow == "both"
  variance_vars <- df_edges$from[variances]
  new_loc <- mapply(function(vv, vloc){
    tb <- table(c("top", "bottom", "left", "right",
                  df_edges$connect_from[df_edges$from == vv & !variances],
                  df_edges$connect_to[df_edges$to == vv & !variances]))
    if(tb[vloc] == 1){
      return(vloc)
    } else {
      tb <- names(tb)[which.min(tb)]
      return(tb[sample.int(length(tb), 1)])
    }
  }, vv = variance_vars, vloc = df_edges$connect_from[variances])
  df_edges$connect_from[variances] <- df_edges$connect_to[variances] <- new_loc
  return(df_edges)
}


bind_list <- function(L, ...){
  L <- L[which(!sapply(L, is.null))]
  all_names <- unique(unlist(lapply(L, names)))
  L <- lapply(L, function(x){
    x[setdiff(all_names, names(x))] <- NA
    x
  })
  Args <- c(L, list(...))
  do.call(rbind, Args)
}

#' @importFrom dbscan pointdensity
.flip_curve <- function(otherpoints, candidatepoints, eps = floor(sqrt(nrow(otherpoints)))){
  (sum(dbscan::pointdensity(rbind(otherpoints, candidatepoints[[1]]), eps = eps)) > sum(dbscan::pointdensity(rbind(otherpoints, candidatepoints[[2]]), eps = eps)))
}

.position_variance <- function(otherpoints, candidatepoints, eps = floor(sqrt(nrow(otherpoints)))){
  which.min(apply(candidatepoints, 1, function(thispoint){
    sum(dbscan::pointdensity(rbind(otherpoints, thispoint), eps = eps))
  }))
}

.reflect_points <- function(points, start, end){
  out <- points
  colnames(out) <- c("x", "y")
  if(end[1] == start[1]){
    out[1] <- end[1] - (out[1]-end[1])
    return(out)
  }
  if(end[2] == start[2]){
    out[2] <- end[2] - (out[2]-end[2])
    return(out)
  }
  b <- (end[1] * start[2] - start[1] * end[2])/(end[1] - start[1])
  m <- (end[2]-start[2])/(end[1]-start[1])
  m3 <- m1 <- diag(3)
  m1[2,3] <- b
  m3[2,3] <- -1 * b
  m2 <- matrix(c((1-m^2)/(1+m^2), 2*m/(1+m^2), 0,
                 2*m/(1+m^2), (m^2-1)/(1+m^2), 0,
                 0, 0, 1), nrow = 3, byrow = TRUE)
  m4 <- rbind(t(points), 1)
  out <- m1 %*% m2 %*% m3 %*% m4
  t(out)[,1:2]
}

#' @importFrom ggplot2 geom_path geom_polygon
.oval_node <- function(p, df, oval_width, oval_height, npoints = 80){
  x <- df$x
  y <- df$y
  point_seq <- seq(0, 2*pi,length.out = npoints)
  
  df_ellipse <- matrix(
    c((.5*oval_width) * cos(point_seq), (.5*oval_height) * sin(point_seq)),
    nrow = npoints, ncol = 2)
  
  df_ellipse <- mapply(function(x, y){
    t(t(df_ellipse) + c(x, y))
  }, x = x, y = y, SIMPLIFY = FALSE)
  df_ellipse <- data.frame(do.call(rbind, df_ellipse))
  df_ellipse$grp <- rep(paste0("g", 1:length(x)), each = npoints)
  df$grp <- paste0("g", 1:length(x))
  df_ellipse <- merge(df_ellipse, df, by = "grp", all.y = TRUE)
  Args <- c("linetype", "size", "colour", "fill", "alpha", "linewidth")
  Args <- as.list(df_ellipse[which(names(df_ellipse) %in% Args)])
  
  Args <- c(list(
    data = df_ellipse,
    mapping = aes(x= .data[["X1"]], y= .data[["X2"]], group = .data[["grp"]])), Args)
  
  p + do.call(geom_polygon, Args)
}

#p <- ggplot(data = NULL)
#oval_node(p, x = c(0, 0), y = c(0, 3), oval_width = 2, oval_height = 1)
