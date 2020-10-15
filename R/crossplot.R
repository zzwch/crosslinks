
#' generate 1-d coord with scale and flankings
#'
#' @param x a vector of numeric or factor
#' @param scale the max coord, return will be in the range of 0 and `scale`
#' @param flank_mult mulitply with `scale`, flankings at both sides,
#'
#' @return new 1-d coordinate
#' @export
#'
#' @examples
#' newCoord(1:10, scale = 110, flank_mult = 0.1)
#'
#' @importFrom scales rescale
#'
newCoord <- function(x, scale = 1, flank_mult = 0.1){
  if(!is.factor(x)) x <- factor(x, x)
  flank <- scale * flank_mult
  rescale(as.numeric(x), c(flank, scale - flank))
}

#' Visualization for links between four types of nodes with annotation plots
#'
#' @param edges data.frame with at least two columsn with nomes of "source" and "target"
#' @param top a vector of nodes on top, the order will be used as level.
#' @param bottom similar to top but on bottom
#' @param left similar to top but on left
#' @param right similar to top but on right
#' @param width plot width, used for control coordinates
#' @param height plot height, used for control coordinates
#' @param flank_mult mulitply with `scale`, flankings at four sides,
#' @param expand_xlim_mult expand x limits with expand_xlim_mult*width. This may be used when show_node_label is TRUE.
#' @param expand_ylim_mult expand y limits with expand_ylim_mult*width
#' @param show_node_label whether to show node label
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#'
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom stats na.omit
#'
squareCross <- function(edges, top, bottom, left, right, # Notice ID of nodes should be unique across four types
                        width = 1, height = 1,
                        show_node_label = F,
                        flank_mult = c(top = 0.1, bottom = 0.1,left = 0.1, right = 0.1),
                        expand_xlim_mult = c(0.2,0.2),
                        expand_ylim_mult = c(0.2,0.2)){
  if(length(flank_mult) == 1) rep(flank_mult, 4)
  square = list(top = factor(top, top), bottom = factor(bottom, bottom),
                left = factor(left, left), right = factor(right, right))
  square_n <- lapply(square, length)
  square_coord <- mapply(newCoord, square, c(rep(width, 2), rep(height, 2)), flank_mult)
  edges$source_coord <- unlist(square_coord)[match(edges$source, c(top, bottom, left, right))]
  edges$target_coord <- unlist(square_coord)[match(edges$target, c(top, bottom, left, right))]

  segments <- data.frame(
    x = case_when(
      edges$source %in% c(top, bottom) ~ edges$source_coord,
      edges$source %in% c(left) ~ 0,
      edges$source %in% c(right) ~ width,
      TRUE ~ NA_real_
    ),
    y = case_when(
      edges$source %in% c(left, right) ~ edges$source_coord,
      edges$source %in% c(bottom) ~ 0,
      edges$source %in% c(top) ~ height,
      TRUE ~ NA_real_
    ),
    xend = case_when(
      edges$target %in% c(top, bottom) ~ edges$target_coord,
      edges$target %in% c(left) ~ 0,
      edges$target %in% c(right) ~ width,
      TRUE ~ NA_real_
    ),
    yend = case_when(
      edges$target %in% c(left, right) ~ edges$target_coord,
      edges$target %in% c(bottom) ~ 0,
      edges$target %in% c(top) ~ height,
      TRUE ~ NA_real_
    ),
    type = paste0(case_when(
      edges$source %in% c(top) ~ "top",
      edges$source %in% c(bottom) ~ "bottom",
      edges$source %in% c(left) ~ "left",
      edges$source %in% c(right) ~ "right",
      TRUE ~ "other"
    )," vs ",case_when(
      edges$target %in% c(top) ~ "top",
      edges$target %in% c(bottom) ~ "bottom",
      edges$target %in% c(left) ~ "left",
      edges$target %in% c(right) ~ "right",
      TRUE ~ "other"
    ))
  )
  p <- ggplot() +
    geom_segment(data = segments %>% na.omit,
                 mapping = aes(x,y, xend = xend, yend = yend, color = type),
                 show.legend = F) +
    expand_limits(x = expand_xlim_mult*width*c(-1, 1) + c(0, width),
                  y = expand_ylim_mult*height*c(-1,1) + c(0, height))+
    theme_void()
  if(show_node_label){
    p <- p +
      geom_text(mapping = aes(x = square_coord[[1]], y = rep(height, length(top)), label = top), hjust = 0, vjust = 0.5, angle = 90) +
      geom_text(mapping = aes(x = square_coord[[2]], y= rep(0, length(bottom)), label = bottom), hjust = 1, vjust = 0.5, angle = 90) +
      geom_text(mapping = aes(y = square_coord[[3]], x= rep(0, length(left)), label = left), hjust = 1, vjust = 0.5, angle = 0) +
      geom_text(mapping = aes(y = square_coord[[4]], x = rep(width, length(right)), label = right), hjust = 0, vjust = 0.5, angle = 0)
  }
  return(p)
}


#' Visualization for links between two types of nodes with annotaion plots
#'
#' @param edges data.frame with at least two columns named with "source" and "target"
#' @param columns a list of multiple columns of nodes on, the order will be used as level.
#' @param height plot height, used for control coordinates
#' @param flank_mult mulitply with `scale`, flankings at four sides,
#' @param segment_shrink text may overlaid on segement, use this to avoid it
#'
#' @return a ggplot object
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#'
#' @import ggplot2
#'
columnCross <- function(edges, columns, # Notice ID of nodes should be unique across four types
                        height = 1,
                        flank_mult = rep(0.1, length(columns)),
                        segment_shrink = 0.1){
  if(length(flank_mult) == 1) flank_mult <- rep(flank_mult, length(columns))
  cols = lapply(columns, factor)
  cols_coord <- mapply(newCoord, cols, rep(height, length(columns)), flank_mult)

  edges$source_coord <- unlist(cols_coord)[match(edges$source, unlist(columns))]
  edges$target_coord <- unlist(cols_coord)[match(edges$target, unlist(columns))]
  cols_x <- seq_len(length(columns))
  segments <- data.frame(
    x = colSums(sapply(edges$source, function(x) mapply(function(l, r) l %in% r, x, columns)) * cols_x),
    y = edges$source_coord,
    xend = colSums(sapply(edges$target, function(x) mapply(function(l, r) l %in% r, x, columns)) * cols_x),
    yend = edges$target_coord
  ) %>% na.omit()
  segments$type <- paste0(names(columns)[segments$x], " vs ", names(columns)[segments$xend])
  ggplot() +
    geom_segment(data = segments,
                 mapping = aes(x+segment_shrink, y,
                               xend = xend-segment_shrink,
                               yend = yend,
                               color = type),
                 show.legend = F) +
    geom_text(data = do.call(
      rbind,
      lapply(1:length(columns),
             function(i) data.frame(x = i, y = cols_coord[[i]], label = columns[[i]]))),
      mapping = aes(x, y, label = label)) +
    theme_void()
}

#' Visualization for links between different types of nodes with shapes and colors supported.
#'
#' @param edges a data.frame with at least two columns named with "source" and "target", which includes different nodes. Notice ID of nodes should be unique across four types
#' @param nodes a data.frame of nodes and node-annotations. first column is the node ID. You could use factor to control node order.
#' @param columns a list of multiple columns of nodes to illustrate, the order will be used as level.
#' @param column_x a vector for x-axis coord of each column
#' @param height plot height, used for control coordinates
#' @param flank_mult mulitply with `scale`, flankings at every sides,
#' @param segment_shrink text may overlaid on segement, use this to avoid it
#' @param linetype a numeric value, or a colname of edges. Allowed value: 0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash.
#' @param line_alpha same to linetype, but allowed value: 0-1.
#' @param line_color a color or a colname of edges.
#' @param line_size a numeric value, or a colname of edges; control line width, allowed value: any numeric >= 0.
#' @param pt_alpha same to line_alpha, but colname of nodes.
#' @param pt_color same to line_color, but colname of nodes.
#' @param pt_fill similar to pt_color, but control the fill color. only used in shapes 21-24.
#' @param pt_shape a value, or colname of nodes. Allowed value:An integer in 0 to 25, or a charactor of length 1. Note that shapes 21-24 have both stroke colour and a fill
#' @param pt_size same to line_size, but colname of nodes.
#' @param pt_stroke a numeric value, or a colname of nodes; control border thickness of point.
#'
#' @return a ggplot object
#' @importFrom dplyr %>%
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' columnCross2(edges, nodes, columns, 1:4,
#' height = 1, flank_mult = rep(0.1, length(columns)), segment_shrink = 0.1,
#' linetype = "type", line_alpha = "alpha", line_color = "color", line_size = "size" ,
#' pt_alpha = "alpha", pt_color = "color", pt_fill = "color",
#' pt_shape = "shape", pt_size = "size", pt_stroke = 1)}
columnCross2 <- function(edges, nodes, columns, column_x = seq_len(length(columns)),
                         height = 1, flank_mult = rep(0.1, length(columns)), segment_shrink = 0.1,
                         linetype = 1, line_alpha = 1, line_color = "black", line_size = 1,
                         pt_alpha = 1, pt_color = "black", pt_fill = "white",
                         pt_shape = 1, pt_size = 1, pt_stroke = 1){
  if(length(flank_mult) == 1) flank_mult <- rep(flank_mult, length(columns))
  cols = lapply(columns, factor)
  cols_coord <- mapply(newCoord, cols, rep(height, length(columns)), flank_mult)

  nodes <- subset(nodes, nodes[,1] %in% unlist(columns))
  edges <- subset(edges, edges$source %in% nodes[,1] & edges$target %in% nodes[,1])

  edges$source_coord <- unlist(cols_coord)[match(edges$source, unlist(columns))]
  edges$target_coord <- unlist(cols_coord)[match(edges$target, unlist(columns))]
  segments <- data.frame(
    x = colSums(sapply(edges$source, function(x) mapply(function(l, r) l %in% r, x, columns)) * column_x),
    y = edges$source_coord,
    xend = colSums(sapply(edges$target, function(x) mapply(function(l, r) l %in% r, x, columns)) * column_x),
    yend = edges$target_coord
  ) %>% na.omit()
  segments$type <- paste0(names(columns)[segments$x], " vs ", names(columns)[segments$xend])

  line_aes_by <- list(type = linetype,
                      alpha = line_alpha,
                      color = line_color,
                      size = line_size)
  point_aes_by <- list(alpha = pt_alpha,
                       color = pt_color,
                       fill = pt_fill,
                       shape = pt_shape,
                       size = pt_size,
                       stroke = pt_stroke)

  line_aes_by <- lapply(line_aes_by, checkAes, df = edges, name = "edges")
  point_aes_by <- lapply(point_aes_by, checkAes, df = nodes, name = "nodes")

  p <- ggplot()
  # aes line color
  p <- p + geom_segment(data = segments,
                        mapping = aes(x+segment_shrink, y,
                                      xend = xend-segment_shrink,
                                      yend = yend),
                        linetype = line_aes_by$type,
                        alpha = line_aes_by$alpha,
                        color = line_aes_by$color,
                        size = line_aes_by$size,
                        show.legend = F)
  # aes point color
  p <- p + geom_point(data = do.call(rbind,
                                     lapply(1:length(columns),
                                            function(i) data.frame(x = column_x[i], y = cols_coord[[i]], label = columns[[i]]))),
                      mapping = aes(x, y),
                      alpha = point_aes_by$alpha,
                      color = point_aes_by$color,
                      fill = point_aes_by$fill,
                      shape = point_aes_by$shape,
                      size = point_aes_by$size,
                      stroke = point_aes_by$stroke,
                      show.legend = F)
  p <- p + geom_text(data = do.call(rbind,
                                    lapply(1:length(columns),
                                           function(i) data.frame(x = column_x[i], y = cols_coord[[i]], label = columns[[i]]))),
                     mapping = aes(x, y, label = label)) +
    theme_void()

  return(p)
}


#' check aes
#'
#' a value or a colname
#'
#' @param x a value or a colname
#' @param df a data.frame
#' @param name report name
#'
#' @return a vector of nrow(df)
#'
#'
#' @examples
#'
checkAes <- function(x, df, name = NULL){
  ret <- NULL
  if(length(x) == 1){
    ret <- if(x %in% colnames(df)) df[,x,drop = T] else rep(x, nrow(df))
  }else{
    if(length(x) == nrow(df))
      ret <- x
    else{
      ret <- NULL
      stop(paste0("length of aes is not equal to 1 or nrow(", name,"). check this!"))
    }
  }
  return(ret)
}


#' check the vector is/are color(s) or not
#'
#' @param x a vector
#'
#' @return a named vector
#' @export
#' @source https://stackoverflow.com/a/13290832
#' @examples
#'
#' \dontrun{
#' is.color(c(NA, "black", "blackk", "1", "#00", "#000000"))
#' }
is.color <- function(x) {
  sapply(x, function(x_) {
    tryCatch(is.matrix(col2rgb(x_)),
             error = function(e) FALSE)
  })
}

#' set aes for vector
#'
#' @param x a vector to be replaced with aes
#' @param dict a vector of colors/shapes named with categories of x.
#' @param type aes type, allowed: color, fill, shape.
#'
#' @return a vector of aes
#' @export
#'
#' @examples
#'
aesReplacer <- function(x, dict, type = "color"){
  if(!type %in% c("color", "fill", "shape")) stop("only color, fill, shape are supported!")
  if(type %in% c("color", "fill")){
    if(all(is.color(x))) return(x) # already colors
  }else{
    if(all(x %in% 0:25)) return(x)
  }

  if(is.null(dict)) return(NULL) # to use default color

  # set colors
  x_cat <- sort(unique(x))
  if(length(dict) < length(x_cat)){
    stop("dict is less than the categories!")
  }
  if(all(x_cat %in% names(dict))) return(dict[x])

  dict <- stats::setNames(dict[1:length(x_cat)], nm = x_cat)
  return(dict[x])
}
