
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
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#'
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom stats na.omit
squareCross <- function(edges, top, bottom, left, right, # Notice ID of nodes should be unique across four types
                        width = 1, height = 1,
                        flank_mult = c(top = 0.1, bottom = 0.1,left = 0.1, right = 0.1)){
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
  ggplot(data = segments %>% na.omit) +
    geom_segment(mapping = aes(x,y, xend = xend, yend = yend, color = type),
                 show.legend = F) +
    theme_void()
}


#' Visualization for links between two types of nodes with annotaion plots
#'
#' @param edges data.frame with at least two columsn with nomes of "source" and "target"
#' @param columns a list of multiple columns of nodes on, the order will be used as level.
#' @param height plot height, used for control coordinates
#' @param flank_mult mulitply with `scale`, flankings at four sides,
#' @param segment_shrink text may overlaid on segement, use this to avoid it
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#'
#' @import ggplot2
columnCross <- function(edges, columns, # Notice ID of nodes should be unique across four types
                        height = 1,
                        flank_mult = rep(0.1, length(columns)),
                        segment_shrink = 0.1){
  if(length(flank_mult) == 1) rep(flank_mult, length(columns))
  cols = lapply(columns, function(x) factor(x, x))
  cols_coord <- mapply(newCoord, cols, rep(height, length(columns)), flank_mult)

  edges$source_coord <- unlist(cols_coord)[match(edges$source, unlist(columns))]
  edges$target_coord <- unlist(cols_coord)[match(edges$target, unlist(columns))]

  segments <- data.frame(
    x = colSums(sapply(edges$source, function(x) mapply(function(l, r) l %in% r, x, columns)) * c(1, 2, 3)),
    y = edges$source_coord,
    xend = colSums(sapply(edges$target, function(x) mapply(function(l, r) l %in% r, x, columns)) * c(1, 2, 3)),
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


