#' Plot user sessions
#'
#' Plot out sessions on a frame of user vs time
#'
#' @export
plot_user_sessions <- function(sessions) {
  ggplot(sessions, aes(x = start, y = user)) +
    geom_segment(aes(xend = finish + 5.5, yend = user),
                 color="blue",
                 size = 2) +
    geom_point(color = "blue", shape = ".", aes(size = duration))
}
