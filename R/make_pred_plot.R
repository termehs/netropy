#' @title Prediction Power Heatmap
#' @description Creates a heatmap for visualizing prediction power from a
#' prediction power matrix.
#' @param mat matrix returned by \code{\link{prediction_power}}. Entries should
#' contain expected conditional entropies \emph{EH(Z|X,Y)}.
#' @param title character string giving the plot title.
#' @param low color for low expected conditional entropy values. Default is
#' \code{"azure4"}.
#' @param high color for high expected conditional entropy values. Default is
#' \code{"white"}.
#' @param text_size numeric value controlling the size of the cell labels.
#' Default is 2.5.
#' @return A \code{ggplot} object showing a heatmap of expected conditional
#' entropy values. Darker cells indicate lower prediction uncertainty and
#' therefore higher prediction power.
#' @details The plot visualizes expected conditional entropies
#' \deqn{EH(Z|X,Y)}
#' where \emph{Z} is the target variable and \emph{X} and \emph{Y} are predictors.
#' Diagonal entries correspond to prediction using a single predictor,
#' \emph{EH(Z|X)}, while off-diagonal entries correspond to prediction using
#' pairs of predictors, \emph{EH(Z|X,Y)}. Lower values indicate stronger
#' predictive power.
#' @seealso \code{\link{prediction_power}}, \code{\link{entropy_trivar}}
#' @examples
#' # use internal data set
#' data(lawdata)
#'
#' # extract node attributes
#' df_att <- lawdata[[4]]
#'
#' # data editing:
#' # 1. discretize 'years' and 'age' into 3 categories
#' # 2. ensure values start at 0 where needed
#' att_var <- data.frame(
#'   status    = df_att$status - 1,
#'   gender    = df_att$gender,
#'   office    = df_att$office - 1,
#'   years     = ifelse(df_att$years <= 3, 0,
#'                 ifelse(df_att$years <= 13, 1, 2)),
#'   age       = ifelse(df_att$age <= 35, 0,
#'                 ifelse(df_att$age <= 45, 1, 2)),
#'   practice  = df_att$practice,
#'   lawschool = df_att$lawschool - 1
#' )
#'
#' # compute prediction power matrix for 'status'
#' pred_mat <- prediction_power("status", att_var)
#'
#' # visualize prediction power
#' make_pred_plot(pred_mat, "Prediction Power for Status")
#' @importFrom ggplot2 .data
#' @export

make_pred_plot <- function(mat,
                           title,
                           low = "azure4",
                           high = "white",
                           text_size = 2.5) {
  df <- as.data.frame(as.table(as.matrix(mat)))
  names(df) <- c("X", "Y", "EH")

  df <- df[!is.na(df$EH), ]

  ggplot2::ggplot(df, ggplot2::aes(x = .data$Y, y = .data$X, fill = .data$EH)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = round(.data$EH, 2)),
      size = text_size
    ) +
    ggplot2::scale_fill_gradient(
      low = low,
      high = high,
      name = "EH"
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = ggplot2::element_text(size = 7),
      plot.title = ggplot2::element_text(size = 10)
    )
}
