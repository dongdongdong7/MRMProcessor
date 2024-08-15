#' @title cal_IE
#' @description
#' Calculate IE.
#'
#'
#' @param int int.
#'
#' @return IE.
#' @export
#'
#' @examples
#' intensity <- c(1701.196, 2121.159, 2401.849, 2043.653, 1542.964,  723.803)
#' cal_IE(int = intensity)
cal_IE = function(int){
  len = length(int)
  int_max = max(int)
  int_max_idx = which(int == int_max)[1] # 因为可能有相同高度的int_max,选择第一个

  int_idx_a = 1:(int_max_idx - 1)
  int_idx_b = (int_max_idx + 1):len

  int_a = int[int_idx_a]
  int_b = int[int_idx_b]

  diff_a = diff(int_a)
  h_a = abs(diff_a[which(diff_a < 0)])
  diff_b = diff(int_b)
  h_b = abs(diff_b[which(diff_b > 0)])
  h = c(h_a,int_max,h_b)
  p = h / sum(h)
  S = -sum(p * log(p))

  return(S)
}
