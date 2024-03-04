## ---- fmt_currency_plain
fmt_currency_plain <- function(data, ..., columns = everything()) {
  fmt(
    data = data,
    columns = columns,
    fns = \(x){
      vec_fmt_currency_plain(x, ...)
    }
  )
}
