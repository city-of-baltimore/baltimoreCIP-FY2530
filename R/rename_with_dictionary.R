rename_with_dictionary <- function(
    data,
    dictionary) {
  rename_with_xwalk(
    x = data,
    xwalk = dictionary_to_xwalk(
      dictionary = dictionary,
      data = data
    )
  )
}
