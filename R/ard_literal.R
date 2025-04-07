# Thoughts:
# - Expected inputs?
#   - Works nicely with a list where each value is a single element
#   - What about when we have "name" = c() where there's multiple values?
#     - One row for each element?
#     - Just store the vector in a single row?
#   - Other input types?
#     - data.frame?
#       - Handled in the same way as lists?
#       - Any kind of groupings supplied in the data.frame?
#     - Anything else?

ard_literal <- function(stats, variable, context = "literal") {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_named_list_and_vector_elements(stats)
  check_string(variable)
  check_string(context)

  # build data frame for calculaton --------------------------------------------
  dplyr::tibble("{variable}" := TRUE) |>
    ard_continuous(
      variables = all_of(variable),
      statistic = everything() ~ list(identity = \(x) stats)
    ) |>
    dplyr::mutate(
      context = .env$context
    )
}
