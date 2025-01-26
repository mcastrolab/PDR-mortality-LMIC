# Calculate Pollard Decomposition
#
# This function computes the Pollard decomposition.
#
#   .data -> Data frame containing the variables.
#   period_var -> Variable for periods.
#   mx_group -> Variable m(x) for the group of interest (e.g., cause of death).
#   mx_main -> Variable for the total population m(x).
#   lx_main -> Variable for the total population l(x).
#   ex_main -> Variable for the total population e(x).
#   agegrpstart -> Variable for the age at the start of each age group (e.g. 0-4 -> 0, 85+ -> 85)
#
# And return a data frame with the Pollard decomposition.
#
# Example usage:
#
# First sort the data frame by the subdivisions of interest (e.g., region, sex)
# plus the variables for the group of interest, for age groups (i.e., agegrpstart) and period_var
#
# dplyr::arrange(data, sub_div_a, sub_div_b, cause_of_death, agegrpstart, period_var)
#
# Then apply the function
#
# ff_pollard(data, period_var, mx_group, mx_main, lx_main, ex_main)

ff_pollard <- function(.data, period_var, mx_group, mx_main, lx_main, ex_main) {
  # Quote arguments
  period_var <- rlang::enquo(period_var)
  mx_group <- rlang::enquo(mx_group)
  mx_main <- rlang::enquo(mx_main)
  lx_main <- rlang::enquo(lx_main)
  ex_main <- rlang::enquo(ex_main)

  # Calculate number of periods
  .data <- dplyr::mutate(
    .data,
    number_of_periods = dplyr::n_distinct(!!period_var),
    number_of_periods1 = length(unique(!!period_var))
  )

  # Perform Pollard decomposition
  .data <- dplyr::mutate(
    .data,
    # Calculate Pollard exclusive component
    pollard_exclusive = dplyr::case_when(
      # Begin calculation for maximum age group
      agegrpstart == max(agegrpstart) ~ (
        (!!mx_group - dplyr::lead(!!mx_group)) *
          (
            0.5 * (
              dplyr::lead(!!lx_main, n = 1) * dplyr::lead(!!ex_main, n = 1) / !!mx_main +
                !!lx_main * !!ex_main / dplyr::lead(!!mx_main, n = 1)
            )
          )
      ),
      TRUE ~ (
        # Default calculation
        (!!mx_group - dplyr::lead(!!mx_group)) *
          (
            (dplyr::lead(agegrpstart, n = number_of_periods[1]) - agegrpstart) / 2 *
              (
                (!!lx_main * !!ex_main +
                  dplyr::lead(!!lx_main, n = number_of_periods[1]) * dplyr::lead(!!ex_main, n = number_of_periods[1]) +
                  dplyr::lead(!!lx_main, n = 1) * dplyr::lead(!!ex_main, n = 1) +
                  dplyr::lead(!!lx_main, n = number_of_periods[1] + 1) * dplyr::lead(!!ex_main, n = number_of_periods[1] + 1)) / 2
              )
          )
      )
    ),
    # Calculate Pollard interaction component
    pollard_interaction = dplyr::case_when(
      # Begin calculation for maximum age group
      agegrpstart == max(agegrpstart) ~ 0,
      TRUE ~ (
        # Default calculation
        (!!mx_group - dplyr::lead(!!mx_group)) *
          (
            (dplyr::lead(agegrpstart, n = number_of_periods[1]) - agegrpstart) / 2 *
              (
                ((dplyr::lead(!!ex_main) - !!ex_main +
                  dplyr::lead(!!ex_main, n = number_of_periods[1] + 1) - dplyr::lead(!!ex_main, n = number_of_periods[1])) / 2) *
                  ((!!lx_main - dplyr::lead(!!lx_main) +
                    dplyr::lead(!!lx_main, n = number_of_periods[1]) - dplyr::lead(!!lx_main, n = number_of_periods[1] + 1)) / 2)
              )
          )
      )
    ),
    # Calculate Pollard total
    pollard_total = pollard_exclusive + pollard_interaction
  )
# 
  # Select all columns except number_of_periods
 # dplyr::select(.data, -number_of_periods)
}
