#' Access the validated reactive value pronoun
#'
#' When used inside the `validation_expr` of a [validated_reactive_val()], this
#' function acts as a pronoun to access the current value (before validation) of
#' that `validated_reactive_val`. This allows the validation expression to
#' reconcile the object's current state with its reactive dependencies.
#'
#' To reference `.vrv()` in a package and avoid `R CMD check` notes, you can
#' either import this function with `#' @importFrom shinybatch .vrv` or call it
#' with the `shinybatch::.vrv()` namespace.
#'
#' @param env (`environment`) The environment in which to find the `.vrv`
#'   pronoun. This should generally not be changed from the default of
#'   [rlang::caller_env()].
#'
#' @returns The current value of the [validated_reactive_val()].
#' @export
.vrv <- function(env = rlang::caller_env()) {
  .with_error_handling(
    {
      get(".vrv", envir = env, inherits = FALSE)()
    },
    message = c(
      "Function {.code .vrv()} not found.",
      i = "The {.code vrv()} pronoun must only be used inside the {.arg validation_expr} of a {.fn shinybatch::validated_reactive_val}."
    ),
    subclass = "pronoun-not-found",
    call = env,
    pass_parent = FALSE
  )
}
