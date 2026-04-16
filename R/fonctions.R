utils::globalVariables(c("diagnostic", "sexe", "age", "cholesterol", "max_bpm", ".data"))

#' Analyser une variable par diagnostic
#'
#' @param data Un data frame
#' @param var_nom Nom de la colonne a analyser (character)
#' @param titre_y Titre de l'axe Y (character)
#' @return Un graphique ggplot2
#' @export
analyser_variable <- function(data, var_nom, titre_y) {
  if(!var_nom %in% colnames(data)) stop("La colonne n'existe pas !")

  ggplot2::ggplot(data, ggplot2::aes(x = diagnostic, y = .data[[var_nom]], fill = diagnostic)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste("Analyse de :", var_nom), y = titre_y, x = "Etat de sante")
}

#' Tracer un boxplot pour une variable
#'
#' @param data Un data frame
#' @param var_y Nom de la colonne Y (character)
#' @param titre_y Titre de l'axe Y (character)
#' @param couleur Couleur du boxplot (default : "lightblue")
#' @return Un graphique ggplot2
#' @export
tracer_boxplot <- function(data, var_y, titre_y, couleur = "lightblue") {
  ggplot2::ggplot(data, ggplot2::aes(x = diagnostic, y = .data[[var_y]], fill = diagnostic)) +
    ggplot2::geom_boxplot(
      width = 0.5,
      outlier.colour = "red",
      outlier.shape = 16,
      fill = couleur,
      notch = FALSE
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Distribution de :", titre_y),
      x = "Etat de sante",
      y = titre_y
    ) +
    ggplot2::theme(legend.position = "none")
}

#' Filtrer les patients par sexe et diagnostic
#'
#' @param data Un data frame
#' @param sexe_filtre Sexe a filtrer ("Homme" ou "Femme")
#' @param diagnostic_filtre Diagnostic a filtrer ("Sain" ou "Malade")
#' @return Un data frame filtre
#' @export
filtrer_patients <- function(data, sexe_filtre, diagnostic_filtre) {
  data |>
    dplyr::filter(sexe == sexe_filtre, diagnostic == diagnostic_filtre)
}

#' Calculer des statistiques groupees
#'
#' @param data Un data frame
#' @param groupe Nom de la colonne de groupement (character)
#' @return Un data frame avec les moyennes par groupe
#' @export
stats_par_groupe <- function(data, groupe) {
  data |>
    dplyr::group_by(.data[[groupe]]) |>
    dplyr::summarise(
      moy_age = mean(age, na.rm = TRUE),
      moy_cholesterol = mean(cholesterol, na.rm = TRUE),
      moy_bpm = mean(max_bpm, na.rm = TRUE)
    )
}
