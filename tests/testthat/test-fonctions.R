test_that("filtrer_patients fonctionne", {
  result <- filtrer_patients(packageexam, "Homme", "Malade")
  expect_true(all(result$sexe == "Homme"))
  expect_true(all(result$diagnostic == "Malade"))
})

test_that("stats_par_groupe fonctionne", {
  result <- stats_par_groupe(packageexam, "diagnostic")
  expect_equal(nrow(result), 2)
  expect_true("moy_age" %in% colnames(result))
})

test_that("analyser_variable erreur si colonne inexistante", {
  expect_error(analyser_variable(packageexam, "colonne_inexistante", "test"))
})

test_that("tracer_boxplot fonctionne", {
  p <- tracer_boxplot(packageexam, "cholesterol", "Cholestérol")
  expect_s3_class(p, "ggplot")
})
