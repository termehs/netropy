test_that("tetravariate entropy works", {
  data(lawdata)
  df.att <- lawdata[[4]]

  att_var <- data.frame(
    status    = df.att$status - 1,
    gender    = df.att$gender,
    office    = df.att$office - 1,
    years     = ifelse(df.att$years <= 3, 0,
                  ifelse(df.att$years <= 13, 1, 2)),
    age       = ifelse(df.att$age <= 35, 0,
                  ifelse(df.att$age <= 45, 1, 2)),
    practice  = df.att$practice,
    lawschool = df.att$lawschool - 1
  )

  h4 <- entropy_tetravar(
    att_var[, c("gender", "years", "age", "office", "practice")]
  )

  expect_identical(
    colnames(h4),
    c("X", "Y", "Z", "U", "H_XYZU", "EH_U_XYZ", "EH_Z_XYU", "EJ_XY_ZU")
  )
  expect_identical(nrow(h4), 30L)

  expect_identical(
    h4$H_XYZU[1:6],
    c(4.22, 4.22, 4.22, 4.22, 4.22, 4.22)
  )
  expect_identical(
    h4$EH_U_XYZ[1:6],
    c(0.88, 0.88, 0.92, 0.88, 0.92, 0.81)
  )
  expect_identical(
    h4$EH_Z_XYU[1:6],
    c(0.92, 0.81, 0.81, 0.46, 0.46, 0.46)
  )
  expect_identical(
    h4$EJ_XY_ZU[1:6],
    c(0.29, 0.14, 0.12, 0.56, 0.14, 0.18)
  )
})

test_that("tetravariate entropy respects dec argument", {
  data(lawdata)
  df.att <- lawdata[[4]]

  att_var <- data.frame(
    gender   = df.att$gender,
    office   = df.att$office - 1,
    practice = df.att$practice,
    status   = df.att$status - 1
  )

  h4_2 <- entropy_tetravar(att_var, dec = 2)
  h4_4 <- entropy_tetravar(att_var, dec = 4)

  expect_identical(h4_2$H_XYZU, round(h4_4$H_XYZU, 2))
  expect_identical(h4_2$EJ_XY_ZU, round(h4_4$EJ_XY_ZU, 2))
})
