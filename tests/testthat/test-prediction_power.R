test_that("prediction power works", {
  data(lawdata)
  df.att <- lawdata[[4]]

  df.att.ed <- data.frame(
    status   = df.att$status,
    gender   = df.att$gender,
    office   = df.att$office-1,
    years    = ifelse(df.att$years<=3,0,
                      ifelse(df.att$years<=13,1,2)),
    age      = ifelse(df.att$age<=35,0,
                      ifelse(df.att$age<=45,1,2)),
    practice = df.att$practice,
    lawschool= df.att$lawschool-1)
    ppower <- prediction_power('status', df.att.ed)
    result <- c(status = NA, gender = 0.695, office = 1.084, years = 0.927,
                age = 1.007, practice = 1.226, lawschool = 1.693)
    expect_identical(diag(ppower),result)

})
