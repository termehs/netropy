test_that("bivariate entropy works", {
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

     H.biv <- entropy_bivar(df.att.ed)
     result <- c(status = 1, gender = 0.817, office = 1.125, years = 1.585,
                 age = 1.585, practice = 0.983, lawschool = 1.533)
     expect_identical(diag(H.biv),result)
})
