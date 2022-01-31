test_that("redundancy works", {
  data(lawdata)
  df.att <- lawdata[[4]]

  df.att.ed <- data.frame(
    senior   = df.att$senior,
    status   = df.att$status,
    gender   = df.att$gender,
    office   = df.att$office-1,
    years    = ifelse(df.att$years<=3,0,
                      ifelse(df.att$years<=13,1,2)),
    age      = ifelse(df.att$age<=35,0,
                      ifelse(df.att$age<=45,1,2)),
    practice = df.att$practice,
    lawschool= df.att$lawschool-1)
  expect_identical(unname(redundancy(df.att.ed)[1,]),c(0,1,1,1,1,1,1,1))
})
