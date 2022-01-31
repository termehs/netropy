test_that("trivariate entropy works", {
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

  H.tri <- entropy_trivar(df.att.ed)
  result <- c(2.745, 2.63, 2.897, 2.67, 3.123, 2.995, 3.194, 3.032, 3.448, 3.079)
  expect_identical(H.tri[["H(V1,V2,V3)"]][1:10],result)
})
