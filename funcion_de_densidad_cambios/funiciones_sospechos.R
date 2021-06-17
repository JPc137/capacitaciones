if (!require("np")) install.packages("np")


densidad_1paso <-
  function(df_cat,
           poutlayer = 0.10,
           var =  c("navevar1", "navevar2")) {
    formula <- formula(paste("~", paste(var, collapse = "+")))
    bw <- np::npudensbw(formula, data = df_cat[, var], bwmethod="normal-reference")
    #f <- npudens(tdat = df_cat[, var])
    f <- npudens(bw)
    df_cat$fdens <- f$dens
    df_cat$sospechoso <- with(df_cat, ifelse(df_cat$fdens <= quantile(df_cat$fdens, poutlayer), 1, 0))
    return(df_cat)
  }


densidad_m_pasos <-
  function(df_cat, poutlayer = 0.10, var =  c("navevar1", "navevar2"),
           numit = 1) {
    #print(head(df_cat, 2))
    sosp  <- NULL
    for (ii in  1:numit) {
      #print(ii)
      #print(dim(df_cat))
      #bw <-npudensbw(formula, data =  df_cat[, var], bwmethod="normal-reference")
      f <- np::npudens( df_cat[, var], bwmethod="normal-reference")
      df_cat$fdens <- f$dens
      df_cat$sospechoso <- with(df_cat, ifelse(df_cat$fdens <= quantile(df_cat$fdens, poutlayer/numit), 1, 0))
      tem_sosp <- subset(df_cat, sospechoso == 1)
      sosp <- rbind(sosp, tem_sosp)
      #print(dim(sosp))
      df_cat_0 <- subset(df_cat, sospechoso != 1)
      #print(dim(df_cat_0))
      df_cat  <- df_cat_0
    }
    total <- rbind(df_cat_0 , sosp)
    return(total)
  }
