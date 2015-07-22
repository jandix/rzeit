saveZeit <- function(df, path = getwd(), format = c("txt", "sps", "sas", "dta"), name = "zeit_df")
  {



  # nach http://www.statmethods.net/input/exportingdata.html

  # create directory if path does not exist

  if (!file.exists(path)) {
    dir.create(path, showWarnings = FALSE)
  }

  # create name and path for saving---------------

  time <- str_replace_all(as.character(Sys.time()), "\\W", "-")
  name <- paste(name, time, sep = "_")
  path <- paste(path, "/", name, sep = "")

  # create directories if not existing


  # function to save as txt----------------------

  txtFunc <- function(x, p){
    p <- paste(p, ".txt", sep = "")
    write.table(x, p, sep="\t")
  }

  # function to save as spss file----------------------

  spssFunc <- function(x, p){
    pTxt <- paste(p, ".txt", sep = "")

    pSpss <- paste(p, ".sps", sep = "")

    write.foreign(x, pTxt, pSpss, package = "SPSS")
  }


  # function to save as sas file----------------------

  sasFunc <- function(x, p){
    pTxt <- paste(p, ".txt", sep = "")

    pSas <- paste(p, ".sas", sep = "")

    write.foreign(x, pTxt, pSas, package = "SAS")
  }

  # function to save as stata file----------------------

  stataFunc <- function(x, p){
    p <- paste(p, ".dta", sep = "")
    write.dta(x, p)
  }



  sortby <- match.arg(format)
  switch(sortby,
         txt = txtFunc(df, path),
         sps = spssFunc(df, path),
         sas = sasFunc(df, path),
         dta = stataFunc(df, path))
}
