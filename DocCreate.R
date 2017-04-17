#' ---
#' title: "Raw data and indicators documentation"
#' output: html_document
#' ---
#' <a id="idZ"></a>
#'
#'
#' General
#'
#' [Variables definition: from A-Z](#id1)
#'
#' [Variable availability: from A-Z](#id2)
#'
#' Sections
#'
#' [Asset-related variables](#id3)
#'
#' [Inflation measures](#id4)
#'
#' [Growth and activity](#id5)
#' 
#' [Other economic series](#id6)
#'
#' [Created in current outline](#id7)
#'
#'
#' # Variables definition: from A-Z
#'
#+ echo = FALSE
load(paste(folder.local, "/DataDefinition.RData", sep=""))
#+
#' <a id="id1"></a>
#+ echo = FALSE, results = "asis"
tab.indicX <- tab.indic
rownames(tab.indicX) <- tab.indicX[, 1]
tab.indicX <- tab.indicX[sort(rownames(tab.indicX)), ]
rownames(tab.indicX) <-NULL
pander::pandoc.table(tab.indicX[, 1:2], justify='left')
#+
#' [Back to top](#idZ)
#'
#'
#' # Asset-related variables
#' <a id="id3"></a>
#+ echo = FALSE, results = "asis"
idx <- which(tab.indicX[,4] == "ASSET")
pander::pandoc.table(tab.indicX[idx, 1:2], justify='left')
#+
#' [Back to top](#idZ)
#'
#'
#' # Inflation measures
#' <a id="id4"></a>
#+ echo = FALSE, results = "asis"
idx <- which(tab.indicX[,4] == "INFLATION")
pander::pandoc.table(tab.indicX[idx, 1:2], justify='left')
#+
#' [Back to top](#idZ)
#'
#'
#' # Growth and activity
#' <a id="id5"></a>
#+ echo = FALSE, results = "asis"
idx <- which(tab.indicX[,4] == "ACTIVITY")
pander::pandoc.table(tab.indicX[idx, 1:2], justify='left')
#+
#' [Back to top](#idZ)
#'
#' # Other economic series
#' <a id="id6"></a>
#+ echo = FALSE, results = "asis"
idx <- which(tab.indicX[,4] == "OTHER")
pander::pandoc.table(tab.indicX[idx, 1:2], justify='left')
#+
#' [Back to top](#idZ)
#'
#' # Created in current outline
#' <a id="id7"></a>
#+ echo = FALSE, results = "asis"
idx <- which(is.na(tab.indicX[,4]))
pander::pandoc.table(tab.indicX[idx, 1:2], justify='left')
#+
#' [Back to top](#idZ)
#'
#'
#' # Variable availability: from A-Z
#' <a id="id2"></a>
#+ echo = FALSE, results = "asis"
pander::pandoc.table(tab.indicX[, c(1, 3)], justify='left')
#+
#' [Back to top](#idZ)
#'
#'

