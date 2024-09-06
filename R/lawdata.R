#' Law Firm
#' @docType data
#' @usage data(lawdata)
#' @description This data set comes from a network study of corporate
#' law partnership that was carried out in a Northeastern US corporate law firm,
#' referred to as SG&R, 1988-1991 in New England.
#' It includes (among others) measurements of networks among the
#' 71 attorneys (partners and associates) of this firm,
#' i.e. their strong- co-worker network, advice network, friendship network,
#' and indirect control networks.
#' Various members' attributes are also part of the data set,
#' including seniority, formal status, office in which they work, gender,
#' law school attended. The ethnography, organizational and network analyses
#' of this case are available in Lazega (2001).
#'
#' \strong{Basic advice network}:
#' "Think back over the past year, consider all the lawyers in your Firm. To whom did you go for basic professional advice? For instance, you want to make sure that you are handling a case right, making a proper decision, and you want to consult someone whose professional opinions are in general of great value to you. By advice I do not mean simply technical advice."
#'
#' \strong{Friendship network:}
#' "Would you go through this list, and check the names of those you socialize with outside work. You know their family, they know yours, for instance. I do not mean all the people you are simply on a friendly level with, or people you happen to meet at Firm functions."
#'
#' \strong{Strong coworkers network:}
#'  "Because most firms like yours are also organized very informally, it is difficult to get a clear idea of how the members really work together. Think back over the past year, consider all the lawyers in your Firm. Would you go through this list and check the names of those with whom you have worked with. (By "worked with" I mean that you have spent time together on at least one case, that you have been assigned to the same case, that they read or used your work product or that you have read or used their work product; this includes professional work done within the Firm like Bar association work, administration, etc.)"
#'
#' @format List containing the following objects as numbered
#'  \enumerate{
#'   \item{adjacency matrix for advice seeking (directed)}
#'   \item{adjacency matrix for friendship (directed)}
#'   \item{adjacency matrix for cowork (undirected)}
#'   \item{dataframe with the following attributes on each lawyer:}
#' \itemize{
#' \item{`senior`: seniority (ranked from most to least senior)}
#' \item{`status`:   1=partner; 2=associate}
#' \item{`gender`: 1=man; 2=woman}
#' \item{`office`: 1=Boston; 2=Hartford; 3=Providence}
#' \item{`years`: years with the firm}
#' \item{`age`: age of attorney}
#' \item{`practice`: 1=litigation; 2=corporate}
#' \item{`lawschool`: 1=harvard/yale; 2=ucon; 3= other}
#'
#'}
#'Note: the first 36 out of 71 respondents are the partners in the firm.
#'}
#'
#'
#' @source https://www.stats.ox.ac.uk/~snijders/siena/Lazega_lawyers_data.htm
#'
#' @examples
#' data(lawdata)
#' ## assign the correct names to the objects in the list
#' adj.advice <- lawdata[[1]]
#' adj.friend <- lawdata[[2]]
#' adj.cowork <-lawdata[[3]]
#' df.att <- lawdata[[4]]
#'
#' @references
#' Emmanuel Lazega, \emph{The Collegial Phenomenon: The Social Mechanisms of Cooperation Among Peers in a Corporate Law Partnership}, Oxford University Press (2001).
#'
#' Tom A.B. Snijders, Philippa E. Pattison, Garry L. Robins, and Mark S. Handcock. New specifications for exponential random graph models. \emph{Sociological Methodology} (2006), 99-153.
"lawdata"



