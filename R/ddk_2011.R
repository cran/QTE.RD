#' Student Achievement Data
#'
#' @description
#' The student achievement data used in Duflo, Dupas, and Kremer (2011) is available from the Harvard Dataverse (see **Source** below).
#' The original dataset contains 7,022 observations and 106 variables.
#' We extracted 5,795 observations with non-missing values for the outcome variable `totalscore` and retained 10 key variables.
#'
#' @usage data(ddk_2011)
#'
#' @format A data frame with 5795 rows and 10 columns.
#' \describe{
#' \item{schoolid}{ID of primary school.}
#' \item{tracking}{School sampled for tracking.}
#' \item{percentile}{Student's percentile in initial distribution.}
#' \item{totalscore}{Total endline score.}
#' \item{etpteacher}{Student assigned to contract teacher.}
#' \item{lowstream}{Student assigned to lower-ability section (if tracking school).}
#' \item{highstream}{Student assigned to higher-ability section (if tracking school).}
#' \item{girl}{Sex of student: female.}
#' \item{agetest}{Age of student at time of test.}
#' \item{ts_std}{Standardized total endline score.}
#' }
#'
#' @source \doi{10.7910/DVN/LWFH9U}
#'
#' @references Esther Duflo, Pascaline Dupas, and Michael Kremer (2011) “Peer Effects, Teacher Incentives, and the Impact of Tracking: Evidence from a Randomized Evaluation in Kenya,” American Economic Review, 1739-1774.
#'
"ddk_2011"
