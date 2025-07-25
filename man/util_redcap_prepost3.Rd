% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_redcap_prepost3.R
\name{util_redcap_prepost3}
\alias{util_redcap_prepost3}
\title{util_redcap_prepost3: Organize Pre/Post visit 3 data from REDCap (called within proc_redcap.R)}
\usage{
util_redcap_prepost3(data, return_data = TRUE)
}
\arguments{
\item{data}{data from REDCap event visit_3_prepost_arm_1}

\item{return_data}{return organized data (default = TRUE)}
}
\value{
If return_data is set to TRUE, will return a list including:
 1) clean raw Pre/Post visit 3 datasets
 2) meta-data/.json for each dataset
}
\description{
This function organizes REDCap data from event visit_3_prepost_arm_1
}
\examples{

# process REDCap data
prepost_visit3_data <- util_redcap_prepost3(data)

\dontrun{
}


}
