% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_task_foodrating.R
\name{util_task_foodrating}
\alias{util_task_foodrating}
\title{util_task_foodrating: Process raw data from the Food Rating Task}
\usage{
util_task_foodrating(
  sub_str,
  ses,
  base_wd,
  overwrite = FALSE,
  return_data = FALSE
)
}
\arguments{
\item{sub_str}{participant string (i.e., sub_###) as character}

\item{ses}{session name (i.e., 'baseline', 'followup')}

\item{base_wd}{absolute path to the data directory}

\item{overwrite}{logical indicating if data should be overwritten. Default = FALSE}

\item{return}{logical indicating if computed summary data should be returned. Default = FALSE}
}
\value{
If return_data is set to TRUE, will return a list including a clean raw dataset with meta-data
}
\description{
This function: 1) cleans data to save in BIDS format in rawdata and 2) generates summary data that can be used to generate a database
}
\details{
To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
}
\examples{

# process task data for the Food Choice Task
foodrating_task_pardat <- util_task_foodrating(sub_str, ses, data_path, return = TRUE)

\dontrun{
}


}
