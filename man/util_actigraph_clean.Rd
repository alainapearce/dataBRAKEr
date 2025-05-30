% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_actigraph_clean.R
\name{util_actigraph_clean}
\alias{util_actigraph_clean}
\title{util_actigraph_clean: Compile and clean up processed actigraphy data}
\usage{
util_actigraph_clean(
  save_path,
  ggir_path,
  mMARCH_path,
  metrics = c("SL", "PA", "CR"),
  overwrite = FALSE,
  return_data = TRUE
)
}
\arguments{
\item{save_path}{full file path to the derivative directory where output should be saved}

\item{ggir_path}{full file path to GGIR derivative directory}

\item{mMARCH_path}{full file path to mMARCH.AC derivative directory}

\item{metrics}{list of metrics. Default: c('SL', 'PA', 'CR'). Options include: \itemize{
\item{SL - sleep}
\item{PA - physical activity}
\item{CR - circadian rhythem}
}}

\item{overwrite}{logical indicating if data should be overwritten. Default = FALSE}

\item{return_data}{return organized data (default = TRUE)}
}
\value{
If return_data is set to TRUE, will return a list including a clean raw dataset with meta-data
}
\description{
This function compiles and cleans up processed actigraphy data from GGIR and mMARCH.AC
}
\details{
To use this function, util_actigraph_org_rawdata.R and util_actigraph_mMARCH.R must be completed so all actigraphy information is available.
}
\examples{

# process task data for the Food Choice Task
sleep_dat <- util_actigraph_clean(save_path, ggir_path, mMARCH_path, metrics = c('SL') return = TRUE)

\dontrun{
}


}
