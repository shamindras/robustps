% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-huber.R
\name{gen_huber_samples}
\alias{gen_huber_samples}
\title{Generate specified number of samples from the required Huber density function
NOTE: we are generating samples for, f, g separately and then combining into
an h sample.
TODO: Consider creating h sample directly given f, g, epsilon}
\usage{
gen_huber_samples(density_p0 = list(type = "normal", mean = 0, sd = 1),
  density_q = list(type = "normal", mean = 0, sd = 1), eps, n_samples)
}
\arguments{
\item{n_samples}{}
}
\description{
Generate specified number of samples from the required Huber density function
NOTE: we are generating samples for, f, g separately and then combining into
an h sample.
TODO: Consider creating h sample directly given f, g, epsilon
}
