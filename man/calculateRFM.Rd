% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateRFM.R
\name{calculateRFM}
\alias{calculateRFM}
\title{calculateRFM}
\usage{
calculateRFM(data, weight_recency = 1, weight_frequency = 1,
  weight_monetary = 1)
}
\arguments{
\item{data}{- a data.table containing the transaction records}

\item{r}{- weight of recency}

\item{f}{- weight of frequency}

\item{m}{- weight of monetary value}
}
\value{
returns a data.table containing the recency, frequency and monetary score as well as well as the weighted
final score and the group membership.
}
\description{
Calculate a weighted RFM score: recency, frequency, monetary value
}
\details{
\code{data} contains the transactional data. The dataset must contain a column labeled "Customer" that allows
            unique customer identification and a column labeled "TransDate", indicating the purchase date.
            The column "PurchAmount" specifies the total spending per  purchase.
}
