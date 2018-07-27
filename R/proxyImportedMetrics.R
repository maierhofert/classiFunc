pr_DB$set_entry(FUN = ucrdtw, names = "rucrdtw",
  loop = TRUE, type = "metric",
  description = "Dynamic Time Warping from UCR",
  reference = "Boersch-Supan (2016). rucrdtw: Fast time series subsequence search in R.
    The Journal of Open Source Software URL http://doi.org/10.21105/joss.00100;
    Rakthanmanon et al. (2012). Searching and mining trillions of time series subsequences
    under dynamic time warping. SIGKDD URL http://doi.org/10.1145/2339530.2339576",
  formula = "minimum of sum(x[xw[i]]-y[yw[i]]) over all monotonic xw, yw");

pr_DB$set_entry(FUN = ucred, names = "rucred",
  loop = TRUE, type = "metric",
  description = "Euclidean Distance from UCR",
  reference = "Boersch-Supan (2016). rucrdtw: Fast time series subsequence search in R.
    The Journal of Open Source
    Software URL http://doi.org/10.21105/joss.00100;
    Rakthanmanon et al. (2012). Searching and mining trillions of time series subsequences
    under dynamic time
    warping. SIGKDD URL http://doi.org/10.1145/2339530.2339576",
  formula = "sqrt(sum((x-y)^2))");
