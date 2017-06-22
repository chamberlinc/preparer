na_fill = function(x, tol){
  # x is a vector of data
  # tol is max number of steps missing (if greater, it retains NA)
  ina = is.na(x)
  csum = cumsum(!ina)
  wg = as.numeric(names(which(table(csum) > tol))) # which gaps are too long
  x[ina] = approx(x, xout=which(ina))$y
  x[which(csum%in%wg)[-1]] = NA
  return(x)
}

# stack data files from the same data logger but different dates
load_stack_file = function(files, gmtoff, logger){
  dates = sub(".*_(.*)_.*\\..*", "\\1", files) # get all dates
  xx = lapply(dates, function(x) load_file(grep(x,files,value=TRUE), gmtoff$offs[which(gmtoff$dnld_date==x)], logger) ) # load data for each date
  xx = Reduce(function(df1,df2) bind_rows(df1,df2), xx) # stack them up
  arrange(xx, DateTimeUTC)
}

# Snap timestamps to the closest interval
snap_ts = function(x, samp_freq, nearest=FALSE){
  # x is a date-time vector to be snapped
  # freq is the frequency of observations as a string
  #   containing the number of units and the unit (S,M,H,D)
  #   e.g., '15M', '1H', '3D', '66S'
  # nearest is logical to snap to floor (default) or to nearest time cut
  re = regexec("([0-9]+)([A-Z])",samp_freq)[[1]]
  if(-1%in%re){
    stop("Please enter a correct string")
  }else{
    ml = attr(re,"match.length")
    nn = as.numeric(substr(samp_freq, re[2], ml[2]))
    uu = substr(samp_freq, re[3], ml[1])
    if(uu=="D"){td = 24*60*60*nn
    }else if(uu=="H"){td = 60*60*nn
    }else if(uu=="M"){td = 60*nn
    }else if(uu=="S"){td = nn
    }else{stop("Please enter a correct string")}
  }
  if(nearest){ # round to closest interval
    as.POSIXct(round(as.double(x)/td)*td,origin="1970-01-01")
  }else{ # round to floor
    as.POSIXct(floor(as.double(x)/td)*td,origin="1970-01-01")
  }
}

# Fold the data together into one data frame
fold_ts = function(...){
  ll = (...)
  if(length(ll)>1){
    x = Reduce(function(df1,df2) full_join(df1,df2,by="DateTime"), ll)
  }else{
    x = ll[[1]]
  }
  cat("Your data are cleaned.\n")
  arrange(x, DateTime)
}
