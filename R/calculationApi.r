calculationApi <- R6Class("calculationApi",
	private = list(),
	public = list(
		serviceBase = NULL,
		authType = NULL,
		username = NULL,
		password = NULL,
		validateSSL = NULL,
		debug = NULL,
		initialize = function(baseUrl, authType, username, password, validateSSL, debug) {
			self$serviceBase <- baseUrl
			self$username <- username
			self$password <- password
			self$authType <- authType
			self$validateSSL <- validateSSL
			self$debug <- debug
		},
convertToDataFrame = function(items) {
            itemsLength <- length(items)
            value <- array(1:itemsLength)
            timestamp <- array(1:itemsLength)
            unitsAbbreviation <- array(1:itemsLength)
            good <- array(1:itemsLength)
            questionable <- array(1:itemsLength)
            substituted <- array(1:itemsLength)
            for (i in 1:itemsLength) {
                if (is.null(items[[i]]$Value) == FALSE)
                {
                  if (is.numeric(items[[i]]$Value) == TRUE)
                  {
                    value[i] <- items[[i]]$Value
                  }
                  else
                  {
                    value[i] <- items[[i]]$Value$Name
                  }
                }
                if (is.null(items[[i]]$Timestamp) == FALSE)
                {
                  timestamp[i] <- items[[i]]$Timestamp
                }
                if (is.null(items[[i]]$UnitsAbbreviation) == FALSE)
                {
                  unitsAbbreviation[i] <- items[[i]]$UnitsAbbreviation
                }
                if (is.null(items[[i]]$Good) == FALSE)
                {
                  good[i] <- items[[i]]$Good
                }
                if (is.null(items[[i]]$Questionable) == FALSE)
                {
                  questionable[i] <- items[[i]]$Questionable
                }
                if (is.null(items[[i]]$Substituted) == FALSE)
                {
                  substituted[i] <- items[[i]]$Substituted
                }
            }


            resDataFrame <- data.frame(value, timestamp, unitsAbbreviation, good, questionable, substituted)
            return(resDataFrame)
        },
		getAtIntervals = function(endTime, expression, sampleInterval, selectedFields, startTime, webId) {
			qs <- customQueryString$new()
			localVarPath <- paste(c(self$serviceBase, '/calculation/intervals'), collapse = "")
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				qs$add('endTime', endTime, FALSE);
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(expression) == FALSE && is.null(expression) == FALSE && expression != "") {
				qs$add('expression', expression, FALSE);
				if (is.character(expression) == FALSE) {
					return (print(paste0("Error: expression must be a string.")))
				}
			}
			if (missing(sampleInterval) == FALSE && is.null(sampleInterval) == FALSE && sampleInterval != "") {
				qs$add('sampleInterval', sampleInterval, FALSE);
				if (is.character(sampleInterval) == FALSE) {
					return (print(paste0("Error: sampleInterval must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				qs$add('startTime', startTime, FALSE);
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(webId) == FALSE && is.null(webId) == FALSE && webId != "") {
				qs$add('webId', webId, FALSE);
				if (is.character(webId) == FALSE) {
					return (print(paste0("Error: webId must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PITimedValues"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getAtRecorded = function(endTime, expression, selectedFields, startTime, webId) {
			qs <- customQueryString$new()
			localVarPath <- paste(c(self$serviceBase, '/calculation/recorded'), collapse = "")
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				qs$add('endTime', endTime, FALSE);
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(expression) == FALSE && is.null(expression) == FALSE && expression != "") {
				qs$add('expression', expression, FALSE);
				if (is.character(expression) == FALSE) {
					return (print(paste0("Error: expression must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				qs$add('startTime', startTime, FALSE);
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(webId) == FALSE && is.null(webId) == FALSE && webId != "") {
				qs$add('webId', webId, FALSE);
				if (is.character(webId) == FALSE) {
					return (print(paste0("Error: webId must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PITimedValues"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		},
		getSummary = function(calculationBasis, endTime, expression, sampleInterval, sampleType, selectedFields, startTime, summaryDuration, summaryType, timeType, webId) {
			qs <- customQueryString$new()
			localVarPath <- paste(c(self$serviceBase, '/streams/', webId,'/summary'), collapse = "")
			if (missing(calculationBasis) == FALSE && is.null(calculationBasis) == FALSE && calculationBasis != "") {
				qs$add('calculationBasis', calculationBasis, FALSE);
				if (is.character(calculationBasis) == FALSE) {
					return (print(paste0("Error: calculationBasis must be a string.")))
				}
			}
			if (missing(endTime) == FALSE && is.null(endTime) == FALSE && endTime != "") {
				qs$add('endTime', endTime, FALSE);
				if (is.character(endTime) == FALSE) {
					return (print(paste0("Error: endTime must be a string.")))
				}
			}
			if (missing(expression) == FALSE && is.null(expression) == FALSE && expression != "") {
				qs$add('expression', expression, FALSE);
				if (is.character(expression) == FALSE) {
					return (print(paste0("Error: expression must be a string.")))
				}
			}
			if (missing(sampleInterval) == FALSE && is.null(sampleInterval) == FALSE && sampleInterval != "") {
				qs$add('sampleInterval', sampleInterval, FALSE);
				if (is.character(sampleInterval) == FALSE) {
					return (print(paste0("Error: sampleInterval must be a string.")))
				}
			}
			if (missing(sampleType) == FALSE && is.null(sampleType) == FALSE && sampleType != "") {
				qs$add('sampleType', sampleType, FALSE);
				if (is.character(sampleType) == FALSE) {
					return (print(paste0("Error: sampleType must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(startTime) == FALSE && is.null(startTime) == FALSE && startTime != "") {
				qs$add('startTime', startTime, FALSE);
				if (is.character(startTime) == FALSE) {
					return (print(paste0("Error: startTime must be a string.")))
				}
			}
			if (missing(summaryDuration) == FALSE && is.null(summaryDuration) == FALSE && summaryDuration != "") {
				qs$add('summaryDuration', summaryDuration, FALSE);
				if (is.character(summaryDuration) == FALSE) {
					return (print(paste0("Error: summaryDuration must be a string.")))
				}
			}
			if (missing(summaryType) == FALSE && is.null(summaryType) == FALSE && summaryType != "") {
				qs$add('summaryType', summaryType, TRUE);
				if (is.vector(summaryType) == FALSE) {
					return (print(paste0("Error: summaryType must be a vector.")))
				}
			}
			if (missing(timeType) == FALSE && is.null(timeType) == FALSE && timeType != "") {
				qs$add('timeType', timeType, FALSE);
				if (is.character(timeType) == FALSE) {
					return (print(paste0("Error: timeType must be a string.")))
				}
			}
			if (missing(webId) == FALSE && is.null(webId) == FALSE && webId != "") {
				qs$add('webId', webId, FALSE);
				if (is.character(webId) == FALSE) {
					return (print(paste0("Error: webId must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PIItemsSummaryValue"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			dataframe <- self$convertToDataFrame(contentResponse$Items)
			return (dataframe)
		},
		getAtTimes = function(expression, selectedFields, sortOrder, time, webId) {
			qs <- customQueryString$new()
			localVarPath <- paste(c(self$serviceBase, '/calculation/times'), collapse = "")
			if (missing(expression) == FALSE && is.null(expression) == FALSE && expression != "") {
				qs$add('expression', expression, FALSE);
				if (is.character(expression) == FALSE) {
					return (print(paste0("Error: expression must be a string.")))
				}
			}
			if (missing(selectedFields) == FALSE && is.null(selectedFields) == FALSE && selectedFields != "") {
				qs$add('selectedFields', selectedFields, FALSE);
				if (is.character(selectedFields) == FALSE) {
					return (print(paste0("Error: selectedFields must be a string.")))
				}
			}
			if (missing(sortOrder) == FALSE && is.null(sortOrder) == FALSE && sortOrder != "") {
				qs$add('sortOrder', sortOrder, FALSE);
				if (is.character(sortOrder) == FALSE) {
					return (print(paste0("Error: sortOrder must be a string.")))
				}
			}
			if (missing(time) == FALSE && is.null(time) == FALSE && time != "") {
				qs$add('time', time, TRUE);
				if (is.vector(time) == FALSE) {
					return (print(paste0("Error: time must be a vector.")))
				}
			}
			if (missing(webId) == FALSE && is.null(webId) == FALSE && webId != "") {
				qs$add('webId', webId, FALSE);
				if (is.character(webId) == FALSE) {
					return (print(paste0("Error: webId must be a string.")))
				}
			}
			res <- getHttpRequest(localVarPath, qs$getQueryParameters(), self$username, self$password, self$authType, self$validateSSL, self$debug)
			contentResponse <- content(res)
			if (res$status == 200) {
				attr(contentResponse, "className") <- "PITimedValues"
			}
			if (res$status == 409) {
				attr(contentResponse, "className") <- "PIErrors"
			}
			return (contentResponse)
		}
	)
)
