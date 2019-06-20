#' 
#' 
#' Gets a list of the air conditioners (pods) IDs of the authenticated user.
#' 
#' @export
#' @param key (character) API key from https://home.sensibo.com/me/api.
#' @return A character vectors with available pods unique IDs.
#' 
#' @examples \dontrun{
#' # Assuming that a valid Sensibo Sky API Key was created on https://home.sensibo.com/me/api
#' # and added to a 'sensibo.sky' global option.
#' #
#' # options("sensibo.key" = <Your Sensibo API Key>)
#'  
#' ## Getting the list of pods available to the user
#' pods.id <- sensibo.pods()
#' 
#' }
sensibo.pods <- function(key = getOption("sensibo.key")) {
   url <- file.path(base_url(), "users/me/pods")
   pods <- xGET(url, query=glue::glue("apiKey={key}"))
   do.call(c, lapply(pods, function(pod){pod$id}))
}

#' 
#' 
#' Gets additional info of a specific air conditioner (pod) of the authenticated user.
#' 
#' @export
#' @param pod (character) Pod unique id.
#' @param key (character) API key from https://home.sensibo.com/me/api.
#' @return A list of the available details for the given pod or an empty list if the pod doesn't exist.
#' 
#' @examples \dontrun{
#' # Assuming that a valid Sensibo Sky API Key was created on https://home.sensibo.com/me/api
#' # and added to a 'sensibo.sky' global option.
#' #
#' # options("sensibo.key" = <Your Sensibo API Key>)
#'  
#' ## Getting the list of pods available to the user
#' pods.id <- sensibo.pods()
#' 
#' ## Getting the details for the first pod
#' pod.details <- sensibo.pod.info(pods.id[1])
#' 
#' }
sensibo.pod.info <- function(pod, key = getOption("sensibo.key")) {
   if(length(pod)>1) stop("You must specify only one pod id here.")
   url <- file.path(base_url(), glue::glue("pods/{pod}"))
   xGET(url, query=glue::glue("apiKey={key}"))
}

#' 
#' 
#' Get the specified air conditioner (pod) current and previous states.
#' 
#' @export
#' @param pod (character) Pod unique id.
#' @param n (integer) The number of states to get (max 20); n=1 will get only the current state.
#' @param key (character) API key from https://home.sensibo.com/me/api.
#' @return A list with the requested states, starting from the current one.
#' 
#' @examples \dontrun{
#' # Assuming that a valid Sensibo Sky API Key was created on https://home.sensibo.com/me/api
#' # and added to a 'sensibo.sky' global option.
#' #
#' # options("sensibo.key" = <Your Sensibo API Key>)
#'  
#' ## Getting the list of pods available to the user
#' pods.id <- sensibo.pods()
#' 
#' ## Getting the current state of the first pod
#' ## Even if we're asking for the current state only, a list is returned anyway
#' pod.current <- sensibo.pod.states(pods.id[1], n = 1) 
#' }
sensibo.pod.states <- function(pod, n = 10, key = getOption("sensibo.key")) {
   if(length(pod)>1) stop("You must specify only one pod id here.")
   if(is.null(n)) n <- 10
   if(n > 20) n <- 20
   url <- file.path(base_url(), glue::glue("pods/{pod}/acStates"))
   states <- xGET(url, query=glue::glue("limit={n}&apiKey={key}"))
   return(states)
}

#' 
#' 
#' Get info from a specific state of a given air conditioner (pod).
#' 
#' @export
#' @param pod (character) Pod unique id.
#' @param state (character) State id to be retrieved.
#' @param key (character) API key from https://home.sensibo.com/me/api.
#' @return A list with the requested state details.
#' 
#' @examples \dontrun{
#' # Assuming that a valid Sensibo Sky API Key was created on https://home.sensibo.com/me/api
#' # and added to a 'sensibo.sky' global option.
#' #
#' # options("sensibo.key" = <Your Sensibo API Key>)
#'  
#' ## Getting the list of pods available to the user
#' pods.id <- sensibo.pods()
#' 
#' ## Getting the current state of the first pod
#' pod.current <- sensibo.pod.states(pods.id[1], n = 1) 
#' 
#' ## Get more details of the given state (if available)
#' pod.state.details <- sensibo.pod.state(pods.id[1], pod.current[1])
#' }
sensibo.pod.state <- function(pod, state, key = getOption("sensibo.key")) {
   if(length(pod)>1) stop("You must specify only one pod id here.")
   if(length(state)>1) stop("You must specify only one state id here.")
   url <- file.path(base_url(), glue::glue("pods/{pod}/acStates/{state}"))
   probe.list <- xGET(url, query=glue::glue("apiKey={key}"))
   return(probe.list)
}

#' 
#' 
#' Probe the last measurements sent by a given air conditioner (pod).
#' In a typical configuration, Sensibo Sky Remote send fresh data to the server every 90 seconds.
#' 
#' @export
#' @param pod (character) Pod unique id.
#' @param key (character) API key from https://home.sensibo.com/me/api.
#' @return A list with time, temperature and humidity values. A 'seconds since last send' field is also provided.
#' 
#' @examples \dontrun{
#' # Assuming that a valid Sensibo Sky API Key was created on https://home.sensibo.com/me/api
#' # and added to a 'sensibo.sky' global option.
#' #
#' # options("sensibo.key" = <Your Sensibo API Key>)
#'  
#' ## Getting the list of pods available to the user
#' pods.id <- sensibo.pods()
#' 
#' ## Get thew most current data probed by the first pod
#' pod.fresh.data <- sensibo.pod.probe(pods.id[1])
#' }
sensibo.pod.probe <- function(pod, key = getOption("sensibo.key")) {
   if(length(pod)>1) stop("You must specify only one pod id here.")
   url <- file.path(base_url(), glue::glue("pods/{pod}/measurements"))
   xGET(url, query=glue::glue("apiKey={key}"))
}

#' 
#' 
#' Probe the historical measurements sent by a given air conditioner (pod) for up to 7 days in the past.
#' 
#' @export
#' @param pod (character) Pod unique id.
#' @param days (integer) The number of days of measurements to get (max 7), including current day.
#' @param key (character) API key from https://home.sensibo.com/me/api.
#' @return A data frame with with time, temperature and humidity values.
#' 
#' @examples \dontrun{
#' # Assuming that a valid Sensibo Sky API Key was created on https://home.sensibo.com/me/api
#' # and added to a 'sensibo.sky' global option.
#' #
#' # options("sensibo.key" = <Your Sensibo API Key>)
#'  
#' ## Getting the list of pods available to the user
#' pods.id <- sensibo.pods()
#' 
#' ## Get a week of data values from the first pod
#' pod.data <- sensibo.pod.historical(pods.id[1], days = 7)
#' }
sensibo.pod.historical <- function(pod, days = 1, key = getOption("sensibo.key")) {
   if(length(pod)>1) stop("You must specify only one pod id here.")
   if(is.null(days)) days <- 1
   if(days > 7) days <- 7
   url <- file.path(base_url(), glue::glue("pods/{pod}/historicalMeasurements"))
   m <- xGET(url, query=glue::glue("days={days}&apiKey={key}"))
   
   df <- merge(do.call(rbind, lapply(m$temperature, function(t){as.data.frame(t, stringsAsFactors = F)})),
         do.call(rbind, lapply(m$humidity, function(t){as.data.frame(t, stringsAsFactors = F)})),
         by = "time")
   
   colnames(df) <- c("time", "temperature", "humidity")
   
   return(df)
}

#' 
#' 
#' Set the air conditioner (pod) state. NULL properties will be left unchanged. Please bear in mind that valid
#' values might depend of the A/C model.
#' 
#' @export
#' @param pod (character) Pod unique id.
#' @param on (logical) Set to TRUE to turn the pod on, FALSE to turn off instead.
#' @param mode (character) Set the mode from 'cool', 'hot', 'dry' and 'fan'
#' @param fan (character) Set the fan speed from 'low', 'medium', 'high' and 'auto'.
#' @param unit (character) Set the temperature unit: 'C' for Celsius, 'F' for Fahrenheit.
#' @param temperature (numeric) Set the target temperature
#' @param swing (character) Set the swing mode from 'stopped' and 'rangeFull'
#' @param key (character) API key from https://home.sensibo.com/me/api.
#' @return A list with the updated state and the result of the command (Success/Failure).
#' 
#' @examples \dontrun{
#' # Assuming that a valid Sensibo Sky API Key was created on https://home.sensibo.com/me/api
#' # and added to a 'sensibo.sky' global option.
#' #
#' # options("sensibo.key" = <Your Sensibo API Key>)
#'  
#' ## Getting the list of pods available to the user
#' pods.id <- sensibo.pods()
#' 
#' ## Update the status of the first pod
#' pod.newstate <- sensibo.pod.set(pods.id[1], on = TRUE, mode = "cool", temperature = 26)
#' }
sensibo.pod.set <- function(pod, on = NULL, mode = NULL, fan = NULL, unit = NULL, temperature = NULL, swing = NULL, key = getOption("sensibo.key")) {
   if(length(pod)>1) stop("You must specify only one pod id here.")
   url <- file.path(base_url(), glue::glue("pods/{pod}/acStates"))
   body.list <- list("on" = on,
                     "mode" = mode,
                     "fan" = fan,
                     "unit" = unit,
                     "temperature" = temperature,
                     "swing" = swing)
   propset <- do.call(c,lapply(body.list, function(body.list){!is.null(body.list)}))
   body <- jsonlite::toJSON(list("acState" = body.list[propset]), auto_unbox = T)
   xPOST(url, query=glue::glue("apiKey={key}"), body = body)
}

#' 
#' 
#' Get the climate react settings for a given air conditioner (pod).
#' 
#' @export
#' @param pod (character) Pod unique id.
#' @param key (character) API key from https://home.sensibo.com/me/api.
#' @return A list with the Climate React feature settings.
#' 
#' @examples \dontrun{
#' # Assuming that a valid Sensibo Sky API Key was created on https://home.sensibo.com/me/api
#' # and added to a 'sensibo.sky' global option.
#' #
#' # options("sensibo.key" = <Your Sensibo API Key>)
#'  
#' ## Getting the list of pods available to the user
#' pods.id <- sensibo.pods()
#' 
#' ## Get the climate react settings for the first pod
#' pod.smartsettings <- sensibo.pod.smartmode(pods.id[1])
#' }
sensibo.pod.smartmode <- function(pod, key = getOption("sensibo.key")) {
   if(length(pod)>1) stop("You must specify only one pod id here.")
   url <- file.path(base_url(), glue::glue("pods/{pod}/smartmode"))
   react.list <- xGET(url, query=glue::glue("apiKey={key}"))
   return(react.list)
}

#' 
#' 
#' Set the air conditioner (pod) Climate Reactmode. Set to TRUE to enable and FALSE to disable.
#' 
#' @export
#' @param pod (character) Pod unique id. Required: TRUE
#' @param enable (logical) Set to TRUE to turn on the climate react, FALSE to turn off instead.
#' @param key (character) API key from https://home.sensibo.com/me/api.
#' @return A list with the updated status of Climate React settings change.
#' 
#' @examples \dontrun{
#' # Assuming that a valid Sensibo Sky API Key was created on https://home.sensibo.com/me/api
#' # and added to a 'sensibo.sky' global option.
#' #
#' # options("sensibo.key" = <Your Sensibo API Key>)
#'  
#' ## Getting the list of pods available to the user
#' pods.id <- sensibo.pods()
#' 
#' ## Enable the Climate React mode for the first pod
#' sensibo.pod.smartmode.set(pods.id[1], TRUE)
#' }
sensibo.pod.smartmode.set <- function(pod, enable = NULL, key = getOption("sensibo.key")) {
   if(length(pod)>1) stop("You must specify only one pod id here.")
   url <- file.path(base_url(), glue::glue("pods/{pod}/smartmode"))
   body.list <- list("enabled" = enable)
   propset <- do.call(c,lapply(body.list, function(body.list){!is.null(body.list)}))
   body <- jsonlite::toJSON(body.list[propset], auto_unbox = T)
   xPOST(url, query=glue::glue("apiKey={key}"), body = body)
}
