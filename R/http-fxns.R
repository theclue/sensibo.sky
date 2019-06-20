#' 
#' 
#' Gets a list of the air conditioners (pods) IDs of the authenticated user.
#' 
#' @export
#' @param key (character) API key from https://home.sensibo.com/me/api
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
#' @param pod (character) Pod unique id. Required: TRUE
#' @param key (character) API key from https://home.sensibo.com/me/api.
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
#' @param pod (character) Pod unique id. Required: TRUE
#' @param n (integer) The number of states to get (max 20); n=1 will get only the current state.
#' @param key (character) API key from https://home.sensibo.com/me/api.
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
#' Get info from a specific state of a given air conditioner (pod)
#' 
#' @export
#' @param pod (character) Pod unique id. Required: TRUE
#' @param state (character) State id to be retrieved. Required: TRUE
#' @param key (character) API key from https://home.sensibo.com/me/api.
sensibo.pod.state <- function(pod, state, key = getOption("sensibo.key")) {
   if(length(pod)>1) stop("You must specify only one pod id here.")
   if(length(state)>1) stop("You must specify only one state id here.")
   url <- file.path(base_url(), glue::glue("pods/{pod}/acStates/{state}"))
   probe.list <- xGET(url, query=glue::glue("apiKey={key}"))
   return(probe.list)
}

#' 
#' 
#' Probe the last measurements sent by a given air conditioner (pod)
#' 
#' @export
#' @param pod (character) Pod unique id. Required: TRUE
#' @param key (character) API key from https://home.sensibo.com/me/api.
sensibo.pod.probe <- function(pod, key = getOption("sensibo.key")) {
   if(length(pod)>1) stop("You must specify only one pod id here.")
   url <- file.path(base_url(), glue::glue("pods/{pod}/measurements"))
   xGET(url, query=glue::glue("apiKey={key}"))
}

#' 
#' 
#' Probe the historical measurements sent by a given air conditioner (pod) for up to 7 days
#' 
#' @export
#' @param pod (character) Pod unique id. Required: TRUE
#' @param days (integer) The number of days of measurements to get (max 7), including the current day.
#' @param key (character) API key from https://home.sensibo.com/me/api.
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
#' Set the air conditioner (pod) state. NULL properties will be left unchanged. Please bear in mind tha valid
#' values might depend of the AC manifacturer.
#' 
#' @export
#' @param pod (character) Pod unique id. Required: TRUE
#' @param on (logical) Set to TRUE to turn the pod on, FALSE to turn off instead.
#' @param mode (character) Set the mode from 'cool', 'hot', 'dry' and 'fan'
#' @param fan (character) Set the fan speed from 'low', 'medium', 'high' and 'auto'.
#' @param unit (character) Set the temperature unit: 'C' for Celsius, 'F' for Fahrenheit.
#' @param temperature (numeric) Set the target temperature
#' @param swing (character) Set the swing mode from 'stopped' and 'rangeFull'
#' @param key (character) API key from https://home.sensibo.com/me/api.
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
#' Get the climate react settings for a given air conditioner (pod)
#' 
#' @export
#' @param pod (character) Pod unique id. Required: TRUE
#' @param key (character) API key from https://home.sensibo.com/me/api.
sensibo.pod.smartmode <- function(pod, key = getOption("sensibo.key")) {
   if(length(pod)>1) stop("You must specify only one pod id here.")
   url <- file.path(base_url(), glue::glue("pods/{pod}/smartmode"))
   react.list <- xGET(url, query=glue::glue("apiKey={key}"))
   return(react.list)
}

#' 
#' 
#' Set the air conditioner (pod) climate react. Set to TRUE to enable and FALSE to disable.
#' 
#' @export
#' @param pod (character) Pod unique id. Required: TRUE
#' @param enable (logical) Set to TRUE to turn on the climate react, FALSE to turn off instead.
#' @param key (character) API key from https://home.sensibo.com/me/api.
sensibo.pod.smartmode.set <- function(pod, enable = NULL, key = getOption("sensibo.key")) {
   if(length(pod)>1) stop("You must specify only one pod id here.")
   url <- file.path(base_url(), glue::glue("pods/{pod}/smartmode"))
   body.list <- list("enabled" = enable)
   propset <- do.call(c,lapply(body.list, function(body.list){!is.null(body.list)}))
   body <- jsonlite::toJSON(body.list[propset], auto_unbox = T)
   xPOST(url, query=glue::glue("apiKey={key}"), body = body)
}


