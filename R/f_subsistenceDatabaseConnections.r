#' loadODBCLibrary
#'
#' Loads ODBC package.
#'
#' @returns Nothing.
#' @export
#'
loadODBCLibrary = function(){
  library(odbc)
}

#' connectToSDS
#'
#' Initializes connection to Subsistence Database server.
#'
#' @param server Server name.
#' @returns Server connection.
#' @export
#'

connectToSDS = function(server)
{
  connectSDS = odbc::dbConnect(odbc::odbc()
                  , driver = 'SQL Server'
                  , server = server
                  , database = 'Sub_SDS'
                  , .connection_string = "Trusted_Connection=True;"
                  , timeout = 10
                  , encoding = "UTF-8")

  return(connectSDS)
}

#' connectToASFDB
#'
#' Initializes connection to Alaska Salmon Fishery Database server.
#'
#' @param server Server name.
#' @returns Server connection.
#' @export
#'

connectToASFDB = function(server)
{
  connectASFDB = odbc::dbConnect(odbc::odbc()
                         , driver = 'SQL Server'
                         , server = server
                         , database = 'Sub_ASFDB'
                         , .connection_string = "Trusted_Connection=True;"
                         , timeout = 10
                         , encoding = "UTF-8")

  return(connectASFDB)
}

#' connectToCSIS
#'
#' Initializes connection to Community Subsistence Information System server.
#'
#' @param server Server name.
#' @returns Server connection.
#' @export
#'

connectToCSIS = function(server)
{
  connectCSIS = odbc::dbConnect(odbc::odbc()
                           , driver = 'SQL Server'
                           , server = server
                           , database = 'Sub_CSIS'
                           , .connection_string = "Trusted_Connection=True;"
                           , timeout = 10
                           , encoding = "UTF-8")

  return(connectCSIS)
}

#' connectToKuskokwim
#'
#' Initializes connection to Kusko database server.
#'
#' @param server Server name.
#' @returns Server connection.
#' @export
#'

connectToKuskokwim = function(server)
{
  connectKusko = odbc::dbConnect(odbc::odbc()
                           , driver = 'SQL Server'
                           , server = server
                           , database = 'Sub_Kuskokwim'
                           , .connection_string = "Trusted_Connection=True;"
                           , timeout = 10
                           , encoding = "UTF-8")

  return(connectKusko)
}

#' connectToHalibut
#'
#' Initializes connection to halibut database server.
#'
#' @param server Server name.
#' @returns Server connection.
#' @export
#'
connectToHalibut = function(server)
{
  connectHalibut = odbc::dbConnect(odbc::odbc()
                             , driver = 'SQL Server'
                             , server = server
                             , database = 'Sub_Halibut'
                             , .connection_string = "Trusted_Connection=True;"
                             , timeout=10
                             , encoding = "UTF-8")
  return(connectHalibut)
}

#' connectToHHDB
#'
#' Initializes connection to household database server.
#'
#' @param server Server name.
#' @returns Server connection.
#' @export
#'

connectToHHDB = function(server)
{
  connectHHDB = odbc::dbConnect(odbc::odbc()
                           , driver = 'SQL Server'
                           , server = server
                           , database = 'Sub_Survey_Household'
                           , .connection_string = "Trusted_Connection=True;"
                           , timeout = 10
                           , encoding = "UTF-8")

  return(connectHHDB)
}

#' connectToCSHDB
#'
#' Initializes connection to community subsistence hunt (GMU 13 moose and caribou) database server.
#'
#' @param server Server name.
#' @returns Server connection.
#' @export
#'
connectToCSHDB = function(server)
{
  connectCSH = odbc::dbConnect(odbc::odbc()
                           , driver = 'SQL Server'
                           , server = server
                           , database = 'Sub_CSH'
                           , .connection_string = "Trusted_Connection=True;"
                           , timeout = 10
                           , encoding = "UTF-8")

  return(connectCSH)
}

#' connectToBBPermit
#'
#' Initializes connection to Bristol Bay subsistence salmon fishery permit database server.
#'
#' @param server Server name.
#' @returns Server connection.
#' @export
#'
connectToBBPermit = function(server)
{
  connectBBPerm = odbc::dbConnect(odbc::odbc()
                               , driver = 'SQL Server'
                               , server = server
                               , database = 'Sub_Permit_BB'
                               , .connection_string = "Trusted_Connection=True;"
                               , timeout = 10
                               , encoding = "UTF-8")

  return(connectBBPerm)
}
