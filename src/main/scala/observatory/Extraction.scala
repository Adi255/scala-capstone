package observatory

import java.time.LocalDate

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stationLocationMap = createStationLookup(stationsFile)
    val temps = readFile(temperaturesFile).map(tempDataFromLine)
    temps.filter(temp => stationLocationMap.contains(temp.stationId))
      .map(temp => (LocalDate.of(year, temp.month, temp.day), stationLocationMap(temp.stationId), temp.tempC))
      .toIterable
  }

  private def createStationLookup(stationsFile: String): Map[(String, String), Location] = {
    val stations = readFile(stationsFile).map(_.split(",", -1)).filterNot(tokens => tokens(2).isEmpty || tokens(3).isEmpty).map {
      case Array(stn, wbn, lat, lon) => Station((stn, wbn), Location(lat.toDouble, lon.toDouble))
    }
    val stationLocationMap = stations.toSeq.map(station => (station.id, station.location)).toMap
    stationLocationMap
  }

  private def readFile(stationsFile: String): Iterator[String] = {
    Source.fromInputStream(getClass.getResourceAsStream(stationsFile)).getLines()
  }

  private def tempDataFromLine(line: String) = {
    val tokens = line.split(",")
    TempData((tokens(0), tokens(1)), tokens(2).toInt, tokens(3).toInt, toCelcius(tokens(4).toDouble))
  }

  private def toCelcius(tempF: Double) = (tempF - 32) * 5 / 9

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.groupBy(_._2).mapValues(values => values.map(_._3).reduce(_ + _) / values.size)
  }

  case class Station(id: (String, String), location: Location)

  case class TempData(stationId: (String, String), month: Int, day: Int, tempC: Double)

}
