package observatory

import java.lang.Math.toRadians

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val EarthRadiusKm = 6371
  private val distanceThreshold: Int = 1
  private val p = 2

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val stationsWithDistance: Iterable[(Double, Double, Double)] =
      temperatures.map { case (stationLoc, temp) => {
        val d = distance(stationLoc, location)
        (d, temp, 1 / Math.pow(d, p))
      }
      }
    val closeStation = stationsWithDistance.find(_._1 <= distanceThreshold)
    closeStation match {
      case Some((_, temp, _)) => temp
      case None => interpolateTemp(stationsWithDistance) //, location)
    }
  }

  def distance(from: Location, to: Location): Double = {
    val latRadDelta = toRadians(to.lat - from.lat)
    val lonRadDelta = toRadians(to.lon - from.lon)
    val fromLatRad = toRadians(from.lat)
    val toLatRad = toRadians(to.lat)
    val a = Math.pow(Math.sin(latRadDelta / 2), 2) + Math.pow(Math.sin(lonRadDelta / 2), 2) * Math.cos(fromLatRad) * Math.cos(toLatRad)
    val c = 2 * Math.asin(Math.sqrt(a))
    EarthRadiusKm * c
  }

  def interpolateTemp(temperatures: Iterable[(Double, Double, Double)]) = {
    val (numerator, denominator) = temperatures.foldLeft((0.0, 0.0)) {
      (z, term) => {
        (z._1 + term._2 * term._3, z._2 + term._3)
      }
    }
    numerator / denominator
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
//    temperatures.foreach(println)
//    colors.foreach(println)
//    val locationColors = temperatures.map { case (location, temp) => (location.lat, location.lon, interpolateColor(colors, temp)) }
    val colorMap = temperatures.map { case (location, temp) => ((location.lat.toInt, location.lon.toInt), interpolateColor(colors, temp)) }.toMap
    val pixels = (-180 to 179).flatMap(i => (90 to -89).map(j => (i,j))).map{case (i,j) => colorMap.getOrElse((i,j), Color(0,0,0))}.map(c => Pixel(c.red, c.green, c.blue, 255))
//    val pixels = locationColors.toList.sortBy(row => (-1 * row._1, row._2)).map { case (_, _, c) => Pixel(c.red, c.green, c.blue, 255) }.toArray
    Image(360, 180, pixels.toArray)
  }

  /**
    * @param points      Pairs containing a value and its associated color
    * @param temperature The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], temperature: Double): Color = {
    val sortedPoints = points.toList.sortBy(_._1)
    if (temperature < sortedPoints.head._1)
      sortedPoints.head._2
    else if (temperature > sortedPoints.reverse.head._1)
      sortedPoints.reverse.head._2
    else {
      val matchColor = points.find(_._1 == temperature)
      matchColor match {
        case Some((_, color)) => color
        case None => {
          val (temp0, color0) = sortedPoints.filter(_._1 <= temperature).sortBy(-1 * _._1).head
          val (temp1, color1) = sortedPoints.filter(_._1 > temperature).head
          colorCalc(temperature, temp0, temp1, color0, color1)
        }
      }
    }
  }

  private def colorCalc(temp: Double, temp0: Double, temp1: Double, color0: Color, color1: Color) = {
    val colorVals = Seq(color0.red, color0.green, color0.blue).zip(Seq(color1.red, color1.green, color1.blue))
    val interpolated = colorVals.map { case (val1, val2) => interpolateVal(temp, temp0, temp1, val1, val2) }
    Color(interpolated(0), interpolated(1), interpolated(2))
  }

  private def interpolateVal(x: Double, x0: Double, x1: Double, y0: Int, y1: Int): Int = {
    Math.round(y0 + (x - x0) * (y1 - y0) / (x1 - x0)).toInt
  }

}

