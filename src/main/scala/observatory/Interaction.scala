package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x    X coordinate
    * @param y    Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val exponent = math.pow(2, zoom)
    val lon_deg = (x / exponent) * 360 - 180.0
    val lat_rad = math.atan(math.sinh(math.Pi * (1 - 2 * y / exponent)))
    val lat_deg = math.toDegrees(lat_rad)
    Location(lat_deg, lon_deg)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param zoom         Zoom level
    * @param x            X coordinate
    * @param y            Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
//    System.err.println(temperatures)
//    System.err.println(colors)
//    System.err.println(s"$x,$y,$zoom")
    val scaledX = 256 * x
    val scaledY = 256 * y
    val locations = for (
      j <- scaledY to scaledY + 255;
      i <- scaledX to scaledX + 255
    ) yield tileLocation(zoom + 8, i, j)
    val pixelColors = locations.map(Visualization.predictTemperature(temperatures, _)).map(Visualization.interpolateColor(colors, _))
    val pixels = pixelColors.map(c => Pixel(c.red, c.green, c.blue, 127)).toArray
    Image(256, 256, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Int, Data)],
                           generateImage: (Int, Int, Int, Int, Data) => Unit
                         ): Unit = {
    yearlyData.foreach {
      case (year, data) =>
        for (
          zoom <- 0 to 3;
          x <- 0 until math.pow(2, zoom).toInt;
          y <- 0 until math.pow(2, zoom).toInt
        ) generateImage(year, zoom, x, y, data)
    }
  }

}
