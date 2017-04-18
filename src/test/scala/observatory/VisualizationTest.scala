package observatory


import java.io.File

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  //  test("Haversine distance formula"){
  //    val from = Location(51.517613, -0.093724) //work
  //    val to = Location(51.535253, -0.182067) //home
  //    val d = Visualization.distance(from, to)
  //    println(d)
  //    assert(d > 0)
  //  }

  test("greatCircleDistance test (extreme case 1)") {
    assert(Visualization.distance(Location(-12.0, 85.0), Location(12.0, -95.0)) == 6371 * math.Pi)
  }

  private val colorThresholds: Seq[(Double, Color)] = Seq(
    (60D, Color(255, 255, 255)),
    (32D, Color(255, 0, 0)),
    (12D, Color(255, 255, 0)),
    (0D, Color(0, 255, 255)),
    (-15D, Color(0, 0, 255)),
    (-27D, Color(255, 0, 255)),
    (-50D, Color(33, 0, 107)),
    (-60D, Color(0, 0, 0))
  )

  test("Can interpolate temperature") {
    val station1: (Location, Double) = (Location(51.535253, -0.182067), 10.0)
    val station2 = (Location(51.517613, -0.093724), 20.0)
    val testLocation: Location = Location(51.514868, -0.153428)
    val predictedTemp = Visualization.predictTemperature(List(station1, station2), testLocation)
    assert(math.abs(predictedTemp - 10) < math.abs(predictedTemp-20))
    val testLocation2 = Location(51.534, -0.181)
    val predictedTemp2 = Visualization.predictTemperature(List(station1, station2), testLocation2)
    assert(predictedTemp2 === 10.0)
  }

  test("Can interpolate color") {
    val tempCol0 = (0.0, Color(0, 0, 0))
    val tempCol1 = (100.0, Color(100, 100, 100))
    val tempCol2 = (200.0, Color(255, 255, 255))
    val testTemp = 50.0
    val testColor = Visualization.interpolateColor(Seq(tempCol0, tempCol1), testTemp)
    assert(testColor === Color(50, 50, 50))
    val testColorB = Visualization.interpolateColor(Seq(tempCol0, tempCol1), -10)
    assert(testColorB === Color(0, 0, 0))
    val testColor2 = Visualization.interpolateColor(Seq(tempCol0, tempCol1), 100)
    assert(testColor2 === Color(100, 100, 100))
    val testColor2a = Visualization.interpolateColor(Seq(tempCol0, tempCol1), 200)
    assert(testColor2a === Color(100, 100, 100))
    val testColor3 = Visualization.interpolateColor(Seq(tempCol0, tempCol2), 100)
    assert(testColor3 === Color(128, 128, 128))
  }

  test("Color interpolation 2") {
    // UNSORTED SCALE!!!
    val colors =
    List((60.0, Color(255, 255, 255)), (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)), (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)), (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)), (-60.0, Color(0, 0, 0)))
    assert(Visualization.interpolateColor(colors, 12.0) == Color(255, 255, 0))
    assert(Visualization.interpolateColor(colors, 62) == Color(255, 255, 255))
    assert(Visualization.interpolateColor(colors, 6) == Color(128, 255, 128))
  }

  test("visualize") {
    val locations = (-89 to 90).flatMap(i => (-179 to 180) map (j => Location(i, j)))
    val locatedTemps = locations.map(loc => (loc, loc.lon))
    val colorMap = locatedTemps.map{case (loc, temp) => (loc, Visualization.interpolateColor(colorThresholds, temp))}.toMap
    val image = Visualization.visualize(locatedTemps, colorThresholds)
    assert(image != null)
    val pixels = image.pixels
    val written = image.output(new File("C:\\Users\\Adam\\Desktop\\some-image.png"))
    assert(written != null)

//    (Location(45.0,-90.0),19.551640848805803)
//    (Location(-45.0,0.0),-100.0)
//    (19.551640848805803,Color(255,0,0))
//    (-100.0,Color(0,0,255))
  }

  test("failing case visualise"){
    val locatedTemps = Seq((Location(45.0,-90.0),19.551640848805803),(Location(-45.0,0.0),-100.0))
    val colors =  Seq((19.551640848805803,Color(255,0,0)),(-100.0,Color(0,0,255)))
    val image = Visualization.visualize(locatedTemps, colors)
    val pixels = image.pixels
    pixels.forall(_ != null)
  }

  //  test("Can produce image") {
  //    val tempsAtStations = Extraction.locateTemperatures(2000, "/stations.csv", "/2000.csv")
  //    val averageTemps = Extraction.locationYearlyAverageRecords(tempsAtStations)
  //    val locations = (90 to -90 by -1).flatMap(i => (-180 to 180 by 10).map(j => Location(i.toDouble, j.toDouble))).toSeq.par
  //    println("Interpolating temps")
  //    val interpolatedTemps = locations.map(location => (location, Visualization.predictTemperature(averageTemps, location)))
  //    val image = Visualization.visualize(interpolatedTemps.seq, colorThresholds)
  //    image.output(new java.io.File("some-image.png"))
  //  }
}
