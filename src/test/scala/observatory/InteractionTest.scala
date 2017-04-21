package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

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

  test("Location from tile") {
    val centre = Interaction.tileLocation(8, 128, 128)
    assert(centre === Location(0.0, 0.0))
    val left = Interaction.tileLocation(4, 0, 0)
    println(left)
  }

  test("Image from tile"){
    val temps = List[(Location, Double)]((Location(-90, 0), 0.0), (Location(90, 0), 100.0))
    val colors = List[(Double, Color)]((0.0, Color(0,0,255)), (100.0, Color(255,0,0)))
    val image = Interaction.tile(temps, colors, 0, 0, 0)
    image.output(new java.io.File("/Users/davidson1/Desktop/some-image.png"))
  }

  test("Generate tiles"){
//    val colors = List[(Double, Color)]((0.0, Color(0,0,255)), (100.0, Color(255,0,0)))

    val locatedTemps = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv")
    val data = Seq[(Int, Iterable[(Location, Double)])]((2021, Extraction.locationYearlyAverageRecords(locatedTemps)))

    val newGenerate = (year: Int, zoom : Int, x : Int, y: Int, v: Iterable[(Location, Double)]) => {
      val image = Interaction.tile(v, colorThresholds, x, y, zoom)
      image.output(s"/Users/davidson1/workspace/training/observatory/target/temperatures/$zoom/$x-$y.png")
      println("Done")
    }
    Interaction.generateTiles[Iterable[(Location, Double)]](data, newGenerate)
  }
}
