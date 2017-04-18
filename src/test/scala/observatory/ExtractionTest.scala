package observatory

import org.scalatest.FunSuite

class ExtractionTest extends FunSuite {

  test("Can map temperatures to location, then produce averages per location") {
    val temps = Extraction.locateTemperatures(1990, "/stations.csv", "/1990.csv")
    println(temps.size)
    assert(!temps.isEmpty)
    val averages = Extraction.locationYearlyAverageRecords(temps)
    assert(!averages.isEmpty)
    averages.filter(_._1.lon < 0).take(5).foreach(println)
  }

}