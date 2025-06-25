package com.example

object Main extends App {

//  chaotic_irregular
//  hole_touching_loop
//  keyhole_bow_loop
//  nested_keyhole
  private def testPolygon(name: String, vertices: Seq[Point]): Unit = {
    println(s"\n=== Testing Polygon: $name ===")
    val intersections = SelfIntersectionFinder.findSelfIntersections(vertices)

    println(s"Found ${intersections.size} self-intersections:")
    intersections.foreach(p =>
      println(f"(${p._x}%.2f, ${p._y}%.2f) from edge ${p._i} to ${p._j}")
    )

    // Show visualization
    PolygonVisualizer.show(vertices, intersections)
    PolygonVisualizer.saveImage(name, vertices, intersections)
  }

  private val bowtie = Seq(
    Point(0, 0),
    Point(2, 2),
    Point(0, 2),
    Point(2, 0)
  )
  testPolygon("Bowtie Hourglass", bowtie)

  private val complex = Seq(
    Point(1, 1),
    Point(5, 1),
    Point(3, 4),
    Point(2, 0),
    Point(4, 0),
    Point(3, 5)
  )
  testPolygon("Complex Star-Like Polygon", complex)

  private val square = Seq(
    Point(1, 1),
    Point(5, 1),
    Point(5, 5),
    Point(1, 5)
  )
  testPolygon("Simple Square No Intersections", square)

  private val bowtieShir = Seq(
    Point(-2, 6),
    Point(3, 7),
    Point(0, 3),
    Point(-1, -1),
    Point(2, 0)
  )
  testPolygon("Bowtie Special from Shir", bowtieShir)

  val star=
    Seq(
      Point(0, 3), Point(1, 1), Point(3, 1), Point(1.5, 0),
      Point(2.5, -2), Point(0, -1), Point(-2.5, -2), Point(-1.5, 0),
      Point(-3, 1), Point(-1, 1)
    )
  testPolygon("star", star)


  private val complexPolygon1 =
    Seq(
      Point(0, 0), Point(4, 3), Point(8, 0), Point(4, -3),
      Point(0, 0), Point(-4, 3), Point(-8, 0), Point(-4, -3),
      Point(0, 0)
    )
  testPolygon("two diamonds", complexPolygon1)

  private val zigzagLoop = Seq(
    Point(0, 0),
    Point(4, 2),
    Point(8, -1),
    Point(12, 3),
    Point(16, 0),
    Point(12, -3),
    Point(8, 1),
    Point(4, -2),
    Point(0, 1),
    Point(4, 3),
    Point(8, 0),
    Point(12, 4),
    Point(16, 2)
  )
  testPolygon("Zig‑Zag Loop", zigzagLoop)

  private val exterior = Seq(
    Point(0,0), Point(10,0), Point(10,10), Point(0,10),
    Point(0,0),
    Point(4,4), Point(6,4), Point(6,6), Point(4,6), Point(4,4)
  )
  testPolygon("Square with Hole‑like Self Crossing", exterior)

  private val starComplex = Seq(
    Point(0,0), Point(5,10), Point(10,0), Point(6,6),
    Point(14,14), Point(8,8), Point(19,4), Point(12,5),
    Point(20,0), Point(0,0)
  )
  testPolygon("Nested Star Intersections", starComplex)

  private val keyhole = Seq(
    Point(-3, 0), Point(3, 6), Point(3, -6),
    Point(-3, 6), Point(3, 0),
    Point(-3, -6), Point(-3,0)
  )
  testPolygon("Keyhole Bow-Loop", keyhole)

  private val holeTouching = Seq(
    Point(0, 0), Point(10, 0), Point(10, 10),
    Point(5, 10), Point(5, 5), Point(10, 5),
    Point(10, 15), Point(0, 15), Point(0, 5),
    Point(5, 5), Point(5, 0), Point(0, 0)
  )
  testPolygon("Hole-Touching Loop", holeTouching)

  private val nestedKeyhole = Seq(
    Point(0, 0), Point(4, 8), Point(8, 0),
    Point(4, 4), Point(6, 2), Point(4, 6),
    Point(2, 2), Point(4, 4), Point(0, 0)
  )
  testPolygon("Nested Keyhole", nestedKeyhole)

  private val windingWisp = Seq(
    Point(0,0), Point(3,1), Point(6,0), Point(8,4),
    Point(6,7), Point(4,4), Point(2,7), Point(0,4),
    Point(2,1), Point(4,3), Point(6,2), Point(8,5),
    Point(6,8)
  )
  testPolygon("Winding Wisp", windingWisp)

  private val chaotic = Seq(
    Point(2,2), Point(8,3), Point(9,7), Point(6,9),
    Point(3,8), Point(7,5), Point(5,2), Point(4,6),
    Point(1,9), Point(0,5), Point(2,7), Point(5,10),
    Point(8,9), Point(10,6), Point(11,2)
  )
  testPolygon("Chaotic Irregular", chaotic)

  private val houseLoop = Seq(
    Point(0,0), Point(4,0), Point(4,3), Point(2,5),
    Point(0,3), Point(0,0),  // outline of house
    Point(1,2), Point(3,2), Point(2,4), Point(1,2) // attic loop
  )
  testPolygon("House with Attic Loop", houseLoop)
}
