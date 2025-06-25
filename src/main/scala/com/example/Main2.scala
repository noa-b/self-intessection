package com.example

object Main2 extends App {

  def testPolygon(name: String, vertices: Seq[Point]): Unit = {
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

  // ▶️ Test Case 1: Simple Bowtie (Hourglass)
  val bowtie = Seq(
    Point(0, 0),
    Point(2, 2),
    Point(0, 2),
    Point(2, 0)
  )
  testPolygon("Bowtie (Hourglass)", bowtie)

  // ▶️ Test Case 2: Complex Self-Intersecting Polygon (star-like shape)
  val complex = Seq(
    Point(1, 1),
    Point(5, 1),
    Point(3, 4),
    Point(2, 0),
    Point(4, 0),
    Point(3, 5)
  )
  testPolygon("Complex Star-Like Polygon", complex)

  // ▶️ Test Case 3: Non-intersecting square (control test)
  val square = Seq(
    Point(1, 1),
    Point(5, 1),
    Point(5, 5),
    Point(1, 5)
  )
  testPolygon("Simple Square (No Intersections)", square)

  val bowtieShir = Seq(
    Point(-2, 6),
    Point(3, 7),
    Point(0, 3),
    Point(-1, -1),
    Point(2, 0)
  )
  testPolygon("Bowtie (Spetial, Shir)", bowtieShir)
}


