package com.example.try1

import org.scalatest.funsuite.AnyFunSuite
import org.locationtech.jts.geom.Coordinate
import javax.swing.SwingUtilities

class PolygonFixerTest extends AnyFunSuite {

  // Toggle this to enable or disable polygon visualizations during tests
  val showVisualization = true

  def visualizeFix(
                    original: Seq[Coordinate],
                    fixed: Seq[Coordinate],
                    added: Seq[Coordinate],
                    title: String
                  ): Unit = {
    if (!showVisualization) return
    SwingUtilities.invokeLater(() => {
      PolygonVisualizer.showComparison(original, fixed, added, title)
    })
    Thread.sleep(4000)
  }

  // === Core Tests ===

  test("Polygon with self-intersection should be fixed") {
    val coords = Seq(
      new Coordinate(0, 0),
      new Coordinate(2, 2),
      new Coordinate(0, 4),
      new Coordinate(4, 0),
      new Coordinate(4, 4),
      new Coordinate(0, 0)
    )
    val (fixedCoords, addedPoints) = PolygonFixer.fixSelfIntersectingPolygon(coords)
    visualizeFix(coords, fixedCoords, addedPoints, "Self-Intersecting Polygon Fix")
    assert(addedPoints.nonEmpty)
    assert(fixedCoords.length > coords.length)
  }

  test("Polygon without intersection remains unchanged") {
    val coords = Seq(
      new Coordinate(0, 0),
      new Coordinate(0, 4),
      new Coordinate(4, 4),
      new Coordinate(4, 0),
      new Coordinate(0, 0)
    )
    val (fixedCoords, addedPoints) = PolygonFixer.fixSelfIntersectingPolygon(coords)
    visualizeFix(coords, fixedCoords, addedPoints, "Clean Polygon")
    assert(addedPoints.isEmpty)
    assert(fixedCoords == coords)
  }

  // === Valid Bowtie Variants ===

  def bowtieVariants: Seq[(String, Seq[Coordinate])] = {
    val A = new Coordinate(0, 0)
    val B = new Coordinate(4, 4)
    val C = new Coordinate(0, 4)
    val D = new Coordinate(4, 0)

    Seq(
      "ABCD (classic bowtie)"       -> Seq(A, B, C, D, A),
      "ADCB (reverse bowtie)"       -> Seq(A, D, C, B, A),
      "BACD (start at B)"           -> Seq(B, A, C, D, B),
      "CABD (start at C)"           -> Seq(C, A, B, D, C),
      "CDAB (rotated)"              -> Seq(C, D, A, B, C),
      "DCBA (reverse all)"          -> Seq(D, C, B, A, D),
      "DBAC (non-sequential cross)" -> Seq(D, B, A, C, D),
      "BDCA (corrected crossing)"   -> Seq(B, D, C, A, B)
    )
  }

  bowtieVariants.foreach { case (label, coords) =>
    test(s"Bowtie variant: $label should be fixed") {
      val (fixedCoords, addedPoints) = PolygonFixer.fixSelfIntersectingPolygon(coords)
      visualizeFix(coords, fixedCoords, addedPoints, s"Bowtie Variant: $label")
      assert(addedPoints.nonEmpty, s"Variant '$label' should have added offset points")
      assert(fixedCoords.length > coords.length, s"Variant '$label' should be fixed")
    }
  }
}
