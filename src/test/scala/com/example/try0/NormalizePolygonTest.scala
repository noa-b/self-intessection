package com.example

import Geometry.Pt
import com.example.PolygonGenerator._
import com.example.AsciiPolygonDrawer._
import org.scalatest.funsuite.AnyFunSuite

class NormalizePolygonTest extends AnyFunSuite {
  import com.example.try0.NormalizePolygon._

  private def pointsApproxEqual(p1: Pt, p2: Pt, epsilon: Double = 1e-6): Boolean =
    (p1.x - p2.x).abs < epsilon && (p1.y - p2.y).abs < epsilon

  private def polygonsApproxEqual(p1: Seq[Pt], p2: Seq[Pt], epsilon: Double = 1e-6): Boolean = {
    if (p1.length != p2.length) return false
    val n = p1.length
    (0 until n).exists { offset =>
      (0 until n).forall { i =>
        pointsApproxEqual(p1(i), p2((i + offset) % n), epsilon)
      }
    }
  }

  private def normalizedPolygonsMatch(result: Seq[Seq[Pt]], expected: Seq[Seq[Pt]]): Boolean = {
    val unmatched = scala.collection.mutable.Set(expected: _*)
    for (rPoly <- result) {
      unmatched.find(ePoly => polygonsApproxEqual(rPoly, ePoly)) match {
        case Some(matched) => unmatched.remove(matched)
        case None => return false
      }
    }
    unmatched.isEmpty
  }

  // Updated: compare polygons visually, including grid diff output
  private def printNormalizedComparison(label: String, input: Seq[Pt], normalized: Seq[Seq[Pt]]): Unit = {
    println(s"\n=== $label ===")
    printPolygon("Input polygon", input, showIndices = true)
    normalized.zipWithIndex.foreach { case (poly, i) =>
      printPolygon(s"Normalized polygon #$i", poly, showIndices = true)
      println(s"Changes from input → normalized #$i:")
      printPolygonChanges(input, poly) // NEW: visual diff grid!
    }
  }

  test("complex star polygon (expected polygons need to be filled manually)") {
    val star = Seq(
      Pt(0, 3), Pt(1, 1), Pt(3, 1), Pt(1.5, 0),
      Pt(2.5, -2), Pt(0, -1), Pt(-2.5, -2), Pt(-1.5, 0),
      Pt(-3, 1), Pt(-1, 1)
    )

    val result = normalize(star)
    printNormalizedComparison("Star polygon normalization", star, result)

    val expectedStar: Seq[Seq[Pt]] = Seq()
    assert(result.nonEmpty)
    if (expectedStar.nonEmpty) {
      assert(normalizedPolygonsMatch(result, expectedStar))
    }
  }

  test("self-intersecting bowtie polygon") {
    val bowtie = Seq(
      Pt(0, 0),
      Pt(2, 2),
      Pt(0, 2),
      Pt(2, 0)
    )

    val result = normalize(bowtie)
    printNormalizedComparison("Bowtie polygon normalization", bowtie, result)

    val expectedBowtie = Seq(
      Seq(Pt(0, 0), Pt(1, 1), Pt(0, 2)),
      Seq(Pt(1, 1), Pt(2, 2), Pt(2, 0))
    )

    assert(normalizedPolygonsMatch(result, expectedBowtie))
  }

//  test("complex self-intersecting polygon (expected polygons need to be filled manually)") {
//    val complexPoly = Seq(
//      Pt(0, 0), Pt(4, 3), Pt(8, 0), Pt(4, -3),
//      Pt(0, 0), Pt(-4, 3), Pt(-8, 0), Pt(-4, -3),
//      Pt(0, 0)
//    )
//    val result = normalize(complexPoly)
//    printNormalizedComparison("Complex self-intersecting polygon normalization", complexPoly, result)
//
//    val expectedComplexPoly: Seq[Seq[Pt]] = Seq()
//    assert(result.nonEmpty)
//    if (expectedComplexPoly.nonEmpty) {
//      assert(normalizedPolygonsMatch(result, expectedComplexPoly))
//    }
//  }
//
//  test("large complex polygon simulating real GIS shape (expected polygons need to be filled manually)") {
//    val poly = generatePolygon(cx = 1000, cy = 1000, r = 500, n = 50)
//    val normalized = normalize(poly)
//    printNormalizedComparison("Large complex polygon normalization", poly, normalized)
//
//    val expected: Seq[Seq[Pt]] = Seq()
//    assert(normalized.nonEmpty)
//    if (expected.nonEmpty) {
//      assert(normalizedPolygonsMatch(normalized, expected))
//    }
//  }
//
//  test("self intersecting polygon simulating complex GIS feature (expected polygons need to be filled manually)") {
//    val poly = generateSelfIntersectingPolygon(cx = 500, cy = 500, r = 300, n = 50)
//    val normalized = normalize(poly)
//    printNormalizedComparison("Self-intersecting GIS polygon normalization", poly, normalized)
//
//    val expected: Seq[Seq[Pt]] = Seq()
//    assert(normalized.nonEmpty)
//    if (expected.nonEmpty) {
//      assert(normalizedPolygonsMatch(normalized, expected))
//    }
//  }
}
