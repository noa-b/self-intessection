package com.example

import Geometry.Pt

import scala.util.Random

object PolygonGenerator {
  // Generates a simple polygon with n points in roughly circular shape
  def generatePolygon(cx: Double, cy: Double, r: Double, n: Int): Seq[Pt] = {
    val rnd = new Random(42)
    (0 until n).map { i =>
      val angle = 2 * math.Pi * i / n
      val radius = r * (0.7 + 0.6 * rnd.nextDouble())
      Pt(cx + radius * math.cos(angle), cy + radius * math.sin(angle))
    }
  }

  // Generates a self-intersecting polygon with n points
  def generateSelfIntersectingPolygon(cx: Double, cy: Double, r: Double, n: Int): Seq[Pt] = {
    val base = generatePolygon(cx, cy, r, n)
    // Introduce self intersections by swapping some points randomly
    val swapped = base.toArray
    val rnd = new Random(123)
    for (_ <- 1 to n / 5) {
      val i = rnd.nextInt(n)
      val j = rnd.nextInt(n)
      val tmp = swapped(i)
      swapped(i) = swapped(j)
      swapped(j) = tmp
    }
    swapped.toSeq
  }
}
