package com.example.try0

import Geometry._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

object NormalizePolygon {
  sealed trait HolePolicy
  case object NonePolicy extends HolePolicy
  case object Aggressive extends HolePolicy
  case object NonZero extends HolePolicy
  case object EvenOdd extends HolePolicy

  def normalize(
                 poly: Seq[Pt],
                 disturbEps: Double = 1e-4,
                 eps: Double = 1e-6,
                 holePolicy: HolePolicy = Aggressive
               ): Seq[Seq[Pt]] = {

    if (poly.length <= 3) return Seq(poly)

    val fixedPoly = fixSelfIntersections(poly, eps)
    val pts = if (disturbEps > 0) disturb(fixedPoly, disturbEps) else fixedPoly

    val verts = buildVertices(pts)
    val isectMirror = mirrorIsects(verts)

    val used = mutable.Set[Int]()
    val usedIsects = mutable.Set[(Int, Int)]()
    val usedEdges = mutable.Set[(Int, Int, Int, Int)]()

    val xmin = fixedPoly.map(_.x).min
    val amin = fixedPoly.indexWhere(_.x == xmin)
    val cw = checkConcave(fixedPoly, amin)

    val result = mutable.ArrayBuffer.empty[Seq[Pt]]

    traceOutline(amin, -1, cw, isOutline = true, verts, isectMirror, used, usedIsects, usedEdges).foreach(result += _)

    if (holePolicy != NonePolicy) {
      val holeTasks = for {
        i <- verts.indices
        j <- verts(i).isects.indices
        if !usedIsects.contains((i, j)) && !usedIsects.contains(isectMirror((i, j)))
      } yield Future {
        traceOutline(i, j, cw, isOutline = false, verts, isectMirror, used, usedIsects, usedEdges).flatMap { hole =>
          val ok = holePolicy match {
            case Aggressive => true
            case NonZero    => polyIsHole(hole, fixedPoly, verifyWinding = true, eps)
            case EvenOdd    => polyIsHole(hole, fixedPoly, verifyWinding = false, eps)
            case _          => false
          }
          if (ok) Some(hole) else None
        }
      }
      val holes = Await.result(Future.sequence(holeTasks), Duration.Inf).flatten
      result ++= holes

      if (holePolicy == EvenOdd) {
        val altHoleTasks = for {
          i <- verts.indices
          j <- verts(i).isects.indices
          if !usedIsects.contains((i, j))
        } yield Future {
          traceOutline(i, j, -cw, isOutline = false, verts, isectMirror, used, usedIsects, usedEdges).flatMap { h =>
            if (polyIsHole(h, fixedPoly, verifyWinding = false, eps)) Some(h) else None
          }
        }
        val altHoles = Await.result(Future.sequence(altHoleTasks), Duration.Inf).flatten
        result ++= altHoles
      }
    }

    result.toSeq
  }

  // Now traceOutline accepts all needed parameters explicitly
  def traceOutline(
                    start: Int,
                    startIsect: Int,
                    cw: Int,
                    isOutline: Boolean,
                    verts: Seq[Vtx],
                    isectMirror: Map[(Int, Int), (Int, Int)],
                    used: mutable.Set[Int],
                    usedIsects: mutable.Set[(Int, Int)],
                    usedEdges: mutable.Set[(Int, Int, Int, Int)]
                  ): Option[Seq[Pt]] = {

    val out = mutable.ArrayBuffer[Pt]()
    val localUsed = mutable.Set[Int]()
    val localIsects = mutable.Set[(Int, Int)]()

    var i0 = start
    var j0 = startIsect
    val zero = (i0, j0)

    val maxIterations = verts.length * 10 + 1000
    var iterations = 0
    val visited = mutable.Set[(Int, Int)]()

    do {
      if (iterations > maxIterations) {
        println(s"[Warning] traceOutline exceeded max iterations ($maxIterations), aborting loop.")
        return None
      }
      iterations += 1

      if (visited.contains((i0, j0))) {
        println(s"[Warning] traceOutline detected loop revisit at vertex $i0, isect $j0, aborting.")
        return None
      }
      visited += ((i0, j0))

      val v = verts(i0)
      if (j0 == -1) {
        localUsed += i0
        out += v.xy
        if (v.isects.isEmpty) {
          i0 = (i0 + cw + verts.length) % verts.length
          j0 = -1
        } else {
          j0 = if (cw < 0) v.isects.size - 1 else 0
        }
      } else if (j0 >= v.isects.size) {
        i0 = (i0 + cw + verts.length) % verts.length
        j0 = -1
      } else {
        val is = v.isects(j0)
        localIsects += ((i0, j0))
        out += is.xy
        val (k, z) = isectMirror.getOrElse((i0, j0), (i0, j0))
        if (k == i0 && z == j0) {
          println(s"[Warning] isectMirror returned same vertex ($i0,$j0), aborting.")
          return None
        }
        localIsects += ((k, z))
        val forward = if (is.side * cw < 0) -cw else cw
        i0 = k
        j0 = z + (if (is.side * cw < 0) -1 else 1)
      }
    } while ((i0, j0) != zero)

    used ++= localUsed
    usedIsects ++= localIsects

    Some(out.toSeq)
  }

  private def fixSelfIntersections(poly: Seq[Pt], eps: Double): Seq[Pt] = {
    val result = mutable.ArrayBuffer[Pt]()
    val n = poly.length

    for (i <- 0 until n) {
      val a1 = poly(i)
      val a2 = poly((i + 1) % n)
      result += a1

      for (j <- 0 until n if j != i && (j + 1) % n != i && j != (i + 1) % n) {
        val b1 = poly(j)
        val b2 = poly((j + 1) % n)

        val intersections = segIntersect(a1.x, a1.y, a2.x, a2.y, b1.x, b1.y, b2.x, b2.y)
        intersections.foreach { isect =>
          val mid = isect.xy
          val bumped = Pt(mid.x + eps, mid.y + eps)
          if (!result.contains(bumped)) {
            result += bumped
          }
        }
      }
    }

    result.distinct.toSeq
  }

  private def mirrorIsects(verts: Seq[Vtx]): Map[(Int, Int), (Int, Int)] = {
    val map = mutable.Map.empty[(Int, Int), (Int, Int)]
    for {
      (v, i) <- verts.zipWithIndex
      (isx, j) <- v.isects.zipWithIndex
    } {
      val k = isx.other
      val z = verts(k).isects.indexWhere(_.other == i)
      if (z >= 0) {
        map((i, j)) = (k, z)
      }
    }
    map.toMap
  }

  def checkConcave(poly: Seq[Pt], idx: Int): Int = {
    val n = poly.length
    val a = poly((idx - 1 + n) % n)
    val b = poly(idx)
    val c = poly((idx + 1) % n)

    val dx1 = b.x - a.x
    val dy1 = b.y - a.y
    val dx2 = c.x - b.x
    val dy2 = c.y - b.y
    val cross = dx1 * dy2 - dy1 * dx2

    if (cross < 0) -1 else 1
  }

  def polyIsHole(hole: Seq[Pt], poly: Seq[Pt], verifyWinding: Boolean, eps: Double): Boolean = {
    val area = polygonArea(hole)
    val reversed = if (area > 0) hole.reverse else hole
    val concaveIdx = reversed.indices.find(i => checkConcave(reversed, i) < 0).getOrElse(-1)
    if (concaveIdx == -1) return false

    val a = reversed((concaveIdx - 1 + reversed.length) % reversed.length)
    val b = reversed(concaveIdx)
    val c = reversed((concaveIdx + 1) % reversed.length)
    val mx = (a.x + c.x) / 2
    val my = (a.y + c.y) / 2
    val dx = mx - b.x
    val dy = my - b.y
    val l = math.sqrt(dx * dx + dy * dy)
    val ux = b.x + dx / l * eps
    val uy = b.y + dy / l * eps

    val intersections = rayIntersections(ux, uy, mx, my, poly)
    val evenOddRule = intersections.length % 2 == 0

    if (!verifyWinding) return evenOddRule

    val winding = intersections.map(_.side).sum
    evenOddRule && winding == 0
  }

  def polygonArea(poly: Seq[Pt]): Double = {
    val n = poly.length
    var a = 0.0
    for (i <- 0 until n) {
      val j = (i + 1) % n
      a += poly(i).x * poly(j).y - poly(j).x * poly(i).y
    }
    a * 0.5
  }

  def rayIntersections(x0: Double, y0: Double, x1: Double, y1: Double, poly: Seq[Pt]): Seq[Isect] = {
    poly.indices.flatMap { i =>
      val a = poly(i)
      val b = poly((i + 1) % poly.size)
      segIntersect(x0, y0, x1, y1, a.x, a.y, b.x, b.y, isRay = true)
    }.toSeq.sortBy(_.t)
  }

  private def disturb(poly: Seq[Pt], disturbEps: Double): Seq[Pt] = {
    poly.zipWithIndex.map { case (pt, i) =>
      val angle = i * 0.61803398875 * 2 * Math.PI
      Pt(pt.x + disturbEps * Math.cos(angle), pt.y + disturbEps * Math.sin(angle))
    }
  }

  private def buildVertices(pts: Seq[Pt]): Seq[Vtx] = {
    val n = pts.length
    val mutableVerts = Array.tabulate(n)(i => mutable.ArrayBuffer.empty[Isect])

    for (i <- 0 until n) {
      val a1 = pts(i)
      val a2 = pts((i + 1) % n)

      for (j <- 0 until n if j != i && (j + 1) % n != i && j != (i + 1) % n) {
        val b1 = pts(j)
        val b2 = pts((j + 1) % n)

        val intersections = segIntersect(a1.x, a1.y, a2.x, a2.y, b1.x, b1.y, b2.x, b2.y)
        intersections.foreach { isect =>
          // We store 'other' as the vertex index opposite this intersection edge
          // Here simplification: assign other = j (the start vertex of other edge)
          mutableVerts(i) += isect.copy(other = j)
        }
      }
    }

    Array.tabulate(n)(i => Vtx(pts(i), mutableVerts(i).toSeq)).toSeq
  }

  // Placeholder stub - replace with your real segment intersection method
  // Returns sequence of intersections between segment (x0,y0)->(x1,y1) and (x2,y2)->(x3,y3)
  // If isRay = true, treats first segment as ray starting at (x0,y0) going through (x1,y1)
  def segIntersect(
                    x0: Double, y0: Double, x1: Double, y1: Double,
                    x2: Double, y2: Double, x3: Double, y3: Double,
                    isRay: Boolean = false
                  ): Seq[Isect] = {
    // Your real intersection logic needed here
    // For now, return empty
    Seq.empty
  }
}
