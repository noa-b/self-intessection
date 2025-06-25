package com.example.try1

import org.locationtech.jts.geom._

import scala.collection.mutable

object PolygonFixer {

  val geometryFactory = new GeometryFactory()

  def pointOnSegment(p: Coordinate, start: Coordinate, end: Coordinate, epsilon: Double = 1e-9): Boolean = {
    val cross = (p.y - start.y) * (end.x - start.x) - (p.x - start.x) * (end.y - start.y)
    if (Math.abs(cross) > epsilon) return false

    val dot = (p.x - start.x) * (end.x - start.x) + (p.y - start.y) * (end.y - start.y)
    if (dot < -epsilon) return false

    val squaredLength = (end.x - start.x) * (end.x - start.x) + (end.y - start.y) * (end.y - start.y)
    if (dot > squaredLength + epsilon) return false

    true
  }

  def findIntersections(coords: Seq[Coordinate]): Seq[(Coordinate, Int)] = {
    val intersections = mutable.ListBuffer[(Coordinate, Int)]()
    val n = coords.length - 1  // polygon is closed

    def nextIndex(idx: Int): Int = (idx + 1) % n

    // Check adjacency: returns true if edges share a vertex (adjacent)
    def edgesAreAdjacent(i: Int, j: Int): Boolean = {
      val ni = nextIndex(i)
      val nj = nextIndex(j)
      i == j || ni == j || i == nj
    }

    for {
      i <- 0 until n
      j <- i + 1 until n if !edgesAreAdjacent(i, j)
    } {
      val seg1 = geometryFactory.createLineString(Array(coords(i), coords(nextIndex(i))))
      val seg2 = geometryFactory.createLineString(Array(coords(j), coords(nextIndex(j))))
      val inter = seg1.intersection(seg2)

      if (!inter.isEmpty) {
        inter match {
          case p: Point =>
            val c = p.getCoordinate
            // Accept intersection points even if they coincide with polygon vertices,
            // as long as they belong to non-adjacent edges.
            if (pointOnSegment(c, coords(i), coords(nextIndex(i))) &&
              pointOnSegment(c, coords(j), coords(nextIndex(j)))) {
              intersections += ((c, i))
              intersections += ((c, j))
            }
          case gc: GeometryCollection =>
            for (k <- 0 until gc.getNumGeometries) {
              gc.getGeometryN(k) match {
                case pt: Point =>
                  val c = pt.getCoordinate
                  if (pointOnSegment(c, coords(i), coords(nextIndex(i))) &&
                    pointOnSegment(c, coords(j), coords(nextIndex(j)))) {
                    intersections += ((c, i))
                    intersections += ((c, j))
                  }
                case _ =>
              }
            }
          case _ =>
        }
      }
    }

    intersections.distinct.toSeq
  }

  def offsetPointNearVertex(vertex: Coordinate, offsetDist: Double = 1e-3): Coordinate = {
    // Create a small offset point near vertex (shift slightly on x and y)
    new Coordinate(vertex.x + offsetDist, vertex.y + offsetDist)
  }

  def addOffsetPoints(coords: Seq[Coordinate], intersections: Seq[(Coordinate, Int)], offset: Double = 1e-3): (Seq[Coordinate], Seq[Coordinate]) = {
    val updatedCoords = coords.toBuffer
    val addedPoints = mutable.ListBuffer[Coordinate]()

    // Process intersections sorted by edge index descending to avoid index shift issues
    val sortedIntersections = intersections.sortBy(-_._2)

    sortedIntersections.foreach { case (_, edgeIdx) =>
      val vertex = updatedCoords(edgeIdx)
      val newPoint = offsetPointNearVertex(vertex, offset)

      if (!updatedCoords.exists(c => c.distance(newPoint) < 1e-10)) {
        updatedCoords.insert(edgeIdx + 1, newPoint)
        addedPoints += newPoint
      }
    }

    // Ensure polygon closed
    if (updatedCoords.head != updatedCoords.last) {
      updatedCoords += updatedCoords.head
    }

    (updatedCoords.toSeq, addedPoints.toSeq)
  }

  def fixSelfIntersectingPolygon(coords: Seq[Coordinate]): (Seq[Coordinate], Seq[Coordinate]) = {
    if (coords.isEmpty) return (Seq.empty, Seq.empty)

    val closedCoords =
      if (coords.head == coords.last) coords
      else coords :+ coords.head

    val polygon = geometryFactory.createPolygon(closedCoords.toArray)

    if (polygon.isValid) {
      (closedCoords, Seq.empty)
    } else {
      val intersections = findIntersections(closedCoords)
      if (intersections.isEmpty) (closedCoords, Seq.empty)
      else addOffsetPoints(closedCoords, intersections)
    }
  }
}
