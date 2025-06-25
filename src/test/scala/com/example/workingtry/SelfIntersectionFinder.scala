package com.example.workingtry

object GeometryUtils {
  private def ccw(a: Point, b: Point, c: Point): Boolean = {
    (c.y - a.y) * (b.x - a.x) > (b.y - a.y) * (c.x - a.x)
  }

  def segmentsIntersect(s1: Segment, s2: Segment): Boolean = {
    val (a, b) = (s1.p1, s1.p2)
    val (c, d) = (s2.p1, s2.p2)
    ccw(a, c, d) != ccw(b, c, d) && ccw(a, b, c) != ccw(a, b, d)
  }

  def intersectionPoint(s1: Segment, s2: Segment): Option[Point] = {
    val (p, r) = (s1.p1, Point(s1.p2.x - s1.p1.x, s1.p2.y - s1.p1.y))
    val (q, s) = (s2.p1, Point(s2.p2.x - s2.p1.x, s2.p2.y - s2.p1.y))

    val rxs = r.x * s.y - r.y * s.x
    if (rxs == 0) return None

    val t = ((q.x - p.x) * s.y - (q.y - p.y) * s.x) / rxs
    val u = ((q.x - p.x) * r.y - (q.y - p.y) * r.x) / rxs

    if (t >= 0 && t <= 1 && u >= 0 && u <= 1) {
      Some(Point(p.x + t * r.x, p.y + t * r.y))
    } else {
      None
    }
  }
}

object SelfIntersectionFinder {

  import GeometryUtils._

  // Convert LineStringCoordinates (outer ring) to Seq[Point]
  private def convertToPoints(line: LineStringCoordinates): Seq[Point] = {
    line.collect {
      case Seq(x, y) => Point(x, y)
    }
  }

  // Check self-intersections for a single ring (polygon boundary)
  def findSelfIntersections(vertices: Seq[Point]): Seq[SelfIntersectionPoint] = {
    val n = vertices.length
    val intersections = scala.collection.mutable.ListBuffer[SelfIntersectionPoint]()

    for {
      i <- 0 until n
      j <- i + 1 until n
      if math.abs(i - j) > 1 && (i != 0 || j != n - 1)
    } {
      val s1 = Segment(vertices(i), vertices((i + 1) % n))
      val s2 = Segment(vertices(j), vertices((j + 1) % n))

      if (segmentsIntersect(s1, s2)) {
        val point = intersectionPoint(s1, s2)
        point.foreach { p =>
          intersections += SelfIntersectionPoint(
            _x = p.x,
            _y = p.y,
            _i = i,
            _j = j,
            _toiI = false,
            _tojI = false,
            _toi = -1,
            _toj = -1
          )
        }
      }
    }

    intersections.toList
  }

  // Top-level method: accepts a list of polygons (GeoJSON style)
  def findIntersectionsInPolygons(polygons: Seq[PolygonCoordinates]): Seq[SelfIntersectionPoint] = {
    polygons.flatMap { polygon =>
      val outerRing = polygon.headOption.getOrElse(Seq.empty)
      val points = convertToPoints(outerRing)
      findSelfIntersections(points)
    }
  }
}
