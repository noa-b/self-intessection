package Geometry

case class Pt(x: Double, y: Double) {
  def xy: Pt = this
}

case class Isect(xy: Pt, other: Int, side: Int, t: Double)

case class Vtx(xy: Pt, isects: Seq[Isect])

object Geometry {

  // Segment intersection function:
  // Returns Seq[Isect], empty or with intersection points.
  // isRay means first segment is infinite in one direction.
  def segIntersect(
                    x1: Double, y1: Double,
                    x2: Double, y2: Double,
                    x3: Double, y3: Double,
                    x4: Double, y4: Double,
                    isRay: Boolean = false
                  ): Seq[Isect] = {

    val dx1 = x2 - x1
    val dy1 = y2 - y1
    val dx2 = x4 - x3
    val dy2 = y4 - y3

    val denom = dx1 * dy2 - dy1 * dx2

    if (math.abs(denom) < 1e-14) {
      return Seq.empty
    }

    val dx3 = x3 - x1
    val dy3 = y3 - y1

    val t = (dx3 * dy2 - dy3 * dx2) / denom
    val u = (dx3 * dy1 - dy3 * dx1) / denom

    val tInRange = t >= 0 && t <= 1
    val uInRange = if (isRay) u >= 0 else u >= 0 && u <= 1

    if (tInRange && uInRange) {
      val ix = x1 + t * dx1
      val iy = y1 + t * dy1
      val side = 1 // or calculate side if needed
      Seq(Isect(Pt(ix, iy), other = -1, side = side, t = t))
    } else {
      Seq.empty
    }
  }
}
