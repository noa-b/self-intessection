package com.example

import Geometry.Pt

object AsciiPolygonDrawer {
  private val gridSize = 30

  private def toGrid(p: Pt, minX: Double, maxX: Double, minY: Double, maxY: Double): (Int, Int) = {
    val width = maxX - minX
    val height = maxY - minY
    val gx = (((p.x - minX) / (if (width == 0) 1 else width)) * (gridSize - 1)).round.toInt
    val gy = (((p.y - minY) / (if (height == 0) 1 else height)) * (gridSize - 1)).round.toInt
    (gx.max(0).min(gridSize - 1), gy.max(0).min(gridSize - 1))
  }

  def drawPolygon(poly: Seq[Pt], showIndices: Boolean = false): Unit = {
    if (poly.isEmpty) {
      println("  [Empty polygon]")
      return
    }

    val (grid, minX, maxX, minY, maxY, pointsGrid) = renderToGrid(poly)

    // Draw indices if requested
    if (showIndices) {
      pointsGrid.zipWithIndex.foreach { case ((x, y), idx) =>
        val label = idx.toString
        if (x + label.length < gridSize) {
          for ((ch, offset) <- label.zipWithIndex) {
            grid(y)(x + 1 + offset) = ch
          }
        }
      }
    }

    printGrid(grid, minX, maxX, minY, maxY)
  }

  def drawPolygonComparison(before: Seq[Pt], after: Seq[Pt]): Unit = {
    if (before.isEmpty && after.isEmpty) {
      println("  [Both polygons empty]")
      return
    }

    val xs = (before ++ after).map(_.x)
    val ys = (before ++ after).map(_.y)
    val minX = xs.min
    val maxX = xs.max
    val minY = ys.min
    val maxY = ys.max

    def toCellSet(poly: Seq[Pt]): Set[(Int, Int)] = poly.map(p => toGrid(p, minX, maxX, minY, maxY)).toSet

    val beforeSet = toCellSet(before)
    val afterSet = toCellSet(after)

    val unchanged = beforeSet intersect afterSet
    val removed   = beforeSet diff afterSet
    val added     = afterSet diff beforeSet

    val grid = Array.fill(gridSize, gridSize)(' ')

    for ((x, y) <- unchanged) grid(y)(x) = '*'
    for ((x, y) <- removed)   grid(y)(x) = '-'
    for ((x, y) <- added)     grid(y)(x) = '+'

    println("Grid legend: '*' = unchanged, '-' = removed, '+' = added")
    printGrid(grid, minX, maxX, minY, maxY)
  }

  private def renderToGrid(poly: Seq[Pt]): (Array[Array[Char]], Double, Double, Double, Double, Seq[(Int, Int)]) = {
    val xs = poly.map(_.x)
    val ys = poly.map(_.y)
    val minX = xs.min
    val maxX = xs.max
    val minY = ys.min
    val maxY = ys.max
    val width = maxX - minX
    val height = maxY - minY

    val grid = Array.fill(gridSize, gridSize)(' ')
    def toGridLocal(p: Pt): (Int, Int) = toGrid(p, minX, maxX, minY, maxY)
    val pointsGrid = poly.map(toGridLocal)

    def drawLine(x0: Int, y0: Int, x1: Int, y1: Int): Unit = {
      var x = x0
      var y = y0
      val dx = math.abs(x1 - x0)
      val dy = math.abs(y1 - y0)
      val sx = if (x0 < x1) 1 else -1
      val sy = if (y0 < y1) 1 else -1
      var err = dx - dy
      val maxSteps = gridSize * gridSize * 2
      var steps = 0

      while (steps < maxSteps) {
        if (grid(y)(x) == ' ') grid(y)(x) = '+'
        if (x == x1 && y == y1) return
        val e2 = 2 * err
        if (e2 > -dy) { err -= dy; x += sx }
        if (e2 < dx) { err += dx; y += sy }
        steps += 1
      }
    }

    // Draw lines
    for (i <- pointsGrid.indices) {
      val (x0, y0) = pointsGrid(i)
      val (x1, y1) = pointsGrid((i + 1) % pointsGrid.length)
      drawLine(x0, y0, x1, y1)
    }

    // Mark vertices
    val vertexCounts = pointsGrid.groupBy(identity).view.mapValues(_.size).toMap
    pointsGrid.foreach { case (x, y) =>
      grid(y)(x) = if (vertexCounts((x, y)) > 1) 'X' else '*'
    }

    (grid, minX, maxX, minY, maxY, pointsGrid)
  }

  private def printGrid(grid: Array[Array[Char]], minX: Double, maxX: Double, minY: Double, maxY: Double): Unit = {
    println(f"Grid x:[${minX}%.2f to ${maxX}%.2f], y:[${minY}%.2f to ${maxY}%.2f]")
    println("    " + (0 until gridSize by 5).map(i => f"$i%2d  ").mkString + " → x")
    for (y <- (0 until gridSize).reverse) {
      val label = f"$y%2d |"
      val row = grid(y).mkString("")
      println(s"$label$row|")
    }
    println("    +" + "-" * gridSize + "+")
  }

  def printPolygon(label: String, poly: Seq[Pt], showIndices: Boolean = false): Unit = {
    val ptsStr = poly.map(p => f"(${p.x}%.3f, ${p.y}%.3f)").mkString(", ")
    println(s"$label: [$ptsStr]")
    drawPolygon(poly, showIndices)
  }

  def printPolygonChanges(before: Seq[Pt], after: Seq[Pt]): Unit = {
    println("[Polygon Changes Visualization]")
    drawPolygonComparison(before, after)
  }
}
