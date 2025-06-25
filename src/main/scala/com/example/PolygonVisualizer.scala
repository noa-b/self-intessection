package com.example

import com.example.Point

import javax.swing._
import java.awt._
import java.awt.geom._
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

case class Bounds(minX: Double, minY: Double, maxX: Double, maxY: Double) {
  def width: Double = maxX - minX
  def height: Double = maxY - minY
}

class PolygonVisualizer(vertices: Seq[Point], intersections: Seq[SelfIntersectionPoint]) extends JPanel {

  val padding = 50

  override def getPreferredSize: Dimension = new Dimension(800, 600)

  def getPolygonBounds(points: Seq[Point]): Bounds = {
    val xs = points.map(_.x)
    val ys = points.map(_.y)
    Bounds(xs.min, ys.min, xs.max, ys.max)
  }

  def transform(p: Point): java.awt.Point = {
    val bounds = getPolygonBounds(vertices)
    val scaleX = (getWidth - 2 * padding).toDouble / bounds.width
    val scaleY = (getHeight - 2 * padding).toDouble / bounds.height
    val scale = Math.min(scaleX, scaleY)

    val offsetX = (getWidth - bounds.width * scale) / 2 - bounds.minX * scale
    val offsetY = (getHeight - bounds.height * scale) / 2 - bounds.minY * scale

    val x = (p.x * scale + offsetX).toInt
    val y = (getHeight - (p.y * scale + offsetY)).toInt // Flip Y axis

    new java.awt.Point(x, y)
  }

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    // Draw polygon edges
    g2.setColor(Color.BLUE)
    for (i <- vertices.indices) {
      val p1 = transform(vertices(i))
      val p2 = transform(vertices((i + 1) % vertices.size))
      g2.drawLine(p1.x, p1.y, p2.x, p2.y)
    }

    // Draw intersection points and labels
    for ((pt, idx) <- intersections.zipWithIndex) {
      val p = transform(Point(pt._x, pt._y))
      g2.setColor(Color.RED)
      g2.fill(new Ellipse2D.Double(p.x - 4, p.y - 4, 8, 8))

      g2.setColor(new Color(128, 0, 0)) // dark red
      g2.drawString(s"I$idx", p.x + 5, p.y - 5)
    }

    // Draw vertex dots and labels
    g2.setColor(Color.BLACK)
    for ((p, idx) <- vertices.zipWithIndex) {
      val pt = transform(p)
      g2.fill(new Ellipse2D.Double(pt.x - 3, pt.y - 3, 6, 6))
      g2.drawString(s"$idx", pt.x + 5, pt.y + 12)
    }
  }
}

object PolygonVisualizer {

  def show(vertices: Seq[Point], intersections: Seq[SelfIntersectionPoint]): Unit = {
    val frame = new JFrame("Polygon with Self-Intersections")
    val panel = new PolygonVisualizer(vertices, intersections)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setSize(800, 600)
    frame.add(panel)
    frame.setVisible(true)
  }

  def saveImage(name: String, vertices: Seq[Point], intersections: Seq[SelfIntersectionPoint]): Unit = {
    val panel = new PolygonVisualizer(vertices, intersections)
    val width = panel.getPreferredSize.width
    val height = panel.getPreferredSize.height
    panel.setSize(width, height)
    panel.doLayout()

    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g2 = image.createGraphics()
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    panel.paintComponent(g2)
    g2.dispose()

    // Create visualization/testname directory
    val baseName = name.replaceAll("[^a-zA-Z0-9]+", "_").toLowerCase
    val baseDir = new File("visualization")
    val testDir = new File(baseDir, baseName)
    if (!testDir.exists()) testDir.mkdirs()

    // Scan for existing files and determine next number
    val pattern = s"${baseName}_(\\d{3})\\.png".r
    val existingFiles = testDir.listFiles().filter(_.getName.matches(pattern.regex))
    val usedNumbers = existingFiles.flatMap {
      case f if pattern.matches(f.getName) =>
        pattern.findFirstMatchIn(f.getName).map(_.group(1).toInt)
      case _ => None
    }

    val nextNumber = if (usedNumbers.isEmpty) 1 else usedNumbers.max + 1
    val filename = f"${baseName}_$nextNumber%03d.png"
    val file = new File(testDir, filename)

    ImageIO.write(image, "png", file)
    println(s"✅ Saved visualization to: ${file.getAbsolutePath}")
  }
}
