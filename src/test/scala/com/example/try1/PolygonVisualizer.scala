package com.example.try1

import java.awt._
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.swing._
import org.locationtech.jts.geom._

object PolygonVisualizer {

  class PolygonPanel(
                      val title: String,
                      val polygonCoords: Seq[Coordinate],
                      val addedPoints: Seq[Coordinate],
                      val drawPolygon: Boolean,
                      val canvasWidth: Int,
                      val canvasHeight: Int
                    ) extends JPanel {

    val padding = 40
    val allCoords = polygonCoords ++ addedPoints
    val xs = allCoords.map(_.x)
    val ys = allCoords.map(_.y)
    val minX = xs.min
    val maxX = xs.max
    val minY = ys.min
    val maxY = ys.max

    val panelWidth = canvasWidth
    val panelHeight = canvasHeight

    val scaleX = (panelWidth - 2 * padding) / (maxX - minX)
    val scaleY = (panelHeight - 2 * padding) / (maxY - minY)
    val scale = Math.min(scaleX, scaleY)

    def toPanelPoint(c: Coordinate): java.awt.Point = {
      val x = padding + (c.x - minX) * scale
      val y = panelHeight - padding - (c.y - minY) * scale
      new java.awt.Point(x.toInt, y.toInt)
    }

    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      val g2 = g.asInstanceOf[Graphics2D]

      g2.setColor(Color.white)
      g2.fillRect(0, 0, panelWidth, panelHeight)

      if (drawPolygon && polygonCoords.nonEmpty) {
        val pts = polygonCoords.map(toPanelPoint)
        val xs = pts.map(_.x).toArray
        val ys = pts.map(_.y).toArray

        g2.setColor(new Color(0, 128, 0, 128))
        g2.fillPolygon(xs, ys, pts.length)

        g2.setColor(Color.darkGray)
        g2.setStroke(new BasicStroke(2))
        g2.drawPolygon(xs, ys, pts.length)

        g2.setColor(Color.black)
        pts.foreach { p =>
          g2.fillOval(p.x - 3, p.y - 3, 6, 6)
        }
      }

      if (addedPoints.nonEmpty) {
        g2.setColor(Color.blue)
        addedPoints.foreach { p =>
          val pt = toPanelPoint(p)
          g2.fillOval(pt.x - 4, pt.y - 4, 8, 8)
        }
      }

      g2.setColor(Color.black)
      g2.drawString(title, 10, 20)
    }
  }

  private def saveImage(img: BufferedImage, saveDir: String, filenameBase: String, index: Int): Unit = {
    val dir = new File(saveDir)
    if (!dir.exists()) {
      val created = dir.mkdirs()
      if (created) println(s"Created directory $saveDir")
      else println(s"Failed to create directory $saveDir")
    }

    val filename = f"$saveDir/$filenameBase$index%03d.png"
    ImageIO.write(img, "png", new File(filename))
    println(s"Saved image to $filename")
  }


  /** Shows a window with 3 side-by-side panels comparing:
   *  - original polygon (red fill)
   *  - added points only (blue dots)
   *  - fixed polygon (green fill)
   *
   *  Also saves a screenshot of the frame to the given directory with incremental filenames.
   */
  def showComparison(
                      original: Seq[Coordinate],
                      fixed: Seq[Coordinate],
                      added: Seq[Coordinate],
                      title: String,
                      saveDir: String = "visualizations"
                    ): Unit = {
    SwingUtilities.invokeLater(() => {
      val frame = new JFrame(title)
      frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
      frame.setSize(1200, 440)
      frame.setLayout(new GridLayout(1, 3))

      val initialPanel = new PolygonPanel(
        title = "Original Polygon (Red)",
        polygonCoords = original,
        addedPoints = Seq.empty,
        drawPolygon = true,
        canvasWidth = 400,
        canvasHeight = 400
      ) {
        override def paintComponent(g: Graphics): Unit = {
          super.paintComponent(g)
          val g2 = g.asInstanceOf[Graphics2D]
          if (this.polygonCoords.nonEmpty) {
            val pts = this.polygonCoords.map(toPanelPoint)
            val xs = pts.map(_.x).toArray
            val ys = pts.map(_.y).toArray
            g2.setColor(new Color(255, 0, 0, 128))
            g2.fillPolygon(xs, ys, pts.length)
            g2.setColor(Color.darkGray)
            g2.setStroke(new BasicStroke(2))
            g2.drawPolygon(xs, ys, pts.length)
            g2.setColor(Color.black)
            pts.foreach(p => g2.fillOval(p.x - 3, p.y - 3, 6, 6))
          }
        }
      }

      val addedPointsPanel = new PolygonPanel(
        title = "Added Offset Points (Blue)",
        polygonCoords = Seq.empty,
        addedPoints = added,
        drawPolygon = false,
        canvasWidth = 400,
        canvasHeight = 400
      )

      val fixedPanel = new PolygonPanel(
        title = "Fixed Polygon (Green)",
        polygonCoords = fixed,
        addedPoints = Seq.empty,
        drawPolygon = true,
        canvasWidth = 400,
        canvasHeight = 400
      )

      frame.add(initialPanel)
      frame.add(addedPointsPanel)
      frame.add(fixedPanel)
      frame.setVisible(true)

      // Give frame some time to paint then save screenshot
      new Thread(() => {
        try {
          Thread.sleep(500) // Wait to ensure panel is rendered
          val img = new BufferedImage(frame.getWidth, frame.getHeight, BufferedImage.TYPE_INT_ARGB)
          val graphics = img.createGraphics()
          frame.paint(graphics)
          graphics.dispose()

          // Save images with incremental index
          val saveIndex = PolygonVisualizer.getNextSaveIndex(saveDir)
          saveImage(img, saveDir, "polygon_fix_", saveIndex)
        } catch {
          case e: Exception => e.printStackTrace()
        }
      }).start()
    })
  }

  private def getNextSaveIndex(dir: String): Int = {
    val d = new File(dir)
    if (!d.exists() || !d.isDirectory) return 0
    val files = d.listFiles()
    if (files == null || files.isEmpty) return 0
    val indices = files.flatMap { f =>
      val name = f.getName
      val pattern = """polygon_fix_(\d+)\.png""".r
      name match {
        case pattern(num) => Some(num.toInt)
        case _ => None
      }
    }
    if (indices.isEmpty) 0 else indices.max + 1
  }
}
