package ru.ifmo.ds.gui

import java.awt.{Color, Graphics2D, RenderingHints}
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

object PictureGenerator {
  private def makeRedCross(g: Graphics2D): Unit = {
    g.setColor(Color.RED)
    g.drawLine(2, 2, 27, 27)
    g.drawLine(3, 2, 27, 26)
    g.drawLine(2, 3, 26, 27)

    g.drawLine(2, 27, 27, 2)
    g.drawLine(3, 27, 27, 3)
    g.drawLine(2, 26, 26, 2)
  }

  private def makeReload(g: Graphics2D): Unit = {
    g.setColor(Color.BLUE)
    g.drawArc(3, 3, 24, 22, 60, -150)
    g.drawArc(3, 3, 24, 22, -120, -150)
    g.drawArc(4, 4, 22, 20, 60, -150)
    g.drawArc(4, 4, 22, 20, -120, -150)

    g.fillPolygon(Array(18, 9, 9), Array(5, 10, 0), 3)
    g.fillPolygon(Array(13, 20, 20), Array(24, 19, 29), 3)
  }

  private def makeChart(g: Graphics2D): Unit = {
    g.setColor(Color.GRAY)
    g.drawPolygon(Array(0, 31, 31, 0), Array(31, 31, 0, 0), 4)
    g.setColor(Color.BLUE)
    g.drawLine(2, 29, 2, 2)
    g.drawLine(2, 29, 29, 29)

    def f1(x: Int): Int = 14 - (math.sin(x / 23.0 * 2 * math.Pi) * 10).toInt
    def f2(x: Int): Int = 27 - (math.sin(x / 58.0 * 2 * math.Pi) * 24).toInt
    def draw(color: Color, f: Int => Int): Unit = {
      g.setColor(color)
      for (x <- 4 to 26) {
        val y1 = f(x - 4)
        val y2 = f(x - 3)
        g.drawLine(x, y1, x + 1, y2)
      }
    }

    draw(Color.RED, f1)
    draw(Color.GREEN, f2)
  }

  private def makeTable(g: Graphics2D): Unit = {
    g.setColor(Color.GRAY)
    g.drawPolygon(Array(0, 31, 31, 0), Array(31, 31, 0, 0), 4)
    g.setColor(Color.BLACK)
    for (y <- 2 to 29 by 3) {
      g.drawLine(3, y, 28, y)
    }
    for (x <- 3 to 28 by 5) {
      g.drawLine(x, 2, x, 29)
    }
  }

  private def makeChartTableV(g: Graphics2D): Unit = {
    g.setClip(0, 0, 16, 32)
    makeChart(g)
    g.setClip(16, 0, 16, 32)
    makeTable(g)
    g.setClip(0, 0, 32, 32)
    g.setColor(Color.GRAY)
    g.drawLine(16, 0, 16, 31)
  }

  private def makeChartTableH(g: Graphics2D): Unit = {
    g.setClip(0, 0, 32, 16)
    makeChart(g)
    g.setClip(0, 16, 32, 16)
    makeTable(g)
    g.setClip(0, 0, 32, 32)
    g.setColor(Color.GRAY)
    g.drawLine(0, 16, 31, 16)
  }

  private def makeDatabase(g: Graphics2D): Unit = {
    g.setColor(Color.GRAY)
    g.fillRect(4, 2, 24, 28)
    for (l <- 5 to 25 by 5) {
      g.setColor(if (l == 5) Color.RED else Color.BLUE)
      g.drawLine(6, l, 8, l)
      g.setColor(if (l == 5) Color.RED else Color.BLACK)
      for (x <- 10 to 22 by 4) {
        g.drawLine(x, l, x + 2, l)
      }
    }
  }

  private def makeFileInFolder(g: Graphics2D): Unit = {
    val verySlantedX = Array(2, 6, 30, 26)
    val verySlantedY = Array(12, 29, 29, 12)
    val slightlySlantedX = Array(4, 6, 30, 28)
    val slightlySlantedY = Array(7, 29, 29, 7)

    g.setColor(Color.YELLOW)
    g.fillRect(6, 2, 24, 25)
    g.setColor(Color.BLACK)
    g.drawRect(6, 2, 24, 25)
    g.setColor(Color.WHITE)
    g.fillPolygon(slightlySlantedX, slightlySlantedY, 4)
    g.setColor(Color.BLACK)
    g.drawPolygon(slightlySlantedX, slightlySlantedY, 4)
    g.setColor(Color.YELLOW)
    g.fillPolygon(verySlantedX, verySlantedY, 4)
    g.setColor(Color.BLACK)
    g.drawPolygon(verySlantedX, verySlantedY, 4)
  }

  private def makeArrow(g: Graphics2D): Unit = {
    val x = Array(0, 4, 4, 9, 4, 4, 0)
    val y = Array(14, 14, 11, 16, 21, 18, 18)
    g.setColor(Color.BLUE)
    g.fillPolygon(x, y, x.length)
    g.setColor(Color.BLACK)
    g.drawPolygon(x, y, x.length)
  }

  private def makeOpenDatabaseFiles(g: Graphics2D): Unit = {
    val left = g.create(0, 0, 32, 32).asInstanceOf[Graphics2D]
    makeFileInFolder(left)
    val right = g.create(40, 0, 32, 32).asInstanceOf[Graphics2D]
    makeDatabase(right)
    val arrow = g.create(32, 0, 10, 32).asInstanceOf[Graphics2D]
    makeArrow(arrow)
  }

  private def makeCreateChart(g: Graphics2D): Unit = {
    val left = g.create(0, 0, 32, 32).asInstanceOf[Graphics2D]
    makeDatabase(left)
    val right = g.create(40, 0, 32, 32).asInstanceOf[Graphics2D]
    makeChart(right)
    val arrow = g.create(30, 0, 10, 32).asInstanceOf[Graphics2D]
    makeArrow(arrow)
  }

  private def write(dir: File, name: String, width: Int, height: Int)(fun: Graphics2D => Any): Unit = {
    val image = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
    val graphics = image.createGraphics()
    graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_LCD_HBGR)
    graphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
    graphics.setRenderingHint(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY)
    graphics.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY)
    graphics.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE)
    fun(graphics)
    ImageIO.write(image, "PNG", new File(dir, name))
  }

  def main(args: Array[String]): Unit = {
    val root = new File("gui/src/main/resources/ru/ifmo/ds/gui")
    if (!root.isDirectory) sys.error(s"The target of picture generation, $root, is not a directory")
    if (!root.canWrite) sys.error(s"The target of picture generation, $root, is not writable")

    println("Generating to " + root)
    write(root, "remove.png", 30, 30)(makeRedCross)
    write(root, "reload.png", 30, 30)(makeReload)

    write(root, "parts/chart.png", 32, 32)(makeChart)
    write(root, "parts/table.png", 32, 32)(makeTable)
    write(root, "parts/chart-table-v.png", 32, 32)(makeChartTableV)
    write(root, "parts/chart-table-h.png", 32, 32)(makeChartTableH)
    write(root, "parts/database.png", 32, 32)(makeDatabase)

    write(root, "actions/open-database-files.png", 72, 32)(makeOpenDatabaseFiles)
    write(root, "actions/create-chart.png", 72, 32)(makeCreateChart)
  }
}
