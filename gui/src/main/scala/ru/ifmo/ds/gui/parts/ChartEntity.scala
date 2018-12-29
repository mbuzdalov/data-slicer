package ru.ifmo.ds.gui.parts

import javax.imageio.ImageIO
import javax.swing.ImageIcon

object ChartEntity {
  private val chartIcon = new ImageIcon(ImageIO.read(getClass.getResource("chart.png")))
}
