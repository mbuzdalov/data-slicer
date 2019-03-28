package ru.ifmo.ds.gui.util

import javax.imageio.ImageIO
import javax.swing.ImageIcon

trait ImageLoadingFacilities {
  protected def imageFromResource(resourceName: String): ImageIcon = {
    new ImageIcon(ImageIO.read(getClass.getResource(resourceName)))
  }
}
