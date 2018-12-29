package ru.ifmo.ds.gui.actions

import javax.imageio.ImageIO
import javax.swing.{ImageIcon, JFileChooser}
import ru.ifmo.ds.gui.EntityContainer
import ru.ifmo.ds.gui.parts.DatabaseEntity

class OpenDatabaseFiles(container: EntityContainer) extends EntityAction("Open database files", OpenDatabaseFiles.dbOpenFilesIcon) {
  private val openFileChooser = new JFileChooser()
  openFileChooser.setMultiSelectionEnabled(true)
  openFileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY)

  override protected def performImpl(): Unit = {
    val chooserResult = openFileChooser.showOpenDialog(container.root)
    if (chooserResult == JFileChooser.APPROVE_OPTION) {
      val files = openFileChooser.getSelectedFiles
      container.add(DatabaseEntity.fromFiles(container, files.toIndexedSeq, "filename"))
    }
  }
}

object OpenDatabaseFiles {
  val dbOpenFilesIcon = new ImageIcon(ImageIO.read(getClass.getResource("open-database-files.png")))
}
