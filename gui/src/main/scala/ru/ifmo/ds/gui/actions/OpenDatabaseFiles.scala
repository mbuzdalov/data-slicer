package ru.ifmo.ds.gui.actions

import javax.swing.JFileChooser
import ru.ifmo.ds.gui.EntityContainer
import ru.ifmo.ds.gui.parts.DatabaseEntity
import ru.ifmo.ds.gui.util.ImageLoadingFacilities

class OpenDatabaseFiles(container: EntityContainer) extends EntityAction("Open database files", OpenDatabaseFiles.dbOpenFilesIcon) {
  private val openFileChooser = new JFileChooser()
  openFileChooser.setMultiSelectionEnabled(true)
  openFileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY)

  override protected def performImpl(): Unit = {
    val chooserResult = openFileChooser.showOpenDialog(container.root)
    if (chooserResult == JFileChooser.APPROVE_OPTION) {
      val files = openFileChooser.getSelectedFiles
      // will add itself to the container
      DatabaseEntity.fromFiles(container, files.toIndexedSeq, "filename")
    }
  }
}

object OpenDatabaseFiles extends ImageLoadingFacilities {
  private val dbOpenFilesIcon = imageFromResource("open-database-files.png")
}
