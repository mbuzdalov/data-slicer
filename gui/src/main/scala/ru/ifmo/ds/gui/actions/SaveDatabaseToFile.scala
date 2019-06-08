package ru.ifmo.ds.gui.actions

import java.awt.event.ActionListener
import java.awt.{BorderLayout, FlowLayout, GridLayout}
import java.io.{PrintWriter, StringWriter}

import javax.swing.{ButtonGroup, JButton, JDialog, JFileChooser, JOptionPane, JPanel, JRadioButton, JTextArea}

import ru.ifmo.ds.Database
import ru.ifmo.ds.gui.EntityContainer
import ru.ifmo.ds.gui.components.DatabaseSelector
import ru.ifmo.ds.gui.parts.DatabaseEntity
import ru.ifmo.ds.gui.util.ImageLoadingFacilities
import ru.ifmo.ds.io.TextInputOutput

class SaveDatabaseToFile(container: EntityContainer) extends EntityAction("Save database to a file", SaveDatabaseToFile.dbSaveToFileIcon) {
  private val saveFileChooser = new JFileChooser()
  saveFileChooser.setMultiSelectionEnabled(true)
  saveFileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY)

  override protected def performImpl(): Unit = {
    val formatChooser = new SaveDatabaseToFile.ChooseFormat(container.getEntitiesByClass(classOf[DatabaseEntity]),
                                                            container.dialogAlignmentSet,
                                                            TextInputOutput.availableSaveOptions)
    formatChooser.setVisible(true)
    formatChooser.getChosenFormat match {
      case None => // do nothing
      case Some(f) =>
        val chooserResult = saveFileChooser.showSaveDialog(container.root)
        if (chooserResult == JFileChooser.APPROVE_OPTION) {
          val file = saveFileChooser.getSelectedFile
          new Thread(() => {
            try {
              val db = Database.merge(formatChooser.getEntities.map(_.getDatabase): _*)
              f.writeToFile(db, file)
            } catch {
              case th: Throwable =>
                val string = new StringWriter()
                val stream = new PrintWriter(string)
                th.printStackTrace(stream)
                stream.close()
                JOptionPane.showMessageDialog(null, string.toString,
                                              "Error while saving!", JOptionPane.ERROR_MESSAGE)
            }
          }, "Database Writing Thread").start()
        }
    }
  }
}

object SaveDatabaseToFile extends ImageLoadingFacilities {
  private val dbSaveToFileIcon = imageFromResource("save-database-files.png")

  private class ChooseFormat(entities: Seq[DatabaseEntity],
                             alignmentSet: EntityContainer.DialogAlignmentInfo,
                             formats: Seq[TextInputOutput.SaveOption])
    extends JDialog(alignmentSet.frame, "Choose Format", true) {

    def getEntities: Seq[DatabaseEntity] = selector.getSelectedEntities
    def getChosenFormat: Option[TextInputOutput] = choice

    private val selector = new DatabaseSelector(entities)
    private var choice: Option[TextInputOutput] = None
    private val buttonGroup = new ButtonGroup
    private val midPane = new JPanel(new GridLayout(0, 2))

    private val buttons = for (f <- formats) yield {
      val button = new JRadioButton(f.name)
      midPane.add(button)
      val text = new JTextArea(f.description)
      text.setLineWrap(true)
      text.setColumns(80)
      midPane.add(text)
      buttonGroup.add(button)
      button
    }

    private val buttonListener: ActionListener = _ => {
      choice = formats.indices.find(i => buttons(i).isSelected).map(i => formats(i).io)
      updateOKButtonVisibility()
    }
    buttons.foreach(_.addActionListener(buttonListener))

    private val okCancelPane = new JPanel(new FlowLayout())
    private val okButton = new JButton("OK")
    private val cancelButton = new JButton("Cancel")
    okCancelPane.add(okButton)
    okCancelPane.add(cancelButton)

    private def updateOKButtonVisibility(): Unit = {
      okButton.setEnabled(choice.isDefined && selector.getSelectedEntities.nonEmpty)
    }

    selector.addSelectionListener(() => {
      updateOKButtonVisibility()
    })

    okButton.setEnabled(false)
    okButton.addActionListener(_ => {
      assert(choice.isDefined)
      setVisible(false)
    })
    cancelButton.addActionListener(_ => {
      choice = None
      setVisible(false)
    })

    setLayout(new BorderLayout())
    add(selector, BorderLayout.PAGE_START)
    add(midPane, BorderLayout.CENTER)
    add(okCancelPane, BorderLayout.PAGE_END)
    pack()
  }
}
