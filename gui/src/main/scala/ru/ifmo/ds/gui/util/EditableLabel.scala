package ru.ifmo.ds.gui.util

import java.awt.CardLayout
import java.awt.event.{MouseAdapter, MouseEvent, MouseListener}

import javax.swing._

class EditableLabel(initialText: String) extends JComponent { self =>
  private val namePanelLayout = new CardLayout()
  private val displayLabel = "Panel"
  private val displayEditor = "Editor"
  private val nameLabel = new JLabel(initialText)
  private val nameEditor = new JTextField()

  def name: String = nameLabel.getText

  setLayout(namePanelLayout)
  add(nameLabel, displayLabel)
  add(nameEditor, displayEditor)

  nameLabel.setHorizontalAlignment(SwingConstants.CENTER)
  nameLabel.setVerticalAlignment(SwingConstants.CENTER)
  nameLabel.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      if (e.getClickCount > 1) {
        nameEditor.setText(nameLabel.getText)
        namePanelLayout.show(self, displayEditor)
        nameEditor.grabFocus()
      }
    }
  })

  nameEditor.addActionListener(_ => {
    nameEditor.transferFocus()
    val oldName = nameLabel.getText
    val newName = nameEditor.getText
    if (oldName != newName) {
      nameLabel.setText(newName)
      firePropertyChange(EditableLabel.NameProperty, oldName, newName)
    }
    namePanelLayout.show(self, displayLabel)
  })

  override def addMouseListener(l: MouseListener): Unit = {
    super.addMouseListener(l)
    nameLabel.addMouseListener(l)
  }

  override def removeMouseListener(l: MouseListener): Unit = {
    super.removeMouseListener(l)
    nameLabel.removeMouseListener(l)
  }
}

object EditableLabel {
  val NameProperty = "Name"
}
