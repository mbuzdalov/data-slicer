package ru.ifmo.ds.gui.components

import javax.swing.{JCheckBox, JLabel, JPanel}

import scala.collection.mutable.ArrayBuffer

import ru.ifmo.ds.Database
import ru.ifmo.ds.gui.parts.DatabaseEntity
import ru.ifmo.ds.gui.util.VerticalFlowLayout

class DatabaseSelector(entities: Seq[DatabaseEntity]) extends JPanel {
  private[this] var database = Database()
  private[this] val entitySelectors = entities.map(e => new JCheckBox(e.getName))
  private[this] val listeners = new ArrayBuffer[DatabaseSelector.SelectionListener]()

  setLayout(new VerticalFlowLayout)
  add(new JLabel("Choose databases"))
  entitySelectors.foreach(add)
  entitySelectors.foreach(_.addActionListener(_ => updateDatabaseBySelection()))

  def getDatabase: Database = database
  def getSelectedEntities: Seq[DatabaseEntity] = getSelectedIndices.map(entities)

  def addSelectionListener(listener: DatabaseSelector.SelectionListener): Unit = {
    listeners += listener
  }

  private def getSelectedIndices: Seq[Int] = entities.indices.filter(i => entitySelectors(i).isSelected)
  private def updateDatabaseBySelection(): Unit = {
    database = Database.merge(getSelectedIndices.map(i => entities(i).getDatabase) :_*)
    listeners.foreach(_.selectionChanged())
  }
}

object DatabaseSelector {
  trait SelectionListener {
    def selectionChanged(): Unit
  }
}
