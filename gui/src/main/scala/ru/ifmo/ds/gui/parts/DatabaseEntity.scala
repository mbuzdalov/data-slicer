package ru.ifmo.ds.gui.parts

import java.io.File

import javax.swing._
import javax.swing.event.{TableModelEvent, TableModelListener}
import javax.swing.table.TableModel

import scala.collection.mutable.ArrayBuffer

import ru.ifmo.ds.Database
import ru.ifmo.ds.gui.{DisplayedEntity, EntityContainer}
import ru.ifmo.ds.io.TextInputOutput
import ru.ifmo.ds.util.OrderingForStringWithNumbers

class DatabaseEntity(parentEntities: Seq[DatabaseEntity], container: EntityContainer,
                     initialName: String, constructionProcedure: Seq[Database] => Database)
  extends DisplayedEntity(parentEntities, container, DisplayedEntity.dbIcon, initialName) {

  private var database: Option[Database] = None
  private var table: DatabaseEntity.MyTableModel = _

  override protected def makeMainUI(): JComponent = {
    val jTable = new JTable()
    if (table == null) {
      table = new DatabaseEntity.MyTableModel(jTable)
    }
    jTable.setModel(table)
    new JScrollPane(jTable)
  }

  override def derive(newEntities: Seq[DisplayedEntity]): DisplayedEntity = {
    val newParents = newEntities.map(_.asInstanceOf[DatabaseEntity])
    new DatabaseEntity(newParents, container, getName, constructionProcedure)
  }

  override protected def runEvaluation(): Unit = {
    val db = constructionProcedure(parentEntities.map(_.database.get))
    database = Some(db)
    val keysValues = db.possibleKeys.map(k => k -> db.valuesUnderKey(k)).toIndexedSeq.sortBy(_._1)
    val (singletons, others) = keysValues.partition(_._2.size == 1)
    table.setData((singletons ++ others).map(p => (p._1, p._2.map(_.getOrElse("<none>")).toIndexedSeq.sorted(OrderingForStringWithNumbers.NoSpecialDotTreatment))))
  }
}

object DatabaseEntity {
  def fromFile(container: EntityContainer, file: File, moreKeys: Map[String, String] = Map.empty): DatabaseEntity = {
    new DatabaseEntity(Seq.empty, container, file.getName, _ => TextInputOutput.fromFile(file, moreKeys))
  }
  def merge(container: EntityContainer, sources: Seq[DatabaseEntity], name: String): DatabaseEntity = {
    new DatabaseEntity(sources, container, name, Database.merge)
  }

  private def alignTable(table: JTable): Unit = {
    val colModel = table.getColumnModel
    val spacing = table.getIntercellSpacing.width + 5
    for (columnIndex <- 0 until table.getColumnCount) {
      val column = colModel.getColumn(columnIndex)
      val renderer0 = column.getHeaderRenderer
      val renderer = if (renderer0 == null) table.getTableHeader.getDefaultRenderer else renderer0
      var width = renderer
        .getTableCellRendererComponent(table, column.getHeaderValue, false, false, -1, columnIndex)
        .getPreferredSize
        .width
      for (rowIndex <- 0 until table.getRowCount) {
        val rowWidth = table
          .prepareRenderer(table.getCellRenderer(rowIndex, columnIndex), rowIndex, columnIndex)
          .getPreferredSize
          .width
        width = math.max(width, rowWidth)
      }
      width += spacing
      if (width < column.getMaxWidth) {
        table.getTableHeader.setResizingColumn(column)
        column.setWidth(width)
      }
    }
  }

  private class MyTableModel(theTable: JTable) extends TableModel {
    private var data: Seq[(String, Seq[String])] = Seq.empty
    private val listeners = new ArrayBuffer[TableModelListener]
    private val columnNames = Seq("Key", "Value count", "Values")

    def setData(data: Seq[(String, Seq[String])]): Unit = synchronized {
      this.data = data

      val listCopy = listeners.synchronized(IndexedSeq(listeners :_*))
      val ev = new TableModelEvent(this, 0, data.size)
      SwingUtilities.invokeLater(() => {
        listCopy.foreach(_.tableChanged(ev))
        alignTable(theTable)
      })
    }

    override def getRowCount: Int = synchronized(data.size)
    override def getColumnCount: Int = 3
    override def getColumnName(columnIndex: Int): String = columnNames(columnIndex)
    override def getColumnClass(columnIndex: Int): Class[_] = classOf[String]
    override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = false
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = synchronized{
      columnIndex match {
        case 0 => data(rowIndex)._1
        case 1 => data(rowIndex)._2.size.toString
        case 2 => data(rowIndex)._2.mkString(", ")
      }
    }
    override def setValueAt(aValue: Any, rowIndex: Int, columnIndex: Int): Unit = throw new UnsupportedOperationException("This table is unmodifiable")
    override def addTableModelListener(l: TableModelListener): Unit = listeners.synchronized(listeners += l)
    override def removeTableModelListener(l: TableModelListener): Unit = listeners.synchronized(listeners -= l)
  }
}
