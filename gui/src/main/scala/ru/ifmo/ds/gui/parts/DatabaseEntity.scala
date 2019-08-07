package ru.ifmo.ds.gui.parts

import java.io.File

import javax.swing._
import javax.swing.event.{TableModelEvent, TableModelListener}
import javax.swing.table.TableModel

import scala.annotation.tailrec

import ru.ifmo.ds.Database
import ru.ifmo.ds.gui.util.{ImageLoadingFacilities, TableUtils}
import ru.ifmo.ds.gui.{DisplayedEntity, EntityContainer}
import ru.ifmo.ds.io.TextInputOutput
import ru.ifmo.ds.util.OrderingForStringWithNumbers
import scala.collection.mutable.ArrayBuffer

class DatabaseEntity(parentEntities: Seq[DatabaseEntity], container: EntityContainer,
                     initialName: String, constructionProcedure: Seq[Database] => Database)
  extends DisplayedEntity(parentEntities, container, DatabaseEntity.dbIcon, initialName) {

  private[this] var database: Option[Database] = None
  private[this] val table = new JTable()
  private[this] val model = new DatabaseEntity.MyTableModel(table)

  table.setModel(model)
  theUI.add(new JScrollPane(table))

  def getDatabase: Database = database.get

  override def derive(newEntities: Seq[DisplayedEntity]): DisplayedEntity = {
    val newParents = newEntities.map(_.asInstanceOf[DatabaseEntity])
    new DatabaseEntity(newParents, container, getName, constructionProcedure)
  }

  override protected def runEvaluation(): Unit = {
    def toDisplayable(value: Option[String]) = value match {
      case None => "<none>"
      case Some(null) => "<null>"
      case Some(s) => if (s.length > 100) s.take(100) + "..." else s
    }

    val db = constructionProcedure(parentEntities.map(_.getDatabase))
    database = Some(db)
    val keysValues = db.possibleKeys.map(k => k -> db.valuesUnderKey(k)).toIndexedSeq.sortBy(_._1)
    val (singletons, others) = keysValues.partition(_._2.size == 1)
    model.setData((singletons ++ others).map(p => (p._1, p._2.map(toDisplayable).toIndexedSeq.sorted(OrderingForStringWithNumbers.NoSpecialDotTreatment))))
  }
}

object DatabaseEntity extends ImageLoadingFacilities {
  private val dbIcon = imageFromResource("database.png")

  def fromFile(container: EntityContainer, file: File, moreKeys: Map[String, String] = Map.empty): DatabaseEntity = {
    new DatabaseEntity(Seq.empty, container, file.getName, _ => TextInputOutput.fromFile(file, moreKeys))
  }
  def fromFiles(container: EntityContainer, files: Seq[File], filenameKey: String, moreKeys: Map[String, String] = Map.empty): DatabaseEntity = {
    var commonName: String = null
    for (f <- files) {
      val name = f.getAbsolutePath
      if (commonName == null) {
        commonName = name
      } else {
        var idx = 0
        while (idx < commonName.length && idx < name.length && commonName(idx) == name(idx)) {
          idx += 1
        }
        commonName = commonName.substring(0, idx)
      }
    }
    if (commonName == null) {
      commonName = "???"
    } else if (commonName.isEmpty) {
      commonName = "<merged>"
    } else {
      try {
        val f = new File(commonName)
        val pf = f.getParentFile
        if (pf != null) {
          val pff = pf.getParentFile
          commonName = (if (pff == null) "" else pff.getName) + "/" +
            pf.getName + "/" + f.getName + (if (f.exists()) "" else "*")
        }
      } catch {
        case th: Throwable =>
          th.printStackTrace()
      }
    }

    new DatabaseEntity(Seq.empty, container, commonName, _ => {
      Database.merge(files.map(f => TextInputOutput.fromFile(f, moreKeys + (filenameKey -> f.getName))): _*)
    })
  }
  def merge(container: EntityContainer, sources: Seq[DatabaseEntity], name: String): DatabaseEntity = {
    new DatabaseEntity(sources, container, name, Database.merge)
  }

  private class MyTableModel(theTable: JTable) extends TableModel {
    private var data: Seq[(String, Seq[String], String)] = Seq.empty
    private val listeners = new ArrayBuffer[TableModelListener]
    private val columnNames = Seq("Key", "Value count", "Values")

    def setData(data: Seq[(String, Seq[String])]): Unit = synchronized {
      this.data = data.map(p => (p._1, p._2, generateDisplayableString(p._2)))

      val listCopy = listeners.synchronized(listeners.toIndexedSeq)
      val ev = new TableModelEvent(this, 0, data.size)
      SwingUtilities.invokeLater(() => {
        listCopy.foreach(_.tableChanged(ev))
        TableUtils.alignTable(theTable)
      })
    }

    @tailrec
    private def generateDisplayableStringImpl(data: Seq[String], builder: StringBuilder, index: Int): String = {
      if (index == data.size) builder.result() else {
        if (index > 0) {
          builder.append(", ")
        }
        builder.append(data(index))
        if (builder.length > 500) {
          builder.setLength(500)
          builder.append("...")
          builder.result()
        } else {
          generateDisplayableStringImpl(data, builder, index + 1)
        }
      }
    }

    private def generateDisplayableString(data: Seq[String]) = generateDisplayableStringImpl(data, new StringBuilder, 0)

    override def getRowCount: Int = synchronized(data.size)
    override def getColumnCount: Int = 3
    override def getColumnName(columnIndex: Int): String = columnNames(columnIndex)
    override def getColumnClass(columnIndex: Int): Class[_] = classOf[String]
    override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = false
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = synchronized {
      columnIndex match {
        case 0 => data(rowIndex)._1
        case 1 => data(rowIndex)._2.size.toString
        case 2 => data(rowIndex)._3
      }
    }
    override def setValueAt(aValue: Any, rowIndex: Int, columnIndex: Int): Unit = throw new UnsupportedOperationException("This table is unmodifiable")
    override def addTableModelListener(l: TableModelListener): Unit = listeners.synchronized(listeners += l)
    override def removeTableModelListener(l: TableModelListener): Unit = listeners.synchronized(listeners -= l)
  }
}
