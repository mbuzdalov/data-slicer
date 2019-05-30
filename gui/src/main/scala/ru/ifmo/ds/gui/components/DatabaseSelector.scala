package ru.ifmo.ds.gui.components

import java.lang.{Double => JDouble}

import javax.swing.{JCheckBox, JLabel, JPanel, SwingUtilities}

import scala.collection.mutable.{ArrayBuffer, HashMap => MuHashMap}

import ru.ifmo.ds.gui.parts.DatabaseEntity
import ru.ifmo.ds.gui.util.VerticalFlowLayout

class DatabaseSelector(entities: Seq[DatabaseEntity]) extends JPanel {
  import DatabaseSelector._

  private[this] val entitySelectors = entities.map(e => new CheckBoxWithEntity(e))
  private[this] val listeners = new ArrayBuffer[SelectionListener]()
  private[this] var nEntries = 0
  private[this] val keyDescriptions = new MuHashMap[String, KeyDescription]()

  setLayout(new VerticalFlowLayout)
  add(new JLabel("Choose databases"))
  entitySelectors.foreach(add)
  entitySelectors.foreach(e => e.addActionListener(_ => updateDatabaseBySelection(e)))

  def hasEntries: Boolean = nEntries > 0

  def getKeyDescriptions: Iterable[KeyDescription] = keyDescriptions.values

  def getSelectedEntities: Seq[DatabaseEntity] = getSelectedIndices.map(entities)

  def addSelectionListener(listener: SelectionListener): Unit = {
    listeners += listener
  }

  private def getSelectedIndices: Seq[Int] = entities.indices.filter(i => entitySelectors(i).isSelected)
  private def updateDatabaseBySelection(box: CheckBoxWithEntity): Unit = {
    new Thread(() => {
      val db = box.entity.getDatabase
      if (box.isSelected) {
        nEntries += db.entries.size
        for (k <- db.possibleKeys) {
          val values = db.valuesUnderKey(k)
          if (values.nonEmpty) {
            val kd = keyDescriptions.getOrElseUpdate(k, KeyDescription(k))
            values.foreach(kd.add)
          }
        }
      } else {
        nEntries -= db.entries.size
        for (k <- db.possibleKeys) {
          val values = db.valuesUnderKey(k)
          if (values.nonEmpty) {
            assert(keyDescriptions.contains(k))
            val kd = keyDescriptions(k)
            values.foreach(kd.remove)
            if (kd.countDifferentValues == 0) {
              keyDescriptions -= k
            }
          }
        }
      }
      SwingUtilities.invokeLater(() => listeners.foreach(_.selectionChanged()))
    }, "Database Selector Worker Thread").start()
  }
}

object DatabaseSelector {
  private class CheckBoxWithEntity(val entity: DatabaseEntity) extends JCheckBox(entity.getName)

  trait SelectionListener {
    def selectionChanged(): Unit
  }

  case class KeyDescription(key: String) {
    private[this] var nDifferentValues: Int = 0
    private[this] var nDifferentDoubleValues: Int = 0
    private[this] var nDifferentPositiveValues: Int = 0
    private[this] var nNones: Int = 0

    override def toString: String = if (nDifferentValues % 10 == 1 && nDifferentValues % 100 != 11) {
      s"$key ($nDifferentValues value)"
    } else {
      s"$key ($nDifferentValues values)"
    }

    def countDifferentValues: Int = nDifferentValues
    def allValuesAreDouble: Boolean = nDifferentDoubleValues + (if (nNones > 0) 1 else 0) == nDifferentValues
    def allValuesArePositive: Boolean = nDifferentPositiveValues + (if (nNones > 0) 1 else 0) == nDifferentValues

    private val valueCount = new MuHashMap[String, Int]()

    private def typify(v: String): Int = {
      try {
        val doubleValue = JDouble.parseDouble(v)
        if (doubleValue > 0) 3 else 1
      } catch {
        case _: NumberFormatException | _: NullPointerException => 0
      }
    }

    def add(value: Option[String]): Unit = {
      value match {
        case Some(v) =>
          valueCount.get(v) match {
            case None =>
              valueCount.put(v, 1)
              nDifferentValues += 1
              val tp = typify(v)
              if ((tp & 1) != 0) nDifferentDoubleValues += 1
              if ((tp & 2) != 0) nDifferentPositiveValues += 1
            case Some(oldCount) =>
              valueCount.put(v, oldCount + 1)
          }
        case None =>
          if (nNones == 0) {
            nDifferentValues += 1
          }
          nNones += 1
      }
    }

    def remove(value: Option[String]): Unit = {
      value match {
        case Some(v) =>
          assert(valueCount.contains(v))
          val oldCount = valueCount(v)
          assert(oldCount > 0)
          if (oldCount == 1) {
            valueCount -= v
            nDifferentValues -= 1
            val tp = typify(v)
            if ((tp & 1) != 0) nDifferentDoubleValues -= 1
            if ((tp & 2) != 0) nDifferentPositiveValues -= 1
            assert(nDifferentValues >= 0)
            assert(nDifferentDoubleValues >= 0)
            assert(nDifferentPositiveValues >= 0)
          } else {
            valueCount.put(v, oldCount - 1)
          }
        case None =>
          nNones -= 1
          assert(nNones >= 0)
          if (nNones == 0) {
            nDifferentValues -= 1
            assert(nDifferentValues >= 0)
          }
      }
    }
  }
}
