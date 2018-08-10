package ru.ifmo.ds.gui

import java.awt.BorderLayout

import javax.swing.{JComponent, JLabel, JTabbedPane, SwingConstants}
import ru.ifmo.ds.Database
import ru.ifmo.ds.util.OrderingForStringWithNumbers

object JComponentExtensions {
  implicit class DirectHierarchy(val comp: JComponent) extends AnyVal {
    def += (that: String): Unit = util.inSwing {
      comp.add(new JLabel(that))
      comp.validate()
    }
    def += (that: JComponent): Unit = util.inSwing {
      comp.add(that)
      comp.validate()
    }
    def -= (that: JComponent): Unit = util.inSwing {
      comp.remove(that)
      comp.validate()
    }
    def byIndex(index: Int): JComponent = comp.getComponent(index).asInstanceOf[JComponent]
  }

  implicit class AsGroupWrapper(val comp: JComponent) extends AnyVal {
    private def ensureInnerComponentSupportsKeyValueInterface(): Unit = {
      if (comp.getComponentCount != 1 || !comp.getComponent(0).isInstanceOf[JTabbedPane]) {
        comp.removeAll()
        comp.setLayout(new BorderLayout())
        comp.add(new JTabbedPane(SwingConstants.TOP, JTabbedPane.WRAP_TAB_LAYOUT))
        comp.validate()
      }
    }

    private def getInnerComponent: JTabbedPane = comp.getComponent(0) match {
      case jtp: JTabbedPane => jtp
    }

    def += (pair: (String, JComponent)): Unit = this += (pair._1, pair._2)
    def += (key: String, value: JComponent): Unit = util.inSwing {
      ensureInnerComponentSupportsKeyValueInterface()
      getInnerComponent.add(key, value)
      getInnerComponent.validate()
    }

    def removeByTitle(title: String): Unit = util.inSwing {
      ensureInnerComponentSupportsKeyValueInterface()
      val tab = getInnerComponent.indexOfTab(title)
      if (tab >= 0) {
        getInnerComponent.removeTabAt(tab)
      }
    }

    def addPlots(db: Database, titlePrefix: String, groupKeys: Seq[String],
      xAxis: Axis, yAxis: Axis, seriesKey: String
    ): Unit = {
      import scala.collection.mutable
      val map = new mutable.HashMap[Seq[String], mutable.ArrayBuffer[Database.Entry]]()
      db foreach { e =>
        map.getOrElseUpdate(groupKeys.map(e.apply), new mutable.ArrayBuffer()) += e
      }
      val tpWithSep = if (titlePrefix.isEmpty) "" else titlePrefix + ": "
      def extractTitle(key: Seq[String]): String = groupKeys.size match {
        case 0 => titlePrefix
        case 1 => tpWithSep + key.head
        case _ => (groupKeys, key).zipped.map((k, v) => k + "=" + v).mkString(tpWithSep, ", ", "")
      }
      val titledData = map.toIndexedSeq.map(p => (extractTitle(p._1), Database(p._2 :_*))).sortBy(_._1)(OrderingForStringWithNumbers.SpecialDotTreatment)
      util.inSwing {
        for ((title, db) <- titledData) {
          val wrapper = new SimpleChartWrapper(comp.getWidth, comp.getHeight, xAxis, yAxis)
          wrapper.addDatabase(db, seriesKey)
          this += (title, wrapper.gui)
        }
      }
    }

    def byTitle(title: String): JComponent = {
      val comp = getInnerComponent
      val index = comp.indexOfTab(title)
      if (index == -1) null else {
        val tab = comp.getComponentAt(index)
        tab.asInstanceOf[JComponent]
      }
    }
  }
}
