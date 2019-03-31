package ru.ifmo.ds.gui.actions

import java.awt.{BorderLayout, FlowLayout}

import javax.swing._

import scala.collection.mutable

import ru.ifmo.ds.gui.EntityContainer
import ru.ifmo.ds.gui.actions.CreateChart.ChartOptionsComponent
import ru.ifmo.ds.gui.components.DatabaseSelector
import ru.ifmo.ds.gui.parts.{ChartEntity, DatabaseEntity}
import ru.ifmo.ds.gui.util.{ImageLoadingFacilities, VerticalFlowLayout}
import ru.ifmo.ds.util.Axis

class CreateChart(container: EntityContainer) extends EntityAction("Create chart", CreateChart.createChart) {
  override protected def performImpl(): Unit = {
    val dbEntities = container.getEntitiesByClass(classOf[DatabaseEntity])
    if (dbEntities.isEmpty) {
      JOptionPane.showMessageDialog(container.root, "No database entities found!",
                                    "Cannot create a chart", JOptionPane.ERROR_MESSAGE)
    } else {
      val options = new ChartOptionsComponent(dbEntities, container.dialogAlignmentSet)
      options.setVisible(true)
      if (options.isOK) {
        // will add itself to the container
        new ChartEntity(options.getSelectedEntities, container,
                        options.getCategoryKeys,
                        options.getSeriesKey,
                        Axis(options.getXAxisKey, options.getXAxisKey, isLogarithmic = true),
                        Axis(options.getYAxisKey, options.getYAxisKey, isLogarithmic = true))
      }
    }
  }
}

object CreateChart extends ImageLoadingFacilities {
  private val createChart = imageFromResource("create-chart.png")

  private case class Selector(combo: JComboBox[CountedOption], wantsMassiveEntries: Boolean) {
    def sortOptions(options: Seq[CountedOption]): Seq[CountedOption] = {
      if (options.isEmpty) options else {
        val (singletons, others) = options.sortBy(_.id).partition(_.count == 1)
        if (wantsMassiveEntries) {
          val threshold = options.sortBy(_.count).takeRight(3).head.count
          val (coolest, average) = others.partition(_.count >= threshold)
          coolest.sortBy(-_.count) ++ average ++ singletons
        } else {
          others ++ singletons
        }
      }
    }

    def selectSimilar(previous: Option[CountedOption]): Unit = {
      combo.setSelectedIndex(previous flatMap { v =>
        val model = combo.getModel
        (0 until model.getSize).find(i => model.getElementAt(i).id == v.id)
      } getOrElse -1)
    }

    def get(index: Int): CountedOption = combo.getModel.getElementAt(index)
    def getSelected: CountedOption = get(combo.getSelectedIndex)
  }

  private case class CountedOption(id: String, count: Int) {
    override def toString: String = if (count % 10 == 1 && count % 100 != 11) {
      s"$id ($count value)"
    } else {
      s"$id ($count values)"
    }
  }

  private class ChartOptionsComponent(entities: Seq[DatabaseEntity], alignmentSet: EntityContainer.DialogAlignmentInfo)
    extends JDialog(alignmentSet.frame, "Create a chart", true) {
    private val selector = new DatabaseSelector(entities)
    private var keySet: Set[CountedOption] = Set.empty
    private val xAxisSelector, seriesKeySelector = createAndConfigureComboBox(false)
    private val yAxisSelector = createAndConfigureComboBox(true)
    private val allSelectors = new mutable.ArrayBuffer[Selector]()
    private val addNewSelectorButtonPanel = new JPanel(new BorderLayout())
    private val addNewSelectorButton = new JButton("+")
    private val okButton = new JButton("OK")
    private val cancelButton = new JButton("Cancel")
    private val okCancelPane = new JPanel(new FlowLayout())
    private var isOkay = false
    private var previousWidth = 0
    private val midPane = new JPanel(new VerticalFlowLayout)

    private var comboUpdateIsHappening = false // is read and written in Swing thread only

    okButton.addActionListener(_ => {
      isOkay = true
      setVisible(false)
    })
    cancelButton.addActionListener(_ => {
      isOkay = false
      setVisible(false)
    })

    private def createAndConfigureComboBox(wantMassiveEntries: Boolean): Selector = {
      val rv = new JComboBox[CountedOption]()
      rv.setSelectedIndex(-1)
      rv.addActionListener(_ => if (!comboUpdateIsHappening) updateWithUpToDateDatabase())
      Selector(rv, wantMassiveEntries)
    }

    allSelectors ++= Seq(xAxisSelector, yAxisSelector, seriesKeySelector)

    midPane.add(new JLabel("Choose a key for the X axis"))
    midPane.add(xAxisSelector.combo)
    midPane.add(new JLabel("Choose a key for the Y axis"))
    midPane.add(yAxisSelector.combo)
    midPane.add(new JLabel("Choose a series key"))
    midPane.add(seriesKeySelector.combo)
    midPane.add(new JLabel("Choose category keys"))

    addNewSelectorButtonPanel.add(addNewSelectorButton, BorderLayout.LINE_START)
    midPane.add(addNewSelectorButtonPanel)

    addNewSelectorButton.addActionListener(_ => {
      val newSelector = createAndConfigureComboBox(false)
      allSelectors += newSelector
      val button = new JButton("<html>&minus;</html>")
      val panel = new JPanel(new BorderLayout())
      panel.add(button, BorderLayout.LINE_START)
      panel.add(newSelector.combo, BorderLayout.CENTER)
      midPane.add(panel, midPane.getComponentCount - 1)
      midPane.revalidate()
      pack()
      midPane.repaint()
      button.addActionListener(_ => {
        allSelectors -= newSelector
        midPane.remove(panel)
        midPane.revalidate()
        pack()
        midPane.repaint()
      })
      updateWithUpToDateDatabase()
    })

    okButton.setEnabled(false)

    okCancelPane.add(okButton)
    okCancelPane.add(cancelButton)

    setLayout(new BorderLayout())
    add(selector, BorderLayout.PAGE_START)
    add(midPane, BorderLayout.CENTER)
    add(okCancelPane, BorderLayout.PAGE_END)

    selector.addSelectionListener(() => updateWithDatabaseChange())
    pack()
    setLocation(alignmentSet.centerX - getWidth / 2, alignmentSet.centerY - getHeight / 2)

    def isOK: Boolean = isOkay
    def getSelectedEntities: Seq[DatabaseEntity] = selector.getSelectedEntities
    def getXAxisKey: String = xAxisSelector.getSelected.id
    def getYAxisKey: String = yAxisSelector.getSelected.id
    def getSeriesKey: String = seriesKeySelector.getSelected.id
    def getCategoryKeys: Seq[String] = allSelectors.drop(3).flatMap(s => {
      val i = s.combo.getSelectedIndex
      if (i >= 0) Some(s.combo.getModel.getElementAt(i).id) else None
    })

    private def updateWithDatabaseChange(): Unit = {
      val database = selector.getDatabase
      val keys = database.possibleKeys
      keySet = keys.map(k => CountedOption(k, database.valuesUnderKey(k).size))
      val width = if (keySet.isEmpty) 0 else keySet.view.map(_.toString.length).max
      updateWithUpToDateDatabase()
      if (width != previousWidth) {
        previousWidth = width
        midPane.revalidate()
        pack()
        midPane.repaint()
      }
    }

    private def updateWithUpToDateDatabase(): Unit = {
      assert(!comboUpdateIsHappening)
      comboUpdateIsHappening = true
      val retained = new mutable.HashSet[CountedOption]
      retained ++= keySet
      for (s <- allSelectors) {
        val availableStrings = s.sortOptions(retained.toIndexedSeq)
        val index = s.combo.getSelectedIndex
        val oldSelected = if (index < 0) None else Some(s.combo.getModel.getElementAt(index))
        s.combo.setModel(new DefaultComboBoxModel[CountedOption](availableStrings.toArray))
        s.selectSimilar(oldSelected)
        val newSelectedIndex = s.combo.getSelectedIndex
        if (newSelectedIndex >= 0) {
          retained -= s.combo.getModel.getElementAt(newSelectedIndex)
        }
      }

      okButton.setEnabled(selector.getDatabase.hasEntries &&
                            xAxisSelector.combo.getSelectedIndex >= 0 &&
                            yAxisSelector.combo.getSelectedIndex >= 0 &&
                            seriesKeySelector.combo.getSelectedIndex >= 0)
      assert(comboUpdateIsHappening)
      comboUpdateIsHappening = false
    }
  }
}
