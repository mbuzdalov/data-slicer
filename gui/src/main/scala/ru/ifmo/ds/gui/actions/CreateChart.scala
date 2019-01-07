package ru.ifmo.ds.gui.actions

import java.awt.{BorderLayout, FlowLayout, Frame}

import javax.imageio.ImageIO
import javax.swing._

import scala.collection.mutable

import ru.ifmo.ds.Database
import ru.ifmo.ds.gui.EntityContainer
import ru.ifmo.ds.gui.actions.CreateChart.ChartOptionsComponent
import ru.ifmo.ds.gui.parts.{ChartEntity, DatabaseEntity}
import ru.ifmo.ds.gui.util.VerticalFlowLayout
import ru.ifmo.ds.util.Axis

class CreateChart(container: EntityContainer) extends EntityAction("Create chart", CreateChart.createChart) {
  override protected def performImpl(): Unit = {
    val dbEntities = container.getEntitiesByClass(classOf[DatabaseEntity])
    if (dbEntities.isEmpty) {
      JOptionPane.showMessageDialog(container.root, "No database entities found!", "Cannot create a chart", JOptionPane.ERROR_MESSAGE)
    } else {
      val options = new ChartOptionsComponent(dbEntities)
      options.pack()
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

object CreateChart {
  private val createChart = new ImageIcon(ImageIO.read(getClass.getResource("create-chart.png")))

  private class ChartOptionsComponent(entities: Seq[DatabaseEntity]) extends JDialog(null: Frame, "Create a chart", true) {
    private var database = Database()
    private var keySet: Set[String] = Set.empty
    private val entitySelectors = entities.map(e => new JCheckBox(e.getName))
    private val xAxisSelector, yAxisSelector, seriesKeySelector = createAndConfigureComboBox()
    private val allSelectors = new mutable.ArrayBuffer[JComboBox[String]]()
    private val addNewSelectorButtonPanel = new JPanel(new BorderLayout())
    private val addNewSelectorButton = new JButton("+")
    private val okButton = new JButton("OK")
    private val cancelButton = new JButton("Cancel")
    private val okCancelPane = new JPanel(new FlowLayout())
    private var isOkay = false
    private val midPane = new JPanel(new VerticalFlowLayout)

    okButton.addActionListener(_ => {
      isOkay = true
      setVisible(false)
    })
    cancelButton.addActionListener(_ => {
      isOkay = false
      setVisible(false)
    })

    private def createAndConfigureComboBox(): JComboBox[String] = {
      val rv = new JComboBox[String]()
      rv.setSelectedIndex(-1)
      rv.addActionListener(_ => updateWithUpToDateDatabase())
      rv
    }

    allSelectors ++= Seq(xAxisSelector, yAxisSelector, seriesKeySelector)

    midPane.add(new JLabel("Choose databases"))
    entitySelectors.foreach(midPane.add)
    midPane.add(new JLabel("Choose a key for the X axis"))
    midPane.add(xAxisSelector)
    midPane.add(new JLabel("Choose a key for the Y axis"))
    midPane.add(yAxisSelector)
    midPane.add(new JLabel("Choose a series key"))
    midPane.add(seriesKeySelector)
    midPane.add(new JLabel("Choose category keys"))

    addNewSelectorButtonPanel.add(addNewSelectorButton, BorderLayout.LINE_START)
    midPane.add(addNewSelectorButtonPanel)

    addNewSelectorButton.addActionListener(_ => {
      val newSelector = createAndConfigureComboBox()
      allSelectors += newSelector
      val button = new JButton("<html>&minus;</html>")
      val panel = new JPanel(new BorderLayout())
      panel.add(button, BorderLayout.LINE_START)
      panel.add(newSelector, BorderLayout.CENTER)
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

    entitySelectors.foreach(_.addActionListener(_ => updateWithDatabaseChange()))

    okButton.setEnabled(false)

    okCancelPane.add(okButton)
    okCancelPane.add(cancelButton)

    setLayout(new BorderLayout())
    add(midPane, BorderLayout.CENTER)
    add(okCancelPane, BorderLayout.PAGE_END)

    def isOK: Boolean = isOkay
    def getSelectedEntities: Seq[DatabaseEntity] = entities.indices.filter(i => entitySelectors(i).isSelected).map(entities)
    def getXAxisKey: String = xAxisSelector.getModel.getElementAt(xAxisSelector.getSelectedIndex)
    def getYAxisKey: String = yAxisSelector.getModel.getElementAt(yAxisSelector.getSelectedIndex)
    def getSeriesKey: String = seriesKeySelector.getModel.getElementAt(seriesKeySelector.getSelectedIndex)
    def getCategoryKeys: Seq[String] = allSelectors.drop(3).flatMap(s => {
      val i = s.getSelectedIndex
      if (i >= 0) Some(s.getModel.getElementAt(i)) else None
    })

    private def updateWithDatabaseChange(): Unit = {
      database = Database.merge(entities.indices.filter(i => entitySelectors(i).isSelected).map(i => entities(i).getDatabase) :_*)
      keySet = database.possibleKeys
      updateWithUpToDateDatabase()
    }

    private def updateWithUpToDateDatabase(): Unit = {
      val retained = new mutable.HashSet[String]
      retained ++= keySet
      for (s <- allSelectors) {
        val availableStrings = retained.toIndexedSeq.sorted
        val index = s.getSelectedIndex
        val oldSelected = if (index < 0) None else Some(s.getModel.getElementAt(index))
        s.setModel(new DefaultComboBoxModel[String](availableStrings.toArray))
        oldSelected match {
          case Some(v) =>
            val newIndex = availableStrings.indexOf(v)
            if (newIndex >= 0) {
              s.setSelectedIndex(newIndex)
            } else {
              s.setSelectedIndex(-1)
            }
          case None =>
            s.setSelectedIndex(-1)
        }
        val newSelectedIndex = s.getSelectedIndex
        if (newSelectedIndex >= 0) {
          retained -= s.getModel.getElementAt(newSelectedIndex)
        }
      }

      okButton.setEnabled(database.hasEntries &&
                            xAxisSelector.getSelectedIndex >= 0 &&
                            yAxisSelector.getSelectedIndex >= 0 &&
                            seriesKeySelector.getSelectedIndex >= 0)
    }
  }
}
