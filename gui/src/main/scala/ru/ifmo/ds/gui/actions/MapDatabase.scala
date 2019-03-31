package ru.ifmo.ds.gui.actions

import java.awt.{BorderLayout, FlowLayout}

import javax.swing._

import scala.collection.mutable.ArrayBuffer

import ru.ifmo.ds.Database
import ru.ifmo.ds.gui.EntityContainer
import ru.ifmo.ds.gui.components.DatabaseSelector
import ru.ifmo.ds.gui.parts.DatabaseEntity
import ru.ifmo.ds.gui.util.{ImageLoadingFacilities, VerticalFlowLayout}

class MapDatabase(container: EntityContainer) extends EntityAction("Map Database", MapDatabase.mapDatabaseIcon) {
  override protected def performImpl(): Unit = {
    val dbEntities = container.getEntitiesByClass(classOf[DatabaseEntity])
    if (dbEntities.isEmpty) {
      JOptionPane.showMessageDialog(container.root, "No database entities found!",
                                    "Cannot map a database", JOptionPane.ERROR_MESSAGE)
    } else {
      val options = new MapDatabase.ConfigurationComponent(dbEntities, container.dialogAlignmentSet)
      options.setVisible(true)
      if (options.isOK) {
        val databases = options.getSelectedEntities
        val mapper = options.getMappingFunction
        new DatabaseEntity(databases, container, "[Result]", bases => mapper(Database.merge(bases :_*)))
      }
    }
  }
}

object MapDatabase extends ImageLoadingFacilities {
  private val mapDatabaseIcon = imageFromResource("map-database.png")

  private sealed trait DatabaseActionFactory {
    def name: String
    override def toString: String = name
    def arity: Int
    def makeFunction(args: Array[String]): Database => Database
  }

  private object Empty extends DatabaseActionFactory {
    override def name: String = "---"
    override def arity: Int = 0
    override def makeFunction(args: Array[String]): Database => Database = identity
  }

  private object RenameKey extends DatabaseActionFactory {
    override def name: String = "Rename Key"
    override def arity: Int = 2
    override def makeFunction(args: Array[String]): Database => Database = _.withRenamedKey(args(0), args(1))
  }

  private object RenameValue extends DatabaseActionFactory {
    override def name: String = "Rename Value"
    override def arity: Int = 3
    override def makeFunction(args: Array[String]): Database => Database = _.withRenamedValue(args(0), args(1), args(2))
  }

  private val allActions = Seq(Empty, RenameKey, RenameValue).sortBy(_.name)

  private class ActionConfigurationPane(updateGUIFunction: () => Unit) extends JPanel {
    private val argumentPane = new JPanel(new FlowLayout())
    private val comboBox = new JComboBox[DatabaseActionFactory](allActions.toArray)

    setLayout(new BorderLayout())
    comboBox.addActionListener(_ => {
      val factory = getSelectedFactory
      val arity = factory.arity
      argumentPane.removeAll()
      for (_ <- 0 until arity) {
        argumentPane.add(new JTextField(10))
      }
      updateGUIFunction()
    })
    add(comboBox, BorderLayout.LINE_START)
    add(argumentPane, BorderLayout.CENTER)

    private def getSelectedFactory: DatabaseActionFactory = comboBox.getModel.getElementAt(comboBox.getSelectedIndex)

    def getFunction: Database => Database = {
      val args = argumentPane.getComponents.map(_.asInstanceOf[JTextField].getText)
      val factory = getSelectedFactory
      if (factory.arity == args.length && args.forall(_.nonEmpty)) {
        factory.makeFunction(args)
      } else {
        identity
      }
    }
  }

  private class ConfigurationComponent(entities: Seq[DatabaseEntity], alignmentSet: EntityContainer.DialogAlignmentInfo)
    extends JDialog(alignmentSet.frame, "Create a chart", true) {
    private val selector = new DatabaseSelector(entities)

    private val addNewActionButtonPanel = new JPanel(new BorderLayout())
    private val addNewActionButton = new JButton("+")
    private val okButton = new JButton("OK")
    private val cancelButton = new JButton("Cancel")
    private val okCancelPane = new JPanel(new FlowLayout())
    private var isOkay = false
    private val midPane = new JPanel(new VerticalFlowLayout)
    private val configurators = new ArrayBuffer[ActionConfigurationPane]()

    okButton.addActionListener(_ => {
      isOkay = true
      setVisible(false)
    })
    cancelButton.addActionListener(_ => {
      isOkay = false
      setVisible(false)
    })

    addNewActionButtonPanel.add(addNewActionButton, BorderLayout.LINE_START)
    midPane.add(addNewActionButtonPanel)
    addNewActionButton.addActionListener(_ => {
      val newSelector = new ActionConfigurationPane(() => updateGUI())
      configurators += newSelector
      val button = new JButton("<html>&minus;</html>")
      val panel = new JPanel(new BorderLayout())
      panel.add(button, BorderLayout.LINE_START)
      panel.add(newSelector, BorderLayout.CENTER)
      midPane.add(panel, midPane.getComponentCount - 1)
      updateGUI()
      button.addActionListener(_ => {
        configurators -= newSelector
        midPane.remove(panel)
        updateGUI()
      })
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

    private def updateGUI(): Unit = {
      midPane.revalidate()
      pack()
      midPane.repaint()
    }

    private def updateWithDatabaseChange(): Unit = {
      okButton.setEnabled(selector.getDatabase.hasEntries)
    }

    def isOK: Boolean = isOkay
    def getSelectedEntities: Seq[DatabaseEntity] = selector.getSelectedEntities
    def getMappingFunction: Database => Database = {
      val functions = configurators.map(_.getFunction)
      functions.reduce(_ andThen _)
    }
  }
}
