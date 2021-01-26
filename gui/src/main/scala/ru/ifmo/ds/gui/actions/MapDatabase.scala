package ru.ifmo.ds.gui.actions

import java.awt.{BorderLayout, FlowLayout}
import java.util.regex.Pattern

import javax.swing._

import scala.collection.mutable.ArrayBuffer

import ru.ifmo.ds.Database
import ru.ifmo.ds.Database.Entry
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
    def captions: Seq[String]
    def makeFunction(args: Array[String]): Database => Database
  }

  private object Empty extends DatabaseActionFactory {
    override def name: String = "---"
    override def captions: Seq[String] = Seq.empty
    override def makeFunction(args: Array[String]): Database => Database = identity
  }

  private object RenameKey extends DatabaseActionFactory {
    override def name: String = "Rename Key"
    override val captions: Seq[String] = IndexedSeq("Old key", "New key")
    override def makeFunction(args: Array[String]): Database => Database = _.withRenamedKey(args(0), args(1))
  }

  private object RenameValue extends DatabaseActionFactory {
    override def name: String = "Rename Value"
    override val captions: Seq[String] = IndexedSeq("Key", "Old value", "New value")
    override def makeFunction(args: Array[String]): Database => Database = _.withRenamedValue(args(0), args(1), args(2))
  }

  private object AddKeyValue extends DatabaseActionFactory {
    override def name: String = "Add New Key/Value Singleton"
    override val captions: Seq[String] = IndexedSeq("Key", "Value")
    override def makeFunction(args: Array[String]): Database => Database = _.withMoreKeys(Map(args(0) -> args(1)))
  }

  private object RemoveAllKeysMatching extends DatabaseActionFactory {
    override def name: String = "Remove All Keys Matching..."
    override val captions: Seq[String] = IndexedSeq("Key regex")
    override def makeFunction(args: Array[String]): Database => Database = {
      val pattern = Pattern.compile(args(0))
      def entryMapper(e: Entry): Option[Entry] = {
        val builder = Map.newBuilder[String, String]
        e.foreach(p => if (!pattern.matcher(p._1).matches()) builder += p)
        val result = builder.result()
        if (result.isEmpty) None else Some(Database.entry(result))
      }
      db => Database(db.entries.flatMap(entryMapper) :_*)
    }
  }

  private object RemoveAllValuesMatching extends DatabaseActionFactory {
    override def name: String = "Remove Entries with Values Matching..."
    override val captions: Seq[String] = IndexedSeq("Key", "Value regex")
    override def makeFunction(args: Array[String]): Database => Database = {
      val key = args(0)
      val pattern = Pattern.compile(args(1))
      _.filter(e => !e.contains(key) || !pattern.matcher(e(key)).matches())
    }
  }

  private object AddNormalizedValues extends DatabaseActionFactory {
    override def name: String = "Normalize One Key By Another"
    override val captions: Seq[String] = IndexedSeq("New Key", "Numerator Key", "Denominator Key")
    override def makeFunction(args: Array[String]): Database => Database = {
      val newKey = args(0)
      val numKey = args(1)
      val denKey = args(2)
      def entryMapper(e: Entry): Entry = {
        if (e.contains(numKey) && e.contains(denKey)) {
          val num = e(numKey).toDoubleOption
          val den = e(denKey).toDoubleOption
          if (num.nonEmpty && den.nonEmpty) {
            val builder = Map.newBuilder[String, String]
            e.foreach(p => builder += p)
            builder += newKey -> (num.get / den.get).toString
            Database.entry(builder.result())
          } else e
        } else e
      }
      db => Database(db.entries.map(entryMapper) :_*)
    }
  }

  private val allActions = Seq(Empty, RenameKey, RenameValue, AddKeyValue,
                               RemoveAllKeysMatching, RemoveAllValuesMatching,
                               AddNormalizedValues).sortBy(_.name)

  private class ActionConfigurationPane(updateGUIFunction: () => Unit) extends JPanel {
    private val argumentPane = new JPanel(new FlowLayout())
    private val comboBox = new JComboBox[DatabaseActionFactory](allActions.toArray)

    setLayout(new BorderLayout())
    comboBox.addActionListener(_ => {
      val factory = getSelectedFactory
      val captions = factory.captions
      argumentPane.removeAll()
      for (caption <- captions) {
        argumentPane.add(new JLabel(caption))
        argumentPane.add(new JTextField(10))
      }
      updateGUIFunction()
    })
    add(comboBox, BorderLayout.LINE_START)
    add(argumentPane, BorderLayout.CENTER)

    private def getSelectedFactory: DatabaseActionFactory = comboBox.getModel.getElementAt(comboBox.getSelectedIndex)

    def getFunction: Database => Database = {
      val args = argumentPane.getComponents.flatMap {
        case tf: JTextField => Some(tf.getText)
        case _ => None
      }
      val factory = getSelectedFactory
      if (factory.captions.size == args.length && args.forall(_.nonEmpty)) {
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
    midPane.add(new JLabel("Add database mappings"))
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
      okButton.setEnabled(selector.hasEntries)
    }

    def isOK: Boolean = isOkay
    def getSelectedEntities: Seq[DatabaseEntity] = selector.getSelectedEntities
    def getMappingFunction: Database => Database = if (configurators.isEmpty) identity else {
      val functions = configurators.map(_.getFunction)
      functions.reduce(_ andThen _)
    }
  }
}
