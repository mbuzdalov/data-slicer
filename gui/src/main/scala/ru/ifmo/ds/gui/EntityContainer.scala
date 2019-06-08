package ru.ifmo.ds.gui

import java.awt._

import javax.swing._

import scala.collection.mutable

import ru.ifmo.ds.gui.actions.{CreateChart, EntityAction, MapDatabase, OpenDatabaseFiles, SaveDatabaseToFile}
import ru.ifmo.ds.gui.util.VerticalFlowLayout

class EntityContainer {
  require(SwingUtilities.isEventDispatchThread,
          s"The EntityContainer constructor shall be called on an AWT dispatch thread")

  private val mainPaneLayout = new CardLayout()
  private val mainPane = new JPanel(mainPaneLayout)
  private val leftPane = new JPanel(new VerticalFlowLayout)
  private val displayPane = new JPanel(new VerticalFlowLayout)
  private val actionPane = new JPanel(new FlowLayout)
  private val rootComponent = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPane, mainPane)
  private val entities = new mutable.ArrayBuffer[DisplayedEntity]()

  private val nothingToShowString = "###"
  private val nothingToShow = new JLabel("Click on an entry on the left to see its contents")
  nothingToShow.setHorizontalAlignment(SwingConstants.CENTER)
  nothingToShow.setVerticalAlignment(SwingConstants.CENTER)

  val openDatabaseFilesAction: EntityAction = new OpenDatabaseFiles(this)
  val createChartAction: EntityAction = new CreateChart(this)
  val mapDatabaseAction: EntityAction = new MapDatabase(this)
  val saveDatabaseToFileAction: EntityAction = new SaveDatabaseToFile(this)

  actionPane.add(openDatabaseFilesAction.makeButton())
  actionPane.add(createChartAction.makeButton())
  actionPane.add(mapDatabaseAction.makeButton())
  actionPane.add(saveDatabaseToFileAction.makeButton())

  leftPane.add(displayPane)
  leftPane.add(actionPane)

  mainPane.add(nothingToShow, nothingToShowString)

  def root: JComponent = {
    ensureInSwing()
    rootComponent
  }

  def dialogAlignmentSet: EntityContainer.DialogAlignmentInfo = {
    ensureInSwing()

    def process(current: Container, x: Int, y: Int): EntityContainer.DialogAlignmentInfo = {
      current.getParent match {
        case null =>
          val rp = JOptionPane.getRootFrame
          EntityContainer.DialogAlignmentInfo(rp, rp.getWidth / 2, rp.getHeight / 2)
        case f: Frame =>
          EntityContainer.DialogAlignmentInfo(f, x + current.getX, y + current.getY)
        case p: Container =>
          process(p, x + current.getX, y + current.getY)
      }
    }

    process(rootComponent, rootComponent.getWidth / 2, rootComponent.getHeight / 2)
  }

  def add(entity: DisplayedEntity): Unit = {
    ensureInSwing()
    val entityString = identifyingString(entity)
    entities += entity
    mainPane.add(entity.getMainUI, entityString)
    displayPane.add(entity.getDisplayUI)
  }

  def show(entity: DisplayedEntity): Unit = {
    ensureInSwing()
    val entityString = identifyingString(entity)
    mainPaneLayout.show(mainPane, entityString)
  }

  def remove(entity: DisplayedEntity): Unit = {
    ensureInSwing()
    entities -= entity
    mainPane.remove(entity.getMainUI)
    mainPaneLayout.show(mainPane, nothingToShowString)
    displayPane.remove(entity.getDisplayUI)
    displayPane.revalidate()
    displayPane.repaint()
  }

  def findAllDescendants(entity: DisplayedEntity): Seq[DisplayedEntity] = {
    ensureInSwing()
    val index = entities.indexOf(entity)
    if (index < 0) IndexedSeq.empty else {
      val rv = IndexedSeq.newBuilder[DisplayedEntity]
      val set = new mutable.HashSet[DisplayedEntity]()
      rv += entity
      set += entity
      var idx = index + 1
      while (idx < entities.size) {
        val e = entities(idx)
        if (e.inputEntities.exists(set.contains)) {
          set += e
          rv += e
        }
        idx += 1
      }
      rv.result()
    }
  }

  def confirmRemoval(entity: DisplayedEntity): Boolean = {
    ensureInSwing()
    val queryResult = JOptionPane.showConfirmDialog(mainPane,
                                                    s"Do you wish to remove '${entity.getName}'?",
                                                    "Confirm removal",
                                                    JOptionPane.OK_CANCEL_OPTION)
    queryResult == JOptionPane.OK_OPTION
  }

  def getEntitiesByClass[T <: DisplayedEntity](clazz: Class[T]): Seq[T] = {
    ensureInSwing()
    val builder = IndexedSeq.newBuilder[T]
    for (e <- entities) {
      if (clazz.isAssignableFrom(e.getClass)) {
        builder += clazz.cast(e)
      }
    }
    builder.result()
  }

  private def ensureInSwing(): Unit = {
    require(SwingUtilities.isEventDispatchThread,
            "The EntityContainer methods shall be called on an AWT dispatch thread")
  }

  private def identifyingString(entity: DisplayedEntity): String = {
    entity.getClass.getName + "@" + System.identityHashCode(entity)
  }
}

object EntityContainer {
  case class DialogAlignmentInfo(frame: Frame, centerX: Int, centerY: Int)
}
