package ru.ifmo.ds.gui

import java.awt._

import javax.swing._

import scala.collection.mutable

import ru.ifmo.ds.gui.actions.{CreateChart, EntityAction, MapDatabase, OpenDatabaseFiles, SaveDatabaseToFile}

class EntityContainer {
  require(SwingUtilities.isEventDispatchThread,
          s"The EntityContainer constructor shall be called on an AWT dispatch thread")
  private val actionPane = new JPanel(new FlowLayout)
  private val rootComponent = new JTabbedPane(SwingConstants.LEFT, JTabbedPane.SCROLL_TAB_LAYOUT)
  private val entities = new mutable.ArrayBuffer[DisplayedEntity]()

  private val nothingToShow = new JLabel("Click on an entry on the left to see its contents")
  nothingToShow.setHorizontalAlignment(SwingConstants.CENTER)
  nothingToShow.setVerticalAlignment(SwingConstants.CENTER)

  val openDatabaseFilesAction: EntityAction = new OpenDatabaseFiles(this)
  val createChartAction: EntityAction = new CreateChart(this)
  val mapDatabaseAction: EntityAction = new MapDatabase(this)
  val saveDatabaseToFileAction: EntityAction = new SaveDatabaseToFile(this)

  actionPane.setOpaque(false)
  actionPane.add(openDatabaseFilesAction.makeButton())
  actionPane.add(createChartAction.makeButton())
  actionPane.add(mapDatabaseAction.makeButton())
  actionPane.add(saveDatabaseToFileAction.makeButton())

  rootComponent.add(nothingToShow)
  rootComponent.setBackgroundAt(0, actionPane.getBackground)
  rootComponent.setTabComponentAt(0, actionPane)

  def root: JComponent = {
    ensureInSwing()
    rootComponent
  }

  def dialogAlignmentSet: EntityContainer.DialogAlignmentInfo = {
    ensureInSwing()

    @scala.annotation.tailrec
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
    entities += entity
    val size = rootComponent.getTabCount
    rootComponent.add(entity.getMainUI, size - 1)
    rootComponent.setBackgroundAt(size - 1, actionPane.getBackground)
    rootComponent.setTabComponentAt(size - 1, entity.getDisplayUI)
  }

  def show(entity: DisplayedEntity): Unit = {
    rootComponent.setSelectedComponent(entity.getMainUI)
  }

  def remove(entity: DisplayedEntity): Unit = {
    ensureInSwing()
    entities -= entity
    rootComponent.remove(entity.getMainUI)
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
    val queryResult = JOptionPane.showConfirmDialog(rootComponent,
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
}

object EntityContainer {
  case class DialogAlignmentInfo(frame: Frame, centerX: Int, centerY: Int)
}
