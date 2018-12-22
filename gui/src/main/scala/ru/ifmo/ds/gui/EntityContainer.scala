package ru.ifmo.ds.gui

import java.awt.CardLayout

import javax.swing._

import scala.collection.mutable

class EntityContainer {
  require(SwingUtilities.isEventDispatchThread,
          "The Context constructor shall be called on an AWT dispatch thread")

  private val mainPaneLayout = new CardLayout()
  private val mainPane = new JPanel(mainPaneLayout)
  private val displayPane = new JPanel(new VerticalFlowLayout)
  private val rootComponent = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, displayPane, mainPane)
  private val entities = new mutable.ArrayBuffer[DisplayedEntity]()

  private val nothingToShowString = "###"
  private val nothingToShow = new JLabel("Click on an entry on the left to see its contents")
  nothingToShow.setHorizontalAlignment(SwingConstants.CENTER)
  nothingToShow.setVerticalAlignment(SwingConstants.CENTER)

  mainPane.add(nothingToShow, nothingToShowString)

  def root: JComponent = {
    ensureInSwing()
    rootComponent
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

  private def ensureInSwing(): Unit = {
    require(SwingUtilities.isEventDispatchThread,
            "The Context methods shall be called on an AWT dispatch thread")
  }

  private def identifyingString(entity: DisplayedEntity): String = {
    entity.getClass.getName + "@" + System.identityHashCode(entity)
  }
}
