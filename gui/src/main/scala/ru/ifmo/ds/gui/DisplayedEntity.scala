package ru.ifmo.ds.gui

import java.awt.event._
import java.awt.{BorderLayout, Color, GridLayout, Insets}
import java.beans.PropertyChangeEvent

import javax.imageio.ImageIO
import javax.swing._

import scala.collection.mutable

abstract class DisplayedEntity(val inputEntities: Seq[DisplayedEntity],
                               container: EntityContainer,
                               icon: Icon,
                               initialName: String) { self =>
  require(SwingUtilities.isEventDispatchThread,
          "The DisplayedEntity constructor shall be called on an AWT dispatch thread")

  private val listeners = new mutable.ArrayBuffer[DisplayedEntity.StateListener]()
  private val children = new mutable.HashSet[DisplayedEntity]()
  private var mainUI: JComponent = _
  private var isAlive = true
  private val displayUI = new JPanel(new BorderLayout(3, 3))
  private val nameLabel = new EditableLabel(initialName)
  private val reloadRemovePane = new JPanel(new GridLayout(1, 2))
  private val displayOnPressListener = new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = container.show(self)
  }

  private val inputUpdateListener: DisplayedEntity.StateListener = new DisplayedEntity.StateListener {
    private val invalidatedSet = new mutable.HashSet[DisplayedEntity]()

    override def nameChanged(entity: DisplayedEntity, newName: String): Unit = {}
    override def contentsInvalidated(entity: DisplayedEntity): Unit = {
      if (invalidatedSet.isEmpty) {
        invalidateContents()
      }
      invalidatedSet += entity
    }

    override def contentsReady(entity: DisplayedEntity): Unit = {
      invalidatedSet -= entity
      if (invalidatedSet.isEmpty) {
        recomputeContents()
      }
    }
  }

  nameLabel.addPropertyChangeListener(EditableLabel.NameProperty, (evt: PropertyChangeEvent) => {
    val name = evt.getNewValue.toString
    listeners.foreach(_.nameChanged(this, name))
  })

  displayUI.addMouseListener(displayOnPressListener)
  nameLabel.addMouseListener(displayOnPressListener)

  reloadRemovePane.add(DisplayedEntity.makeSmallButton(DisplayedEntity.reloadIcon, _ => {invalidateContents(); recomputeContents()}))
  reloadRemovePane.add(DisplayedEntity.makeSmallButton(DisplayedEntity.removeIcon, _ => tryDispose()))

  displayUI.add(new JLabel(icon), BorderLayout.LINE_START)
  displayUI.add(nameLabel, BorderLayout.CENTER)
  displayUI.add(reloadRemovePane, BorderLayout.LINE_END)

  for (e <- inputEntities) {
    e.children += this
    e.listeners += inputUpdateListener
  }

  container.add(this)

  protected def invalidateContents(): Unit /* called from Swing */
  protected def recomputeContents(): Unit /* called from Swing */
  protected def makeMainUI(): JComponent /* called from Swing */

  final def getName: String = {
    ensureIsAlive()
    nameLabel.name
  }

  final def getDisplayUI: JComponent = {
    ensureIsAlive()
    displayUI
  }

  final def getMainUI: JComponent = {
    ensureIsAlive()
    mainUI = makeMainUI()
    mainUI
  }

  def dispose(): Unit = {
    ensureIsAlive()
    inputEntities.foreach(_.children -= this)
    container.remove(this)
    isAlive = false
  }

  private def tryDispose(): Unit = {
    if (children.isEmpty) {
      if (container.confirmRemoval(this)) {
        dispose()
      }
    } else {
      val desc = container.findAllDescendants(this).tail
      val defaultBackground = displayUI.getBackground
      new Thread(() => {
        for (c <- DisplayedEntity.fadingOutRedColors) {
          SwingUtilities.invokeAndWait(() => desc.foreach(_.displayUI.setBackground(c)))
          Thread.sleep(10)
        }
        SwingUtilities.invokeAndWait(() => desc.foreach(_.displayUI.setBackground(defaultBackground)))
      }).start()
    }
  }

  private def ensureIsAlive(): Unit = {
    require(SwingUtilities.isEventDispatchThread,
            "Public methods of DisplayedEntity shall be called on an AWT dispatch thread")
    if (!isAlive) {
      throw new IllegalStateException("dispose() has already been called on this DisplayedEntity")
    }
  }
}

object DisplayedEntity {
  // Free Mono Icon Set is distributed under Creative Commons Attribution-Noncommercial Works 3.0 Unported license.
  // Attribution link: gentleface.com
  val reloadIcon = new ImageIcon(ImageIO.read(classOf[DisplayedEntity].getResource("reload.png")))
  val removeIcon = new ImageIcon(ImageIO.read(classOf[DisplayedEntity].getResource("remove.png")))
  val chartIcon = new ImageIcon(ImageIO.read(classOf[DisplayedEntity].getResource("chart.png")))

  private def makeSmallButton(icon: Icon, action: ActionListener): JButton = {
    val rv = new JButton(icon)
    rv.addActionListener(action)
    rv.setMargin(new Insets(0, 0, 0, 0))
    rv.setIconTextGap(0)
    rv.setContentAreaFilled(false)
    rv
  }

  private val fadingOutRedColors = (0 until 100).map(i => new Color(255, i * 2, i * 2))

  trait StateListener {
    def nameChanged(entity: DisplayedEntity, newName: String): Unit
    def contentsInvalidated(entity: DisplayedEntity): Unit
    def contentsReady(entity: DisplayedEntity): Unit
  }
}
