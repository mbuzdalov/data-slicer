package ru.ifmo.ds.gui

import java.awt._
import java.awt.event._
import java.io.{PrintWriter, StringWriter}

import javax.swing._

import ru.ifmo.ds.gui.components.EditableLabel
import ru.ifmo.ds.gui.util.ImageLoadingFacilities

abstract class DisplayedEntity(val inputEntities: Seq[DisplayedEntity],
                               container: EntityContainer,
                               icon: Icon,
                               initialName: String) extends AsyncEvaluationDAGNode(inputEntities, initialName) { self =>
  require(SwingUtilities.isEventDispatchThread,
          "The DisplayedEntity constructor shall be called on an AWT dispatch thread")

  private val mainUILayout = new CardLayout()
  private val mainUILayoutConstructor = "Constructor"
  private val mainUILayoutDependencies = "Dependencies"
  private val mainUILayoutEvaluating = "Evaluating"
  private val mainUILayoutReady = "Ready"
  private val mainUILayoutFailed = "Failed"
  private val mainUI = new JPanel(mainUILayout)
  private var isAlive = true
  private val displayUI = new JPanel(new BorderLayout(3, 3))
  private val nameLabel = new EditableLabel(initialName)
  private val reloadRemovePane = new JPanel(new GridLayout(1, 2))
  private val displayOnPressListener = new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = container.show(self)
  }
  private val exceptionTextArea = new JTextArea()

  /**
    * This is the panel which the descending classes shall use to put their main UIs.
    */
  protected val theUI: JPanel = new JPanel(new GridLayout(1, 1))

  mainUI.add(DisplayedEntity.makeLargeLabel("Constructor is executed..."), mainUILayoutConstructor)
  mainUI.add(DisplayedEntity.makeLargeLabel("Evaluating..."), mainUILayoutEvaluating)
  mainUI.add(DisplayedEntity.makeLargeLabel("Waiting for dependencies..."), mainUILayoutDependencies)
  mainUI.add(theUI, mainUILayoutReady)
  mainUI.add(exceptionTextArea, mainUILayoutFailed)

  mainUILayout.show(mainUI, mainUILayoutConstructor)

  displayUI.addMouseListener(displayOnPressListener)
  nameLabel.addMouseListener(displayOnPressListener)

  reloadRemovePane.add(DisplayedEntity.makeSmallButton(DisplayedEntity.reloadIcon, _ => initiateReloading()))
  reloadRemovePane.add(DisplayedEntity.makeSmallButton(DisplayedEntity.removeIcon, _ => tryDispose()))

  displayUI.add(new JLabel(icon), BorderLayout.LINE_START)
  displayUI.add(nameLabel, BorderLayout.CENTER)
  displayUI.add(reloadRemovePane, BorderLayout.LINE_END)

  container.add(this)

  def derive(newEntities: Seq[DisplayedEntity]): DisplayedEntity /* called from Swing */

  override protected def notifyWaitingForDependencies(): Unit = {
    super.notifyWaitingForDependencies()
    ensureIsAlive()
    mainUILayout.show(mainUI, mainUILayoutDependencies)
  }

  override protected def notifyEvaluationStarting(): Unit = {
    super.notifyEvaluationStarting()
    ensureIsAlive()
    mainUILayout.show(mainUI, mainUILayoutEvaluating)
  }

  override protected def notifyEvaluationFinished(): Unit = {
    super.notifyEvaluationFinished()
    ensureIsAlive()
    mainUILayout.show(mainUI, mainUILayoutReady)
  }


  override protected def notifyEvaluationFailed(throwable: Throwable): Unit = {
    super.notifyEvaluationFailed(throwable)
    ensureIsAlive()
    val string = new StringWriter()
    val stream = new PrintWriter(string)
    throwable.printStackTrace(stream)
    stream.close()
    exceptionTextArea.setText(string.toString)
    mainUILayout.show(mainUI, mainUILayoutFailed)
  }

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
    mainUI
  }

  def dispose(): Unit = {
    ensureIsAlive()
    disconnectFromInputs()
    container.remove(this)
    isAlive = false
  }

  private def tryDispose(): Unit = {
    if (getChildren.isEmpty) {
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

object DisplayedEntity extends ImageLoadingFacilities {
  private val reloadIcon = imageFromResource("reload.png")
  private val removeIcon = imageFromResource("remove.png")

  private def makeLargeLabel(text: String): JLabel = {
    val rv = new JLabel(text)
    rv.setHorizontalAlignment(SwingConstants.CENTER)
    rv.setVerticalAlignment(SwingConstants.CENTER)
    rv.setFont(rv.getFont.deriveFont(28.0f))
    rv
  }

  private def makeSmallButton(icon: Icon, action: ActionListener): JButton = {
    val rv = new JButton(icon)
    rv.addActionListener(action)
    rv.setMargin(new Insets(0, 0, 0, 0))
    rv.setIconTextGap(0)
    rv.setContentAreaFilled(false)
    rv
  }

  private val fadingOutRedColors = (0 until 100).map(i => new Color(255, i * 2, i * 2))
}
