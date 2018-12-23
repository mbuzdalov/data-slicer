package ru.ifmo.ds.gui

import java.awt._
import java.awt.event._

import javax.imageio.ImageIO
import javax.swing._

abstract class DisplayedEntity(val inputEntities: Seq[DisplayedEntity],
                               container: EntityContainer,
                               icon: Icon,
                               initialName: String) extends AsyncEvaluationDAGNode(inputEntities) { self =>
  require(SwingUtilities.isEventDispatchThread,
          "The DisplayedEntity constructor shall be called on an AWT dispatch thread")

  private val mainUILayout = new CardLayout()
  private val mainUILayoutDependencies = "Dependencies"
  private val mainUILayoutEvaluating = "Evaluating"
  private val mainUILayoutReady = "Ready"
  private val mainUI = new JPanel(mainUILayout)
  private val isAfterVariableInitialization = true
  private var isAlive = true
  private val displayUI = new JPanel(new BorderLayout(3, 3))
  private val nameLabel = new EditableLabel(initialName)
  private val reloadRemovePane = new JPanel(new GridLayout(1, 2))
  private val displayOnPressListener = new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = container.show(self)
  }
  private val evaluatingLabel = DisplayedEntity.makeLargeLabel("Evaluating...")
  private val dependenciesLabel = DisplayedEntity.makeLargeLabel("Waiting for dependencies...")

  /* intentionally no init, is initialized before the constructor runs */
  private var areDependenciesReadyInConstructor: Boolean = _

  mainUI.add(evaluatingLabel, mainUILayoutEvaluating)
  mainUI.add(dependenciesLabel, mainUILayoutDependencies)
  mainUI.add(makeMainUI(), mainUILayoutReady)

  mainUILayout.show(mainUI, if (areDependenciesReadyInConstructor) mainUILayoutEvaluating else mainUILayoutDependencies)

  displayUI.addMouseListener(displayOnPressListener)
  nameLabel.addMouseListener(displayOnPressListener)

  reloadRemovePane.add(DisplayedEntity.makeSmallButton(DisplayedEntity.reloadIcon, _ => initiateReloading()))
  reloadRemovePane.add(DisplayedEntity.makeSmallButton(DisplayedEntity.removeIcon, _ => tryDispose()))

  displayUI.add(new JLabel(icon), BorderLayout.LINE_START)
  displayUI.add(nameLabel, BorderLayout.CENTER)
  displayUI.add(reloadRemovePane, BorderLayout.LINE_END)

  container.add(this)

  protected def makeMainUI(): JComponent /* called from Swing */

  def derive(newEntities: Seq[DisplayedEntity]): DisplayedEntity /* called from Swing */

  override protected def notifyWaitingForDependencies(): Unit = {
    super.notifyWaitingForDependencies()
    if (isAfterVariableInitialization) {
      ensureIsAlive()
      mainUILayout.show(mainUI, mainUILayoutDependencies)
    } else {
      areDependenciesReadyInConstructor = false
    }
  }

  override protected def notifyEvaluationStarting(): Unit = {
    super.notifyEvaluationStarting()
    if (isAfterVariableInitialization) {
      ensureIsAlive()
      mainUILayout.show(mainUI, mainUILayoutEvaluating)
    } else {
      areDependenciesReadyInConstructor = true
    }
  }

  override protected def notifyEvaluationFinished(): Unit = {
    super.notifyEvaluationFinished()
    ensureIsAlive()
    mainUILayout.show(mainUI, mainUILayoutReady)
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

object DisplayedEntity {
  val reloadIcon = new ImageIcon(ImageIO.read(classOf[DisplayedEntity].getResource("reload.png")))
  val removeIcon = new ImageIcon(ImageIO.read(classOf[DisplayedEntity].getResource("remove.png")))
  val chartIcon = new ImageIcon(ImageIO.read(classOf[DisplayedEntity].getResource("chart.png")))

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
