package ru.ifmo.ds.gui.util

import java.awt.{BorderLayout, Insets}

import javax.swing._

class ManagedSplitter(left: JComponent, leftButtonIcon: Icon, leftButtonTooltip: String,
                      right: JComponent, rightButtonIcon: Icon, rightButtonTooltip: String,
                      sideBySideIcon: Icon, atopEachOtherIcon: Icon) extends JPanel {
  private val split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, left, right)
  private var lastHorizontalRelativeLocation = 0.5
  private var lastVerticalRelativeLocation = 0.5
  private val buttonRightOnly = ManagedSplitter.makeButton(rightButtonIcon, rightButtonTooltip)
  private val buttonLeftOnly = ManagedSplitter.makeButton(leftButtonIcon, leftButtonTooltip)
  private val buttonH = ManagedSplitter.makeButton(sideBySideIcon, "Show both side-to-side")
  private val buttonV = ManagedSplitter.makeButton(atopEachOtherIcon, "Show both atop one another")
  private val buttonPane = new JPanel()
  private val buttonPaneLayout = new GroupLayout(buttonPane)

  split.setResizeWeight(0.5)
  split.setDividerLocation(0.5)

  private def saveLocation(): Unit = {
    if (left.isVisible && right.isVisible) {
      if (split.getOrientation == JSplitPane.HORIZONTAL_SPLIT) {
        lastHorizontalRelativeLocation = split.getDividerLocation.toDouble / (split.getWidth - split.getDividerSize)
      } else {
        lastVerticalRelativeLocation = split.getDividerLocation.toDouble / (split.getHeight - split.getDividerSize)
      }
    }
  }

  buttonRightOnly.addActionListener(_ => {
    saveLocation()
    right.setVisible(true)
    left.setVisible(false)
    split.revalidate()
  })
  buttonLeftOnly.addActionListener(_ => {
    saveLocation()
    right.setVisible(false)
    left.setVisible(true)
    split.revalidate()
  })
  buttonH.addActionListener(_ => {
    saveLocation()
    right.setVisible(true)
    left.setVisible(true)
    split.setOrientation(JSplitPane.HORIZONTAL_SPLIT)
    split.setDividerLocation(lastHorizontalRelativeLocation)
    split.revalidate()
  })
  buttonV.addActionListener(_ => {
    saveLocation()
    right.setVisible(true)
    left.setVisible(true)
    split.setOrientation(JSplitPane.VERTICAL_SPLIT)
    split.setDividerLocation(lastVerticalRelativeLocation)
    split.revalidate()
  })

  buttonPaneLayout.setAutoCreateGaps(true)
  buttonPaneLayout.setAutoCreateContainerGaps(true)
  buttonPane.setLayout(buttonPaneLayout)

  buttonPaneLayout.setVerticalGroup(buttonPaneLayout
    .createSequentialGroup()
    .addGroup(buttonPaneLayout
      .createParallelGroup()
      .addComponent(buttonLeftOnly)
      .addComponent(buttonRightOnly))
    .addGroup(buttonPaneLayout
      .createParallelGroup()
      .addComponent(buttonH)
      .addComponent(buttonV)))
  buttonPaneLayout.setHorizontalGroup(buttonPaneLayout
    .createSequentialGroup()
    .addGroup(buttonPaneLayout
      .createParallelGroup()
      .addComponent(buttonLeftOnly)
      .addComponent(buttonH))
    .addGroup(buttonPaneLayout
      .createParallelGroup()
      .addComponent(buttonRightOnly)
      .addComponent(buttonV)))

  setLayout(new BorderLayout())
  add(split, BorderLayout.CENTER)
  add(buttonPane, BorderLayout.LINE_START)
}

object ManagedSplitter {
  private def makeButton(icon: Icon, text: String): JButton = {
    val rv = new JButton(icon)
    rv.setToolTipText(text)
    rv.setMargin(new Insets(0, 0, 0, 0))
    rv.setIconTextGap(0)
    rv.setContentAreaFilled(false)
    rv
  }
}
