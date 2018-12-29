package ru.ifmo.ds.gui.actions

import java.awt.Insets

import javax.swing.{Icon, JButton, SwingUtilities}

abstract class EntityAction(name: String, icon: Icon) {
  protected def performImpl(): Unit

  def perform(): Unit = {
    require(SwingUtilities.isEventDispatchThread, "Entity actions should be performed in the Swing thread")
    performImpl()
  }

  def makeButton(): JButton = {
    val rv = new JButton(icon)
    rv.addActionListener(_ => perform())
    rv.setMargin(new Insets(0, 0, 0, 0))
    rv.setIconTextGap(0)
    rv.setContentAreaFilled(false)
    rv.setToolTipText(name)
    rv
  }
}
