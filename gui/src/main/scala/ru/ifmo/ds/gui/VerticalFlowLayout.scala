package ru.ifmo.ds.gui

import java.awt.{Component, Container, Dimension, LayoutManager}

class VerticalFlowLayout extends LayoutManager {
  override def addLayoutComponent(name: String, comp: Component): Unit = {}
  override def removeLayoutComponent(comp: Component): Unit = {}

  override def preferredLayoutSize(parent: Container): Dimension = collect(parent, _.getPreferredSize)
  override def minimumLayoutSize(parent: Container): Dimension = collect(parent, _.getMinimumSize)

  override def layoutContainer(parent: Container): Unit = parent.getTreeLock.synchronized {
    var height, i = 0
    val parentWidth = parent.getWidth
    val cc = parent.getComponentCount
    while (i < cc) {
      height += parent.getComponent(i).getPreferredSize.height
      i += 1
    }
    val usePreferred = height <= parent.getHeight
    i = 0
    height = 0
    while (i < cc) {
      val comp = parent.getComponent(i)
      val theHeight = if (usePreferred) comp.getPreferredSize.height else comp.getMinimumSize.height
      comp.setBounds(0, height, parentWidth, theHeight)
      i += 1
      height += theHeight
    }
  }

  private def collect(parent: Container, fun: Component => Dimension): Dimension = {
    parent.getTreeLock.synchronized {
      var width, height, i = 0
      val cc = parent.getComponentCount
      while (i < cc) {
        val compSize = fun(parent.getComponent(i))
        width = math.max(width, compSize.width)
        height += compSize.height
        i += 1
      }
      new Dimension(width, height)
    }
  }
}
