package ru.ifmo.ds.gui.util

import javax.swing.{JComponent, JTabbedPane}

import ru.ifmo.ds.util.PartitionTree

object PartitionTreeUtils {
  def createTabbedPanes[T](tree: PartitionTree[String, T], function: T => JComponent): JComponent = tree match {
    case PartitionTree.Leaf(v) => function(v)
    case PartitionTree.Node(children) =>
      val rv = new JTabbedPane()
      for ((k, v) <- children) {
        rv.add(k, createTabbedPanes(v, function))
      }
      rv
  }
}
