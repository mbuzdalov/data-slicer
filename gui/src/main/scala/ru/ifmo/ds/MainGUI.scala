package ru.ifmo.ds

import java.awt.{Frame, GridLayout}

import javax.swing._

import ru.ifmo.ds.gui.EntityContainer

object MainGUI {
  def main(args: Array[String]): Unit = {
    System.setProperty("awt.useSystemAAFontSettings", "on")
    System.setProperty("swing.aatext", "true")

    SwingUtilities.invokeLater(() => {
      val context = new EntityContainer
      val frame = new JFrame("DataSlicer")
      frame.setLayout(new GridLayout(1, 1))
      frame.add(context.root)
      frame.setExtendedState(Frame.MAXIMIZED_BOTH)
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      frame.setVisible(true)
    })
  }
}
