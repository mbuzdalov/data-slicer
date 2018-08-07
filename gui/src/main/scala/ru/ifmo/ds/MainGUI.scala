package ru.ifmo.ds

import java.awt.{BorderLayout, Dimension, Frame}

import javax.swing._
import ru.ifmo.ds.gui.ConsolePane.Builder

object MainGUI {
  private[this] def designMainPane(): JPanel = {
    val editablePane = new JPanel(true)
    editablePane.setMinimumSize(new Dimension(400, 300))
    editablePane
  }

  def main(args: Array[String]): Unit = {
    System.setProperty("awt.useSystemAAFontSettings", "on")
    System.setProperty("swing.aatext", "true")

    SwingUtilities.invokeLater(() => {
      val frame = new JFrame("DataSlicer Graphical Console")
      val editablePane = designMainPane()

      val split = new Builder()
        .addBinding("args", "Array[String]", args)
        .addFieldHelp("args: Array[String] -- the arguments to the application")
        .resultBoundTo(editablePane)

      frame.setLayout(new BorderLayout())
      frame.add(split, BorderLayout.CENTER)
      frame.setSize(800, 600)
      frame.setExtendedState(Frame.MAXIMIZED_BOTH)
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      frame.setVisible(true)
    })
  }
}
