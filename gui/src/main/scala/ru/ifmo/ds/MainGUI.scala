package ru.ifmo.ds

import java.awt.{BorderLayout, Dimension, Frame}

import javax.swing._

object MainGUI {
  def main(args: Array[String]): Unit = {
    System.setProperty("awt.useSystemAAFontSettings", "on")
    System.setProperty("swing.aatext", "true")

    SwingUtilities.invokeLater(() => {
      val frame = new JFrame("DataSlicer Graphical Console")
      val editablePane = new JPanel(true)
      editablePane.setMinimumSize(new Dimension(400, 300))
      val consolePane = new ConsolePane.Builder()
        .addBinding("pane", editablePane).addFieldHelp("pane: JPanel -- the panel above")
        .result()

      val consoleScroll = new JScrollPane(
        consolePane,
        ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
      val split = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, editablePane, consoleScroll)
      split.setDividerLocation(0.8)

      frame.setLayout(new BorderLayout())
      frame.add(split, BorderLayout.CENTER)
      frame.setSize(800, 600)
      frame.setExtendedState(Frame.MAXIMIZED_BOTH)
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      frame.setVisible(true)
    })
  }
}
