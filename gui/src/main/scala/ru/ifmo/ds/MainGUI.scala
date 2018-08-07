package ru.ifmo.ds

import java.awt.{BorderLayout, Dimension, Frame}

import javax.swing._
import ru.ifmo.ds.gui._

object MainGUI {
  private[this] def startConsole(consoleEnvelope: JPanel, editablePane: JPanel,
                                 args: Array[String], starter: JPanel, split: JSplitPane): Unit = {
    consoleEnvelope.removeAll()
    consoleEnvelope.repaint()
    new Thread(() => {
      val consolePaneBuilder = new ConsolePane.Builder()
        .addBinding("args", "Array[String]", args).addFieldHelp("args: Array[String] -- the arguments to the application")
        .addBinding("pane", editablePane).addFieldHelp("pane: JPanel -- the panel above")
        .addPrelude(s"import ${JComponentExtensions.getClass.getCanonicalName.init}._")
        .addQuitHook(() => SwingUtilities.invokeLater(() => {
          consoleEnvelope.removeAll()
          consoleEnvelope.repaint()
          consoleEnvelope.add(starter)
          split.setDividerLocation(-1)
        }))
      SwingUtilities.invokeLater(() => {
        val consoleScroll = new JScrollPane(
          consolePaneBuilder.result(),
          ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
          ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS)
        consoleEnvelope.add(consoleScroll)
        split.setDividerLocation(0.7)
        consoleEnvelope.validate()
        consoleEnvelope.repaint()
      })
    }).start()
  }

  def main(args: Array[String]): Unit = {
    System.setProperty("awt.useSystemAAFontSettings", "on")
    System.setProperty("swing.aatext", "true")

    SwingUtilities.invokeLater(() => {
      val frame = new JFrame("DataSlicer Graphical Console")

      val editablePane = new JPanel(true)
      editablePane.setMinimumSize(new Dimension(400, 300))

      val consoleEnvelope = new JPanel(new BorderLayout())
      val split = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, editablePane, consoleEnvelope)

      val consoleStarter = new JPanel()
      val consoleStartButton = new JButton("Start Console")
      consoleStarter.add(consoleStartButton)
      consoleEnvelope.add(consoleStarter)
      consoleStartButton.addActionListener(_ => {
        startConsole(consoleEnvelope, editablePane, args, consoleStarter, split)
      })

      split.setOneTouchExpandable(true)
      split.setResizeWeight(1)

      frame.setLayout(new BorderLayout())
      frame.add(split, BorderLayout.CENTER)
      frame.setSize(800, 600)
      frame.setExtendedState(Frame.MAXIMIZED_BOTH)
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      frame.setVisible(true)
    })
  }
}
