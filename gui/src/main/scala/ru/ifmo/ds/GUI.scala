package ru.ifmo.ds

import java.awt.event.{KeyAdapter, KeyEvent}
import java.awt.{BorderLayout, Frame}
import java.io._
import java.util.concurrent.{Executors, Future, FutureTask}

import javax.swing._

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.ILoop

object GUI {
  private val nonGUIThreads = Executors.newSingleThreadExecutor()

  def main(args: Array[String]): Unit = {
    SwingUtilities.invokeLater(() => {
      val frame = new JFrame("DataSlicer Graphical Console")
      val editablePane = new Pane
      val consolePane = new JTextPane()
      val consoleScroll = new JScrollPane(
        consolePane,
        ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED)
      val split = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, editablePane, consoleScroll)
      split.setDividerLocation(0.8)

      bindInterpreter(consolePane, editablePane)

      frame.setLayout(new BorderLayout())
      frame.add(split, BorderLayout.CENTER)
      frame.setSize(800, 600)
      frame.setExtendedState(Frame.MAXIMIZED_BOTH)
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      frame.setVisible(true)
    })
  }

  private def bindInterpreter(consolePane: JTextPane, editablePane: Pane): Unit = {
    nonGUIThreads.submit(() => {
      val consolePrintWriter = new PrintWriter(new Writer() {
        override def write(chars: Array[Char], off: Int, len: Int): Unit = {
          val str = String.valueOf(chars, off, len)
          SwingUtilities.invokeLater(() => {
            val doc = consolePane.getStyledDocument
            val attr = consolePane.getCharacterAttributes
            doc.insertString(doc.getLength, str, attr)
          })
        }
        override def flush(): Unit = {}
        override def close(): Unit = {}
      })

      // TODO: redirect System.err and System.out; they are PrintStreams, so the above writer cannot be directly used.

      // TODO: text insertions (Ctrl+V etc) do not work (as if no text was inserted): key listeners are not called.

      // TODO: only the last character can be changed. Better than nothing but still rudimentary.

      val readerPart = new PipedReader()
      val writerPart = new PipedWriter(readerPart)

      SwingUtilities.invokeLater(() => {
        consolePane.addKeyListener(new KeyAdapter {
          private[this] val buffer = new StringBuilder()

          override def keyPressed(e: KeyEvent): Unit = {
            if (e.getKeyChar == 8 && buffer.isEmpty) {
              e.consume()
            }
            consolePane.setCaretPosition(consolePane.getDocument.getLength)
          }

          override def keyTyped(e: KeyEvent): Unit = {
            val char = e.getKeyChar
            if (char != 8) {
              buffer += char
            } else if (buffer.isEmpty) {
              e.consume()
            } else {
              buffer.length -= 1
            }
            if (char == 13 || char == 10) {
              writerPart.append(buffer)
              writerPart.flush()
              buffer.clear()
            }
            consolePane.setCaretPosition(consolePane.getDocument.getLength)
          }
        })
      })

      val settings = new Settings()
      settings.embeddedDefaults[GUI.type]
      settings.usejavacp.value = true
      val loop = new ILoop(new BufferedReader(readerPart), consolePrintWriter) {
        override def createInterpreter(): Unit = {
          super.createInterpreter()
          intp.directBind("pane", editablePane)
        }

        override def closeInterpreter(): Unit = {
          super.closeInterpreter()
          System.exit(0)
        }
      }
      loop.process(settings)
      loop.loop()
    })
  }

  class Pane extends JPanel(true) {
    def inSwing[T](fun: => T): Unit = {
      if (SwingUtilities.isEventDispatchThread) {
        fun
      } else {
        SwingUtilities.invokeLater(() => fun)
      }
    }
    def submit[T](fun: => T): Future[T] = {
      if (SwingUtilities.isEventDispatchThread) {
        nonGUIThreads.submit(() => fun)
      } else new FutureTask[T](() => fun)
    }
  }
}
