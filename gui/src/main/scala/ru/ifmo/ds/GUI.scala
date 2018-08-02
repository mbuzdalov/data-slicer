package ru.ifmo.ds

import java.awt.event.{KeyAdapter, KeyEvent}
import java.awt.{BorderLayout, Color, Frame}
import java.io._
import java.util.concurrent.{Executors, Future, FutureTask}

import javax.swing._
import javax.swing.text.{AttributeSet, SimpleAttributeSet, StyleConstants}

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.ILoop

class GUI(val pane: JPanel, val console: JTextPane) {
  def inSwing[T](fun: => T): Unit = {
    if (SwingUtilities.isEventDispatchThread) {
      fun
    } else {
      SwingUtilities.invokeLater(() => fun)
    }
  }

  def submit[T](fun: => T): Future[T] = {
    if (SwingUtilities.isEventDispatchThread) {
      GUI.nonGUIThreads.submit(() => fun)
    } else new FutureTask[T](() => fun)
  }

  def print(color: Color)(data: Any): Unit = {
    val plainAttributes = console.getCharacterAttributes
    val currAttributes = new SimpleAttributeSet()
    currAttributes.addAttributes(plainAttributes)
    StyleConstants.setForeground(currAttributes, color)

    SwingUtilities.invokeLater(new GUI.AppendString(console, data.toString, currAttributes))
  }

  def println(color: java.awt.Color)(data: Any): Unit = print(color)(data + "\n")

  override def toString: String = "current GUI"

  def help(): Unit = print(Color.GREEN) {
    """
      |You may use the following additional fields and methods:
      |    pane: JPanel -- the pane above
      |    console: JTextPane -- this console
      |
      |    print(java.awt.Color)(Any): Unit -- print a colored message
      |    println(java.awt.Color)(Any): Unit -- print a colored message with a newline
      |
      |    inSwing(fun: => T): Unit -- runs something in the Swing thread
      |    submit(fun: => T): Future[T] -- runs something in a dedicated non-Swing thread
    """.stripMargin
  }
}

object GUI {
  private val nonGUIThreads = Executors.newSingleThreadExecutor()

  def main(args: Array[String]): Unit = {
    System.setProperty("awt.useSystemAAFontSettings", "on")
    System.setProperty("swing.aatext", "true")

    SwingUtilities.invokeLater(() => {
      val frame = new JFrame("DataSlicer Graphical Console")
      val editablePane = new JPanel(true)
      val consolePane = new JTextPane()

      consolePane.setBackground(Color.BLACK)
      consolePane.setForeground(Color.LIGHT_GRAY)

      val consoleScroll = new JScrollPane(
        consolePane,
        ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
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

  private class AppendString(pane: JTextPane, str: String, attr: AttributeSet) extends Runnable {
    override def run(): Unit = {
      val doc = pane.getStyledDocument
      doc.insertString(doc.getLength, str, attr)
    }
  }

  private def bindInterpreter(consolePane: JTextPane, editablePane: JPanel): Unit = {
    val plainAttributes = consolePane.getCharacterAttributes
    val errorTextAttributes = new SimpleAttributeSet()
    errorTextAttributes.addAttributes(plainAttributes)
    StyleConstants.setForeground(errorTextAttributes, Color.RED)

    class PipeDrainer(reader: Reader, attr: AttributeSet) extends Runnable {
      override def run(): Unit = readOut(Array.ofDim(8192))
      private[this] final def readOut(buffer: Array[Char]): Unit = {
        val howMuch = reader.read(buffer)
        if (howMuch > 0) {
          SwingUtilities.invokeLater(new AppendString(consolePane, String.valueOf(buffer, 0, howMuch), attr))
          readOut(buffer)
        }
      }
    }

    def makeSwingOutputStream(attributeSet: AttributeSet, threadName: String): PrintStream = {
      val sysOutPipeIn = new PipedInputStream()
      val sysOutPipeOut = new PipedOutputStream(sysOutPipeIn)
      val sysOutPipeReader = new InputStreamReader(sysOutPipeIn, "utf-8")
      new Thread(new PipeDrainer(sysOutPipeReader, attributeSet), threadName).start()
      new PrintStream(sysOutPipeOut, true, "utf-8")
    }

    System.setOut(makeSwingOutputStream(plainAttributes, "system-out-to-swing"))
    System.setErr(makeSwingOutputStream(errorTextAttributes, "system-err-to-swing"))

    val gui = new GUI(editablePane, consolePane)

    nonGUIThreads.submit(() => {
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
      val loop = new ILoop(new BufferedReader(readerPart), new PrintWriter(System.out)) {
        override def createInterpreter(): Unit = {
          super.createInterpreter()
          intp.directBind("gui", gui)
          intp.interpret("import java.awt._")
          intp.interpret("import java.awt.event._")
          intp.interpret("import javax.swing._")
          intp.interpret("import gui._")
          intp.interpret("gui.help()")
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
}
