package ru.ifmo.ds

import java.awt.event.{KeyAdapter, KeyEvent}
import java.awt.{BorderLayout, Color, Font, Frame}
import java.io._
import java.util.concurrent.{Executors, Future, FutureTask}

import javax.swing._
import javax.swing.text._

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.ILoop

class GUI(val pane: JPanel, val console: JTextPane, val doc: AbstractDocument with StyledDocument) {
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

  def colorPrint(color: Color)(data: Any): Unit = {
    val plainAttributes = console.getCharacterAttributes
    val currAttributes = new SimpleAttributeSet()
    currAttributes.addAttributes(plainAttributes)
    StyleConstants.setForeground(currAttributes, color)

    GUI.appendString(doc, data.toString, currAttributes)
  }

  def colorPrintln(color: java.awt.Color)(data: Any): Unit = colorPrint(color)(data + "\n")

  override def toString: String = "current GUI"

  def help(): Unit = colorPrintln(Color.GREEN) {
    """
      |You may use the following additional fields and methods:
      |    pane: JPanel -- the pane above
      |    console: JTextPane -- the text pane for this console
      |    doc: AbstractDocument with StyledDocument -- this console's contents
      |
      |    colorPrint(java.awt.Color)(Any): Unit -- print a colored message
      |    colorPrintln(java.awt.Color)(Any): Unit -- print a colored message with a newline
      |
      |    inSwing(fun: => T): Unit -- runs something in the Swing thread
      |    submit(fun: => T): Future[T] -- runs something in a dedicated non-Swing thread""".stripMargin
  }
}

object GUI {
  private val nonGUIThreads = Executors.newSingleThreadExecutor()

  private class ConsoleFilter(putCaretAction: Int => Unit) extends DocumentFilter {
    private[this] val scalaInputReader = new PipedReader()
    private[this] val scalaInputWriter = new PipedWriter(scalaInputReader)

    private[this] var firstEditablePosition: Int = 0

    def getScalaInput: BufferedReader = new BufferedReader(scalaInputReader)

    private[this] def updateNewlineByString(fb: DocumentFilter.FilterBypass, offset: Int, string: String): Unit = {
      if (!SwingUtilities.isEventDispatchThread) {
        // The update comes from Scala.
        val documentLength = fb.getDocument.getLength
        val documentLengthBeforeUpdate = documentLength - string.length
        // First of all, we know it must append only.
        assert(offset == documentLengthBeforeUpdate)
        // Everything from firstEditablePosition to the end is the current portion of the user input - flush it.
        scalaInputWriter.append(fb.getDocument.getText(firstEditablePosition, documentLengthBeforeUpdate - firstEditablePosition))
        // The user shall not be able to erase - ensuring it.
        firstEditablePosition = documentLength
        putCaretAction(firstEditablePosition)
      } else {
        // this is the user's update, which can come either from keyboard or from pasting
        val lastNewline = string.lastIndexOf('\n')
        if (lastNewline >= 0) {
          // there is at least one newline in the user's input - flush everything that comes before the last newline.
          val oldFirst = firstEditablePosition
          firstEditablePosition = lastNewline + offset + 1
          scalaInputWriter.append(fb.getDocument.getText(oldFirst, firstEditablePosition - oldFirst))
          putCaretAction(fb.getDocument.getLength)
        }
      }
    }

    override def remove(fb: DocumentFilter.FilterBypass, offset: Int, length: Int): Unit = {
      if (offset >= firstEditablePosition) {
        super.remove(fb, offset, length)
      }
    }

    override def insertString(fb: DocumentFilter.FilterBypass, offset: Int, string: String, attr: AttributeSet): Unit = {
      if (offset >= firstEditablePosition) {
        super.insertString(fb, offset, string, attr)
        updateNewlineByString(fb, offset, string)
      }
    }

    override def replace(fb: DocumentFilter.FilterBypass, offset: Int, length: Int, text: String, attrs: AttributeSet): Unit = {
      if (offset >= firstEditablePosition) {
        super.replace(fb, offset, length, text, attrs)
        updateNewlineByString(fb, offset, text)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    System.setProperty("awt.useSystemAAFontSettings", "on")
    System.setProperty("swing.aatext", "true")

    SwingUtilities.invokeLater(() => {
      val frame = new JFrame("DataSlicer Graphical Console")
      val editablePane = new JPanel(true)
      val doc = new DefaultStyledDocument()
      val consolePane = new JTextPane(doc)
      val filter = new ConsoleFilter(i => SwingUtilities.invokeLater(() => consolePane.setCaretPosition(i)))
      doc.setDocumentFilter(filter)

      consolePane.setBackground(Color.BLACK)
      consolePane.setForeground(Color.LIGHT_GRAY)
      consolePane.setCaretColor(Color.GREEN)
      consolePane.setFont(new Font(Font.MONOSPACED, Font.BOLD, 12))

      consolePane.addKeyListener(new KeyAdapter {
        override def keyPressed(e: KeyEvent): Unit = {
          if (e.getKeyChar == 10) {
            // Enter: one needs to be at the end of the text for this to work
            consolePane.setCaretPosition(doc.getLength)
          }
        }
      })

      val consoleScroll = new JScrollPane(
        consolePane,
        ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
      val split = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, editablePane, consoleScroll)
      split.setDividerLocation(0.8)

      bindInterpreter(consolePane, doc, filter, editablePane)

      frame.setLayout(new BorderLayout())
      frame.add(split, BorderLayout.CENTER)
      frame.setSize(800, 600)
      frame.setExtendedState(Frame.MAXIMIZED_BOTH)
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      frame.setVisible(true)
    })
  }

  private def appendString(doc: AbstractDocument, str: String, attr: AttributeSet): Unit = {
    doc.insertString(doc.getLength, str, attr)
  }

  private def bindInterpreter(consolePane: JTextPane,
                              doc: AbstractDocument with StyledDocument,
                              filter: ConsoleFilter,
                              editablePane: JPanel): Unit = {
    val plainAttributes = consolePane.getCharacterAttributes
    val errorTextAttributes = new SimpleAttributeSet()
    errorTextAttributes.addAttributes(plainAttributes)
    StyleConstants.setForeground(errorTextAttributes, Color.RED)

    class PipeDrainer(reader: Reader, attr: AttributeSet) extends Runnable {
      override def run(): Unit = readOut(Array.ofDim(8192))
      private[this] final def readOut(buffer: Array[Char]): Unit = {
        val howMuch = reader.read(buffer)
        if (howMuch.>(0)) {
          appendString(doc, String.valueOf(buffer, 0, howMuch), attr)
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

    val gui = new GUI(editablePane, consolePane, doc)

    nonGUIThreads.submit(() => {
      val settings = new Settings()
      settings.embeddedDefaults[GUI.type]
      settings.usejavacp.value = true

      val loop = new ILoop(new BufferedReader(filter.getScalaInput), new PrintWriter(System.out)) {
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
