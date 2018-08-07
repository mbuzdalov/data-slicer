package ru.ifmo.ds.gui

import java.awt.event._
import java.awt.{BorderLayout, Color, Font}
import java.io._

import javax.swing._
import javax.swing.text._

import scala.collection.immutable.VectorBuilder
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.ILoop

class ConsolePane private (
  prelude: Seq[String],
  bindings: Seq[(String, String, Any)],
  private val helpFields: Seq[String],
  private val helpFunctions: Seq[String],
  private val helpUtils: Seq[String],
  private val quitHooks: Seq[Runnable]
) extends JTextPane { cp =>
  private val doc = new DefaultStyledDocument()
  private[this] val filter = new ConsoleFilter

  setStyledDocument(doc)
  doc.setDocumentFilter(filter)
  setBackground(Color.BLACK)
  setForeground(Color.LIGHT_GRAY)
  setCaretColor(Color.GREEN)
  setFont(new Font(Font.MONOSPACED, Font.BOLD, 12))

  private[this] val plainAttributes = getCharacterAttributes
  private[this] val errorTextAttributes = new SimpleAttributeSet()
  errorTextAttributes.addAttributes(plainAttributes)
  StyleConstants.setForeground(errorTextAttributes, Color.RED)

  private[this] val infoTextAttributes = new SimpleAttributeSet()
  infoTextAttributes.addAttributes(plainAttributes)
  StyleConstants.setForeground(infoTextAttributes, Color.BLUE)

  addKeyListener(new KeyAdapter {
    override def keyPressed(e: KeyEvent): Unit = {
      if (e.getKeyChar == KeyEvent.VK_ENTER) {
        // Enter: one needs to be at the end of the text for this to work.
        setCaretPosition(doc.getLength)
      } else if (getCaretPosition < filter.getFirstEditablePosition && e.getKeyChar >= ' ') {
        // One tries to write/paste somewhere in the unmodifiable section, which is useless.
        setCaretPosition(doc.getLength)
      }
      if (e.getExtendedKeyCode == KeyEvent.VK_HOME) {
        setCaretPosition(filter.getFirstEditablePosition)
        e.consume()
      } else if (e.getExtendedKeyCode == KeyEvent.VK_END) {
        setCaretPosition(doc.getLength)
        e.consume()
      }
    }
  })

  System.setOut(makeSwingOutputStream(plainAttributes, "system-out-to-swing"))
  System.setErr(makeSwingOutputStream(errorTextAttributes, "system-err-to-swing"))

  private[this] val scalaRunner = new Thread(() => {
    val settings = new Settings()
    settings.embeddedDefaults[ConsolePane]
    settings.usejavacp.value = true

    val loop = new MyLoop(filter.getScalaInput, new PrintWriter(System.out))
    loop.process(settings)
    loop.loop()
    ()
  }, "scala-repl-runner")
  scalaRunner.setDaemon(true)
  scalaRunner.start()

  private[this] class PipeDrainer(reader: Reader, attr: AttributeSet) extends Runnable {
    override def run(): Unit = readOut(Array.ofDim(8192))
    private[this] final def readOut(buffer: Array[Char]): Unit = {
      val howMuch = reader.read(buffer)
      if (howMuch.>(0)) {
        doc.insertString(doc.getLength, String.valueOf(buffer, 0, howMuch), attr)
        readOut(buffer)
      }
    }
  }

  private[this] def makeSwingOutputStream(attributeSet: AttributeSet, threadName: String): PrintStream = {
    val sysOutPipeIn = new PipedInputStream()
    val sysOutPipeOut = new PipedOutputStream(sysOutPipeIn)
    val sysOutPipeReader = new InputStreamReader(sysOutPipeIn, "utf-8")
    val thread = new Thread(new PipeDrainer(sysOutPipeReader, attributeSet), threadName)
    thread.setDaemon(true)
    thread.start()
    new PrintStream(sysOutPipeOut, true, "utf-8")
  }

  private[this] class MyLoop(reader: BufferedReader, writer: PrintWriter) extends ILoop(reader, writer) {
    override def createInterpreter(): Unit = {
      super.createInterpreter()
      doc.insertString(doc.getLength, "// initializing...\n", infoTextAttributes)
      intp.interpret("import java.awt._")
      intp.interpret("import java.awt.event._")
      intp.interpret("import javax.swing._")
      intp.interpret("import ru.ifmo.ds._")
      intp.interpret("import ru.ifmo.ds.gui._")

      mumly {
        intp.directBind("$$gui", new ConsolePane.Access(cp))
        intp.directBind("intp", intp)
        intp.interpret("import $$gui._")
        intp.interpret(s"import ${util.getClass.getCanonicalName.init}._")
      }

      bindings.foreach(t => intp.directBind(t._1, t._2, t._3))
      prelude.foreach(intp.interpret)

      intp.interpret("$$gui.help()")

      SwingUtilities.invokeLater(() => doc.insertString(doc.getLength, "\n", plainAttributes))
    }

    override def closeInterpreter(): Unit = {
      super.closeInterpreter()
      SwingUtilities.invokeLater(() => {
        quitHooks.foreach(_.run())
      })
    }
  }

  private[this] class ConsoleFilter extends DocumentFilter {
    private[this] val scalaInputReader = new PipedReader()
    private[this] val scalaInputWriter = new PipedWriter(scalaInputReader)

    private[this] var firstEditablePosition: Int = 0

    def getFirstEditablePosition: Int = firstEditablePosition
    def getScalaInput: BufferedReader = new BufferedReader(scalaInputReader)

    private[this] def updateNewlineByString(fb: DocumentFilter.FilterBypass, offset: Int, string: String): Unit = {
      if (!SwingUtilities.isEventDispatchThread) {
        // The update comes from Scala.
        val documentLength = fb.getDocument.getLength
        val documentLengthBeforeUpdate = documentLength - string.length
        // First of all, we know it must append only.
        assert(offset == documentLengthBeforeUpdate)
        // Everything from firstEditablePosition to the end is the current portion of the user input - flush it.
        if (documentLengthBeforeUpdate != firstEditablePosition) {
          scalaInputWriter.append(fb.getDocument.getText(firstEditablePosition,
            documentLengthBeforeUpdate - firstEditablePosition))
        }
        // The user shall not be able to erase - ensuring it.
        firstEditablePosition = documentLength
        setCaretPosition(firstEditablePosition)
      } else {
        // this is the user's update, which can come either from keyboard or from pasting
        val lastNewline = string.lastIndexOf('\n')
        if (lastNewline >= 0) {
          // there is at least one newline in the user's input - flush everything that comes before the last newline.
          val oldFirst = firstEditablePosition
          firstEditablePosition = lastNewline + offset + 1
          scalaInputWriter.append(fb.getDocument.getText(oldFirst, firstEditablePosition - oldFirst))
          scalaInputWriter.flush()
          setCaretPosition(fb.getDocument.getLength)
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
}

object ConsolePane {
  class Builder {
    private val prelude = new VectorBuilder[String]
    private val bindings = new VectorBuilder[(String, String, Any)]
    private val helpFields = new VectorBuilder[String]
    private val helpFunctions = new VectorBuilder[String]
    private val helpUtils = new VectorBuilder[String]
    private val quitHooks = new VectorBuilder[Runnable]

    private def setFrom(that: Builder): Builder = {
      prelude.clear();        prelude        ++= that.prelude.result()
      bindings.clear();       bindings       ++= that.bindings.result()
      helpFields.clear();     helpFields     ++= that.helpFields.result()
      helpFunctions.clear();  helpFunctions  ++= that.helpFunctions.result()
      helpUtils.clear();      helpUtils      ++= that.helpUtils.result()
      quitHooks.clear();      quitHooks      ++= that.quitHooks.result()
      this
    }

    helpFields += "console: JTextPane -- the text pane for this console"
    helpFields += "doc: AbstractDocument with StyledDocument -- this console's contents"
    helpFields += "intp: IMain -- the interpreter"

    helpFunctions += "help(): Unit -- print this help message"
    helpFunctions += "colorPrint(Color)(Any): Unit   -- print a colored message"
    helpFunctions += "colorPrintln(Color)(Any): Unit -- print a colored message with a newline"

    helpUtils += "inSwing(fun: => T): Unit         -- runs something in the Swing thread"
    helpUtils += "notInSwing(fun: => T): Future[T] -- runs something in a dedicated non-Swing thread"

    def addPrelude(line: String):      Builder = { prelude       += line; this }
    def addUtilHelp(line: String):     Builder = { helpUtils     += line; this }
    def addFieldHelp(line: String):    Builder = { helpFields    += line; this }
    def addFunctionHelp(line: String): Builder = { helpFunctions += line; this }

    def addBinding(name: String, tpe: String, value: Any): Builder = {
      bindings += ((name, tpe, value))
      this
    }

    def addQuitHook(fun: Runnable): Builder = { quitHooks += fun; this }

    def result(): ConsolePane = new ConsolePane(
      prelude = prelude.result(),
      bindings = bindings.result(),
      helpFields = helpFields.result(),
      helpFunctions = helpFunctions.result(),
      helpUtils = helpUtils.result(),
      quitHooks = quitHooks.result()
    )

    def resultBoundTo(boundPane: JPanel): JSplitPane = new Builder().setFrom(this).destructiveResultBoundTo(boundPane)

    private def destructiveResultBoundTo(boundPane: JPanel): JSplitPane = {
      def startConsole(consoleEnvelope: JPanel, boundPane: JPanel, split: JSplitPane): Unit = {
        val placeholderLabel = new JLabel("Starting the interpreter")
        placeholderLabel.setHorizontalAlignment(SwingConstants.CENTER)
        placeholderLabel.setVerticalAlignment(SwingConstants.CENTER)
        consoleEnvelope.removeAll()
        consoleEnvelope.add(placeholderLabel)
        split.setDividerLocation(0.7)
        consoleEnvelope.repaint()
        new Thread(() => {
          SwingUtilities.invokeLater(() => {
            val consoleScroll = new JScrollPane(
              result(),
              ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
              ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS)
            consoleEnvelope.removeAll()
            consoleEnvelope.add(consoleScroll)
            consoleEnvelope.validate()
            consoleEnvelope.repaint()
          })
        }).start()
      }

      val consoleEnvelope = new JPanel(new BorderLayout())
      val split = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, boundPane, consoleEnvelope)
      val consoleStarter = new JPanel()
      val consoleStartButton = new JButton("Start Console")
      consoleStarter.add(consoleStartButton)
      consoleEnvelope.add(consoleStarter)
      consoleStartButton.addActionListener(_ => {
        startConsole(consoleEnvelope, boundPane, split)
      })

      addBinding("pane", "javax.swing.JPanel", boundPane)
      addFieldHelp("pane: JPanel -- the panel above")
      addPrelude(s"import ${JComponentExtensions.getClass.getCanonicalName.init}._")
      addQuitHook(() => SwingUtilities.invokeLater(() => {
        consoleEnvelope.removeAll()
        consoleEnvelope.repaint()
        consoleEnvelope.add(consoleStarter)
        split.setDividerLocation(-1)
      }))

      split.setOneTouchExpandable(true)
      split.setResizeWeight(1)
      split
    }
  }

  final class Access private[ConsolePane] (cp: ConsolePane) {
    val console: JTextPane = cp
    val doc: AbstractDocument with StyledDocument = cp.doc

    def colorPrint(color: Color)(data: Any): Unit = util.notInSwing {
      val plainAttributes = console.getCharacterAttributes
      val currAttributes = new SimpleAttributeSet()
      currAttributes.addAttributes(plainAttributes)
      StyleConstants.setForeground(currAttributes, color)

      doc.insertString(doc.getLength, data.toString, currAttributes)
    }

    def colorPrintln(color: java.awt.Color)(data: Any): Unit = colorPrint(color)(data + "\n")

    override def toString: String = "current GUI"

    def help(): Unit = colorPrint(Color.GREEN)(helpText)

    def helpText: String = s"\nYou may use the following additional fields and methods:\n${
      cp.helpFields.mkString("  ", "\n  ", "\n")
    }\n${
      cp.helpFunctions.mkString("  ", "\n  ", "\n")
    }\n${
      cp.helpUtils.mkString("  ", "\n  ", "")
    }"
  }
}
