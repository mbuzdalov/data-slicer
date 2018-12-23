package ru.ifmo.ds

import java.awt.{Frame, GridLayout}

import javax.swing._

import ru.ifmo.ds.gui.{EntityContainer, DisplayedEntity}

object MainGUI {
  private class TestDisplay(parents: Seq[DisplayedEntity], context: EntityContainer, text: String)
    extends DisplayedEntity(parents, context, DisplayedEntity.chartIcon, text) {
    override protected def makeMainUI(): JComponent = {
      val rv = new JLabel(text)
      rv.setHorizontalAlignment(SwingConstants.CENTER)
      rv.setVerticalAlignment(SwingConstants.CENTER)
      rv.setFont(rv.getFont.deriveFont(28.0f))
      rv
    }

    override protected def runEvaluation(): Unit = {
      Thread.sleep(2000)
    }
  }

  def main(args: Array[String]): Unit = {
    System.setProperty("awt.useSystemAAFontSettings", "on")
    System.setProperty("swing.aatext", "true")

    SwingUtilities.invokeLater(() => {
      val context = new EntityContainer
      val t0 = new TestDisplay(Seq.empty, context, "Test!")
      for (i <- 1 to 10) {
        new TestDisplay(Seq(t0), context, "Dependent!" + i)
      }

      val frame = new JFrame("DataSlicer")
      frame.setLayout(new GridLayout(1, 1))
      frame.add(context.root)
      frame.setExtendedState(Frame.MAXIMIZED_BOTH)
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      frame.setVisible(true)
    })
  }
}
