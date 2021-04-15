package com.github.mbuzdalov.swingasync.ng.demo

import java.awt.{BorderLayout, FlowLayout}

import javax.swing.{JComboBox, JFrame, JLabel, JPanel, SwingUtilities, WindowConstants}

import com.github.mbuzdalov.swingasync.ng.{Binding, Lifting, StatePrinter}

object Calculator {
  private def theMagicFunction(a: Int, b: Int, c: Int): Int = a * b + c

  def main(args: Array[String]): Unit = {
    System.setProperty("awt.useSystemAAFontSettings", "on")
    System.setProperty("swing.aatext", "true")

    SwingUtilities.invokeLater(() => {
      val firstComboBox, secondComboBox, thirdComboBox = new JComboBox[String](Array("0", "1", "2", "3", "4", "5", "What?"))
      val timesLabel = new JLabel("*")
      val plusLabel = new JLabel("+")
      val equalsLabel = new JLabel("=")
      val resultLabel = new JLabel("???")

      val panel = new JPanel(new FlowLayout())
      panel.add(firstComboBox)
      panel.add(timesLabel)
      panel.add(secondComboBox)
      panel.add(plusLabel)
      panel.add(thirdComboBox)
      panel.add(equalsLabel)
      panel.add(resultLabel)

      val frame = new JFrame("Calculator Messaging Test")
      frame.setLayout(new BorderLayout())
      frame.add(panel, BorderLayout.CENTER)
      frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
      frame.pack()
      frame.setVisible(true)

      val firstValue = Binding.fromJComboBox(firstComboBox)
      val secondValue = Binding.fromJComboBox(secondComboBox)
      val thirdValue = Binding.fromJComboBox(thirdComboBox)

      val parsingFunctor = Lifting.liftNoneToFail((a: String) => a.toIntOption, "Not int!")
      val firstParsedValue = parsingFunctor(firstValue)
      val secondParsedValue = parsingFunctor(secondValue)
      val thirdParsedValue = parsingFunctor(thirdValue)

      val liftedMagicFunction = Lifting.liftInSwing(theMagicFunction _)
      val finalValue = liftedMagicFunction(firstParsedValue, secondParsedValue, thirdParsedValue)
      val resultLabelListener = Binding.forJLabel(StatePrinter.toStringOrShortText, resultLabel)
      finalValue.addListener(resultLabelListener)
    })
  }
}
