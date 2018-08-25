package ru.ifmo.ds.cli

class CommandLineOption(val key: String,
                        val minArguments: Int = 0,
                        val maxArguments: Int = Int.MaxValue,
                        val isEnabledByDefault: Boolean = true,
                        callOnFirstAdd: => Any = {}) {
  private[this] var enabled = isEnabledByDefault
  private[this] var enabledStateChanged = false
  private[this] var reason: String = _
  private[this] val values = IndexedSeq.newBuilder[String]
  private[this] var nValues = 0

  def enable(): Unit = {
    enabled = true
    enabledStateChanged = true
  }

  def disable(reason: String): Unit = {
    enabled = false
    enabledStateChanged = true
    this.reason = reason
  }

  private[this] def checkIfTooFewValues(infixString: String): Unit = {
    if (nValues < minArguments) {
      throw new IllegalArgumentException(if (maxArguments == minArguments) {
        s"Option $key$infixString accepts exactly $minArguments arguments"
      } else {
        s"Option $key$infixString accepts at least $minArguments arguments"
      })
    }
  }

  def stopBlock(): Unit = checkIfTooFewValues("")

  def result(): IndexedSeq[String] = {
    checkIfTooFewValues(" is currently required and")
    values.result()
  }

  def resultOrElse(defaultValue: => IndexedSeq[String]): IndexedSeq[String] = try {
    result()
  } catch {
    case _: IllegalArgumentException => defaultValue
  }

  def add(value: String): Unit = if (enabled) {
    values += value
    if (nValues == 0) {
      callOnFirstAdd
    }
    nValues += 1
    if (nValues > maxArguments) {
      throw new IllegalArgumentException(if (minArguments == maxArguments) {
        s"Option $key accepts exactly $maxArguments arguments"
      } else {
        s"Option $key accepts at most $maxArguments arguments"
      })
    }
  } else {
    val errorMessage = if (enabledStateChanged) {
      if (reason != null) {
        s"Option $key is disabled: $reason"
      } else {
        s"Option $key is disabled"
      }
    } else {
      s"Option $key is disabled by default"
    }
    throw new IllegalArgumentException(errorMessage)
  }
}

object CommandLineOption {
  def submit(options: Iterable[CommandLineOption], args: String*): Unit = {
    val optionMap = options.map(o => (o.key, o)).toMap
    var currentOption: Option[CommandLineOption] = optionMap.get("")
    for (a <- args) {
      if (optionMap.contains(a)) {
        currentOption.foreach(_.stopBlock())
        currentOption = optionMap.get(a)
      } else {
        currentOption.getOrElse(
          throw new IllegalArgumentException(s"Cannot process an argument '$a' not being an argument to any option")
        ).add(a)
      }
    }
    currentOption.foreach(_.stopBlock())
  }
}
