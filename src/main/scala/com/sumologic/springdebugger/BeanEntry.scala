package com.sumologic.springdebugger

import scala.collection.mutable.ListBuffer


class BeanEntry(val fullName: String, val registeredAtLogLine: Int) {

  def typeString: String = "Unknown"

  var clazz: String = _

  val fieldsInWarn = new ListBuffer[String]()

  val (parentName, shortName): (String, String) = {
    val index = fullName.lastIndexOf('.')
    if (index < 0) {
      ("", fullName)
    } else {
      val s = if (fullName.indexOf('(') > 0) {
        fullName.substring(index + 1, fullName.indexOf('('))
      } else {
        fullName.substring(fullName.lastIndexOf('.') + 1)
      }
      (fullName.substring(0, index), s)
    }
  }

}

class BeanClass(name: String, registeredAtLogLine: Int) extends BeanEntry(name, registeredAtLogLine) {
  override def typeString: String = "Class"
  var enhancedName: String = _
  clazz = name
}

class BeanItem(name: String, registeredAtLogLine: Int)
  extends BeanEntry(name, registeredAtLogLine) {

  var definedIn: BeanClass = _
}

class BeanField(name: String, registeredAtLogLine: Int)
  extends BeanItem(name, registeredAtLogLine) {
  override def typeString: String = "Field"
}

class BeanMethod(name: String, registeredAtLogLine: Int)
  extends BeanItem(name, registeredAtLogLine) {
  override def typeString: String = "Method"
}
