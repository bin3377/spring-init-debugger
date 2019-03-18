package com.sumologic.springdebugger

import java.io.File

import org.apache.commons.io.FileUtils

import scala.collection.mutable
import Logger.logger._

class LogParser(logPath: String) {

  val logFile = new File(logPath)

  val nameToClassMap = new mutable.HashMap[String, BeanClass]()

  val shortNameToClassMap = new mutable.HashMap[String, BeanClass]()

  val nameToMethodMap = new mutable.HashMap[String, BeanMethod]()

  val shortNameToMethodMap = new mutable.HashMap[String, BeanMethod]()

  val nameToFieldMap = new mutable.HashMap[String, BeanField]()

  val shortNameToFieldMap = new mutable.HashMap[(String, String), BeanField]()

  val classToMemberMap = new mutable.HashMap[BeanClass, mutable.ListBuffer[BeanItem]]()

  def loadUnits(): Unit = {

    val RootBeanClass = """.*STARTUP: Loading JavaConfig spring application context from: '(.*)'""".r
    val BeanFactory = """.*Bean factory for (.*) defining beans \[(.*)\]; root of factory hierarchy""".r
    val BeanSingletons = """.*Pre-instantiating singletons in (.*) defining beans \[(.*)\]; root of factory hierarchy""".r
    val BeanClassRegistered = """.*Registered bean definition for imported class '(.*)'$""".r
    val BeanClassEnhanced = """.*Successfully enhanced (.*); enhanced class name is: (.*)$""".r
    val BeanMethodRegistered = """.*Registering bean definition for @Bean method (.*)$""".r
    val AutowiredRegistered = """.*Registered injected element on class \[(.*)\]: AutowiredFieldElement for private (.*) (.*)""".r

    val it = FileUtils.lineIterator(logFile, "UTF-8")
    try {
      var counter = 0

      def dbg(line: String) = {
        debug(String.format("%1$-8s", counter.toString) + line)
      }

      while (it.hasNext) {
        val line = it.nextLine
        counter += 1
        line match {

          case BeanFactory(_, factory) =>
            val factories = factory.split(",")
            factories.filterNot(name => name.startsWith("org.springframework")).foreach { name =>
              // createBeanClass(name, counter)
            }

          case BeanSingletons(_, factory) =>
            val factories = factory.split(",")
            factories.filterNot(name => name.startsWith("org.springframework")).foreach { name =>
              // createBeanClass(name, counter)
            }

          case RootBeanClass(name) =>
            createBeanClass(name, counter)
            dbg(s"Root bean class is defined - $name")
            rootBeanClass = nameToClassMap(name)

          case BeanClassRegistered(name) =>
            createBeanClass(name, counter)
            dbg(s"Bean class is defined - $name")

          case BeanClassEnhanced(from, to) =>
            val beanClass = nameToClassMap(from)
            beanClass.enhancedName = to
            dbg(s"Bean class enhanced: $from => $to")

          case BeanMethodRegistered(name) =>
            createBeanMethod(name, counter)
            dbg(s"Bean method is defined - $name")

          case AutowiredRegistered(enhancedBeanName, clazz, fullName) =>
            createBeanField(fullName, counter, enhancedBeanName, clazz)
            dbg(s"Bean field is defined - $fullName")

          case _ =>
        }
      }
    } finally {
      if (it != null) {
        it.close()
      }
    }
  }

  def createBeanClass(name: String, counter: Int): Unit = {
    val bc = new BeanClass(name, counter)
    if (!nameToClassMap.contains(name)) {
      nameToClassMap.put(name, bc)
      shortNameToClassMap.put(bc.shortName, bc)
      classToMemberMap.put(bc, new mutable.ListBuffer[BeanItem]())
    } else {
      warn(s"$counter Duplicated class defined $name")
    }
  }

  def createBeanMethod(name: String, counter: Int): Unit = {
    val newItem = new BeanMethod(name, counter)
    if (!nameToClassMap.contains(newItem.parentName)) {
      throw new RuntimeException(s"Cannot find parent class when register a bean $name")
    }
    val beanClass = nameToClassMap(newItem.parentName)
    newItem.definedIn = beanClass
    if (nameToMethodMap.contains(newItem.fullName)) {
      throw new RuntimeException(s"Duplicated bean method entry defined ${newItem.fullName}")
    }
    nameToMethodMap.put(newItem.fullName, newItem)
    if (shortNameToMethodMap.contains(newItem.shortName)) {
      val oldItem = shortNameToMethodMap(newItem.shortName)
      warn(String.format("%1$-8s", counter.toString) + s"Register function '${newItem.shortName}' more than once: '${newItem.fullName}' will overwrite '${oldItem.fullName}'")
    }
    shortNameToMethodMap.put(newItem.shortName, newItem)
    classToMemberMap(beanClass) += newItem
  }

  def createBeanField(name: String, counter: Int, enhancedBeanName: String, clazz: String): Unit = {
    val newItem = new BeanField(name, counter)
    if (!nameToClassMap.contains(newItem.parentName)) {
      throw new RuntimeException(s"Cannot find parent class when register an autowired $name")
    }
    val beanClass = nameToClassMap(newItem.parentName)
    if (beanClass.fullName != enhancedBeanName && beanClass.enhancedName != enhancedBeanName) {
      throw new RuntimeException(s"bean enhanced class name not match: " +
        s"expected ${beanClass.enhancedName}, actual $enhancedBeanName")
    }
    newItem.definedIn = beanClass
    newItem.clazz = clazz
    if (nameToFieldMap.contains(newItem.fullName)) {
      throw new RuntimeException(s"Duplicated bean autowired entry defined ${newItem.fullName}")
    }
    nameToFieldMap.put(newItem.fullName, newItem)
    shortNameToFieldMap.put((beanClass.shortName, newItem.shortName), newItem)
    classToMemberMap(beanClass) += newItem
  }

  var rootBeanClass: BeanClass = _

  var dependencyTree: DependencyTreeNode = _

  val suspiciousNodes = new mutable.HashSet[DependencyTreeNode]

  def lookupClass(name: String): BeanClass = {
    if (nameToClassMap.contains(name)) {
      nameToClassMap(name)
    } else if (name.toLowerCase() == rootBeanClass.shortName.toLowerCase) {
      rootBeanClass
    } else if (shortNameToClassMap.contains(name)) {
      shortNameToClassMap(name)
    } else {
      // warn(s"Failed to lookup class with name $name")
      null
    }
  }

  def lookupMethod(currentClass: BeanClass, name: String): BeanMethod = {
    if (nameToMethodMap.contains(name)) {
      nameToMethodMap(name)
    } else if (nameToMethodMap.contains(s"${currentClass.fullName}.$name")) {
      nameToMethodMap(s"${currentClass.fullName}.$name")
    } else if (shortNameToMethodMap.contains(name)) {
      shortNameToMethodMap(name)
    } else {
      // warn(s"Failed to lookup method with name $name in context ${currentClass.fullName}")
      null
    }
  }

  def lookupField(currentClass: BeanClass, name: String): BeanField = {
    if (nameToFieldMap.contains(name)) {
      nameToFieldMap(name)
    } else if (nameToFieldMap.contains(s"${currentClass.fullName}.$name")) {
      nameToFieldMap(s"${currentClass.fullName}.$name")
    } else if (shortNameToFieldMap.contains(currentClass.shortName, name)) {
      shortNameToFieldMap(currentClass.shortName, name)
    } else {
      // warn(s"Failed to lookup field with name $name in context ${currentClass.fullName}")
      null
    }
  }

  def loadRelationships(): Unit = {

    val BeforeCreatingBeanInstance = """.*Creating instance of bean '(.*)'$""".r
    val AfterCreatingBeanInstance = """.*Finished creating instance of bean '(.*)'$""".r
    val BeforeProcessingAutowiredField = """.*Processing injected element of bean '(.*)': AutowiredFieldElement for private (.*) (.*)$""".r
    val AfterProcessingAutowiredField = """.*Autowiring by type from bean name '(.*)' to bean named '(.*)'$""".r

    val IssueDetected = """.*AutoWiringIssueFound! beanName: (.*), fieldName: (.*), fieldType: (.*)$""".r

    val classStack = new mutable.ArrayStack[BeanClass]()
    val treeStack = new mutable.ArrayStack[DependencyTreeNode]()

    var counter = 0

    def dbg(line: String) = {
      debug(String.format("%1$-8s", counter.toString) + line)
    }

    val it = FileUtils.lineIterator(logFile, "UTF-8")

    var currentClass = new BeanClass("<ROOT>", -1)
    dependencyTree = DependencyTreeNode(currentClass, null)
    var currentNode = dependencyTree

    treeStack.push(currentNode)
    classStack.push(currentClass)

    try {
      while (it.hasNext) {
        val line = it.nextLine
        counter += 1
        line match {
          case BeforeCreatingBeanInstance(name) =>
            if (name.startsWith("org.springframework")) {
            } else if (lookupClass(name) != null) {
              val operatingClass = lookupClass(name)
              val newNode = new DependencyTreeNode(operatingClass, currentNode)
              treeStack.push(currentNode)
              classStack.push(operatingClass)
              currentNode.dependencies += newNode
              currentNode = newNode
              currentClass = operatingClass
              dbg(s"Start to init class: ${currentNode.beanEntry.fullName}")
            } else if (lookupMethod(currentClass, name) != null) {
              val operatingMethod: BeanMethod = lookupMethod(currentClass, name)
              val newNode = DependencyTreeNode(operatingMethod, currentNode)
              treeStack.push(currentNode)
              currentNode.dependencies += newNode
              currentNode = newNode
              dbg(s"Start to init method: ${currentNode.beanEntry.fullName}")
            } else {
              warn(s"Start creating instance on unregistered entry ${currentClass.shortName}::$name")
            }
          case AfterCreatingBeanInstance(name) =>
            if (name.startsWith("org.springframework")) {
              dbg(s"skip spring framework class - $name")
            } else if (lookupClass(name) != null) {
              dbg(s"End to init class: ${currentNode.beanEntry.fullName}")
              currentNode = treeStack.pop()
              currentClass = classStack.pop()
            } else if (lookupMethod(currentClass, name) != null) {
              dbg(s"End to init method: ${currentNode.beanEntry.fullName}")
              currentNode = treeStack.pop()
            } else {
              warn(s"Finish creating instance on unregistered entry ${currentClass.shortName}::$name")
            }
          case BeforeProcessingAutowiredField(beanName, clazz, fieldName) =>
            val beanClass = lookupClass(beanName)
            val lookup = if (lookupField(beanClass, fieldName) == null) {
              lookupField(currentClass, fieldName)
            } else {
              lookupField(beanClass, fieldName)
            }
            if (lookup != null) {
              val newNode = DependencyTreeNode(lookup, currentNode)
              currentNode.dependencies += newNode
              if ("java.lang.String" == clazz) {
                dbg(s"$beanName::$fieldName is a string, no initializing required.")
              } else {
                treeStack.push(currentNode)
                currentNode = newNode
                dbg(s"Start to init field: ${currentNode.beanEntry.fullName} (${currentNode.beanEntry.clazz})")
              }
            } else {
              warn(s"Start creating field on unregistered entry $beanName, $fieldName ($clazz)")
            }
          case AfterProcessingAutowiredField(beanName, fieldName) =>
            dbg(s"End to init field: ${currentNode.beanEntry.fullName} (${currentNode.beanEntry.clazz})")
            currentNode = treeStack.pop()
          case IssueDetected(beanName, fieldName, fieldType) =>
            currentNode.beanEntry.fieldsInWarn += s"$fieldName($fieldType)"
            dbg(s"NPE detected: $beanName - $fieldName($fieldType)")
            suspiciousNodes += currentNode
          case _ =>
        }
      }
    } finally {
      if (it != null) {
        it.close()
      }
    }
  }

  def getDefinitions(): String = {

    val builder = new mutable.StringBuilder()

    def getLine(beanEntry: BeanEntry): String = {
      String.format("%1$-8s|", beanEntry.typeString) +
        String.format("%1$-50s|", beanEntry.shortName) +
        beanEntry.fullName
    }

    nameToClassMap.values.toList.sortBy(_.fullName).foreach { bc =>
      builder.append(getLine(bc)).append("\n")
      classToMemberMap(bc).toList.sortBy(_.fullName).foreach { entry =>
        builder.append(getLine(entry)).append("\n")
      }
      builder.append("\n")
    }
    builder.toString()
  }

  def getAllSuspiciousNodes: String = {
    val builder = new mutable.StringBuilder()
    suspiciousNodes.foreach { node =>
      builder.append("Suspicious creator: ").append("\n")
      builder.append("  ").append(node.beanEntry.fullName).append("\n")
      node.beanEntry.fieldsInWarn.foreach(f => builder.append("  ").append(s"* $f is null\n"))
      builder.append("Initialized from: ").append("\n")
      builder.append(DependencyTreeNode.getDependencyPath(node))
    }
    builder.toString()
  }

  def getDependencyTree: String = {
    DependencyTreeNode.getDependencyTree(dependencyTree)
  }
}

