import java.io.{File, PrintWriter, StringReader}
import javax.xml.XMLConstants
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.soap.Node
import javax.xml.stream.{XMLEventReader, XMLInputFactory}
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.{SchemaFactory, Validator => SValidator}

import org.xml.sax._
import org.xml.sax.helpers.XMLFilterImpl

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Validator {
  def main(args: Array[String]) {
    require(args.length >= 3, "Params: xmlFile, xsdFile, output")

    time {
      val result =
        if (validate(args(0), args(1), args(2)))
          "Valid!"
        else
          "Not valid."
      println(result)
    }
  }

  def printToFile(str: String, validationErrors: ArrayBuffer[ParseException]): Unit = {
    val file = new File(str)
    file.createNewFile()
    val printWriter = new PrintWriter(file)
    printWriter.write(validationErrors.length.toString + "\n")
    val sb: StringBuilder = new StringBuilder()
    validationErrors.foreach(x => printString(x, sb))
    printWriter.append(sb.toString)
    printWriter.close()

  }

  def printString(error: ParseException, sb: StringBuilder): Unit = {
    error match {
      case parseException: ParseException =>
        val lineNumber = parseException.exception.getLineNumber
        val columnNumber = parseException.exception.getColumnNumber
        val message = parseException.exception.getMessage
        if (lineNumber != -1) sb.append("; lineNumber: ").append(lineNumber)
        if (columnNumber != -1) sb.append("; columnNumber: ").append(columnNumber)
        if (message != null) sb.append("; ").append(message)
        sb.append("\n")
    }
  }

  def validate(xmlFile: String, xsdFile: String, outputFile: String): Boolean = {
    val validator = SchemaFactory
      .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
      .newSchema(new StreamSource(xsdFile))
      .newValidator()
    val reader = XMLInputFactory
      .newFactory()
      .createXMLEventReader(new StreamSource(xmlFile))
    val handler = new AllErrorsHandler(reader, validator)
    //
    val anotherHandler = new DomErrorHandler(validator)


    val dbf = DocumentBuilderFactory.newInstance
    dbf.setNamespaceAware(true)
    val db = dbf.newDocumentBuilder()
    val doc = db.parse(new InputSource(new StringReader(xmlFile)))
    validator.setErrorHandler(anotherHandler)
    validator.validate(new DOMSource(doc))
    printToFile(outputFile, handler.validationErrors)
    handler.isValid
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000000 + "s")
    result
  }
}

class DomErrorHandler(val validator: SValidator) extends ErrorHandler {
  val property = "http://apache.org/xml/properties/dom/current-element-node"

  override def fatalError(exception: SAXParseException): Unit = warning(exception)

  override def error(exception: SAXParseException): Unit = warning(exception)

  override def warning(exception: SAXParseException): Unit = {
    var node: Node = null
    try {
      node = validator.getProperty(property).asInstanceOf[Node]
    } catch {
      case ex: SAXException => println(ex.getMessage)
      case _ =>
    }
  }
}

class AllErrorsHandler(val reader: XMLEventReader, val validator: SValidator) extends ErrorHandler {
  var validationErrors: ArrayBuffer[ParseException] = ArrayBuffer[ParseException]()

  override def fatalError(exception: SAXParseException): Unit = warning(exception)

  override def error(exception: SAXParseException): Unit = warning(exception)

  override def warning(exception: SAXParseException): Unit = validationErrors += ParseException(exception)


  def isValid: Boolean = validationErrors.isEmpty

}

class RecordingClassReader(parent: XMLReader) extends XMLFilterImpl(parent) {
  val queue: TwoQueue[String] = new TwoQueue[String]

  override def startElement(uri: String, localName: String, qName: String, atts: Attributes): Unit = {
    queue.add(qName)
    super.startElement(uri, localName, qName, atts)
  }

}

case class ParseException(exception: SAXParseException)

class TwoQueue[T] {
  private val queue = new mutable.Queue[T]

  def add(t: T): Unit = {
    if (queue.length < 2) queue.enqueue(t) else {
      queue.dequeue()
      queue.enqueue(t)
    }
  }

  def get(): Seq[T] = queue
}