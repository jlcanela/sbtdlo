
/**
 * Created by IntelliJ IDEA.
 * User: rstradling
 * Date: Aug 17, 2009
 * Time: 9:02:51 AM
 * To change this template use File | Settings | File Templates.
 */
import java.io._
import scala.util.parsing.combinator._
import scala.io.Source
import syntactical.StandardTokenParsers

/**
 * This class contains the column information for the table to create
 */
class Column(val name : String, val typ : String, val keyType : Option[String] ) {
  override def toString = "Column== Name=" + name + " type=" + typ + " keyType=" + keyType
}

/**
 * This class contains the parameters for the SQL DDL statements
 */
class Parameter(val name : String ) extends RegexParsers {
  def dataName: Parser[String] = """[a-zA-Z_]\w*""".r
  def dataType: Parser[String] = "INTEGER" | "INT" | "BIGINT" | "BIT" | "DECIMAL" | "INTEGER" | "MONEY" | "NUMERIC" |
                                   "SMALLINT" |
                                  "SMALLMONEY" | "TINYINT" | "FLOAT" | "DATE" | "DATETIME2" | "DATETIME" | "REAL" |
                                  "TIME" | "CHAR" | "TEXT" | "VARCHAR" | "NCHAR" | "NTEXT" | "NVARCHAR" | "BINARY" |
                                  "IMAGE" | "VARBINARY"      
  def Null : Parser[String] = "NULL" | "NOT NULL"
  def newKeyType : Parser[String] = "PRIMARY KEY" //| "UNIQUE"
  def parameters : Parser[Column] = dataName ~ dataType ~ opt(newKeyType) ^^
          {case nme ~ dt ~ keytyp =>  //println ("values = " + keytyp)
                                   var newName = nme.replace("type", "typeOf")
                                   new Column(newName, dt, keytyp)}

  /**
   * Returns a column instance if there was a successful parse
   */
  def DoMatch() = {
       //println ("name = " + name)
       parse(parameters, name) match {
        case Success(item, _) => item
        case Failure(msg, _) => throw new RuntimeException(msg)
        case Error(msg, _) => throw new RuntimeException(msg)
       }
  }
}

/**
 * The class that represents the CREATE TABLE
 */
class Instruction(val action : String, val actionOn : String, val item : String, val columns : List[Column]) {
  override def toString = "Action=" + action + " actionOn=" + actionOn + " item=" + item + " columns=" + columns +
                           " Columns Count= " + columns.size


}

/**
 * Parses the sql ddl data
 */
class DDLParser extends RegexParsers {
  def create: Parser[String] = "CREATE"
  def table: Parser[String] = "TABLE"
  def item: Parser[String] = """[a-zA-Z_]\w*""".r
  def IndexCreate: Parser[Any] = "CREATE INDEX" ~ """.*""".r
  def template : Parser[Any] = instr | IndexCreate
  def instr: Parser[Instruction] = create ~ table ~ item ~ ColumnInfo ~ ";" ^^
          {case create ~ table ~ itm ~ col ~ semi  => new Instruction(create, table, CapitalizeFirstLetter(itm), col) }
  def ColumnInfo : Parser[List[Column]] = "("~> repsep(ColumnData,",") <~")" ^^
          {case data => for (param <- data) yield ((new Parameter(param)).DoMatch()) }
  def ColumnData : Parser[String] = """(\w*\s*)*""".r
  def CapitalizeFirstLetter(word : String) = {
    val firstLetter = word.substring(0,1)
    val remainder = word.substring(1)
    firstLetter.toUpperCase() + remainder
  }

  /**
   * Writes the column data to the print stream passed in representing
   * the class information
   *
   * @param columns The columns to process
   * @param p The print stream
   * @return  Unit
   */
  def WriteColumnDataToClass(columns : List[Column], p : PrintStream) = {
      def MapTypes(column : Column) = {
        column.typ match {
          case "INTEGER" => "MappedLong(this)"
          case "INT" => "MappedLong(this)"
          case "TEXT" => "MappedString(this, 100)"
          case "NUMERIC" => "MappedDouble(this)"
          case _ => throw new RuntimeException("Failure to parse name " + column.typ)
        }
      }
      for (column <- columns ) {
        column.keyType match {
          case None => p.println("  object " + column.name + " extends " + MapTypes(column))
          case Some(s) => () // Do nothing because we ignore the uniqe key
        }
      }
  }

  /**Writes the column data to the object class using the print stream passed in
   *
   * @param columns The columns to process
   * @param p The print stream to write to
   * @return Unit
   */
  def WriteColumnDataToObject(columns : List[Column], p : PrintStream) = {
    def WriteColumn(count : Int, name : String) = {
      if (count == 0) p.print(name)
      else p.print(", " + name)
    }
    p.print("  override def fieldOrder = List(")
    var counter = 0
    for (column <- columns) {
      column.keyType match {
        case None => WriteColumn(counter, column.name); counter=counter+1; ()
        case Some(s) => ()
      }
    }
    p.print(")")
    p.println("")
  }

  /**
   * Processes the instruction creating an Lift orm for it
   *
   * @param item The instruction to process
   * @return Unit
   */

  def processItem(item : Instruction) = {
    // Create the file information
    val filename = ParseExpr.path + item.item + ".scala"
    val out = new FileOutputStream(filename)
    val p = new PrintStream(out)

    p.println("package com.eanc.careerlogic\n")
    p.println("import _root_.net.liftweb.mapper._\n")
    p.println("class " + item.item + " extends LongKeyedMapper[" + item.item + "] with IdPK {")
      p.println("  def getSingleton = " + item.item)
      WriteColumnDataToClass(item.columns, p)
    p.println("}")

    // Write out the object information
    p.println("object " + item.item + " extends " + item.item + " with LongKeyedMetaMapper[" + item.item + "] {")
      WriteColumnDataToObject(item.columns, p)
    p.println("}")

    p.close()

  }

  /**
   * Matches the input line to the parser above
   * <p>
   * Currently does nothing unless it is a CREATE TABLE construct. IGNORES Create INDEX construct, and errors
   * on anything else
   */
  def DoMatch(line : String, lineNum : Int) = {
    parse(template, line) match {
      case Success(item, _) => item match {
                                 case s : Instruction => processItem(s)
                                 case _  => () // Do nothing
                              }
      case Failure(msg, _) => val errorMsg = "Error " + msg + " at line number " + lineNum
                              throw new RuntimeException(errorMsg)
      case Error(msg, _) => val errorMsg = "Error " + msg + " at line number " + lineNum
                              throw new RuntimeException(errorMsg)
    }
  }
}

object ParseExpr {
  var path : String = _
  def main(args: Array[String]) {
    val parser = new DDLParser
    var lineNum = 1;
    path = args(1)
    for (line <- Source.fromFile(args(0)).getLines) {
      println("Processing line " + lineNum + " Info= " + parser.DoMatch(line, lineNum))
      lineNum = lineNum + 1
    }
  }
}
