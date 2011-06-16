package example

import java.lang.Integer
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.matching.Regex

object Edi {
  /**
   * Will parse a string in EDI Syntax style and return an xpath for the corresponding edi-xml
   * See EdiSpecs for examples.
   */
  def toXpath(ediPath:String): String = {
    val parseResult = EdiParser.parse(ediPath)
    val segment = parseResult.getOrElse(throw ParseFailure(parseResult.toString))
    segment.asXpath
  }
}
object EdiParser extends JavaTokenParsers {
    def regexMatch(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
      def apply(in: Input) = {
	    val source = in.source
	    val offset = in.offset
	    val start = handleWhiteSpace(source, offset)
	      (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
	        case Some(matched) =>
	          Success(matched,
	                  in.drop(start + matched.end - offset))
	        case None =>
	          Failure("string matching regex `"+r+"' expected but `"+in.first+"' found", in.drop(start - offset))
	      }
	    }
	  }
  val segmentRegex = new Regex("([A-Z][A-Z][A-Z])")
  val subelementRegex = new Regex("([^:+']|(?<=\\?)[:+'])*")

  def segment:Parser[Segment] = (opt(regexMatch(segmentRegex)) <~ "+") ~ repsep(element,"+") ^^ {
    case segment ~ elements => {  
         segment match {
           case None => new Segment("",elements) 
           case Some(s) => new Segment(s.group(0),elements) 
         } 
    }  
  }
  def element:Parser[Element] = repsep(regexMatch(subelementRegex),":") ^^ {
    case subelements => {
      val subs = subelements match {
        case subs:List[_] => {
         subelements map ( se => se match { case contents:Regex.Match => new Subelement(contents.group(0)) } )
        }
      }

      // underscore handling, if the last element is an _ it should be replaced with an empty element 
      val underscoreIndex = subs.map(_.contents).indexOf("_")
      val withlastElement = if (underscoreIndex >= 0) {
        subs.slice(0,underscoreIndex-1) ++ (new Subelement("")::Nil)
      } else { // if the string ends with : then add an empty subelement to the end.
        subs 
      }
      // this is catching the case of an + with nothing behind it. Needs to be populated with a Subelement(""), not Nil
      val completeElements = withlastElement ++ {if (withlastElement.isEmpty) new Subelement("")::Nil else Nil}
      
      if (subs.size == 1) {
        new Element(List(new Ambiguous(subs.head.contents)))
      } else {
        new Element(completeElements)
      }
    }
  }
  def parse(ediPath: String)  = {
    parseAll(segment,ediPath) 

  } 
  def parseElement(ediPath: String)  = {
    parseAll(element,ediPath) 

  } 
}

case class ParseFailure(error:String) extends RuntimeException(error)
case class Segment(val name:String, elements:List[Element]) extends Xpath {
  def asEdifact:String = "%s+%s".format(name,elements.map (_.asEdifact) reduceLeft (_ + "+" + _))

  def asXpath = {
    
    val nameXpath = if (name != "") Some("""@Id="%s"""".format(name)) else None

    
    "//segment%s/element%s%s".format(
                                        segmentXpath(this),
                                        selectedElementXpath(elements.last,this),
                                        optionalSubelement(this))
  }

                  

}
case class Element(val subelements:List[Subelement]) {
  def asEdifact:String = subelements map (_.contents) reduceLeft (_ + ":" + _)
}
class Subelement(val contents:String) {
}
case class Ambiguous(c:String) extends Subelement(c)
