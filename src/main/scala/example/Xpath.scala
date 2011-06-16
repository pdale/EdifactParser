package example
import java.lang.Integer

trait Xpath {
 
 def bracketXpath(xpath:String) = if (xpath ne "") "[%s]".format(xpath) else ""


 implicit def xpathJoinI(descriptors:List[Option[String]]) = new { 
   def xpathJoin = 
     descriptors.filter(!_.isEmpty).map(_.get).reduceLeftOption(_ + " and " + _)
  }
 

 def segmentXpath(segment:Segment):String = {
   val id = segment.name match {
     case "" => None
     case s:String => Some("""@Id="%s"""".format(s))
   }
   def subXpath(subelements:List[Subelement]):Option[String] = {
     val subs = subelements.zipWithIndex

                                  val ambiguous = subs.map(subelement => subelement._1 match {
                                     case a:Ambiguous => a.contents match {
                                       case "" => None
                                       case c:String => Some("""descendant::text()="%s"""".format(c))
                                     }
                                    case s:Subelement => None
                                  }) xpathJoin

                                  val elementSubs = subs.map(subelement => subelement._1 match {
                                     case a:Ambiguous => None
                                     case s:Subelement => subelementXpath(s,subelement._2+1,true) match {
                                       case None => None
                                       case Some(x) => Some("subelement[%s]".format(x))
                                     }
                                 }) xpathJoin match {
                                    case None => None
                                    case Some(x) => Some("element[%s]".format(x))
                                 }
      List(ambiguous,elementSubs) xpathJoin
   } 

   /** The last element identifies the element we're looking for
    *  Any previous elements identify the particular segment.
    */
   val elementsIdentifierXpath:Option[String] = segment.elements.slice(0,segment.elements.size-1).
                                                      map (element => subXpath(element.subelements)) xpathJoin
                                                        

  List(id,elementsIdentifierXpath) xpathJoin match {
     case None => ""
     case Some(x) => "[%s]".format(x)
   }
 }


 /**
  * This is used to generate the xpath for the element you are selecting.
  * An element is defined by it's segment's name, subelements, if it has any.
  * The last subelement is expected to be empty and is not
  * used to identify the element.
  */
 def selectedElementXpath(element:Element,segment:Segment):String = {
   val segmentName = segment.name
   val index = segment.elements.size
   val id = segmentName match {
     case "" => None
     case name:String => Some("""@Id="%s0%s"""".format(name,index))
   }
   val subs = element.subelements
   val subXpath = subs.slice(0,subs.size-1).zipWithIndex.
                            map(sub => subelementXpath(sub._1,sub._2+1,true)).
                              filter(!_.isEmpty).map(x => Some("subelement[%s]".format(x.get))) xpathJoin

   // Oracle won't let you do contains() on mixed elements, they have to be all text
   // so in the case that we're searching at the element level we have to specifically tell
   // it to ignore the elements with subelements
   val directElementText = optionalSubelement(segment) match {
                             case "" => Some("not(subelement)")
                             case _    => None 
   }

   List(id,subXpath,directElementText) xpathJoin match {
     case None => ""
     case Some(s) => "[%s]".format(s)
   }
 }

 /**
  * A subelement is identified by it's Sequence (position) and contents (if any).
  */
 def subelementXpath(subelement:Subelement,index:Integer,noEmpty:Boolean=false):Option[String] = {
   val contents = subelement.contents match {
                     case "" => None
                     case c:String => Some("""text()="%s"""".format(c))
                  }
   val id = Some("""@Sequence="%s"""".format(index))

   if (noEmpty) {
    contents match {
      case None => None
      case Some(s) => List(id,contents) xpathJoin
    }
   } else {
     List(id,contents) xpathJoin
   }
 }
 def selectedSubelementXpath(element:Element):String = {
   subelementXpath(element.subelements.last,element.subelements.size) match {
     case None => ""
     case Some(s) => "[%s]".format(s)
   }
 }
 def optionalSubelement(segment:Segment):String = {
   val element = segment.elements.last
   val index = segment.elements.size
   element.subelements.last match {
     case a:Ambiguous => ""
     case s:Subelement => "/subelement%s".format(selectedSubelementXpath(element))
   }
 }

}
