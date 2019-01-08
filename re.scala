// Part 1 about Regular Expression Matching
//==========================================

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp   // alternative 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp   // sequence
case class STAR(r: Rexp) extends Rexp             // star


// some convenience for typing in regular expressions

import com.sun.org.apache.xpath.internal.operations.And

import scala.language.implicitConversions
import scala.language.reflectiveCalls

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s: String): Rexp = charlist2rexp(s.toList)

implicit def RexpOps (r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps (s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
}

// (1a) Complete the function nullable according to
// the definition given in the coursework; this 
// function checks whether a regular expression
// can match the empty string

def nullable (r: Rexp) : Boolean = r match{
  case ZERO => false
  case ONE => true
  case CHAR(c) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(r)=> true
}


// (1b) Complete the function der according to
// the definition given in the coursework; this
// function calculates the derivative of a 
// regular expression w.r.t. a character

def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c==d) {
    ONE
  } else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => if (nullable(r1)) ALT(SEQ(der(c,r1), r2), der(c,r2))
    else SEQ(der(c, r1),(r2))
  case STAR(r) => SEQ(der(c, r),(STAR(r)))
}


// (1c) Complete the function der according to
// the specification given in the coursework; this
// function simplifies a regular expression;
// however it does not simplify inside STAR-regular
// expressions

def simp(r: Rexp) : Rexp = r match {

  case SEQ(r1, r2) => {
    val firstVAL = simp(r1)
    val secondVAL = simp(r2)

    (firstVAL, secondVAL) match {
      case (_, ZERO) => ZERO
      case (ZERO,_) => ZERO
      case (_, ONE) => firstVAL
      case (ONE,_) => secondVAL
      case (_,_) => SEQ(firstVAL, secondVAL)
    }
  }

    case ALT(r1, r2) => {
      val firstVAL = simp(r1)
      val secondVAL = simp(r2)

      (firstVAL, secondVAL) match {
        case (ZERO,_) => secondVAL
        case (_,ZERO) => firstVAL
        case _=> if (firstVAL == secondVAL) firstVAL
        else ALT(firstVAL, secondVAL)
      }
    }
  case r => r
}
// (1d) Complete the two functions below; the first 
// calculates the derivative w.r.t. a string; the second
// is the regular expression matcher taking a regular
// expression and a string and checks whether the
// string matches the regular expression

def ders (s: List[Char], r: Rexp) : Rexp = s match{
  case Nil => r
  case c::s => ders(s, simp(der(c, r)))
}

def matcher(r: Rexp, s: String): Boolean = {
  nullable(ders(s.toList,r))
}


// (1e) Complete the function below: it searches (from the left to 
// right) in string s1 all the non-empty substrings that match the 
// regular expression -- these substrings are assumed to be
// the longest substrings matched by the regular expression and
// assumed to be non-overlapping. All these substrings in s1 are replaced
// by s2.

//def replace(r: Rexp, s1: String, s2: String): String = ...



// some testing data
// the supposedly 'evil' regular expression (a*)* b

val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))
println(matcher(EVIL, "a" * 1000 ++ "b"))
println(matcher(EVIL, "a" * 1000))


def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}

for (i <- 1 to 5000001 by 500000) {
  println(i + " " + "%.5f".format(time_needed(2, matcher(EVIL, "a" * i))))
}



