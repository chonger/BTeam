package parse

object ParseTypes {

  type Symbol = Int
  type Split = Char
  type Terminal = Int
  type SSPair = Tuple2[Symbol,Split]
  val Root : Symbol = 0
  val UNK : Terminal = 1
  val Empty : Terminal = 0
  val Unsplit : Split = 0
  val RootString : String = "ROOT"
  val EmptyString : String = "EPSILON"
  val UnkString : String = "UNK"
  
  def split(s : Split) : List[Split] = {
    List((s * 2 + 1).toChar,(s * 2 + 2).toChar)
  }
  
  def isPair(a : Split, b : Split) = {
    a == b + 1 && b % 2 == 0
  }
  
  def merge(a : Split) : Split = {
    (a / 2).toChar
  }
  
  def parseSplit(s : String) : Split = {
    Integer.parseInt(s).toChar
  }
  
}
