package parse

abstract class TreeNode(val symbol : TreeSymbol)
case class TerminalNode(override val symbol : Terminal) extends TreeNode(symbol)
case class EmptyNode() extends TerminalNode(new EmptyTerminal)
abstract case class NonTerminalNode(override val symbol : NonTerminal) extends TreeNode(symbol) {
  def rules : List[TreeRule]
  def children : List[TreeNode]
}
case class ProtoTreeNode(override val symbol : NonTerminal,
                         val kids : List[TreeNode]) extends NonTerminalNode(symbol) {
    def rules = List()
    def children = kids
}
case class UnaryTreeNode(override val symbol : NonTerminal, 
                         val kid : NonTerminalNode) extends NonTerminalNode(symbol) {
  def rules : List[UnaryRule] = {
    for{mysplit <- symbol.splits
        kidsplit <- kid.symbol.splits} 
    yield new UnaryRule(symbol.id,mysplit,(kid.symbol.id,kidsplit))
  }
  def children = List(kid)
}
case class PreTerminalNode(override val symbol : NonTerminal,
						   val kid : TerminalNode) extends NonTerminalNode(symbol) {
	def rules : List[TerminalRule] = {
	  for{mysplit <- symbol.splits} 
	  yield new TerminalRule(symbol.id,mysplit,kid.symbol.id)
	}
	def children : List[TerminalNode]= List(kid)
}
case class BinaryTreeNode(override val symbol : NonTerminal,
                          val left : NonTerminalNode, 
                          val right : NonTerminalNode) extends NonTerminalNode(symbol) {
	def rules : List[BinaryRule] = {
		for{mysplit <- symbol.splits
			lsplit <- left.symbol.splits
			rsplit <- right.symbol.splits} 
		yield new BinaryRule(symbol.id,mysplit,
                       (left.symbol.id,lsplit),
                       (right.symbol.id,rsplit))
	}
    def children = List(left,right)                     
}

//TODO : If we can make this per instance of the parser, then that would be nice.
//right now we can only have one trainer per process
object NonTerminal {
  var splits = Array(List(ParseTypes.Unsplit))
}

abstract class TreeSymbol
case class NonTerminal(val id : ParseTypes.Symbol) extends TreeSymbol {
  def splits = NonTerminal.splits(id)
}
case class Terminal(val id : ParseTypes.Terminal) extends TreeSymbol
case class Root() extends NonTerminal(ParseTypes.Root)
case class EmptyTerminal() extends Terminal(ParseTypes.Empty)


abstract class TreeRule(val lhs : ParseTypes.Symbol, val split : ParseTypes.Split) 
case class UnaryRule(override val lhs : ParseTypes.Symbol, 
                     override val split : ParseTypes.Split, 
                     val kid : ParseTypes.SSPair) extends TreeRule(lhs,split) {
	override def hashCode() : Int = {
	  lhs ^ split ^ kid._1 ^ kid._2
	}
	override def equals(any : Any) : Boolean = {
	  any match {
	    case UnaryRule(lhs,split,kid) => lhs == this.lhs && split == this.split && kid == this.kid
	    case _ => false	
	  }
	}
}
case class TerminalRule(override val lhs : ParseTypes.Symbol,
                            override val split : ParseTypes.Split,
                            val term : ParseTypes.Terminal) extends TreeRule(lhs,split) {
	override def hashCode() : Int = {
		lhs ^ split ^ term
	}
	override def equals(any : Any) : Boolean = {
	  any match {
	    case TerminalRule(lhs,split,term) => lhs == this.lhs && split == this.split && term == this.term
	    case _ => false
	  }
	}                            
  
}
case class BinaryRule(override val lhs : ParseTypes.Symbol,
                     override val split : ParseTypes.Split,
                     val left : ParseTypes.SSPair,
                     val right : ParseTypes.SSPair) extends TreeRule(lhs,split) {
	override def hashCode() : Int = {
	  lhs ^ split ^ left._1 ^ left._2 ^ right._1 ^ right._2
	}
	override def equals(any : Any) : Boolean = {
	  any match {
	    case BinaryRule(lhs,split,left,right) => lhs == this.lhs && split == this.split && 
	    	left == this.left && right == this.right
	    case _ => false
	  }
	}
}