package parse

object TreeNode {
  def deepCopy(n : NonTerminalNode) : NonTerminalNode = {
	  n match {
	    case BinaryTreeNode(s,l,r) => new BinaryTreeNode(s,deepCopy(l),deepCopy(r))
	    case UnaryTreeNode(s,k) => new UnaryTreeNode(s,deepCopy(k)) 
	    case PreTerminalNode(s,k) => new PreTerminalNode(s,TerminalNode(k.symbol))
	    case ProtoTreeNode(s,kk) => new ProtoTreeNode(s,kk.map(deepCopy(_)))
	    case un : UnderspecifiedNode => { 
	      var ret = new UnderspecifiedNode(un.symbol)
	      if(un.assignment != null) 
	    	  ret.assignment = deepCopy(un.assignment).asInstanceOf[NonTerminalNode]
	      ret                                                                             
        }                             
	  }
  }
}

abstract class TreeNode(val symbol : TreeSymbol)
case class TerminalNode(override val symbol : Terminal) extends TreeNode(symbol) {
  def this(s : String,pcfg : PCFG with PreProcessor) = this(Terminal(pcfg.addTerm(s)))
}
case class EmptyNode() extends TerminalNode(new EmptyTerminal)
abstract case class NonTerminalNode(override val symbol : NonTerminal) extends TreeNode(symbol) {
  def rules : List[TreeRule]
  def children : List[TreeNode]
  def this(s : String,pcfg : PCFG with PreProcessor) = this(NonTerminal(pcfg.addSymbol(s)))
 
}
case class UnderspecifiedNode(override val symbol : NonTerminal) extends NonTerminalNode(symbol) {
  def rules() = Nil
  override def children = if(assignment == null) Nil else List(assignment)
  var assignment : NonTerminalNode = null
  def this(s : String,pcfg : PCFG with PreProcessor) = this(NonTerminal(pcfg.addSymbol(s)))
} 
case class ProtoTreeNode(override val symbol : NonTerminal,
                         val kids : List[NonTerminalNode]) extends NonTerminalNode(symbol) {
    def rules() = Nil
	def children = kids
    def this(s : String,pcfg : PCFG with PreProcessor,kids : List[NonTerminalNode]) = 
      this(NonTerminal(pcfg.addSymbol(s)),kids)
}
case class UnaryTreeNode(override val symbol : NonTerminal, 
                         val kid : NonTerminalNode) extends NonTerminalNode(symbol) {
  def rules() : List[UnaryRule] = {
    for{mysplit <- symbol.splits
        kidsplit <- kid.symbol.splits} 
    yield new UnaryRule(symbol.id,mysplit,(kid.symbol.id,kidsplit))
  }
  def children = List(kid)
  def this(s : String,pcfg : PCFG with PreProcessor, kid : NonTerminalNode) = 
    this(NonTerminal(pcfg.addSymbol(s)),kid)
}
case class PreTerminalNode(override val symbol : NonTerminal,
						   val kid : TerminalNode) extends NonTerminalNode(symbol) {
	def rules : List[TerminalRule] = {
	  for{mysplit <- symbol.splits} 
	  yield new TerminalRule(symbol.id,mysplit,kid.symbol.id)
	}
	def children : List[TerminalNode]= List(kid)
	def this(s : String,pcfg : PCFG with PreProcessor, kid : TerminalNode) = 
		this(NonTerminal(pcfg.addSymbol(s)),kid)
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
    def this(s : String,pcfg : PCFG with PreProcessor,
             l : NonTerminalNode, r : NonTerminalNode) = 
      this(NonTerminal(pcfg.addSymbol(s)),l,r)
}

//This object constrains us to one grammar per process
object NonTerminal {
  //splits are a tree structure, represented as a list for each depth with parent indices
  //start off with the unsplit 0 symbol at depth 0
  var maxSplit = 0
  var splitTrees = Array(List(List((ParseTypes.Unsplit,0)))) 
}

abstract class TreeSymbol
case class NonTerminal(val id : ParseTypes.Symbol) extends TreeSymbol {
  def splits = NonTerminal.splitTrees(id)(NonTerminal.maxSplit).map(_._1)
}
case class SplitNT(override val id : ParseTypes.Symbol, split : ParseTypes.Split) extends NonTerminal(id) {
	override def hashCode() : Int = {
			id ^ (split << 6) 
	}
	override def equals(any : Any) : Boolean = {
	  any match {
	    case SplitNT(id,split) => id == this.id && split == this.split
	    case _ => false	
	  }
	}
}
case class Terminal(val id : ParseTypes.Terminal) extends TreeSymbol
case class Root() extends SplitNT(ParseTypes.Root,ParseTypes.Unsplit)
case class EmptyTerminal() extends Terminal(ParseTypes.Empty)

abstract class TreeRule(val lhs : ParseTypes.Symbol, val split : ParseTypes.Split) 
abstract class NTRule(l : ParseTypes.Symbol,s : ParseTypes.Split) extends TreeRule(l,s)
case class UnaryRule(override val lhs : ParseTypes.Symbol, 
                     override val split : ParseTypes.Split, 
                     val kid : ParseTypes.SSPair) extends NTRule(lhs,split) {
	override def hashCode() : Int = {
	  lhs ^ (split << 6) ^ (kid._1 << 10) ^ (kid._2 << 14)
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
		lhs ^ (split << 10) ^ (term << 16)
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
                     val right : ParseTypes.SSPair) extends NTRule(lhs,split) {
	override def hashCode() : Int = {
	  lhs ^ (split << 4) ^ (left._1 << 8) ^ (left._2 << 12) ^ (right._1 << 16) ^ (right._2 << 20)
	}
	override def equals(any : Any) : Boolean = {
	  any match {
	    case BinaryRule(lhs,split,left,right) => lhs == this.lhs && split == this.split && 
	    	left == this.left && right == this.right
	    case _ => false
	  }
	}
}