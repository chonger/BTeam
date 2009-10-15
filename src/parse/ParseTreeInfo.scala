package parse

/**
 *  Various print and benchmarking functions
 */

trait PrintablePCFG extends PCFG {
  def printRules() = {
    rules.map(r => println(ruleString(r._1) + " = " + r._2))
  }

  def printTree(tree : ParseTree) = {
	  recPrintTree(tree.root,"")
  }
  
  def printRules(tree : ParseTree) : Unit = {
	if(tree.root == null) {println("NULL") ; return}
    recPrintConstituents(tree.root)
  }
  
  def recPrintConstituents(n : TreeNode) : Unit = {
    n match {
      case nt : NonTerminalNode => 
	    nt.rules.map(r => println(ruleString(r)))
      case _ => {}
    }
  }
  
  def ruleString(r : TreeRule) = {
	  symbolStrings(r.lhs) + "/" + r.split.toInt + " --> " + {r match {
	  	case UnaryRule(lhs,split,kid) =>  symbolStrings(kid._1) + "/" + kid._2.toInt 
	  	case BinaryRule(lhs,split,left,right) => symbolStrings(left._1) + "/" + left._2.toInt + 
	  		" , " + symbolStrings(right._1) + "/" + right._2.toInt
	  	case TerminalRule(lhs,split,term) => terminalStrings(term)
    }}
  }
  
  def symString(s : TreeSymbol) = {
    s match {
      case Terminal(term) => terminalStrings(term)
      //case nt : NonTerminal => (symbolStrings(nt.id) /: nt.splits)(_ + "/" + _.toInt)
      case nt : NonTerminal => symbolStrings(nt.id)
    }
  }
  
  def nodeString(n : TreeNode) : String = {
    n match {
      case un : UnderspecifiedNode => {
        val kidStr = if(un.assignment == null) "" else " -> " + symString(un.assignment.symbol)
        "*" + symString(un.symbol) + "*" + kidStr
      }
      case nt : NonTerminalNode => 
        ((symString(nt.symbol) + " -> ") /: (nt.children))((a,b) => a + " " + symString(b.symbol))
      case TerminalNode(term) => symString(term)
    }
  }
  
  def getSpcStr(n : Int) = {
    var ret = ""
    for(i <- 1 to n)
      ret += " "
    ret
  }
  
  def recPrintTree(n : TreeNode, offset : String) : String = {
    n match {
      case un : UnderspecifiedNode => {
        var symstr = symbolStrings(un.symbol.id)
        var ret = "(*" + symstr + " "
        if(un.assignment != null) {
        	var spcstr = getSpcStr(symstr.length + 2)
        	ret += recPrintTree(un.assignment,offset)
        }
        ret + "*)"
      }
      case nt : NonTerminalNode =>
        //var symstr = (symbolStrings(nt.symbol.id) /: nt.symbol.splits)(_ + "/" + _.toInt.toString)
        var symstr = symbolStrings(nt.symbol.id)
    	var ret = "(" + symstr + " "
    	var spcstr = getSpcStr(symstr.length + 2)
    	ret += (recPrintTree(nt.children(0),offset + spcstr) /: nt.children.drop(1))((a,b) => {
    	    a + "\n" + offset + spcstr + recPrintTree(b,offset + spcstr)})
    	ret + ")"
      case TerminalNode(symbol) => {
        terminalStrings(symbol.id)  
      }
    }
  }
 
  
  def printSplits(id : ParseTypes.Symbol) = {
    var x = NonTerminal.splitTrees(id)
    var ret = for{i <- 0 to x.length - 1} yield printSplitsAtDepth(id,i)
    ("" /: ret)(_ + "\n" + _)
  }
  def printSplitsAtDepth(id : ParseTypes.Symbol, depth : Int) : String = {
    var x = NonTerminal.splitTrees(id)(depth)
    ("" /: x)((a,b) => a + " " + b._1.toInt + "/" + b._2.toInt)
  }
  
  
  
}