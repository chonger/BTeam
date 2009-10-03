package parse.transform

trait LeftFactorizeNullary extends LeftFactorize {override var nullary = true}
trait LeftFactorizeUnary extends LeftFactorize {override var nullary = false}

abstract trait LeftFactorize extends PreProcessor {
	var nullary : Boolean
  
	abstract override def transform(data : List[ParseTreeData]) = {
	  	println("LF!")
		super.transform(data)
		data.map(d => d.tree.root = recLF(d.tree.root))
	}
 
	abstract override def revert(data : List[ParseTreeData]) = {
		data.map(d => d.tree.root = recUnLF(d.tree.root))
		super.revert(data)
	}
 
	def recUnLF(n : NonTerminalNode) : NonTerminalNode = {
	  null
	}
	
	def combineSymbolsLF(base : NonTerminal, next : NonTerminal) : NonTerminal = {
	 	var s = symbolStrings(base.id)
	 	if(s.indexOf('-') < 0) { //not yet a combo
	 	  s = s + "-" + symbolStrings(next.id)
	 	} else {
	 	  s = s + "," + symbolStrings(next.id)
	 	}
	 	NonTerminal(addSymbol(s))
	}
 
	def recLF(n : NonTerminalNode) : NonTerminalNode = {
	  n match {
	  case pt : PreTerminalNode => pt
      case nt : NonTerminalNode => {
    	  val kids = nt.children.asInstanceOf[List[NonTerminalNode]] //already checked for PT
    	  val sym = nt.symbol
    	  if(kids.length == 1) {
    		  if(sym.isInstanceOf[Root]) {
    			  new UnaryTreeNode(Root(),recLF(kids(0)))
    		  } else if(nullary) {
    			  kids(0) match {
    			  case t : TerminalNode => new PreTerminalNode(sym,t)
    			  case nt : NonTerminalNode => {
    				  var left = recLF(kids(0))
    				  var newSym = combineSymbolsLF(sym,kids(0).symbol)
    			      var right = recLF(new PreTerminalNode(newSym,new EmptyNode))
    				  new BinaryTreeNode(sym,left,right)
    			  	}
    			  }
    		  } else {
    			  kids(0) match {
    			  	case t : TerminalNode => new PreTerminalNode(sym,t)
    			  	case _ => new UnaryTreeNode(sym,recLF(kids(0)))
    			  }
    		  }
    	  }
    	  else if (kids.length == 2) { //both children must be nonterminals
    		  var newSym = combineSymbolsLF(sym,kids(0).symbol)
    		  var newRight = new UnaryTreeNode(newSym,kids(1))
    		  new BinaryTreeNode(sym,recLF(kids(0)),recLF(newRight))
    	  } else { //binarize
    	    var newSym = combineSymbolsLF(sym,kids(0).symbol)
    	    var left = recLF(kids(0))
    		val right = recLF(new ProtoTreeNode(newSym,kids.drop(1)))
    	  	new BinaryTreeNode(sym,left,right)
         }
       
      } 
    }
  }
}
