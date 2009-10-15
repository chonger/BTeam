
package parse.transform

import collection.mutable.Stack

trait LeftFactorizeNullary extends LeftFactorize {override var nullary = true}
trait LeftFactorizeUnary extends LeftFactorize {override var nullary = false}

abstract trait LeftFactorize extends PreProcessor with PrintablePCFG {
	var nullary : Boolean
  
	abstract override def transform(data : List[ParseTreeData]) = {
		super.transform(data)
		//println("LF")
		data.map(d => d.tree.root = recLF(d.tree.root))
	}
 
	abstract override def revert(data : List[ParseTreeData]) = {
	  	println("-LF")
		data.map(d => d.tree.root = recUnLF(d.tree.root))
		data.foreach(d => println(printTree(d.tree)))
		super.revert(data)
	}
 
	def recUnLF(n : NonTerminalNode) : NonTerminalNode = {	
	  if(n.symbol.isInstanceOf[Root]) {
		  return UnaryTreeNode(n.symbol,recUnLF(n.children.asInstanceOf[List[NonTerminalNode]](0)))
  	  }
  	  if(n.isInstanceOf[PreTerminalNode]) {
  		  return n
  	  }
  	  
  	  //all nodes are binary if we are nullary, else there are unary too
  	  n match {
  	    case BinaryTreeNode(s,left,right) => {
  	      var splitsyms = splitLFSymbols(right.symbol)
  	      if(splitsyms.length > 1) //its a combination and must have the same base
  	      	  accumFactors(s,List(left),right)
  	      else
  	    	  new BinaryTreeNode(s,recUnLF(left),recUnLF(right))
  	    }
  	    case UnaryTreeNode(s,kid) => new UnaryTreeNode(s,recUnLF(kid))
  	  }
  	  
	}
 	
 	def accumFactors(sym : NonTerminal,left : List[NonTerminalNode], right : NonTerminalNode) : NonTerminalNode = {
 	  right match {
 	    case PreTerminalNode(s,e : EmptyNode) => {
 	      var kids = left.reverse
 	      if(kids.length == 1) 
 	    	  UnaryTreeNode(sym,recUnLF(kids(0)))
 	      else if(kids.length == 2)
 	    	  BinaryTreeNode(sym,recUnLF(kids(0)),recUnLF(kids(1)))
          else
        	  ProtoTreeNode(sym,kids.map(recUnLF(_)))
 	    }
 	    case BinaryTreeNode(s,aleft,aright) => {
 	    	accumFactors(sym,aleft :: left,aright)
 	    }
 	    case UnaryTreeNode(s,kid) => {
 	      //fool it into acting like this was factored to nullary
 	      accumFactors(sym,kid :: left,PreTerminalNode(null,EmptyNode()))
 	    }
      } 
 	}
	
 	def splitLFSymbols(nt : NonTerminal) : List[NonTerminal] = {
 		symbolStrings(nt.id).split(",").toList.map(s => NonTerminal(symbolIDs(s))) 
 	}
 
	def combineSymbolsLF(base : NonTerminal, next : NonTerminal) : NonTerminal = {
	 	var s = symbolStrings(base.id)
	 	if(s.indexOf(',') < 0) { //not yet a combo
	 	  s = s + "," + symbolStrings(next.id)
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
