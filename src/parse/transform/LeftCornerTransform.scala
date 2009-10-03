package parse.transform

import collection.mutable.Stack

trait LeftCornerTransform extends PreProcessor {

  abstract override def transform(data : List[ParseTreeData]) = {
	  	println("LC!")
		super.transform(data)
		data.map(d => d.tree.root = recLC(d.tree.root))
	}
 
	abstract override def revert(data : List[ParseTreeData]) = {
		data.map(d => d.tree.root = recLC(d.tree.root))
		super.revert(data)
	}
 
  	
 
  	def recLC(n : NonTerminalNode) : NonTerminalNode = {
  	 	if(n.symbol.isInstanceOf[Root])
  	 		return UnaryTreeNode(n.symbol,recLC(n.children.asInstanceOf[List[NonTerminalNode]](0)))
  	 	if(n.isInstanceOf[PreTerminalNode])
  	 		return n
     
  	 	var stack = new Stack[NonTerminalNode]()
     
  	 	def findLeftCorner(n : NonTerminalNode) : NonTerminalNode = {
  	 		stack.push(n)
  	 		n match {
  	 			case pt : PreTerminalNode => pt
  	 			case BinaryTreeNode(s,left,right) => findLeftCorner(left) 
  	 			case UnaryTreeNode(s,kid) =>  findLeftCorner(kid)
  	 			case ProtoTreeNode(s,kids) => findLeftCorner(kids(0).asInstanceOf[NonTerminalNode])
  	 		}
  	 	}
  	 	var leftCorner = findLeftCorner(n)
     
  	 	new BinaryTreeNode(n.symbol,leftCorner,buildSpine(n.symbol,stack)) 
  	}  
   
   	def buildSpine(sym : NonTerminal, stack : Stack[NonTerminalNode]) : NonTerminalNode = {
	  	if(stack.size == 1)
	  		return PreTerminalNode(combineSymbols(sym,sym),EmptyNode())
	
	  	var newSym = combineSymbols(sym,stack.top.symbol)
	  	stack.pop
	  	stack.top match {
	  	  case pt : PreTerminalNode => throw new Exception("Shouldn't happen")
	  	  case BinaryTreeNode(s,left,right) => {
	  		  new BinaryTreeNode(newSym,recLC(right),buildSpine(sym,stack))
	  	  }
	  	  case UnaryTreeNode(s,kid) => {
	  	      new UnaryTreeNode(newSym,buildSpine(sym,stack))
	  	  }
	  	  case ProtoTreeNode(s,kids) => {
	  		  var newKids = kids.drop(1).map(n => recLC(n.asInstanceOf[NonTerminalNode]))
	  		  newKids ++= List(buildSpine(sym,stack))
	  		  new ProtoTreeNode(newSym,newKids)
	  	  }
	  	}
   	}
    
    def combineSymbols(base : NonTerminal, other : NonTerminal) : NonTerminal = {
      	var s = symbolStrings(base.id)
      	s = s + "-" + symbolStrings(other.id)
	 	NonTerminal(addSymbol(s))
    }
}
