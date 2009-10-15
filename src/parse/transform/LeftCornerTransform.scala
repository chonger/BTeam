package parse.transform

import collection.mutable.Stack

//does left corner transform, with null emissions

trait LeftCornerTransform extends PreProcessor with PrintablePCFG {
  
  def lcVerbose(s : String) = println(s)
  
  abstract override def transform(data : List[ParseTreeData]) = {
		super.transform(data)
		//println("LC")
		data.map(d => d.tree.root = recLC(d.tree.root))
		//data.foreach(d => println(printTree(d.tree)))
	}
 
	abstract override def revert(data : List[ParseTreeData]) = {
	  	println("-LC")
		data.map(d => d.tree.root = revLC(d.tree.root))
		super.revert(data)
	}
 
  	def revLC(n : NonTerminalNode) : NonTerminalNode = {
  	  /**
  	  if(n.symbol.isInstanceOf[Root]) {
  	 		return UnaryTreeNode(n.symbol,revLC(n.children.asInstanceOf[List[NonTerminalNode]](0)))
  	  }
      */ 
  	  if(n.isInstanceOf[PreTerminalNode]) {
  		  return n
  	  }
  	   
  	  
  	  var stack = new Stack[NonTerminalNode]()
  	  
  	  var cur = n
  	  
  	  while(!cur.isInstanceOf[PreTerminalNode]) {
  	    stack.push(cur)   
  	    cur = cur match {
  	  		case BinaryTreeNode(s,left,right) => right 
  	  		case UnaryTreeNode(s,kid) =>  kid
  	  		case ProtoTreeNode(s,kids) => kids.last
  	    }
  	  }
  	  
  	  var topn = stack.top
  	  var (s1,s2) = splitLCSymbol(topn.symbol)
  	  var ret = readSpine(stack,s1)
  	  
  	  ret
  	}
 
   	def readSpine(stack : Stack[NonTerminalNode], n : NonTerminal) : NonTerminalNode = {
   	  var cur = stack.top
      stack.pop
      
      if(stack.isEmpty) {
    	  cur match {
   		  	case BinaryTreeNode(s,left,right) => left 
   		  }
      } else {
    	  var (s1,s2) = splitLCSymbol(cur.symbol)
    	  cur match {
    	  	case BinaryTreeNode(s,left,right) => new BinaryTreeNode(n,readSpine(stack,s2),revLC(left)) 
   		  	case UnaryTreeNode(s,kid) =>  new UnaryTreeNode(n,readSpine(stack,s2))
   		  	case ProtoTreeNode(s,kids) => new ProtoTreeNode(n,
                         readSpine(stack,s2) :: kids.reverse.drop(1).reverse.map(revLC(_)))
    	  } 
      }
   	} 
   
   	private val mySplitSymbol = "-" 
    private val otherSplitSymbols = "[,]"
    
   	def splitLCSymbol(n : NonTerminal) = {
   	  var str = symbolStrings(n.id).split(mySplitSymbol)
      var nt1 = NonTerminal(symbolIDs(str(0)))
      var nt2 = if(str.length > 1) NonTerminal(symbolIDs(str(1).split(otherSplitSymbols)(0))) else null
      
      (nt1,nt2)
   	}
   
  	def recLC(n : NonTerminalNode) : NonTerminalNode = {
  	  	/*
  	 	if(n.symbol.isInstanceOf[Root]) {
  	 		return UnaryTreeNode(n.symbol,recLC(n.children.asInstanceOf[List[NonTerminalNode]](0)))
  	 	}
        */
  	 	if(n.isInstanceOf[PreTerminalNode]) {
  	 		return n
  	 	}
  
  	 	var stack = new Stack[NonTerminalNode]()
     
  	 	def findLeftCorner(n : NonTerminalNode) : PreTerminalNode = {
  	 		stack.push(n)
  	 		var ret = n match {
  	 			case pt : PreTerminalNode => pt
  	 			case BinaryTreeNode(s,left,right) => findLeftCorner(left) 
  	 			case UnaryTreeNode(s,kid) =>  findLeftCorner(kid)
  	 			case ProtoTreeNode(s,kids) => findLeftCorner(kids(0).asInstanceOf[NonTerminalNode])
  	 		}
  	 		ret
  	 	}
  	 	var leftCorner = findLeftCorner(n)
  	 	new BinaryTreeNode(n.symbol,leftCorner,buildSpine(n.symbol,stack)) 
  	}  
   
   	def buildSpine(sym : NonTerminal, stack : Stack[NonTerminalNode]) : NonTerminalNode = {
   	  	
	  	if(stack.size == 1) {
	  		var newSym = combineSymbols(sym,sym)
	  		
	  		return PreTerminalNode(combineSymbols(sym,sym),EmptyNode())
	  	}
	  	var newSym = combineSymbols(sym,stack.top.symbol)
	  	stack.pop
	  	stack.top match {
	  	  case pt : PreTerminalNode => throw new Exception("Shouldn't happen")
	  	  case BinaryTreeNode(s,left,right) => {
	  		  var newLeft = recLC(right)
	  		  new BinaryTreeNode(newSym,newLeft,buildSpine(sym,stack))
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
