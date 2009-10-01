package parse
import collection.mutable.{HashMap,HashSet}

trait XBar extends PreProcessor{
  
  def xbar(tree : ParseTree) = {    
	  tree.root = recXbar(tree.root) 	
  }	
  
  def makeBar(id : ParseTypes.Symbol) : ParseTypes.Symbol = {
    var s = symbolStrings(id)
    if(s.charAt(s.length() - 1) != '\'')
      s += "'"
    addSymbol(s)
  }
  
  def recXbar(n : TreeNode) : TreeNode = {
    n match {
      case ProtoTreeNode(symbol,kids) => {
    	  if(kids.length == 1) {
    		  kids(0) match {
    		    case t : TerminalNode => new PreTerminalNode(symbol.asInstanceOf[NonTerminal],t)
    		    case _ => new UnaryTreeNode(symbol.asInstanceOf[NonTerminal],
                                      recXbar(kids(0)).asInstanceOf[NonTerminalNode])
    		  }
    	  }
    	  else if (kids.length == 2)
    		  new BinaryTreeNode(symbol,
                           recXbar(kids(0)).asInstanceOf[NonTerminalNode],
                           recXbar(kids(1)).asInstanceOf[NonTerminalNode])
    	  else { //binarize
    	    var newID = makeBar(symbol.id)
    	    var newSym = new NonTerminal(newID)
    		val newLeftBranch = new BinaryTreeNode(newSym,
                                             recXbar(kids(0)).asInstanceOf[NonTerminalNode],
                                             recXbar(kids(1)).asInstanceOf[NonTerminalNode])
    		val newKids = (newLeftBranch :: n.asInstanceOf[ProtoTreeNode].children.drop(2).toList)
    		recXbar(new ProtoTreeNode(symbol,newKids))		
         }
       
      } 
      case _ => n
    }
  }
}
