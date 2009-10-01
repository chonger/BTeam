package corpora.treebank

import java.io.{File,FileWriter,BufferedWriter}
import parse._

class TreebankParseException(val raw : String) extends DataParseException

object TreebankData {
	def read(file : File) : List[TreebankData] = {
	  List[TreebankData]()
	} 
}

class TreebankData(raw : String) extends ParseTreeData {
 
	def init(pcfg : PCFG with PreProcessor with PrintablePCFG with XBar) : ParseTree = {
		//println("INIT" + raw)
		val myRaw = "\n".r.replaceAllIn(raw,"")
	    tree = new ParseTree
		try {
	    	tree.root = parseRaw(myRaw.toList,0,pcfg)._1 //leave the real raw string alone
	    	pcfg.printTree(tree)
	    	tree
	    } catch {
	    	case e :DataParseException => tree = null; throw new TreebankParseException(raw)
	    }	    
	}
 
	override def write(file : File) {
		var bw = new BufferedWriter(new FileWriter(file))
		bw.write(tree.toString)
		bw.close
	}
  
    def parseRaw(raw : List[char],index : Int,pcfg : PCFG with PreProcessor) : (TreeNode,Int) = {
		var i = index
		var symbolDone = false
		var symbol = ""
		var kids = List[TreeNode]()
		var term = ""
		while(i < raw.length) {
			raw(i) match {
				case '(' => {
				  if(symbol != "") {
					  var (n,ind) = parseRaw(raw,i,pcfg)
					  i = ind
					  kids ::= n
				  }
				}
				case ')' =>{
				  if(term.length > 0) {
					  var newT = new TerminalNode(Terminal(pcfg.addTerm(term)))
					  kids ::= newT
				  }
				  var nt : TreeNode = null
				  if(symbol == "ROOT") {
					  nt = new ProtoTreeNode(Root(),kids.reverse)
				  } else {
					  nt = new ProtoTreeNode(NonTerminal(pcfg.addSymbol(symbol)),kids.reverse)
				  }
				  return (nt,i)
				}	
				case ' ' => {
					if(symbol.length > 0)
						symbolDone = true
				}
				case c => {
					if(symbolDone)
						term += c
					else
						symbol += c
				}
			}
			i += 1
		}
		throw new DataParseException
  }
}





