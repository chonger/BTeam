package corpora.treebank

import java.io.{File,FileWriter,BufferedWriter}
import parse._
import io.Source

class TreebankParseException(val raw : String) extends DataParseException

object TreebankData {
  
	def read(filename : String) : List[TreebankData] = {
	  print("Reading " + filename + " for treebank format trees ... ")
	  val filedata = Source.fromFile(filename).getLines
	  val treestrs = (List[String]() /: filedata)((a,b) => if(b.charAt(0) != '(') (a(0) + b) :: a.drop(1) else b :: a)
	  println("Got " + treestrs.length + " trees")
	  treestrs.map(new TreebankData(_))
	} 
 
	def write(filename : String, data : List[TreebankData]) = {
		var bw = new BufferedWriter(new FileWriter(new File(filename)))
		data.foreach(d => bw.write(d.tree.toString))
		bw.close
	}
}

class TreebankData(raw : String) extends ParseTreeData {
	override def init(pcfg : PreProcessor) : ParseTree = {
		val myRaw = "\n".r.replaceAllIn(raw,"")
	    tree = new ParseTree
		try {			
	    	tree.root = parseRaw(myRaw.toList,0,pcfg)._1.asInstanceOf[NonTerminalNode] //leave the real raw string alone
	    	tree
	    } catch {
	    	case e :DataParseException => tree = null; throw new TreebankParseException(raw)
	    }	    
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
					  return (PreTerminalNode(NonTerminal(pcfg.addSymbol(symbol)),newT),i)
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





