package corpora.treebank

import java.io.{File,FileWriter,BufferedWriter}
import parse._
import io.Source
import java.io.StringReader
import java.lang.StringBuffer

class TreebankParseException(val raw : String) extends DataParseException

object TreebankData {
  
	def read(filename : String) : List[TreebankData] = {
	  print("Reading " + filename + " for treebank format trees ... ")
	  val filedata = Source.fromFile(filename).getLines
	  val treestrs = (List[String]() /: filedata)((a,b) => if(b.charAt(0) != '(') (a(0) + b) :: a.drop(1) else b :: a)
	  println("Got " + treestrs.length + " trees")
	  treestrs.map(new TreebankData(_))
	} 
 
	def write(filename : String, data : List[TreebankData], pcfg : PCFG with PrintablePCFG) = {
		var bw = new BufferedWriter(new FileWriter(new File(filename)))
		data.foreach(d => bw.write("\\s+".r.replaceAllIn(pcfg.printTree(d.tree)," ") + "\n"))
		bw.close
	}
}

class TreebankData(raw : String) extends ParseTreeData {
	override def init(pcfg : PreProcessor) : ParseTree = {	
		tree = new ParseTree
		tree.root = readTree(pcfg)
		//println(pcfg.asInstanceOf[PrintablePCFG].printTree(tree))
		tree
	}
 
	def isWhitespace(c : Char) = List(' ','\t','\r','\n') contains c
 
    def readTree(pcfg : PCFG with PreProcessor) : NonTerminalNode = {	
    
		def readNode(stream : StringReader) : NonTerminalNode = {
		  var ntStr = new StringBuffer()
		  var c = stream.read.toChar
		  while(!isWhitespace(c)) {ntStr append c; c = stream.read.toChar}
		  while(isWhitespace(c)) {c = stream.read.toChar} 
		  var kids = List[NonTerminalNode]()
		  while(stream.ready) {
			  c match {
			   case '(' => kids ::= readNode(stream)
			   case ')' => return ProtoTreeNode(NonTerminal(pcfg.addSymbol(ntStr.toString)),kids.reverse)
			   case _ if isWhitespace(c) => {}
			   case _ => {
				   var termStr = new StringBuffer()
				   while(c != ')') {
				     termStr append c
				     c = stream.read.toChar
				   }
				   return PreTerminalNode(NonTerminal(pcfg.addSymbol(ntStr.toString)),
                                  			  TerminalNode(Terminal(pcfg.addTerm(termStr.toString))))
			   }
			  }
			  c = stream.read.toChar
		  }
		  null
		}
		var stringstream = new StringReader(raw)
		stringstream.read 
		readNode(stringstream)
	}
 
}





