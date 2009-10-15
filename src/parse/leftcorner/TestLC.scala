package parse.leftcorner

import io.Source
import corpora.treebank._
import parse.transform._
object TestLC {
  def main(args : Array[String]) : Unit = {
    
	val data = TreebankData.read(args(0))	   
	var pcfg = new PCFG with PrintablePCFG with PreProcessor with LeftCornerTransform with LeftFactorizeUnary
	pcfg.init(data)
 
	println("Starting transform")
	//data.foreach(d => println(pcfg.printTree(d.tree)))
	pcfg.transform(data)
	//data.foreach(d => println(pcfg.printTree(d.tree)))
	TreebankData.write(args(1),data,pcfg)
	//pcfg.revert(data)
	//data.foreach(d => println(pcfg.printTree(d.tree)))
	-1
  }
}
