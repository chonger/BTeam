package parse.generate

import io.Source
import corpora.treebank._
import parse.transform._
object TestGen {
  def main(args : Array[String]) : Unit = {
    
	//val data = TreebankData.read(args(0))	   
	var pcfg = new PCFG with PrintablePCFG with PreProcessor with LeftFactorizeUnary with LeftCornerTransform
	/** 
	pcfg.init(data)
	println("ORIGINAL TREE")
	data.foreach(d => println(pcfg.printTree(d.tree)))
	pcfg.transform(data)
	data.foreach(d => println(pcfg.printTree(d.tree)))
 */
 
 	var full = new FuncTree(new UnaryTreeNode(Root(),new BinaryTreeNode("S",pcfg,
                                  new UnderspecifiedNode("NP",pcfg),
                                  new BinaryTreeNode("VP",pcfg,
                                                     new PreTerminalNode("VPZ",pcfg,new TerminalNode("ate",pcfg)),
                                                     new UnderspecifiedNode("NP",pcfg)))))
	var cpfull : FuncTree = full.deepCopy
 
	var thenp = new FuncTree(new BinaryTreeNode("NP",pcfg,
                                  new PreTerminalNode("DT",pcfg,new TerminalNode("the",pcfg)),
                                  new UnderspecifiedNode("NNP",pcfg)))
                                  
	var dog = new FuncTree(new PreTerminalNode("NNP",pcfg,new TerminalNode("dog",pcfg)))
	var man = new FuncTree(new PreTerminalNode("NNP",pcfg,new TerminalNode("man",pcfg)))
 
	println(pcfg.printTree(full))
	println(full.sentence(pcfg))
	println(pcfg.printTree(thenp))
	println(thenp.sentence(pcfg))
	println(pcfg.printTree(dog))
	println(dog.sentence(pcfg))
	println(pcfg.printTree(man))
	println(man.sentence(pcfg))
	var comp = full(thenp)(thenp)(dog)(man)
	println(pcfg.printTree(comp))
	println(comp.sentence(pcfg))
	comp.collapseUNodes
	println(pcfg.printTree(comp))
	println(comp.sentence(pcfg))
 	var comp2 = cpfull(thenp)(thenp)(man)(dog)
	println(pcfg.printTree(comp2))
	println(comp2.sentence(pcfg))
	comp2.collapseUNodes
	println(pcfg.printTree(comp2))
	println(comp2.sentence(pcfg))
 	
	-1
  }
}
