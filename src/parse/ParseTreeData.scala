package parse

import corpora._
import parse._

abstract class ParseTreeData extends BTeamData {
	var tree : ParseTree = null
	def init(pcfg : PCFG with PreProcessor with PrintablePCFG with XBar) : ParseTree 
}
