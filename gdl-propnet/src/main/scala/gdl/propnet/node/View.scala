package gdl.propnet.node

import gdl.lang.AtomicSentence

case class View(sentence: AtomicSentence) extends ViewNode
case class Terminal() extends ViewNode
