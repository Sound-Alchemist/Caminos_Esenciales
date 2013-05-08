class Nodo(newValue: Int) {
  private var value: Int = newValue
  private var next: List[Nodo] = Nil
  private var status: Boolean = false

  def getValue(): Int = this.value

  def getWeight(): Int = this.value

  def getNext(position: Int = 0): Nodo = this.next.reverse(position)

  def getStatus(): Boolean = this.status

  def setValue(value: Int): Unit = this.value = value

  def setWeight(weight: Int): Unit = this.value = weight

  def setNext(next: Nodo): Unit = this.next = next :: this.next

  def setStatus(status: Boolean): Unit = this.status = status

  def childs(): List[Nodo] = this.next

  def minChild(): Nodo = {
    var list = this.next.sortWith((s,t) => s.getValue() < t.getValue())
    if (list != Nil) list(0) else null
  }

  def numChilds(): Int = this.next.length

}


class Grafo {
  private var Nodos: List[Nodo] = Nil

  def createNodo(value: Int): Unit = {
    val tempNodo: Nodo = new Nodo(value)
    Nodos = tempNodo :: Nodos
  }

  def allNodos(): List[Nodo] = this.Nodos

  def setChilds(me: Int, childs: List[Int]): Unit = {
    var Me: Nodo = searchNodo(me)

    if (Me != null) {
      var Childs: List[Nodo] = Nil

      for (child <- childs) {
        var tempNodo: Nodo = searchNodo(child)
        if (tempNodo != null)
          Childs = tempNodo :: Childs
      }

      if (Childs != Nil)
        for (temp <- Childs)
          Me.setNext(temp)
    }
  }

  def searchNodo(value: Int): Nodo = {
    var tempNodo: List[Nodo] = Nil

    for (nodo <- Nodos)
      if (nodo.getValue() == value)
        tempNodo = nodo :: tempNodo

    if (tempNodo != Nil) tempNodo(0) else null
  }

}


// val cinco = new Nodo(5)
// val seis = new Nodo(6)
// val siete = new Nodo(7)
// val ocho = new Nodo(8)

// println("Numero de hijos: " + cinco.numChilds())
// cinco.setNext(ocho)
// cinco.setNext(siete)
// cinco.setNext(seis)
// println("Numero de hijos: " + cinco.numChilds())
// println(cinco.getNext(2).getValue())

// for (nodo <- cinco.childs()) 
//   println(nodo.getValue())


var grafo = new Grafo()

for (value <- List(5,6,7,8))
  grafo.createNodo(value)

for (child <- List(5,6,7,8) zip (List(List(6,7,8), List(8,7), List(5,6), List(5,8))))
  grafo.setChilds(child._1, child._2)

for (nodo <- grafo.allNodos()){
  println(nodo.getValue())
  for (child <- nodo.childs())
    println("\t hijo: " + child.getValue())
  println("\t\t Nodo minimo: " + nodo.minChild().getValue())
}
