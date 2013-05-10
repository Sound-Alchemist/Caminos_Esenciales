import java.io.PrintWriter
import scala.io.Source
import scala.sys.process._
import scala.util.control.Breaks._

class Stack {
  private var stack: List[Nodo] = Nil

  def push(value: Nodo) = stack = value :: stack

  def pop(): Nodo = {
    val head = stack.head
    stack = stack.tail
    head
  }

  def length(): Int = stack.length

  def stackToList(value: Int): List[Nodo] = {
    var list: List[Nodo] = Nil
    list = stack.head :: list
    breakable {
      for (nodo <- stack.tail){
        list = nodo :: list
        if (nodo.getValue() == value)
          break
      }}
    list.reverse
  }


  def exist(nodo: Nodo): Boolean = {
    for (element <- stack)
      if (nodo.getValue() == element.getValue())
        return true
    false
    }
}

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

  def childs(): List[Nodo] = this.next.reverse

  def minChild(): List[Nodo] = 
    this.next.sortWith((s,t) => s.getValue() < t.getValue())

  def numChilds(): Int = this.next.length

}


class Grafo {
  private val rDigit = "\\d+".r
  private val orDigit = "^\\d+$".r
  private var Nodos: List[Nodo] = Nil

  private var stack = new Stack()
  var listCycle: List[List[Nodo]] = Nil

  def createNodo(value: Int): Nodo = {
    val tempNodo: Nodo = new Nodo(value)
    Nodos = tempNodo :: Nodos
    tempNodo
  }

  def allNodos(): List[Nodo] = this.Nodos.reverse

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
        Childs map (child => Me.setNext(child))
    }
  }

  def searchNodo(value: Int): Nodo = {
    var tempNodo: List[Nodo] = Nil

    for (nodo <- Nodos)
      if (nodo.getValue() == value)
        tempNodo = nodo :: tempNodo

    if (tempNodo != Nil) tempNodo(0) else null
  }

  def matrixFile(file: String): Unit = {
    try {
      val lines = Source.fromFile(file).getLines.toList

      if (this.orDigit.findFirstIn(lines(0)) != None)
        println("Numero de nodos: " + lines(0).toInt)

      for (nodo <- 1 to lines(0).toInt)
        createNodo(nodo)

      for (line <- lines.tail) {
        val nChild = line.split("\\s")

        if (nChild.length > 1){
          setChilds(nChild(0).toInt,
            line.split("\\s").tail.map(x => x.toInt).toList)}
      }
    } catch {
      case e: Exception =>
        println("ERROR: " + e)
    }
  }

  def bfs(root: Nodo, 
          father: Nodo = null,
          grafo: Grafo = null): Grafo = {

    var newGrafo: Grafo = null

    if (grafo == null) {
      println("\n== Busqueda en profundidad ==")
      newGrafo= new Grafo()
      listCycle = Nil
    }
    else
      newGrafo = grafo
      
    stack.push(newGrafo.createNodo(root.getValue()))

    if (father != null) 
      newGrafo.setChilds(father.getValue(), List(root.getValue()))

    print(root.getValue() + " ")
    root.setStatus(true)

    for (child <- root.childs()){
      if (!child.getStatus()) 
        bfs(child, root, newGrafo)
      if (stack.exist(child)) {
        // println(root.getValue() + " --> " + child.getValue())
        stack.push(child)
        newGrafo.setChilds(root.getValue(), List(child.getValue()))
        listCycle = stack.stackToList(child.getValue()) :: listCycle
        stack.pop()
        // return newGrafo
      }
    }
    
    stack.pop()
    newGrafo
  }

  def cleanNodos(): Unit = Nodos.map(nodo => nodo.setStatus(false))

  def Bfs(Number: Int = -1): List[Grafo] = {
    var newList: List[Grafo] = Nil
    listCycle = Nil
    if (Number != -1) newList = bfs(searchNodo(Number)) :: newList
    for (nodo <- allNodos())
      if (!nodo.getStatus())
        newList = bfs(nodo) :: newList
    newList
  }


  def printMatrix(): Unit = {
    for (nodo <- allNodos()){
      println("Nodo :: " + nodo.getValue())
      for (child <- nodo.childs())
        println("     * hijo: " + child.getValue())
    }
  }

  def generateGrafo(Numb: Int = -1, 
                    name :String = "grafo") : Unit = {
    val ciclos = new PrintWriter(name + ".gv")
    var Number = -1

    ciclos.println("digraph G {")
    if (Numb == -1)
      Number = allNodos().head.getValue()
    else
      Number = Numb

    println("\n--> Generando grafo <--")
    for (nodo <- bfs(searchNodo(Number)).allNodos()){
      for (child <- nodo.childs()){
        // print("\n"+nodo.getValue() + " -> " + 
        //         child.getValue() + ";")
        ciclos.println("    " + nodo.getValue() +
          " -> " + child.getValue() + ";")
      }
    }

    ciclos.println("}")
    ciclos.close()
    def compileGrafo() =
      Seq("dot", "-Tpng", name+".gv", "-o", name+".png").! == 0
    println("\n")
    if (compileGrafo()) println("-*- Grafo generado correctamente -*-")

  }


  def cycles(): Unit = {
    println("*** Ciclos Particulares del grafo ***")
    for (Cycle <- grafo.listCycle) {
      for (nodo <- Cycle.reverse) {
        print(nodo.getValue() + " -> ")
      }
      println()
    }
  }

}


var grafo = new Grafo()

grafo.matrixFile("entrada.dat")
grafo.printMatrix()

for (grafos <- grafo.Bfs(7))
  grafos.generateGrafo(name="grafo-" + 
                       grafos.allNodos().head.getValue())

println()
grafo.cleanNodos()
grafo.generateGrafo(1)
grafo.cycles()

 
