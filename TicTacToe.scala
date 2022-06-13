import scala.annotation.tailrec
import scala.io.StdIn.readLine

object TicTacToe {
  val gameboard: Array[Char] = ('1' to '9').toArray
  val combinations: Array[Array[Int]] = Array(
    Array(0, 1, 2),
    Array(3, 4, 5),
    Array(6, 7, 8),
    Array(0, 3, 6),
    Array(1, 4, 7),
    Array(2, 5, 8),
    Array(0, 4, 8),
    Array(2, 4, 6)
  )

  def main(args: Array[String]): Unit = {
    startgame()
  }

  def startgame(): Unit ={
    println("Tic Tac Toe")
    println("Enter a number (1 to 9) to play the game")
    dispBoard(gameboard)
    swapplayer(gameboard)
  }

  def swapplayer(board: Array[Char]): Unit = {
    val nextMove = getposition(board)
    board(nextMove) = nextPlayer(board)
    dispBoard(board)
    if (winner(board)) {
      return
    }
    if (game(tiegame, board)) {
      print("It's a TIE")
      return
    }
    swapplayer(board)
  }

  def getposition(board: Array[Char]): Int = {
    val input = readLine("Enter a position in the board: ")
    if (input.matches("[1-9]")) {
      val move = input.toInt - 1
      if (board(move).isDigit) {
        move
      } else {
        println("The position is already filled")
        getposition(board)
      }
    } else {
      println("Invalid Input, The number should be between 1 and 9")
      getposition(board)
    }
  }

  def nextPlayer(board: Array[Char]): Char = {
    val remainingTurns = board.count(_.toString.matches("[1-9]"))
    if(remainingTurns%2 == 0) 'O' else 'X'
  }

  def dispBoard(board: Array[Char]): Unit = {
  println(
    board.grouped(3)
      .map(_.mkString(" | "))
      .mkString("---------\n", "\n---------\n", "\n---------\n")
      )
      println("Player " + nextPlayer(board))
  }
 
  def winner(board: Array[Char]): Boolean = {
    combinations.foreach(pattern =>{
      if(pattern.forall(board(_) == board(pattern.head))) {
        print("Winner is player " + board(pattern.head))
        return true
      }
    })
    return false
  }

  def game(f: Array[Char] => Boolean, x: Array[Char]) = f(x)
  def tiegame(board: Array[Char]): Boolean = {
    for( i <- 0 to board.length-1 )
    {
      if(board(i).isDigit){
        return false
      }
    }
    return true
  }
}
