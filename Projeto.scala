import akka.actor._
import akka.actor.ActorSystem

import scala.collection.mutable
import scala.collection.mutable._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object ProduzRelatorio {
  case class Saque(conta: Int, quantia: Double, Agencia: BancoDeDados)
  case class Deposito(conta: Int, quantia: Double, Agencia: BancoDeDados)
  case class Transferencia(mensageiro: Int, quantia: Double, receptor: Int, Agencia: BancoDeDados)
  case class Extrato(conta: Int, Agencia: BancoDeDados, tipo: Int) // coloquei esse terceiro parametro para CC ou Poupanca
  case class Resposta(conta: Int, montante: Double, Agencia: BancoDeDados)
  case class NovaVazia(conta: Int, Agencia: BancoDeDados)
  case class NovaCheia(conta: Int, quantia: Double, Agencia: BancoDeDados)

  class BancoDeDados {
    var Banco: Map[Int, Double] = Map()
    def Consultar(x:Int) = synchronized{  Banco(x)  }
    def Colocar(conta: Int, quantia: Double) = synchronized{  Banco = Banco + (conta -> quantia)  }
    def Colocar(conta: Int) = synchronized{  Banco = Banco + (conta -> 0)  }
    def Remover(conta: Int) = synchronized{  Banco = Banco - conta }
  }

  //Esta classe funciona como um adm, ela recebe uma tarefa e responde o montante que ficará na conta que sofreu a alteração
  class Adm extends Actor {

    def receive = {

      case Saque(conta, quantia, agencia) => {
        println("A conta " +conta+ " estava com " +agencia.Consultar(conta)+ " mas perdeu " +quantia)
        if(agencia.Consultar(conta) > quantia)  sender() ! Resposta(conta, agencia.Consultar(conta) - quantia, agencia)
        else sender() ! Resposta(conta, -1, agencia)
      }

      case Deposito(conta, quantia, agencia) => {
        println("A conta " +conta+ " estava com " +agencia.Consultar(conta)+ " mas ganhou " +quantia)
        sender() ! Resposta(conta, quantia + agencia.Consultar(conta), agencia)
      }
    }
  }

  class Cliente(servidor: ActorRef) extends Actor {

    def receive = {

      case Resposta(c,-1, agencia) => println("Valor indisponível para saque na conta "+c)

      case Resposta(c, q, agencia) => {
        println("A conta " +c+ " estava com " +agencia.Consultar(c)+ " ficou com "+q)
        agencia.Colocar(c,q)
      }
      case Deposito(c, q, agencia) => servidor ! Deposito(c, q, agencia)

      case Saque(c, q, agencia) => servidor ! Saque(c, q, agencia)

      case Transferencia(msg, qtd, rcp, agencia) => {
        if(agencia.Consultar(msg) >= qtd){
          servidor ! Saque(msg, qtd, agencia)
          servidor ! Deposito(rcp, qtd, agencia)
        }
        else{
          println("Saque não efetuado, pois o usuário não possui essa quantia")
        }
      }

      case Extrato(c, agencia, 1) => { // 1 para conta corrente (Se adicionarmos parametros como Nome, cpf, etc, podemos colocar aqui tbm
        println("--------- Extrato da conta "+c+" ---------")
        println("Nome: ")
        println("CPF: ")
        println("Saldo: "+ agencia.Consultar(c))
        println("------------------------------------------")
      }

      case Extrato(c, agencia, 2) => { // 2 para poupanca
        println("-------- Extrato da poupanca "+c+" --------")
        println("Nome: ")
        println("CPF: ")
        println("Saldo: "+ agencia.Consultar(c))
        println("Aniversario: ")
        println("-------------------------------------------")
      }
    }
  }

  class Gerador extends Actor {

    def receive = {
      case NovaVazia(a, agencia) => agencia.Colocar(a)

      case NovaCheia(a, qtd, agencia) => agencia.Colocar(a, qtd)
    }
  }

  def repeatLoop(body: => Unit) = new Until(body)
  // do-while recursivo para criacao do menu no main
  class Until(body: => Unit) {
    def until(cond: => Boolean): Unit = { // tailrec
      body
      if (cond) until(cond)
    }
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("System")
    val servidor = system.actorOf(Props[Adm])
    val gerador = system.actorOf(Props[Gerador])
    val cliente = system.actorOf(Props(new Cliente(servidor)))
    val Agencia = new BancoDeDados;

    //Por motivos de teste, vamos adicionar alguns usuários
    gerador ! NovaVazia(1, Agencia)
    gerador ! NovaCheia(2,1000,Agencia)
    gerador ! NovaCheia(3,40000,Agencia)
    gerador ! NovaCheia(4,2750,Agencia)

    var x = -1
    repeatLoop {
      println()
      println("-------------------------------------------")
      println("---------- 1 - Saque         --------------")
      println("---------- 2 - Transferencia --------------")
      println("---------- 3 - Deposito      --------------")
      println("---------- 0 - Sair          --------------")
      println("-------------------------------------------")
      println()
      x = scala.io.StdIn.readInt()
        x match {
        case 1 =>
          cliente ! Saque(3, 10000, Agencia)
          Thread.sleep(200)
        case 2 =>
          cliente ! Transferencia(2,500,1,Agencia)
          Thread.sleep(200)

          cliente ! Transferencia(2,800,4,Agencia)
          Thread.sleep(200)
        case 3 =>
          cliente ! Deposito(2,700,Agencia)
          Thread.sleep(200)
        case 0 =>
          x = -1
      }
    } until (x != -1)

    Await.result(Future{Agencia}, Duration.Inf).Banco.foreach((a) => println("A conta " +a._1+ " possui " + a._2 + " reais."))
  }


}
