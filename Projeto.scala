import akka.actor._
import akka.actor.ActorSystem
import scala.collection.mutable._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Success, Failure }
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object ProduzRelatorio {
  case class Saque(conta: Int,quantia: Double)
  case class Deposito(conta: Int,quantia: Double)
  case class Transferencia(mensageiro: Int,quantia: Double,receptor:Int)
  case class Extrato(conta: Int)
  case class Resposta(conta: Int, montante: Double)
  
  
  var Banco: Map[Int, Double] = Map();
  
  //Esta classe funciona como um adm, ela recebe uma tarefa e responde o montante que ficará na conta que sofreu a alteração
  class Adm extends Actor {

    def receive = {
      case Saque(conta, quantia) => {
        if(Banco(conta)>quantia)  sender() ! Resposta(conta,Banco(conta) - quantia)
        else sender() ! Resposta(conta,-1)
      }
      case Deposito(conta, quantia) => sender() ! Resposta(conta,quantia + Banco(conta))
    }
    
  }
  
  def Colocar(conta: Int,qtd: Double): Unit= synchronized{ 
    Banco = Banco+(conta -> qtd)
  }
  
  class Cliente(servidor: ActorRef) extends Actor {

    def receive = {
      case Resposta(c,-1) => println("Valor indisponível para saque na conta "+c) 
      case Resposta(c,q) => Colocar(c,q)
      case Deposito(c,q) => servidor ! Deposito(c,q)
      case Saque(c,q) => servidor ! Saque(c,q)
      case Transferencia(msg,qtd,rcp) =>{
        servidor ! Saque(msg, qtd)
        servidor ! Deposito(rcp, qtd)
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("System")
    val servidor = system.actorOf(Props[Adm])
    val cliente = system.actorOf(Props(new Cliente(servidor)))
    
    
  }
    

}
