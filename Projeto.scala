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
  case class Saque(conta: Int,quantia: Double,Agencia:BancoDeDados)
  case class Deposito(conta: Int,quantia: Double,Agencia:BancoDeDados)
  case class Transferencia(mensageiro: Int,quantia: Double,receptor:Int,Agencia:BancoDeDados)
  case class Extrato(conta: Int,Agencia:BancoDeDados)
  case class Resposta(conta: Int, montante: Double)
  
  
  class BancoDeDados{
    var Banco: Map[Int, Double] = Map();
    def Colocar(conta: Int, quantia: Double) = synchronized{Banco = Banco+(conta->quantia)}
    def Remover(conta:Int) = synchronized{Banco = Banco - conta}
  }
  
  //Esta classe funciona como um adm, ela recebe uma tarefa e responde o montante que ficará na conta que sofreu a alteração
  class Adm extends Actor {

    def receive = {
      case Saque(conta, quantia,agencia) => {
        if(agencia.Banco(conta)>quantia)  sender() ! Resposta(conta,agencia.Banco(conta) - quantia)
        else sender() ! Resposta(conta,-1)
      }
      case Deposito(conta, quantia,agencia) => sender() ! Resposta(conta,quantia + agencia.Banco(conta))
    }
    
  }
  
  
  class Cliente(servidor: ActorRef, Agencia: BancoDeDados) extends Actor {

    def receive = {
      case Resposta(c,-1) => println("Valor indisponível para saque na conta "+c) 
      case Resposta(c,q) => Agencia.Colocar(c,q)
      case Deposito(c,q,Agencia) => servidor ! Deposito(c,q,Agencia)
      case Saque(c,q,Agencia) => servidor ! Saque(c,q,Agencia)
      case Transferencia(msg,qtd,rcp,Agencia) =>{
        if(Agencia.Banco(msg)>=qtd){
          servidor ! Saque(msg, qtd,Agencia)
          servidor ! Deposito(rcp, qtd,Agencia)
        }
        else{
          println("Saque não efetuado, pois o usuário não possui essa quantia")
        }
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("System")
    val servidor = system.actorOf(Props[Adm])
    val Agencia = new BancoDeDados;
    val cliente = system.actorOf(Props(new Cliente(servidor,Agencia)))
    
    
  }
    

}
