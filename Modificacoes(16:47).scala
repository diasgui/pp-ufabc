//As modificações foram:
//Eu mudei o banco de dados de um Map para uma classe para poder sofisticar mais o projeto
//Eu escrevi um exemplo de main para exemplificar o que o programa faz atualmente

//O que era importante fazer:
//Modificar o banco para podermos inserir nome, cpf, aniversario...
//Tentar utilizar o foldLeft em algum momento (se bem que eu uso o foreach pra imprimir, que é bem similar)
//Criar um extrato
//Criar uma trait que modifica o extrato

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
    def Colocar(conta:Int) = synchronized{Banco = Banco + (conta->0)}
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

    //Por motivos de teste, vamos adicionar alguns usuários
    Agencia.Colocar(1)
    Agencia.Colocar(2,1000)
    Agencia.Colocar(3,40000)
    Agencia.Colocar(4,2750)
    
    val cliente = system.actorOf(Props(new Cliente(servidor,Agencia)))
    cliente ! Saque(3, 10000, Agencia)
    cliente ! Transferencia(2,500,1,Agencia)
    cliente ! Deposito(2,700,Agencia)
    
    Await.result(Future{Agencia}, Duration.Inf).Banco.foreach(println(_))
  }
    

}
