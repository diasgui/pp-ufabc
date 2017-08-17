
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
  case class Saque(conta: Int, quantia: Double, Agencia: BancoDeDados, op: Int) // Coloquei esse op para ser operação de imprimir ou nao
  case class Deposito(conta: Int, quantia: Double, Agencia: BancoDeDados, op: Int)
  case class Transferencia(mensageiro: Int, quantia: Double, receptor: Int, Agencia: BancoDeDados, op: Int)
  case class Extrato(conta: Int, Agencia: BancoDeDados, tipo: Int) // coloquei esse terceiro parametro para CC ou Poupanca
  case class Resposta(conta: Int, montante: Double, Agencia: BancoDeDados, op: Int)
  case class NovaVazia(conta: Int, Agencia: BancoDeDados, nome:String, cpf:String)
  case class NovaCheia(conta: Int, quantia: Double, Agencia: BancoDeDados, nome:String, cpf:String)

  class DadosCli(val nome: String, val cpf: String){
  }
  
  trait Fancy extends BancoDeDados{
    override def Extrato(c:Int,tipo:Int)={
      println("-----  Esta aqui o seu extrato querido cliente: -----")
      super.Extrato(c,tipo)
    }
  }
 
  
  class BancoDeDados {
    var Banco: Map[Int, Double] = Map()
    var DadosBanco: Map[Int, DadosCli] = Map()
    def NovoUser(c:Int, nome:String, cpf: String): Unit = synchronized{ DadosBanco = DadosBanco+(c -> new DadosCli(nome,cpf)) }
    def Consultar(x:Int) = synchronized{  Banco(x)  }
    def Colocar(conta: Int, quantia: Double) = synchronized{  Banco = Banco + (conta -> quantia)  }
    def Colocar(conta: Int) = synchronized{  Banco = Banco + (conta -> 0)  }
    def Remover(conta: Int) = synchronized{  Banco = Banco - conta }
    def Extrato(c:Int, tipo:Int): Unit={
      tipo match{
        case 1 =>{
          println("--------- Extrato conta normal "+c+" ---------")
          println("Nome: "+ DadosBanco(c).nome)
          println("CPF: " + DadosBanco(c).cpf)
          println("Saldo: "+ Consultar(c))
          println("------------------------------------------")
        }
        case 2 =>{
          println("-------- Extrato conta premier "+c+" --------")
          println("Nome: "+ DadosBanco(c).nome)
          println("CPF: "+ DadosBanco(c).cpf)
          println("Saldo: "+ Consultar(c))
          println("Aniversario: ")
          println("-------------------------------------------")
        }
      }
    }
  }

  //Esta classe funciona como um adm, ela recebe uma tarefa e responde o montante que ficará na conta que sofreu a alteração
  class Adm extends Actor {

    def receive = {

      case Saque(conta, quantia, agencia, op) => {
        if (op == 1) // op == 1 -> imprimir o resultado do saque. op == 0 -> Não imprimir, saque sendo realizado em background
          println("Conta "+conta+": Saldo antes do saque: R$ "+agencia.Consultar(conta)+". Foi realizado o saque de: R$ "+quantia+".")

        if(agencia.Consultar(conta) > quantia)  sender() ! Resposta(conta, agencia.Consultar(conta) - quantia, agencia, op)
        else sender() ! Resposta(conta, -1, agencia, op)
      }

      case Deposito(conta, quantia, agencia, op) => {
        if (op == 1)
          println("Conta "+conta+": Saldo antes do depósito: R$" +agencia.Consultar(conta)+". Foi realizado o depósito de: R$ "+quantia+".")

        sender() ! Resposta(conta, quantia + agencia.Consultar(conta), agencia, op)
      }
    }
  }

  class Cliente(servidor: ActorRef) extends Actor {

    def receive = {

      case Resposta(c,-1, agencia, op) => if (op == 1) println("Valor indisponível para saque na conta "+c)

      case Resposta(c, q, agencia, op) => {
        if (op == 1)
          println("Conta " +c+ ": Estava com saldo de R$ " +agencia.Consultar(c)+ ". Passou a ter saldo de R$ "+q+".")

        agencia.Colocar(c,q)
      }
      case Deposito(c, q, agencia, op) => servidor ! Deposito(c, q, agencia, op)

      case Saque(c, q, agencia, op) => servidor ! Saque(c, q, agencia, op)

      case Transferencia(msg, qtd, rcp, agencia, op) => {
        if(agencia.Consultar(msg) >= qtd){
          servidor ! Saque(msg, qtd, agencia, op)
          servidor ! Deposito(rcp, qtd, agencia, op)
        }
        else if (op == 1) {
          println("Saque não efetuado, pois o usuário não saldo suficiente.")
        }
      }
      case Extrato(c, agencia, t) => agencia.Extrato(c, t)
    }
  }

  class Gerador extends Actor {

    def receive = {
      case NovaVazia(a, agencia, nome, cpf) =>{
        agencia.Colocar(a)
        agencia.NovoUser(a, nome, cpf)
      }

      case NovaCheia(a, qtd, agencia, nome, cpf) =>{
        agencia.Colocar(a, qtd)
        agencia.NovoUser(a, nome, cpf)
      }
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
    gerador ! NovaVazia(1, Agencia, "Carlos", "4502308971")
    gerador ! NovaCheia(2,1000,Agencia, "Monica", "6547328218")
    gerador ! NovaCheia(3,40000,Agencia, "Charles", "1765489012")
    gerador ! NovaCheia(4,2750,Agencia, "Bianca", "1429807765")

    println("Contas Iniciais: ")
    Await.result(Future{Agencia}, Duration.Inf).Banco.foreach((a) => println("Conta numero: " +a._1+ " tem saldo de: " + a._2 + " reais."))
    println()

    var opcao1 = -1
    var cc = 0

      repeatLoop {
        println()
        println("-------------------------------------------")
        println("---------- 1 - Criar Conta   --------------")
        println("---------- 2 - Acessar Conta --------------")
        println("---------- 0 - Sair          --------------")
        println("-------------------------------------------")
        println()
        opcao1 = scala.io.StdIn.readInt()

        opcao1 match {
          case 1 =>
            println("Digite o número da conta:") // para criar conta
            cc = scala.io.StdIn.readInt()

            println("Deseja fazer um depósito inicial? (Caso não, digite 0)")
            val valor = scala.io.StdIn.readDouble()
            
            println("Insira seu nome: ")
            val nome = scala.io.StdIn.readLine
            
            println("Insira seu cpf: ")
            val cpf = scala.io.StdIn.readLine

            if (valor == 0) gerador ! NovaVazia(cc, Agencia, nome, cpf)
            else            gerador ! NovaCheia(cc, valor, Agencia, nome, cpf)
          case 2 =>
            println("Digite o número da conta: ") // para acessar a conta
            cc = scala.io.StdIn.readInt() // precisa fazer a verificacao da conta (se ela já existe ou não)
          case 0 =>
            // Antes de sair do sistema, ele imprime todas as contas após as operacoes
            println("Contas resultantes (Após as operações):\n")
            Await.result(Future{Agencia}, Duration.Inf).Banco.foreach((a) => println("Conta numero: " +a._1+ " tem saldo de: " + a._2 + " reais."))
            System.exit(0)
        }

        var opcao2 = -1
        repeatLoop {
          println()
          println("-------------------------------------------")
          println("---------- 1 - Saque         --------------")
          println("---------- 2 - Transferencia --------------")
          println("---------- 3 - Deposito      --------------")
          println("---------- 4 - Listar Contas Existentes ---")
          println("---------- 5 - Extrato       --------------")
          println("---------- 0 - Sair da Conta --------------")
          println("-------------------------------------------")
          println()
          opcao2 = scala.io.StdIn.readInt()
          opcao2 match {
            // Deixei as contas que foram criadas de testes e suas operaçes para mostra a concorrencia
            // enquanto o cliente faz a operação, ocorre simultaneamente operaçes em outras contas
            case 1 =>
              println("Digite o valor para saque:")
              cliente ! Saque(cc, scala.io.StdIn.readDouble(), Agencia, 1) // 1 para imprimir o resultado do saque
              Thread.sleep(200)

              cliente ! Saque(3, 10000, Agencia, 0)
              Thread.sleep(200)
            case 2 =>
              println("Digite o valor para transferencia: ")
              val value = scala.io.StdIn.readDouble()

              println("Digite o numero da conta destino: ")
              cliente ! Transferencia(cc, value, scala.io.StdIn.readInt(), Agencia, 1) // 1 para imprimir o resultado da transferencia
              Thread.sleep(200)

              cliente ! Transferencia(2, 500, 1, Agencia, 0)
              Thread.sleep(200)

              cliente ! Transferencia(2, 800, 4, Agencia, 0)
              Thread.sleep(200)
            case 3 =>
              println("Digite o valor para depósito: ")
              cliente ! Deposito(cc, scala.io.StdIn.readDouble(), Agencia, 1) // 1 para imprimir o resultado do deposito
              Thread.sleep(200)

              cliente ! Deposito(2, 700, Agencia, 0)
              Thread.sleep(200)
            case 4 =>
              println("Existe as contas disponíveis: ")
              Await.result(Future {
                Agencia
              }, Duration.Inf).Banco.foreach((a) => println("Conta: " + a._1 + " disponível para operações."))
            case 5 =>
              println("Não implementado..")
            //case 0 =>
              //opcao2 = 0
          }
        } until (opcao2 != 0)
      } until (opcao1 != 0)
  }
}
