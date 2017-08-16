//As modificações acumuladas:
//Eu mudei o banco de dados de um Map para uma classe para poder sofisticar mais o projeto
//Eu escrevi um exemplo de main para exemplificar o que o programa faz atualmente
//Mudancas nos prints
//Aprimoraçao do menu principal

//O que seria legal ainda fazer:
//Implementar o extrato
//Criar mais opcoes de contas (cc, poupanca)
//Aprimorar o armazenamento das contas (colocar nome, cpf, aniversario (poupanca))
//Já temos funçoes de alta ordem??

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

            if (valor == 0) gerador ! NovaVazia(cc, Agencia)
            else            gerador ! NovaCheia(cc, valor, Agencia)
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
            case 0 =>
              opcao2 = 0
          }
        } until (opcao2 != 0)
      } until (opcao1 != 0)
  }
}
