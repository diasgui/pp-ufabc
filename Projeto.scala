
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
  case class Extrato(conta: Int, Agencia: BancoDeDados) // coloquei esse terceiro parametro para CC ou Poupanca
  case class Resposta(conta: Int, montante: Double, Agencia: BancoDeDados, op: Int)
  case class NovaVazia(conta: Int, Agencia: BancoDeDados, nome:String, cpf:String, tipo:Int)
  case class NovaCheia(conta: Int, quantia: Double, Agencia: BancoDeDados, nome:String, cpf:String, tipo:Int)

  class DadosCli(val nome: String, val cpf: String){
    val tipo=1;
  }

  trait VIP extends DadosCli{
    override val tipo=2;
  }


  class BancoDeDados {
    var Banco: Map[Int, Double] = Map()
    var DadosBanco: Map[Int, DadosCli] = Map()
    def NovoUser(c:Int, nome:String, cpf: String): Unit = synchronized{ DadosBanco = DadosBanco+(c -> new DadosCli(nome,cpf)) }
    def NovoUserPremium(c:Int, nome:String, cpf: String): Unit = synchronized{ DadosBanco = DadosBanco+(c -> new DadosCli(nome,cpf) with VIP) }
    def Consultar(x:Int) = synchronized{  Banco(x)  }
    def Colocar(conta: Int, quantia: Double) = synchronized{  Banco = Banco + (conta -> quantia)  }
    def Colocar(conta: Int) = synchronized{  Banco = Banco + (conta -> 0)  }
    def Remover(conta: Int) = synchronized{  Banco = Banco - conta }
    
    def ContaInteiros(x:Int,ret:Int):Int ={
      if(x/10>0){
        ContaInteiros(x/10,ret+1)
      }
      else ret
    }
    def ContaDouble(x:Double,ret:Int): Int={
      val int = ContaInteiros(x.toInt,ret)
      int+1+((x.toString).replace(".",",").split(",").toList)(1).length
    }
    def Completa[A](ant:Int, x:A,f: A=>Int): String={
      val a = ((42-ant)-f(x))
      def go(cont:Int,ret:String): String={
        if(cont==a) ret
        else go(cont+1,ret+"-")
      }
      " "+go(0,"")
    }
    def Extrato(c:Int): Unit={
      val tipo = DadosBanco(c).tipo
      tipo match{
        case 1 =>{
          println("-------------------------------------------")
          println("------- Extrato conta normal "+c+Completa[Int](29,c, c=>ContaInteiros(c,1)))
          println("-------- Saldo: R$"+ Consultar(c)+Completa[Double](18,Consultar(c), c=>ContaDouble(c,1)))
          println("-------------------------------------------")
        }
        case 2 =>{
          println("-------------------------------------------")
          println("-------- Extrato conta premier "+c+ Completa[Int](31,c, c=>ContaInteiros(c,1)))
          println("-------- Nome: "+ DadosBanco(c).nome + Completa[String](15,DadosBanco(c).nome, c=>c.length))
          println("-------- CPF: "+ DadosBanco(c).cpf + Completa[String](14,DadosBanco(c).cpf, c=>c.length))
          println("-------- Saldo: R$"+ Consultar(c) + Completa[Double](18,Consultar(c), c=>ContaDouble(c,1)))
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
      case Extrato(c, agencia) => agencia.Extrato(c)
    }
  }

  class Gerador extends Actor {

    def receive = {
      case NovaVazia(a, agencia, nome, cpf, tipo) =>{
        agencia.Colocar(a)
        tipo match{
          case 1 => agencia.NovoUser(a, nome, cpf)
          case 2 => agencia.NovoUserPremium(a, nome, cpf)
          case _ =>{
            println("Tipo desconhecido, criado usuario padrao")
            agencia.NovoUser(a, nome, cpf)
          }
        }

      }

      case NovaCheia(a, qtd, agencia, nome, cpf, tipo) =>{
        agencia.Colocar(a, qtd)
        tipo match{
          case 1 => agencia.NovoUser(a, nome, cpf)
          case 2 => agencia.NovoUserPremium(a, nome, cpf)
          case _ =>{
            println("Tipo desconhecido, criado usuario padrao")
            agencia.NovoUser(a, nome, cpf)
          }
        }
      }
    }
  }

  // metodo para verificar se a conta já existe no banco de dados
  def verifica (cc: Option[Double]) : Boolean = cc match {
    case Some(s) => true
    case None    => false
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
    gerador ! NovaVazia(1, Agencia, "Carlos", "4502308971", 1)
    gerador ! NovaCheia(2,1000,Agencia, "Monica", "6547328218", 2)
    gerador ! NovaCheia(3,40000,Agencia, "Charles", "1765489012", 3)
    gerador ! NovaCheia(4,2750,Agencia, "Bianca", "1429807765", 1)

    println("Contas Iniciais: ")
    Await.result(Future{Agencia}, Duration.Inf).Banco.foreach((a) => println("Conta numero: " +a._1+ " tem saldo de: " + a._2 + " reais."))
    println()

    var opcao1 = -1

    repeatLoop {
      println()
      println("-------------------------------------------")
      println("---------- 1 - Criar Conta   --------------")
      println("---------- 2 - Acessar Conta --------------")
      println("---------- 3 - Listar Contas Existentes ---")
      println("---------- 0 - Sair          --------------")
      println("-------------------------------------------")
      println()
      opcao1 = scala.io.StdIn.readInt()

      opcao1 match {
        case 1 =>
          println("Digite o número da conta:") // para criar conta
          val cc = scala.io.StdIn.readInt()

          if (!verifica(Agencia.Banco.get(cc))) {

            println("Deseja fazer um depósito inicial? (Caso não, digite 0)")
            val valor = scala.io.StdIn.readDouble()

            println("Insira seu nome: ")
            val nome = scala.io.StdIn.readLine

            println("Insira seu cpf: ")
            val cpf = scala.io.StdIn.readLine

            println("Voce deseja ser um usuario premium? (Digite 2 para sim e 1 para nao)")
            val premium = scala.io.StdIn.readInt


            if (valor == 0) gerador ! NovaVazia(cc, Agencia, nome, cpf, premium)
            else gerador ! NovaCheia(cc, valor, Agencia, nome, cpf, premium)
            Logar(cc)
          }
          else
            println("Conta já existe.")
        case 2 =>
          println("Digite o número da conta: ") // para acessar a conta
          val cc = scala.io.StdIn.readInt()
          if (verifica(Agencia.Banco.get(cc)))
            Logar(cc) // precisa fazer a verificacao da conta (se ela já existe ou não)
          else
            println("Conta não existe.")
        case 3 =>
          println("Existe as contas disponíveis: ")
          Await.result(Future {
            Agencia
          }, Duration.Inf).Banco.foreach((a) => println("Conta: " + a._1 + " disponível para operações."))
        case 0 =>
          // Antes de sair do sistema, ele imprime todas as contas após as operacoes
          println("Contas resultantes (Após as operações):\n")
          Await.result(Future{Agencia}, Duration.Inf).Banco.foreach((a) => println("Conta numero: " +a._1+ " tem saldo de: " + a._2 + " reais."))
          System.exit(0)
      }

      def Logar(cc: Int): Unit = {
        var opcao2 = -1
        repeatLoop {
          println()
          println("-------------------------------------------")
          println("---------- 1 - Saque         --------------")
          println("---------- 2 - Transferencia --------------")
          println("---------- 3 - Deposito      --------------")
          println("---------- 4 - Extrato       --------------")
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
              Agencia.Extrato(cc)
            case 0 => println("Logged out")
            case _ => println("Caso invalido")
          }
        } until (opcao2 != 0)
      }
    } until (opcao1 != 0)
  }
}
