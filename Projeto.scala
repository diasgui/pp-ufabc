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
  /*Parametros:
   conta -> numero da conta;                                      quantia -> valor a ser sacado;
   Agencia -> Onde está armazenado as informações do cliente;     op -> opcao para impressao na tela do saque;
   mensageiro -> conta origem;                                    receptor -> conta destino;
   montante -> valor total do saldo;
  */

  // Classe utilizada como mensagem pelos atores para realizar o saque
  case class Saque(conta: Int, quantia: Double, Agencia: BancoDeDados, op: Int)
  // Classe utilizada como mensagem pelos atores para realizar o deposito
  case class Deposito(conta: Int, quantia: Double, Agencia: BancoDeDados, op: Int)
  // Classe utilizada como mensagem pelos atores para realizar a transferencia
  case class Transferencia(mensageiro: Int, quantia: Double, receptor: Int, Agencia: BancoDeDados, op: Int)
  // Classe utilizada como mensagem pelos atores para imprimir o extrato
  case class Extrato(conta: Int, Agencia: BancoDeDados)
  // Classe utilizada como mensagem resposta para as operações do banco
  case class Resposta(conta: Int, montante: Double, Agencia: BancoDeDados, op: Int)
  // Classe utilizada como mensagem para criação de uma conta vazia (sem saldo)
  case class NovaVazia(conta: Int, Agencia: BancoDeDados, nome:String, cpf:String, tipo:Int)
  // Classe utilizada como mensagem para criação de uma conta com um saldo (quantia)
  case class NovaCheia(conta: Int, quantia: Double, Agencia: BancoDeDados, nome:String, cpf:String, tipo:Int)

  // Objeto que poussui os dados do cliente
  class DadosCli(val nome: String, val cpf: String) {
    val tipo = 1 // Tipo 1 -> conta normal; Tipo 2 -> conta VIP
  }

  // trait que modifica o comportamento da classe DadosCli para que seja VIP
  trait VIP extends DadosCli {
    override val tipo = 2
  }

  class BancoDeDados {
    var Banco: Map[Int, Double] = Map() // Armazena as informações do banco. Numero da conta -> Saldo
    var DadosBanco: Map[Int, DadosCli] = Map() // Armazena as informações do cliente. Numero da conta -> DadosCli
    // cria um novo usuario do banco. Sincronizado para haver consistência nas informações.
    def NovoUser(c:Int, nome:String, cpf: String): Unit = synchronized{ DadosBanco = DadosBanco+(c -> new DadosCli(nome,cpf)) }
    // cria um novo usuário premium do banco. Também sincronizado.
    def NovoUserPremium(c:Int, nome:String, cpf: String): Unit = synchronized{ DadosBanco = DadosBanco+(c -> new DadosCli(nome,cpf) with VIP) }
    // método para consultar o banco, retorna o saldo. Também sincronizado.
    def Consultar(x:Int) = synchronized{  Banco(x)  }
    // método para colocar a quantia desejada na conta. Também sincronizado.
    def Colocar(conta: Int, quantia: Double) = synchronized{  Banco = Banco + (conta -> quantia)  }
    // método para colocar o valor 0 no saldo. Para o caso de criar uma conta sem saldo. Também sincronizado.
    def Colocar(conta: Int) = synchronized{  Banco = Banco + (conta -> 0)  }
    // método para realizar saque. Também sincronizado.
    def Remover(conta: Int) = synchronized{  Banco = Banco - conta }

    /*
    ContaInteiros, ContaDouble e Completa:
    Métodos criados para alinhar a quantidade de "-" nos extratos.
    O método completa é uma função de alta ordem pois recebe uma função f como parâmetro. Possui também
    o polimorfismo paramétrico, visto que será usada com diferentes tipos de dados (int, double, string)
     */

    //tail rec
    def ContaInteiros(x:Int,ret:Int):Int ={
      if(x/10>0)
        ContaInteiros(x/10,ret+1)
      else ret
    }

    def ContaDouble(x:Double,ret:Int): Int={
      val int = ContaInteiros(x.toInt,ret)
      int+1+((x.toString).replace(".",",").split(",").toList)(1).length
    }


    def Completa[A](ant:Int, x:A,f: A=>Int): String={
      val a = ((42-ant)-f(x))
      @annotation.tailrec
      def go(cont:Int,ret:String): String={
        if(cont==a) ret
        else go(cont+1,ret+"-")
      }
      " "+go(0,"")
    }

    // Método para impressão do extrato
    def Extrato(c:Int): Unit={
      val tipo = DadosBanco(c).tipo
      tipo match {
        case 1 => {
          println("-------------------------------------------")
          println("------- Extrato conta normal "+c+Completa[Int](29,c, c=>ContaInteiros(c,1))) // Numéro da conta: Int => Completa[Int]
          println("-------- Saldo: R$"+ Consultar(c)+Completa[Double](18,Consultar(c), c=>ContaDouble(c,1))) // Saldo: Double => Completa[Double]
          println("-------------------------------------------")
        }
        case 2 => {
          println("-------------------------------------------")
          println("-------- Extrato conta premier "+c+ Completa[Int](31,c, c=>ContaInteiros(c,1))) // Número da conta: Int => Completa[Int]
          println("-------- Nome: "+ DadosBanco(c).nome + Completa[String](15,DadosBanco(c).nome, c=>c.length)) // Nome: String => Completa[String]
          println("-------- CPF: "+ DadosBanco(c).cpf + Completa[String](14,DadosBanco(c).cpf, c=>c.length)) // CPF: String => Completa[String]
          println("-------- Saldo: R$"+ Consultar(c) + Completa[Double](18,Consultar(c), c=>ContaDouble(c,1))) // Saldo: Double => Completa[String]
          println("-------------------------------------------")
        }
      }
    }
  }

  //Esta classe funciona como um administrador, ela recebe uma tarefa e responde o montante que ficará na conta que sofreu a alteração
  class Adm extends Actor {

    def receive = {
      // tarefa de saque.
      case Saque(conta, quantia, agencia, op) => {
        if(agencia.Consultar(conta) >= quantia)  sender() ! Resposta(conta, agencia.Consultar(conta) - quantia, agencia, op) // Tem saldo para saque
        else sender() ! Resposta(conta, -1, agencia, op) // É passado o "-1" na resposta pois não tem saldo suficiente para saque
      }

      // tarefa de saque
      case Deposito(conta, quantia, agencia, op) => {
        sender() ! Resposta(conta, quantia + agencia.Consultar(conta), agencia, op)
      }
    }
  }

  // Classe Cliente que se comunica com o Adminstrador (passado por parâmetro)
  class Cliente(servidor: ActorRef) extends Actor {

    def receive = {
      // Não conseguiu realizar o saque
      case Resposta(c,-1, agencia, op) => if (op == 1) println("Valor indisponível para saque na conta "+c)

      // Conseguiu realizar a operação. Seja ela de Saque, Transferência ou Depósito.
      case Resposta(c, q, agencia, op) => {
        if (op == 1)
          println("Conta " +c+ ": Estava com saldo de R$ " +agencia.Consultar(c)+ ". Passou a ter saldo de R$ "+q+".")

        agencia.Colocar(c,q)
      }

      // Envia a tarefa de depósito para o Administrador
      case Deposito(c, q, agencia, op) => servidor ! Deposito(c, q, agencia, op)

      // Envia a tarefa de saque para o Administrador
      case Saque(c, q, agencia, op) => servidor ! Saque(c, q, agencia, op)

      // Envia a tarefa de transferencia para o Administrador
      case Transferencia(msg, qtd, rcp, agencia, op) => {
        if(agencia.Consultar(msg) >= qtd){
          servidor ! Saque(msg, qtd, agencia, op)
          servidor ! Deposito(rcp, qtd, agencia, op)
        }
        else if (op == 1) { // op == 1 -> opcao valida para impressao, caso o cliente não tenha saldo para transferencia
          println("Transferência não efetuada, pois o usuário não tem saldo suficiente.")
        }
      }
      // Envia mensagem para impressão do extrato
      case Extrato(c, agencia) => agencia.Extrato(c)
    }
  }

  // Classe para gerar contas
  class Gerador extends Actor {

    def receive = {
      case NovaVazia(a, agencia, nome, cpf, tipo) => { // Gera uma conta vazia
        agencia.Colocar(a)
        tipo match {
          case 1 => agencia.NovoUser(a, nome, cpf) // Usuário normal
          case 2 => agencia.NovoUserPremium(a, nome, cpf) // Usuário premium
          case _ => { // Caso passem um tipo não especificado, é criado uma conta do tipo normal
            println("Tipo desconhecido, criado usuario padrao")
            agencia.NovoUser(a, nome, cpf)
          }
        }
      }

      case NovaCheia(a, qtd, agencia, nome, cpf, tipo) => { // Gera uma conta com saldo qtd
        agencia.Colocar(a, qtd)
        tipo match {
          case 1 => agencia.NovoUser(a, nome, cpf) // usuário normal
          case 2 => agencia.NovoUserPremium(a, nome, cpf) // usuario premium
          case _ => { // Caso passem um tipo não especificado, é criado uma conta do tipo normal
            println("Tipo desconhecido, criado usuario padrao")
            agencia.NovoUser(a, nome, cpf)
          }
        }
      }
    }
  }

  // metodo para verificar se a conta já existe no banco de dados
  // É passado um Option[Double] pois se a conta existir, retornará um Some, caso não, um None.
  def verifica (cc: Option[Double]) : Boolean = cc match {
    case Some(s) => true
    case None    => false
  }

  // Criado para o do-while
  def repeatLoop(body: => Unit) = new Until(body)

  // do-while recursivo em cauda para criacao do menu no main
  class Until(body: => Unit) {
    def until(cond: => Boolean): Unit = { // tailrec
      body
      if (cond) until(cond)
    }
  }

  def main(args: Array[String]): Unit = {
    // declaracao dos atores
    val system = ActorSystem("System")
    val servidor = system.actorOf(Props[Adm])
    val gerador = system.actorOf(Props[Gerador])
    val cliente = system.actorOf(Props(new Cliente(servidor)))
    val Agencia = new BancoDeDados

    //Criacao de alguns usuários "padrão" do sistema. Para demonstrar a concorrência
    gerador ! NovaVazia(1, Agencia, "Carlos", "4502308971", 1)
    gerador ! NovaCheia(2, 1000, Agencia, "Monica", "6547328218", 2)
    gerador ! NovaCheia(3, 40000, Agencia, "Charles", "1765489012", 2)
    gerador ! NovaCheia(4, 2750, Agencia, "Bianca", "1429807765", 1)

    println("Contas Iniciais: ") // Imprimi os estados (número de conta e saldo) iniciais das contas criadas
    Await.result(Future{Agencia}, Duration.Inf).Banco.foreach((a) => println("Conta numero: " +a._1+ " tem saldo de: " + a._2 + " reais."))
    println()

    // var criada para condicao do loop do menu
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

          if (!verifica(Agencia.Banco.get(cc))) { // verificar se o número de conta já existe

            println("Deseja fazer um depósito inicial? (Caso não, digite 0)")
            val valor = scala.io.StdIn.readDouble()

            println("Insira seu nome: ")
            val nome = scala.io.StdIn.readLine

            println("Insira seu CPF: ")
            val cpf = scala.io.StdIn.readLine

            println("Voce deseja ser um usuario premium? (Digite 2 para sim e 1 para nao)")
            val premium = scala.io.StdIn.readInt()


            if (valor == 0) gerador ! NovaVazia(cc, Agencia, nome, cpf, premium) // criar conta vazia
            else gerador ! NovaCheia(cc, valor, Agencia, nome, cpf, premium) // criar conta com saldo "valor"
            Logar(cc) // acessar o menu de operações do cliente
          }
          else
            println("Conta já existe.")
        case 2 =>
          println("Digite o número da conta: ") // para acessar a conta
        val cc = scala.io.StdIn.readInt()
          if (verifica(Agencia.Banco.get(cc))) // verificar se a conta já existe
            Logar(cc)
          else
            println("Conta não existe.")
        case 3 =>
          println("Existe as contas disponíveis: ") // Lista as contas (apenas os números de conta) cadastrados no sistema
          Await.result(Future{Agencia}, Duration.Inf).Banco.foreach((a) => println("Conta: " + a._1 + " disponível para operações."))
        case 0 =>
          // Antes de sair do sistema, ele imprime todas as contas (número da conta e saldo) após as operacoes
          println("Contas resultantes (Após as operações):\n")
          Await.result(Future{Agencia}, Duration.Inf).Banco.foreach((a) => println("Conta numero: " +a._1+ " tem saldo de: " + a._2 + " reais."))
          System.exit(0)
      }
        
      // menu com as operações
      def Logar(cc: Int): Unit = {
        var opcao2 = -1 // var criada para condicao do loop
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
            // As contas que foram criadas de testes realizam suas operaçes (não impressas na tela) para mostrar a concorrencia.
            // Enquanto o cliente faz sua operação, ocorre paralelamente outras operaçes em outras contas.
            case 1 =>
              println("Digite o valor para saque:")
              cliente ! Saque(cc, scala.io.StdIn.readDouble(), Agencia, 1) // 1 para imprimir o resultado do saque
              Thread.sleep(200)

              cliente ! Saque(3, 10000, Agencia, 0) // Saque em uma das contas testes
              Thread.sleep(200)
            case 2 =>
              println("Digite o valor para transferencia: ")
              val value = scala.io.StdIn.readDouble()

              println("Digite o numero da conta destino: ")
              cliente ! Transferencia(cc, value, scala.io.StdIn.readInt(), Agencia, 1) // 1 para imprimir o resultado da transferencia
              Thread.sleep(200)

              cliente ! Transferencia(2, 500, 1, Agencia, 0) // Transferencia em uma das contas testes
              Thread.sleep(200)

              cliente ! Transferencia(2, 800, 4, Agencia, 0) // Transferencia em uma das contas testes
              Thread.sleep(200)
            case 3 =>
              println("Digite o valor para depósito: ")
              cliente ! Deposito(cc, scala.io.StdIn.readDouble(), Agencia, 1) // 1 para imprimir o resultado do deposito
              Thread.sleep(200)

              cliente ! Deposito(2, 700, Agencia, 0) // Deposito em uma das contas testes
              Thread.sleep(200)
            case 4 =>
              Agencia.Extrato(cc) // consulta extrato 
            case 0 => println("Logged out")
            case _ => println("Caso invalido")
          }
        } until (opcao2 != 0)
      }
    } until (opcao1 != 0)
  }
}
