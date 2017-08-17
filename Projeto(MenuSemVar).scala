def Logar(cc: Int): Unit = {
        Menu2
        def Menu2: Unit = {
          println()
          println("-------------------------------------------")
          println("---------- 1 - Saque         --------------")
          println("---------- 2 - Transferencia --------------")
          println("---------- 3 - Deposito      --------------")
          println("---------- 4 - Extrato       --------------")
          println("---------- 0 - Sair da Conta --------------")
          println("-------------------------------------------")
          println()
          val opcao2 = scala.io.StdIn.readInt()
          opcao2 match {
            // As contas que foram criadas para testes realizam suas operações (não impressas na tela) para mostrar a concorrencia.
            // Enquanto o cliente faz sua operação, ocorrem paralelamente outras operaçes em outras contas.
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
              
              Menu2
            case 3 =>
              println("Digite o valor para depósito: ")
              cliente ! Deposito(cc, scala.io.StdIn.readDouble(), Agencia, 1) // 1 para imprimir o resultado do deposito
              Thread.sleep(200)

              cliente ! Deposito(2, 700, Agencia, 0) // Deposito em uma das contas testes
              Thread.sleep(200)
              
              Menu2
            case 4 =>
              Agencia.Extrato(cc) // consulta extrato 
              Menu2
            case 0 => println("Logged out")
            case _ =>{
              println("Opcao invalida")
              Menu2
            }
          }
        }
      }
