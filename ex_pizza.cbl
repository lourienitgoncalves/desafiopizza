      *Divisão de identificação do programa
       identification division.
       program-id. "ex_pizza".
       author. "Lourieni Talita T Gonçalves".
       installation. "PC".
       date-written. 10/07/2020.
       date-compiled. 10/07/2020.



      *Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *-----Declaração dos recursos externos
       input-output section.
       file-control.
       i-o-control.

      *Declaração de variáveis
       data division.

      *----Variaveis de arquivos
       file section.


      *----Variaveis de trabalho
       working-storage section.

       01  relatorio  occurs  20.
           05 nome                                 pic x(15)
                                                   value spaces.
           05 filler                               pic x(03)
                                                   value " - ".
           05 diametro                             pic 9(03).
           05 filler                               pic x(03)
                                                   value " - ".
           05 preco                                pic 9(03)v99.
           05 filler                               pic x(03)
                                                   value " - ".
           05 preco_cm2                            pic 9(03)v99
                                                   value 0.
           05 filler                               pic x(03)
                                                   value " - ".
           05 diferenca_rel                        pic 9(03)v99.

       77  aux                                     pic 9(03)v99.
       77  aux2                                    pic x(03).
       77  ind                                     pic 9(02).
       77  menu                                    pic x(01).
       77  delta_preco_cm2                         pic 9(03)v99.
       77  are_a                                   pic 9(03)v99.
       77  pi                                      pic 9(02)v99999.
       77  controle                                pic x(12).


      *----Variaveis para comunicação entre programas
       linkage section.


      *----Declaração de tela
       screen section.


      *Declaração do corpo do programa
       procedure division.


           perform inicializa.
           perform processamento.
           perform finaliza.

      * Inicilizacao de variaveis, abertura de arquivos
      * procedimentos que serao realizados apenas uma vez
       inicializa section.
           move   "S"       to     menu

           move   3,14159   to     pi

           move "trocou" to controle

           .
       inicializa-exit.
           exit.


       processamento section.
           display erase
           move 0 to ind
           perform until menu <> "S"
               add 1 to ind

               if ind > 20 then
                   display "Vc atingiu o limite de 20 pizzas"
               else
                   display "Informe o nome da pizza "
                   accept nome(ind)

                   display "Informe o diametro "
                   accept diametro(ind)

                   display "Informe o preco "
                   accept preco(ind)
                   perform calculo-preco-cm2

               end-if

               display "deseja cadastrar mais uma pizza? ('S'/'N')"
               accept menu
           end-perform
           perform calculo-diferenca
           perform ordenar-tabela
           perform imprimi
           .
       processamento-exit.
           exit.
      *----------------------calculo preço por cm2----------------------
       calculo-preco-cm2 section.
           compute are_a = pi *((diametro(ind)/2) *
           (diametro(ind)/2))

           compute preco_cm2(ind) = preco(ind) / are_a
           .

       calculo-preco-cm2-exit.
           exit
           .
      *----------------------calculo direfenca----------------------
       calculo-diferenca section.
           move 1 to ind
           perform until ind = 20

               compute delta_preco_cm2 = preco_cm2(ind + 1) -
                                         preco_cm2(ind)

               compute diferenca_rel(ind + 1) = (delta_preco_cm2 * 100)
                                                /preco_cm2(ind)

               add 1 to ind
           end-perform
           .
       calculo-diferenca-exit.
           exit
           .

       ordenar-tabela section.
           perform until controle <> "trocou"
               move 1          to      ind
               move "N_trocou" to      controle
               perform until ind = 20
                       or    nome(ind + 1) = space

                   if preco_cm2(ind) > preco_cm2(ind + 1) then

                       move nome(ind + 1)      to aux2
                       move nome(ind)          to nome(ind + 1)
                       move aux2               to nome(ind)

                       move diametro(ind + 1)  to aux
                       move diametro(ind)      to diametro(ind + 1)
                       move aux                to diametro(ind)

                       move preco(ind + 1)     to aux
                       move preco(ind)         to preco(ind + 1)
                       move aux                to preco(ind)

                       move diametro(ind + 1)  to aux
                       move diametro(ind)      to diametro(ind + 1)
                       move aux                to diametro(ind)

                       move preco_cm2(ind + 1) to aux
                       move preco_cm2(ind)     to preco_cm2(ind + 1)
                       move aux                to preco_cm2(ind)



                       move "trocou"           to controle

                   end-if
                   add 1   to ind
               end-perform
           end-perform

           .
       ordenar-tabela-exit.
           exit
           .

       imprimi section.

           perform varying ind from 1 by 1 until ind > 20
                                           or nome(ind) = space

               display relatorio(ind)


           end-perform
             .
       imprimi-exit.
           exit
           .

       finaliza section.
           stop run
           .
       finaliza-exit.
           exit.













