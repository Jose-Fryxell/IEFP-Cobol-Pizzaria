      ******************************************************************
      * Author: JOSE SERRA
      * Date: 03-03-2021
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAREFA06.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      ****** INFO PEDIDO ***********************************************
       77 NUMERO-PEDIDO                    PIC 9(2)  VALUE 0.

       77 CLIENTE                          PIC A(35) VALUE SPACES.

       77 TEMP-CONTACTO                    PIC X(9)  VALUE SPACES.
       77 CONTACTO                         PIC 9(9)  VALUE 0.
           88 VALIDAR-CONTACTO                       VALUES
                                           210000000 THROUGH 296999999,
                                           910000000 THROUGH 939999999,
                                           960000000 THROUGH 969999999.
      ****** BASE PIZZA ************************************************
       77 TEMP-PIZZA                       PIC X     VALUE SPACES.
       77 PIZZA                            PIC 9     VALUE 0.
           88 VALIDAR-PIZZA                          VALUES 1 THRU 3.
      ****** INGREDIENTES **********************************************
       77 TEMP-INGREDIENTES                PIC XX    VALUE SPACES.
       77 INGREDIENTES                     PIC 99    VALUE 0.
           88 VALIDAR-INGREDIENTES                   VALUES 0 THRU 11.
       77 NUMERO-INGREDIENTES              PIC 9     VALUE 1.
       77 MAX                              PIC 9     VALUE 0.
      ****** PRECO *****************************************************
       77 PRECO-BASE                       PIC 9V99  VALUE 0.
       77 PRECO-INGREDIENTES               PIC 9V99  VALUE 0.
       77 PRECO-PIZZA                      PIC 9V99  VALUE 0.
       77 PRECO-IVA                        PIC 9V99  VALUE 0.
       77 PRECO-TOTAL                      PIC 99V99 VALUE 0.

       77 PRECO-SAIDA                      PIC Z9.99 VALUE SPACES.
      ****** LINHA TABELA **********************************************
       77 NT                               PIC 99    VALUE 12.
      ****** REPETIR PROGRAMA ******************************************
       77 REPETIR                          PIC A.
           88 VALIDAR-REPETIR              VALUES "S", "s", "N", "n".
           88 SIM                          VALUES "S", "s".
           88 NAO                          VALUES "N", "n".
      ****** DATA ******************************************************
       01 DATA-ATUAL.
           05 ANO PIC 9(4).
           05 MES PIC 99.
           05 DIA PIC 99.
      ******************************************************************
       SCREEN SECTION.
       01 CLS BLANK SCREEN.
      ******************************************************************
       01 JANELA.
           05 LINE 01 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "+--------------------------------------------------".
           05 LINE 01 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "---------------+------------------+".
           05 LINE 02 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "|                        Pizzaria Ramalho          ".
           05 LINE 02 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "               | Data:            |".
           05 LINE 03 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "+----------------------------------------------+---".
           05 LINE 03 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "---------------+--+---------------+".
           05 LINE 04 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "| Cliente:                                     | Co".
           05 LINE 04 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "ntacto:           | No Pedido:    |".
           05 LINE 05 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "+----------------------------------------------+---".
           05 LINE 05 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "-------+----------+---------------+".
           05 LINE 06 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "| Tipo de Pizza:   (1/2/3)                         ".
           05 LINE 06 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "       |     Tabela de Precos     |".
           05 LINE 07 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "+--------------------------------------------------".
           05 LINE 07 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "-------+--------------------------+".
           05 LINE 08 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "| Ingredientes:    (1-10) (0 p/ finalizar, 11 p/ re".
           05 LINE 08 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "petir) | (1) Pequena.........3.00 |".
           05 LINE 09 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "+------------------+-----------+-------------------".
           05 LINE 09 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "-------+ (2) Media...........4.00 |".
           05 LINE 10 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "|   Ingredientes   |   Preco   |                   ".
           05 LINE 10 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "       | (3) Grande..........5.00 |".
           05 LINE 11 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "+------------------+-----------+                   ".
           05 LINE 11 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "       +--------(MAXIMO 5)--------+".
           05 LINE 12 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "|                  |           |                   ".
           05 LINE 12 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "       | (1) Fiambre.........0.50 |".
           05 LINE 13 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "|                  |           |                   ".
           05 LINE 13 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "       | (2) Atum............0.70 |".
           05 LINE 14 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "|                  |           |                   ".
           05 LINE 14 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "       | (3) Anchovas........0.40 |".
           05 LINE 15 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "|                  |           |                   ".
           05 LINE 15 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "       | (4) Camarao.........0.80 |".
           05 LINE 16 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "|                  |           |                   ".
           05 LINE 16 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "       | (5) Bacon...........0.90 |".
           05 LINE 17 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "|                  |           |                   ".
           05 LINE 17 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "       | (6) Banana..........0.30 |".
           05 LINE 18 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "|                  |           |                   ".
           05 LINE 18 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "       | (7) Ananas..........0.40 |".
           05 LINE 19 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "|                  |           |                   ".
           05 LINE 19 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "       | (8) Azeitonas.......0.30 |".
           05 LINE 20 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "+------------------+-----------+                   ".
           05 LINE 20 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "       | (9) Cogumelos.......0.60 |".
           05 LINE 21 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "|                  |           |                   ".
           05 LINE 21 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "       | (10) Milho..........0.50 |".
           05 LINE 22 COL 01 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "+------------------+-----------+-------------------".
           05 LINE 22 COL 52 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "-------+--------------------------+".
      ******************************************************************
       01 LIMPAR-JANELINHA.
           05 LINE 10 COL 34 VALUE "                        ".
           05 LINE 11 COL 34 VALUE "                        ".
           05 LINE 12 COL 34 VALUE "                        ".
           05 LINE 13 COL 34 VALUE "                        ".
           05 LINE 14 COL 34 VALUE "                        ".
           05 LINE 15 COL 34 VALUE "                        ".
           05 LINE 16 COL 34 VALUE "                        ".
           05 LINE 17 COL 34 VALUE "                        ".
           05 LINE 18 COL 34 VALUE "                        ".
           05 LINE 19 COL 34 VALUE "                        ".
           05 LINE 20 COL 34 VALUE "                        ".
           05 LINE 21 COL 34 VALUE "                        ".
      ******************************************************************
       01 LIMPAR-TABELA.
           05 LINE 12 COL 3 VALUE "             ".
           05 LINE 13 COL 3 VALUE "             ".
           05 LINE 14 COL 3 VALUE "             ".
           05 LINE 15 COL 3 VALUE "             ".
           05 LINE 16 COL 3 VALUE "             ".
           05 LINE 17 COL 3 VALUE "             ".
           05 LINE 18 COL 3 VALUE "             ".
           05 LINE 19 COL 3 VALUE "             ".
           05 LINE 21 COL 3 VALUE "             ".
           05 LINE 12 COL 22 VALUE "         ".
           05 LINE 13 COL 22 VALUE "         ".
           05 LINE 14 COL 22 VALUE "         ".
           05 LINE 15 COL 22 VALUE "         ".
           05 LINE 16 COL 22 VALUE "         ".
           05 LINE 17 COL 22 VALUE "         ".
           05 LINE 18 COL 22 VALUE "         ".
           05 LINE 19 COL 22 VALUE "         ".
           05 LINE 21 COL 22 VALUE "         ".
      ******************************************************************
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY CLS.
           DISPLAY JANELA.
      ****** DATA ******************************************************
           MOVE FUNCTION CURRENT-DATE TO DATA-ATUAL.
           DISPLAY FUNCTION CONCATENATE(DIA,"-",MES,"-",ANO)
           HIGHLIGHT AT 0275.
      ****** NUMERO PEDIDO *********************************************
           ADD 1 TO NUMERO-PEDIDO.
           DISPLAY NUMERO-PEDIDO AT 0483 HIGHLIGHT.
      ****** INFO CLIENTE **********************************************
           ACCEPT CLIENTE AT 0412 HIGHLIGHT.
       VAL-CONTACTO.
           ACCEPT TEMP-CONTACTO AT 0460 HIGHLIGHT.
           MOVE FUNCTION NUMVAL(TEMP-CONTACTO) TO CONTACTO.
           IF (NOT VALIDAR-CONTACTO) THEN
               DISPLAY "Por favor digite um"
               AT 1034 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "numero valido."
               AT 1134 FOREGROUND-COLOR 4 HIGHLIGHT
               GO VAL-CONTACTO
           ELSE
               DISPLAY LIMPAR-JANELINHA
           END-IF.
      ****** PIZZA *****************************************************
       RESET-PEDIDO.
           MOVE 0 TO PIZZA, PRECO-BASE, MAX, PRECO-INGREDIENTES.
           MOVE 1 TO NUMERO-INGREDIENTES.
           MOVE 12 TO NT.
           DISPLAY "  (1/2/3)" AT 0618 FOREGROUND-COLOR 3 HIGHLIGHT.
       VAL-PIZZA.
           ACCEPT TEMP-PIZZA AT 0618 HIGHLIGHT.
           MOVE FUNCTION NUMVAL(TEMP-PIZZA) TO PIZZA.
           IF (NOT VALIDAR-PIZZA) THEN
               DISPLAY "Por favor digite um"
               AT 1034 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "valor entre 1 e 3."
               AT 1134 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "Digite 1 para PEQUENA."
               AT 1334 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "Digite 2 para MEDIA."
               AT 1434 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "Digite 3 para GRANDE."
               AT 1534 FOREGROUND-COLOR 4 HIGHLIGHT
               GO VAL-PIZZA
           ELSE
               DISPLAY LIMPAR-JANELINHA
           END-IF.

           EVALUATE PIZZA
               WHEN 1
                   DISPLAY "Pequena  " AT 0618 HIGHLIGHT
                   ADD 3 TO PRECO-BASE
                   DISPLAY "Base Pequena" AT LINE NT COL 3 HIGHLIGHT
                   DISPLAY "3.00" AT LINE NT COL 27 HIGHLIGHT
               WHEN 2
                   DISPLAY "Media    " AT 0618 HIGHLIGHT
                   ADD 4 TO PRECO-BASE
                   DISPLAY "Base Media" AT LINE NT COL 3 HIGHLIGHT
                   DISPLAY "4.00" AT LINE NT COL 27 HIGHLIGHT
               WHEN 3
                   DISPLAY "Grande   " AT 0618 HIGHLIGHT
                   ADD 5 TO PRECO-BASE
                   DISPLAY "Base Grande" AT LINE NT COL 3 HIGHLIGHT
                   DISPLAY "5.00" AT LINE NT COL 27 HIGHLIGHT
           END-EVALUATE.
           ADD 1 TO NT.
      ****** INGREDIENTES **********************************************
       VAL-INGREDIENTES.
           IF (MAX <5) THEN
               ACCEPT TEMP-INGREDIENTES AT 0817 HIGHLIGHT
               MOVE FUNCTION NUMVAL(TEMP-INGREDIENTES) TO INGREDIENTES
               IF (NOT VALIDAR-INGREDIENTES) THEN
                   DISPLAY "Por favor digite um"
                   AT 1034 FOREGROUND-COLOR 4 HIGHLIGHT
                   DISPLAY "valor entre 1 e 10 para"
                   AT 1134 FOREGROUND-COLOR 4 HIGHLIGHT
                   DISPLAY "selecionar ingredientes."
                   AT 1234 FOREGROUND-COLOR 4 HIGHLIGHT
                   DISPLAY "Digite 0 para finalizar."
                   AT 1434 FOREGROUND-COLOR 4 HIGHLIGHT
                   DISPLAY "Digite 11 para repetir."
                   AT 1534 FOREGROUND-COLOR 4 HIGHLIGHT
                   GO VAL-INGREDIENTES
               ELSE
                   DISPLAY LIMPAR-JANELINHA
               END-IF

               EVALUATE INGREDIENTES
                   WHEN 0
                      SUBTRACT 1 FROM NUMERO-INGREDIENTES
                      SUBTRACT 1 FROM NT
                      MOVE 5 TO MAX
                   WHEN 1
                      DISPLAY "Fiambre" AT LINE NT COL 3 HIGHLIGHT
                      DISPLAY "0.50" AT LINE NT COL 27 HIGHLIGHT
                      ADD 0.5 TO PRECO-INGREDIENTES
                   WHEN 2
                      DISPLAY "Atum" AT LINE NT COL 3 HIGHLIGHT
                      DISPLAY "0.70" AT LINE NT COL 27 HIGHLIGHT
                      ADD 0.7 TO PRECO-INGREDIENTES
                   WHEN 3
                      DISPLAY "Anchovas" AT LINE NT COL 3 HIGHLIGHT
                      DISPLAY "0.40" AT LINE NT COL 27 HIGHLIGHT
                      ADD 0.4 TO PRECO-INGREDIENTES
                   WHEN 4
                      DISPLAY "Camarao" AT LINE NT COL 3 HIGHLIGHT
                      DISPLAY "0.80" AT LINE NT COL 27 HIGHLIGHT
                      ADD 0.8 TO PRECO-INGREDIENTES
                   WHEN 5
                      DISPLAY "Bacon" AT LINE NT COL 3 HIGHLIGHT
                      DISPLAY "0.90" AT LINE NT COL 27 HIGHLIGHT
                      ADD 0.9 TO PRECO-INGREDIENTES
                   WHEN 6
                      DISPLAY "Banana" AT LINE NT COL 3 HIGHLIGHT
                      DISPLAY "0.30" AT LINE NT COL 27 HIGHLIGHT
                      ADD 0.3 TO PRECO-INGREDIENTES
                   WHEN 7
                      DISPLAY "Ananas" AT LINE NT COL 3 HIGHLIGHT
                      DISPLAY "0.40" AT LINE NT COL 27 HIGHLIGHT
                      ADD 0.4 TO PRECO-INGREDIENTES
                   WHEN 8
                      DISPLAY "Azeitonas" AT LINE NT COL 3 HIGHLIGHT
                      DISPLAY "0.30" AT LINE NT COL 27 HIGHLIGHT
                      ADD 0.3 TO PRECO-INGREDIENTES
                   WHEN 9
                      DISPLAY "Cogumelos" AT LINE NT COL 3 HIGHLIGHT
                      DISPLAY "0.60" AT LINE NT COL 27 HIGHLIGHT
                      ADD 0.6 TO PRECO-INGREDIENTES
                   WHEN 10
                      DISPLAY "Milho" AT LINE NT COL 3 HIGHLIGHT
                      DISPLAY "0.50" AT LINE NT COL 27 HIGHLIGHT
                      ADD 0.5 TO PRECO-INGREDIENTES
                   WHEN 11
                      DISPLAY "  " AT 0817
                      DISPLAY LIMPAR-TABELA
                      GO RESET-PEDIDO
               END-EVALUATE
               ADD 1 TO MAX
               ADD 1 TO NT
               ADD 1 TO NUMERO-INGREDIENTES
               GO VAL-INGREDIENTES
           END-IF.
           SUBTRACT 1 FROM NUMERO-INGREDIENTES.
           DISPLAY NUMERO-INGREDIENTES AT 0817 HIGHLIGHT.
           DISPLAY "                                      " AT 0820
      ****** CALCULO PRECOS ********************************************
           COMPUTE PRECO-PIZZA = PRECO-BASE + PRECO-INGREDIENTES.
           COMPUTE PRECO-IVA = PRECO-PIZZA * 0.23.
           COMPUTE PRECO-TOTAL = PRECO-PIZZA + PRECO-IVA.
      ****** APRESENTAR PRECOS *****************************************
           DISPLAY "Total Ingred." AT LINE NT COL 3 HIGHLIGHT.
           MOVE PRECO-INGREDIENTES TO PRECO-SAIDA.
           DISPLAY FUNCTION CONCATENATE(PRECO-SAIDA)
           AT LINE NT COL 26 HIGHLIGHT.

           DISPLAY "IVA (23%)" AT 1903 HIGHLIGHT.
           MOVE PRECO-IVA TO PRECO-SAIDA.
           DISPLAY FUNCTION CONCATENATE(PRECO-SAIDA) AT 1926 HIGHLIGHT.

           DISPLAY "TOTAL" AT 2103 HIGHLIGHT.
           MOVE PRECO-TOTAL TO PRECO-SAIDA.
           DISPLAY FUNCTION CONCATENATE(PRECO-SAIDA) AT 2126 HIGHLIGHT.
      ****** REPETIR ***************************************************
           DISPLAY "Deseja registar um novo"
           AT 1034 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "pedido?   (S/N)"
           AT 1134 FOREGROUND-COLOR 2 HIGHLIGHT.
       REPETIR-PROGRAMA.
           ACCEPT REPETIR AT 1142 HIGHLIGHT.
           IF (NOT VALIDAR-REPETIR) THEN
               DISPLAY "Por favor"
               AT 1334 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "digite 'S' ou 'N'."
               AT 1434 FOREGROUND-COLOR 4 HIGHLIGHT
               GO REPETIR-PROGRAMA
           END-IF.
           IF (SIM) THEN
               GO INICIO
           END-IF.
           STOP RUN.
       END PROGRAM TAREFA06.
