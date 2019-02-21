package fr.istic.cal.prettyprinter

import scala.util.Try

/**
 * définition d'une exception pour le cas des listes vides de commandes
 */
case object ExceptionListeVide extends Exception

object Prettyprinter {

  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractère représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s: String): Expression = {
    WhileParser.analyserexpression(s)
  }

  /**
   * @param s : une chaine de caractère représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s: String): Command = {
    WhileParser.analysercommand(s)
  }

  /**
   * @param s : une chaine de caractère représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s: String): Program = {
    WhileParser.analyserprogram(s)
  }

  /**
   * UN PRETTY-PRINTER POUR LE LANGAGE WHILE
   *
   */

  /**
   * définition d'un type pour les spécifications d'indentation
   */
  type IndentSpec = List[(String, Int)]

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une chaîne représentant la syntaxe concrète de l'expression
   */
  def prettyPrintExpr(expression: Expression): String = {
    expression match {
      case Nl         => "nil"
      case Cst(s)     => s.toLowerCase()
      case VarExp(s)  => s.toUpperCase()
      case Cons(x, y) => "(cons " + prettyPrintExpr(x) + " " + prettyPrintExpr(y) + ")"
      case Hd(x)      => "(hd " + prettyPrintExpr(x) + ")"
      case Tl(y)      => "(tl " + prettyPrintExpr(y) + ")"
      case Eq(x, y)   => prettyPrintExpr(x) + " =? " + prettyPrintExpr(y)
    }
  }

  /**
   *  FONCTIONS AUXILIAIRES DE TRAITEMENT DE CHAINES POUR L'INDENTATION DES COMMANDES
   *  OU LA PRESENTATION DU PROGRAMME
   */

  /**
   * * définition d'une valeur d'indentation par défaut
   */
  val indentDefault: Int = 1

  /**
   * recherche d'une valeur d'indentation dans une liste de spécifications d'indentation
   *
   * @param context une chaîne de caractères décrivant un contexte d'indentation
   * @param is une liste de spécifications d'indentation, chaque spécification étant un couple (contexte, indentation)
   * @return l'indentation correspondant à contexte
   */
  def indentSearch(context: String, is: IndentSpec): Int = {
    for (x <- is) {
      if (context == x._1) {
        return x._2
      }
    }
    /* si l'indentation n'a pas été trouvée dans la liste, on retourne indentDefault */
    return indentDefault
  }

  /**
   * création d'une indentation
   *
   * @param n un nombre d'espaces
   * @return une chaîne de n espaces
   */
  def makeIndent(n: Int): String = {
    var t = ""
    for (i <- 0 to n - 1) {
      t = t + " "
    }
    t
  }

  /**
   * ajout d'une chaîne devant chaque élément d'une liste de chaînes
   *
   * @param pref une chaîne
   * @param strings une liste de chaînes
   * @return une liste de chaînes obtenue par la concaténation de pref devant chaque élément de strings
   */
  def appendStringBeforeAll(pref: String, strings: List[String]): List[String] = {
    var res: List[String] = Nil
    for (x <- strings) {
      res = res :+ pref + x
    }
    res
  }

  /**
   * ajout d'une chaîne après chaque élément d'une liste de chaînes
   *
   * @param suff une chaîne
   * @param strings une liste de chaînes
   * @return une liste de chaînes obtenue par la concaténation de suff après chaque élément de strings
   */
  def appendStringAfterAll(suff: String, strings: List[String]): List[String] = {
    var res: List[String] = Nil
    for (x <- strings) {
      res = res :+ x + suff
    }
    res
  }

  /**
   * ajout d'une chaîne après chaque élément d'une liste de chaînes sauf le dernier
   *
   * @param suff une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de pref après chaque élément de strings
   *         sauf le dernier
   */
  def appendStringAfterAllButLast(suff: String, strings: List[String]): List[String] = {
    var res: List[String] = Nil
    for (x <- 0 to strings.length - 1) {
      if (x != strings.length - 1) {
        res = res :+ strings(x) + suff
      } else {
        res = res :+ strings(x)
      }
    }
    res
  }

  /**
   * ajout d'une chaîne après le dernier élément d'une liste de chaînes
   *
   * @param suff une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de suff après le dernier élément de strings
   */
  def appendStringAfterLast(suff: String, strings: List[String]): List[String] = {
    var res: List[String] = Nil
    var dernierElement = strings.last
    for (x <- 0 to strings.length - 2) {
      res = res :+ strings(x)
    }
    res = res :+ dernierElement + suff
    res
  }

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */

  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète de la commande
   */
  // TODO TP2
  def prettyPrintCommand(command: Command, is: IndentSpec): List[String] = {
    var res: List[String] = Nil
    command match {
      case Nop                                        => res = res :+ "nop"
      case Set(variable: Var, expression: Expression) => res = res :+ variable.nom + " := " + prettyPrintExpr(expression)
      case While(condition: Expression, body: List[Command]) => {
        res = res :+ "while " + prettyPrintExpr(condition) + " do";
        res = res ++ appendStringBeforeAll(makeIndent(indentSearch("WHILE", is)), prettyPrintCommands(body, is)) ++ List("od")
      }
      case For(count: Expression, body: List[Command]) => {
        res = res :+ "for " + prettyPrintExpr(count) + " do";
        res = res ++ appendStringBeforeAll(makeIndent(indentSearch("FOR", is)), prettyPrintCommands(body, is)) ++ List("od")
      }
      case If(condition: Expression,
        then_commands: List[Command],
        else_commands: List[Command]) => {
        res = res :+ "if " + prettyPrintExpr(condition) + " then";
        res = res ++ appendStringBeforeAll(makeIndent(indentSearch("IF", is)), prettyPrintCommands(then_commands, is)) ++ List("else")
        res = res ++ appendStringBeforeAll(makeIndent(indentSearch("IF", is)), prettyPrintCommands(else_commands, is)) ++ List("fi")
      }
    }
    res
  }

  /**
   * @param commands : une liste d'AST décrivant une liste de commandes du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète de la listes de commandes
   */
  // /!\ TODO pb il faut ajouter ; après "od" s'il reste des instructions => Comment trouver qu'on a des instructions imbriquées ?
  def prettyPrintCommands(commands: List[Command], is: IndentSpec): List[String] = {
    var res: List[String] = List()
    commands match {
      case Nil             => Nil
      // S'il n'y a qu'un élément sans suite, pas de ;
      case x :: Nil        => res = res ++ prettyPrintCommand(x, is)
      case x :: y :: Nil   => res = res ++ appendStringAfterAll(" ;", prettyPrintCommand(x, is)) ++ prettyPrintCommand(y, is)
      case x :: y :: reste => res = res ++ appendStringAfterAll(" ;", prettyPrintCommand(x, is)) ++ prettyPrintCommand(y, is) ++ prettyPrintCommands(reste, is)
    }
    res
  }

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste décrivant les paramètres d'entrée d'un programme du langage WHILE
   * @return une liste de chaînes représentant la syntaxe concrète des paramètres d'entrée du programme
   */
  // TODO TP2
  def prettyPrintIn(vars: List[Variable]): String = { ??? }

  /**
   * @param vars : une liste décrivant les paramètres de sortie d'un programme du langage WHILE
   * @return une liste de chaînes représentant la syntaxe concrète des paramètres de sortie du programme
   */
  // TODO TP2
  def prettyPrintOut(vars: List[Variable]): String = { ??? }

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète du programme
   */
  // TODO TP2
  def prettyPrintProgram(program: Program, is: IndentSpec): List[String] = { ??? }

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une chaîne représentant la syntaxe concrète du programme
   */
  // TODO TP2
  def prettyPrint(program: Program, is: IndentSpec): String = { ??? }

  val program: Program =
    Progr(
      List(Var("X")),
      List(
        Set(Var("Y"), Nl),
        While(
          VarExp("X"),
          List(
            Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
            Set(Var("X"), Tl(VarExp("X")))))),
      List(Var("Y")));
  val is: IndentSpec = List(("PROGR", 2), ("WHILE", 5));

  def main(args: Array[String]): Unit = {

    // vous pouvez ici tester manuellement vos fonctions par des print

  }
}