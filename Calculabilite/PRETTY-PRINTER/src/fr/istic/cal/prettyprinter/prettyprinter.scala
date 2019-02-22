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
     * Recherche d'une valeur d'indentation dans une liste de spécifications d'indentation
     *
     * @param context une chaîne de caractères décrivant un contexte d'indentation
     * @param is une liste de spécifications d'indentation, chaque spécification étant un couple (contexte, indentation)
     * @return l'indentation correspondant à contexte
     */
    def indentSearch(context: String, is: IndentSpec): Int = {
        is match {
            // Si l'indentation n'a pas été trouvée dans la liste, on retourne l'indentation par défaut
            case Nil                    => indentDefault
            // Si l'indentation n'a pas été trouvée dans la liste, on retourne l'indentation par défaut
            case (ctx, indent) :: Nil   => if (ctx == context) indent else indentDefault
            case (ctx, indent) :: reste => if (ctx == context) indent else indentSearch(context, reste)
        }
    }

    /**
     * Création d'une indentation
     *
     * @param n un nombre d'espaces
     * @return une chaîne de n espaces
     */
    def makeIndent(n: Int): String = {
        n match {
            case 0 => ""
            case x => " " + makeIndent(n - 1)
        }
    }

    /**
     * ajout d'une chaîne devant chaque élément d'une liste de chaînes
     *
     * @param pref une chaîne
     * @param strings une liste de chaînes
     * @return une liste de chaînes obtenue par la concaténation de pref devant chaque élément de strings
     */
    def appendStringBeforeAll(pref: String, strings: List[String]): List[String] = {
        strings match {
            case Nil        => Nil
            case x :: Nil   => (pref + x) :: Nil
            case x :: reste => (pref + x) :: appendStringBeforeAll(pref, reste)
        }
    }

    /**
     * ajout d'une chaîne après chaque élément d'une liste de chaînes
     *
     * @param suff une chaîne
     * @param strings une liste de chaînes
     * @return une liste de chaînes obtenue par la concaténation de suff après chaque élément de strings
     */
    def appendStringAfterAll(suff: String, strings: List[String]): List[String] = {
        strings match {
            case Nil        => Nil
            case x :: Nil   => (x + suff) :: Nil
            case x :: reste => (x + suff) :: appendStringAfterAll(suff, reste)
        }
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
        strings match {
            case Nil             => Nil
            case x :: Nil        => x :: Nil
            case x :: y :: Nil   => (x + suff) :: y :: Nil
            case x :: y :: reste => (x + suff) :: (y + suff) :: appendStringAfterAllButLast(suff, reste)
        }
    }

    /**
     * Ajout d'une chaîne après le dernier élément d'une liste de chaînes
     *
     * @param suff une chaîne
     * @param strings une liste non vide de chaînes
     * @return une liste de chaînes obtenue par la concaténation de suff après le dernier élément de strings
     */
    def appendStringAfterLast(suff: String, strings: List[String]): List[String] = {
        strings match {
            case Nil => Nil
            case x :: Nil => (x + suff) :: Nil
            case x :: reste => x :: appendStringAfterLast(suff, reste)
        }
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
    def prettyPrintCommand(command: Command, is: IndentSpec): List[String] = {
        command match {
            case Nop                                        => "nop" :: Nil
            case Set(variable: Var, expression: Expression) => variable.nom + " := " + prettyPrintExpr(expression) :: Nil
            case While(condition: Expression, body: List[Command]) => {
                List("while " + prettyPrintExpr(condition) + " do") ++
                    appendStringBeforeAll(makeIndent(indentSearch("WHILE", is)), prettyPrintCommands(body, is)) ++ List("od")
            }
            case For(count: Expression, body: List[Command]) => {
                List("for " + prettyPrintExpr(count) + " do") ++
                    appendStringBeforeAll(makeIndent(indentSearch("FOR", is)), prettyPrintCommands(body, is)) ++ List("od")
            }
            case If(condition: Expression,
                then_commands: List[Command],
                else_commands: List[Command]) => {
                List("if " + prettyPrintExpr(condition) + " then") ++
                    appendStringBeforeAll(makeIndent(indentSearch("IF", is)), prettyPrintCommands(then_commands, is)) ++ List("else") ++
                    appendStringBeforeAll(makeIndent(indentSearch("IF", is)), prettyPrintCommands(else_commands, is)) ++ List("fi")
            }
        }
    }

    /**
     * @param commands : une liste d'AST décrivant une liste de commandes du langage WHILE
     * @param is : une liste de spécifications d'indentation
     * @return une liste de chaînes représentant la syntaxe concrète de la listes de commandes
     */
    def prettyPrintCommands(commands: List[Command], is: IndentSpec): List[String] = {
        commands match {
            case Nil        => Nil
            // S'il n'y a qu'un élément sans suite, pas de ";"
            case x :: Nil   => prettyPrintCommand(x, is) ++ Nil
            // Il reste des instructions après x. On ajoute un ";" après le dernier élément
            case x :: reste => appendStringAfterLast(" ;", prettyPrintCommand(x, is)) ++ prettyPrintCommands(reste, is)
        }
    }

    /**
     *
     *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
     */

    /**
     * @param vars : une liste décrivant les paramètres d'entrée d'un programme du langage WHILE
     * @return une liste de chaînes représentant la syntaxe concrète des paramètres d'entrée du programme
     */
    def prettyPrintIn(vars: List[Variable]): String = {
        vars match {
            // S'il n'y a qu'une variable dans la liste, retourne son nom.
            case List(Var(nom))        => nom
            // S'il y a plusieurs variables dans la liste, retourne le nom de la première variable
            // puis une virgule, un espace et concatène le reste sous forme d'appel récursif.
            case List(Var(nom), reste) => nom + ", " + prettyPrintIn(List(reste))
        }
    }

    /**
     * @param vars : une liste décrivant les paramètres de sortie d'un programme du langage WHILE
     * @return une liste de chaînes représentant la syntaxe concrète des paramètres de sortie du programme
     */
    def prettyPrintOut(vars: List[Variable]): String = {
        vars match {
            // S'il n'y a qu'une variable dans la liste, retourne son nom.
            case List(Var(nom))        => nom
            // S'il y a plusieurs variables dans la liste, retourne le nom de la première variable
            // puis une virgule, un espace et concatène le reste sous forme d'appel récursif.
            case List(Var(nom), reste) => nom + ", " + prettyPrintIn(List(reste))
        }
    }

    /**
     * @param program : un AST décrivant un programme du langage WHILE
     * @param is : une liste de spécifications d'indentation
     * @return une liste de chaînes représentant la syntaxe concrète du programme
     */
    def prettyPrintProgram(program: Program, is: IndentSpec): List[String] = {
        program match {
            case Progr(in, body, out) => {
                List("read " + prettyPrintIn(in)) ++
                    List("%") ++
                    appendStringBeforeAll(makeIndent(indentSearch("PROGR", is)), prettyPrintCommands(body, is)) ++
                    List("%") ++
                    List("write " + prettyPrintOut(out))
            }
        }
    }

    /**
     * @param program : un AST décrivant un programme du langage WHILE
     * @param is : une liste de spécifications d'indentation
     * @return une chaîne représentant la syntaxe concrète du programme
     */
    def prettyPrint(program: Program, is: IndentSpec): String = {
        program match {
            case Progr(in, body, out) => {
                "read " + prettyPrintIn(in) + "\n%\n" +
                    listeStringToString(appendStringBeforeAll(makeIndent(indentSearch("PROGR", is)), prettyPrintCommands(body, is))) +
                    "%\n" +
                    "write " + prettyPrintOut(out)
            }
        }
    }

    /**
     * Fonction auxiliaire servant à la fonction prettyPrint.
     * Convertit une liste de chaînes de caractères en une chaîne de caractères
     * comprenant tous les éléments de la liste avec un retour charriot entre chaque élément.
     * @param l une liste de String à convertir en String bien indentée
     * @return la String comprenant les éléments de l avec un retour charriot entre chacun
     */
    def listeStringToString(l: List[String]): String = {
        l match {
            case Nil        => ""
            case x :: Nil   => x + "\n"
            case x :: reste => x + "\n" + listeStringToString(reste)
        }
    }

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