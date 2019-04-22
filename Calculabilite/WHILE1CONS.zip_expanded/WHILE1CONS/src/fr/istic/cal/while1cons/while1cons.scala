package fr.istic.cal.while1cons

import scala.util.Try

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

object While1cons {

    /**
     * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
     *
     * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
     * respectivement pour une expression, une commande, un programme
     */

    /**
     * @param s : une chaine de caractères représentant la syntaxe concrète d'une expression du langage WHILE
     * @return un arbre de syntaxe abstraite pour cette expression
     */
    def readWhileExpression(s: String): Expression = { WhileParser.analyserexpression(s) }

    /**
     * @param s : une chaine de caractères représentant la syntaxe concrète d'une commande du langage WHILE
     * @return un arbre de syntaxe abstraite pour cette commande
     */
    def readWhileCommand(s: String): Command = { WhileParser.analysercommand(s) }

    /**
     * @param s : une chaine de caractères représentant la syntaxe concrète d'un programme du langage WHILE
     * @return un arbre de syntaxe abstraite pour ce programme
     */
    def readWhileProgram(s: String): Program = { WhileParser.analyserprogram(s) }

    /**
     * UN ELIMINATEUR D'EXPRESSIONS COMPLEXES POUR LE LANGAGE WHILE
     *
     */

    /**
     *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
     */

    /**
     * @param expression : un AST décrivant une expression du langage WHILE
     * @return une paire constituée d'une liste d'affectations ayant le même effet
     * que l'expression et la variable qui contient le résultat
     */
    def while1ConsExprV(expression: Expression): (List[Command], Variable) = {
        expression match {
            case Nl =>
                val x = NewVar.make(); (List(Set(x, Nl)), x)
            case Cst(name) =>
                val x = NewVar.make(); (List(Set(x, Cst(name))), x)
            case VarExp(name)     => (List(), Var(name))
            case Cons(arg1, arg2) => {
                val x = NewVar.make()
                val y = NewVar.make()
                val z = NewVar.make()
                var nameX = ""
                var nameY = ""
                x match {
                    case Var(name) => nameX = name
                }
                y match {
                    case Var(name) => nameY = name
                }
                val l: List[Command] = List(Set(x, arg1), Set(y, arg2), Set(z, Cons(VarExp(nameX), VarExp(nameY))))
                (l, z)
            }
            case Hd(arg) =>
                val x = NewVar.make(); (List(Set(x, Hd(arg))), x)
            case Tl(arg) =>
                val x = NewVar.make(); (List(Set(x, Tl(arg))), x)
            case Eq(arg1, arg2) => val x = NewVar.make(); (List(Set(x, Eq(arg1, arg2))), x)
        }
    }

    /**
     * @param expression : un AST décrivant une expression du langage WHILE
     * @return une paire constituée d'une liste d'affectations ayant le même effet
     * que l'expression et une expression qui contient le résultat
     */
    // TODO TP4
    def while1ConsExprSE(expression: Expression): (List[Command], Expression) = { 
        ???
    }

    /**
     *
     *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
     */
    /**
     * @param command : un AST décrivant une commande du langage WHILE
     * @return une liste de commandes ayant un seul construteur par expression
     * et ayant le même effet que la commande
     */
    // TODO TP4
    def while1ConsCommand(command: Command): List[Command] = { ??? }

    /**
     * @param commands : une liste d'AST décrivant une liste de commandes du langage WHILE
     * @return une liste de commandes ayant un seul construteur par expression
     * et ayant le même effet que la commande
     */
    // TODO TP4
    def while1ConsCommands(commands: List[Command]): List[Command] = { ??? }

    /**
     *
     *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
     */

    /**
     * @param program : un AST décrivant un programme du langage WHILE
     * @param is : une liste de spécifications d'indentation
     * @return une liste de chaînes représentant la syntaxe concrète du programme
     */
    // TODO TP4
    def while1ConsProgr(program: Program): Program = { ??? }

    def main(args: Array[String]): Unit = {

        // vous pouvez ici tester manuellement vos fonctions par des print

    }
}