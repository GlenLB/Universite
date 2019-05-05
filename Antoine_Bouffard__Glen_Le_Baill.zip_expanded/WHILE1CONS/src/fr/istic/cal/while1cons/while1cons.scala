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
     * Méthode auxiliaire pour transformer une variable en expression
     */
    def varToVarExp(v: Variable): Expression = {
        v match {
            case Var(name) => return VarExp(name)
        }
    }

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
            case VarExp(name) => (List(), Var(name))
            case Cons(arg1, arg2) => {
                val retourArg1 = while1ConsExprV(arg1)
                val retourArg2 = while1ConsExprV(arg2)
                val var1 = retourArg1._2
                val var2 = retourArg2._2
                var l: List[Command] = retourArg1._1 ++ retourArg2._1
                val x = NewVar.make()
                l = l :+ Set(x, Cons(varToVarExp(var1), varToVarExp(var2)))
                (l, x)
            }
            case Hd(arg) =>
                val retourArg = while1ConsExprV(arg)
                val varArg = retourArg._2
                val x = NewVar.make(); (retourArg._1 ++ List(Set(x, Hd(varToVarExp(varArg)))), x)
            case Tl(arg) =>
                val retourArg = while1ConsExprV(arg)
                val varArg = retourArg._2
                val x = NewVar.make(); (retourArg._1 ++ List(Set(x, Tl(varToVarExp(varArg)))), x)
            case Eq(arg1, arg2) => {
                val retourArg1 = while1ConsExprV(arg1)
                val retourArg2 = while1ConsExprV(arg2)
                val var1 = retourArg1._2
                val var2 = retourArg2._2
                val x = NewVar.make(); (retourArg1._1 ++ retourArg2._1 ++ List(Set(x, Eq(varToVarExp(var1), varToVarExp(var2)))), x)
            }
        }
    }

    /**
     * @param expression : un AST décrivant une expression du langage WHILE
     * @return une paire constituée d'une liste d'affectations ayant le même effet
     * que l'expression et une expression qui contient le résultat
     */
    def while1ConsExprSE(expression: Expression): (List[Command], Expression) = {
        expression match {
            case Nl           => (List(), Nl)
            case Cst(name)    => (List(), Cst(name))
            case VarExp(name) => (List(), VarExp(name))
            case Hd(arg) => {
                val retourArg = while1ConsExprV(arg)
                val varArg = retourArg._2
                (retourArg._1, Hd(varToVarExp(varArg)))
            }
            case Tl(arg) => {
                val retourArg = while1ConsExprV(arg)
                val varArg = retourArg._2
                (retourArg._1, Tl(varToVarExp(varArg)))
            }
            case Cons(arg1, arg2) => {
                val retourArg1 = while1ConsExprV(arg1)
                val retourArg2 = while1ConsExprV(arg2)
                val var1 = retourArg1._2
                val var2 = retourArg2._2
                var l: List[Command] = retourArg1._1 ++ retourArg2._1
                (l, Cons(varToVarExp(var1), varToVarExp(var2)))
            }
            case Eq(arg1, arg2) => {
                val retourArg1 = while1ConsExprV(arg1)
                val retourArg2 = while1ConsExprV(arg2)
                val var1 = retourArg1._2
                val var2 = retourArg2._2
                (retourArg1._1 ++ retourArg2._1, Eq(varToVarExp(var1), varToVarExp(var2)))
            }
        }
    }

    /**
     *
     *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
     */
    /**
     * @param command : un AST décrivant une commande du langage WHILE
     * @return une liste de commandes ayant un seul constructeur par expression
     * et ayant le même effet que la commande
     */
    def while1ConsCommand(command: Command): List[Command] = {
        command match {
            case Nop => List(Nop)
            case Set(variable, expression) => {
                val res = while1ConsExprSE(expression)
                res._1 ++ List(Set(variable, res._2))
            }
            case While(condition, body) => {
                val v = while1ConsExprV(condition)
                var b = while1ConsCommands(body)
                b = b ++ v._1
                v._1 ++ List(While(varToVarExp(v._2), b))
            }
            case For(count, body) => {
                val v = while1ConsExprV(count)
                var b = while1ConsCommands(body)
                v._1 ++ List(For(varToVarExp(v._2), b))
            }
            case If(condition, then_commands, else_commands) => {
                val v = while1ConsExprV(condition)
                var thenC: List[Command] = while1ConsCommands(then_commands)
                var elseC: List[Command] = while1ConsCommands(else_commands)
                v._1 ++ List(If(varToVarExp(v._2), thenC, elseC))
            }
        }
    }

    /**
     * @param commands : une liste d'AST décrivant une liste de commandes du langage WHILE
     * @return une liste de commandes ayant un seul construteur par expression
     * et ayant le même effet que la commande
     */
    def while1ConsCommands(commands: List[Command]): List[Command] = {
        var res: List[Command] = Nil
        commands.foreach(c => res = res ++ while1ConsCommand(c))
        res
    }

    /**
     *
     *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
     */

    /**
     * @param program : un AST décrivant un programme du langage WHILE
     * @param is : une liste de spécifications d'indentation
     * @return une liste de chaînes représentant la syntaxe concrète du programme
     */
    def while1ConsProgr(program: Program): Program = {
        program match {
            case Progr(in, body, out) => {
                Progr(in, while1ConsCommands(body), out)
            }
        }
    }

    def main(args: Array[String]): Unit = {

        // vous pouvez ici tester manuellement vos fonctions par des print

    }
}