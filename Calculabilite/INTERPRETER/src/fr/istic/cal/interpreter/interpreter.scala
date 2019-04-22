package fr.istic.cal.interpreter

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

object Interpreter {

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
     * UN INTERPRETER POUR LE LANGAGE WHILE
     *
     */

    /**
     *  GESTION DE LA MEMOIRE DE L'INTERPRETEUR
     */

    /**
     *  définition d'un type Memory pour représenter une mémoire
     */
    type Memory = List[(Variable, Value)]

    /**
     * @param v : une variable
     * @param mem : une mémoire
     * @return la valeur de la variable v dans la mémoire mem,
     * la valeur par défaut si la variable v n'est pas présente dans la mémoire mem
     */
    def lookUp(v: Variable, mem: Memory): Value = {
        val listeFiltree = mem.filter((tuple) => tuple._1 == v)
        if (listeFiltree.length > 0) {
            listeFiltree(0)._2
        } else {
            NlValue
        }
    }

    /**
     * @param v : une variable
     * @param e : une expression
     * @param mem : une mémoire
     * @return la mémoire augmentée de l'assignation [v->d] si v n'était pas présente dans la mémoire,
     * modifiée pour prendre en compte la nouvelle valeur de v sinon
     */
    def assign(v: Variable, e: Value, mem: Memory): Memory = {
        val listeFiltree = mem.filter((tuple) => tuple._1 == v)
        // v est dans la mémoire
        if (listeFiltree.length > 0) {
            mem.map((tuple) => if (tuple._1 == v) (tuple._1, e) else tuple)
        } else {
            // v n'est pas dans la mémoire
            mem :+ (v, e)
        }
    }

    /**
     *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
     */

    /**
     * @param expression : un AST décrivant une expression du langage WHILE
     * @return la valeur de l'expression
     */
    def interpreterExpr(expression: Expression, mem: Memory): Value = {
        expression match {
            case Nl         => NlValue
            case Cst(x)     => CstValue(x)
            case VarExp(x)  => lookUp(Var(x), mem)
            case Cons(x, y) => ConsValue(interpreterExpr(x, mem), interpreterExpr(y, mem))
            case Hd(x) => interpreterExpr(x, mem) match {
                case ConsValue(x, y) => x
                case _               => NlValue
            }
            case Tl(x) => interpreterExpr(x, mem) match {
                case ConsValue(x, y) => y
                case _               => NlValue
            }
            case Eq(x, y) => if (interpreterExpr(x, mem) != interpreterExpr(y, mem)) NlValue else ConsValue(NlValue, NlValue)
        }
    }

    /**
     * la fonction interpreterExpr ci-dessus calcule la valeur associée à une expression
     * il peut être utile de produire à l'inverse une expression associée à une valeur
     * la fonction valueToExpression ci-dessous construira l'expression la plus simple associée à une valeur
     *
     * @param value : une valeur du langage WHILE
     * @return l'AST décrivant une expression de cette valeur
     */
    def valueToExpression(value: Value): Expression = {
        value match {
            case NlValue         => Nl
            case CstValue(x)     => Cst(x)
            case ConsValue(x, y) => Cons(valueToExpression(x), valueToExpression(y))
        }
    }

    /**
     *
     *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
     */

    /**
     * @param command : un AST décrivant une commande du langage WHILE
     * @param memory : une mémoire
     * @return la mémoire après l'interprétation de command
     */
    def interpreterCommand(command: Command, memory: Memory): Memory = {
        command match {
            case Nop         => memory
            case Set(x, y)   => assign(x, interpreterExpr(y, memory), memory)
            case While(x, y) => if (interpreterExpr(x, memory) == NlValue) memory else interpreterCommand(While(x, y), interpreterCommands(y, memory))
            case For(x, y) => {
                var v = interpreterExpr(x, memory)
                if (v == NlValue) memory else interpreterCommand(For(Tl(valueToExpression(v)), y), interpreterCommands(y, memory))
            }
            case If(x, y, z) => if (interpreterExpr(x, memory) == NlValue) interpreterCommands(z, memory) else interpreterCommands(y, memory)
        }
    }

    /**
     * @param commands : une liste d'AST décrivant une liste de commandes du langage WHILE
     * @param memory : une mémoire
     * @return la mémoire après l'interprétation de la liste de commandes
     */
    def interpreterCommands(commands: List[Command], memory: Memory): Memory = {
        var res: Memory = memory
        commands.foreach(cmd => {
            res = interpreterCommand(cmd, res)
        })
        res
    }

    /**
     *
     *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
     */

    /**
     * @param vars : une liste décrivant les variables d'entrée d'un programme du langage WHILE
     * @param vals : une liste de valeurs
     * @return une mémoire associant chaque valeur à la variable d'entrée correspondant
     */
    def interpreterMemorySet(vars: List[Variable], vals: List[Value]): Memory = {
        var resMemory: Memory = Nil
        for (i <- 0 to vars.length - 1) {
            resMemory = assign(vars(i), vals(i), resMemory)
        }
        resMemory
    }

    /**
     * @param vars : une liste décrivant les variables de sortie d'un programme du langage WHILE
     * @param memory : une mémoire
     * @return la liste des valeurs des variables de sortie
     */
    def interpreterMemoryGet(vars: List[Variable], memory: Memory): List[Value] = {
        vars.map(variable => lookUp(variable, memory))
    }

    /**
     * @param program : un AST décrivant un programme du langage WHILE
     * @param vals : une liste de valeurs
     * @return la liste des valeurs des variables de sortie
     */
    def interpreter(program: Program, vals: List[Value]): List[Value] = {
        program match {
            case Progr(in, body, out) => {
                var memory: Memory = interpreterMemorySet(in, vals)
                memory = interpreterCommands(body, memory)
                val res = interpreterMemoryGet(out, memory)
                res
            }
        }
    }

}