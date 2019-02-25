package compteur

/**
 * Un Compteur simule un compteur mécanique à n roues.
 * Chaque roue a un range qui peut être différent de celui des autres roues du compteur.
 * Exemple:  List(1, 100, 4) représente un compteur à 3 roues. La 1ère a comme valeurs possibles
 * 0 et 1, la 2ème de 0 à 100, la 3ème de 0 à 4.
 * L'ensemble des valeurs possibles de chaque roue est un sous-ensemble des entiers naturels commençant à 0 (0 à +inf, pas de valeurs négatives).
 * Comme dans les compteurs mécaniques, lorsqu’une roue aura fait un tour complet,
 * elle fera avancer d’un cran la roue située à sa gauche (si elle existe et que sa valeur n'est pas à sa valeur maximale).
 * La liste représentant un compteur ne peut pas être vide.
 * Un compteur ne peut que s'incrémenter, pas se décrémenter.
 * On supposera qu’un compteur, s’il n’est pas initialisé avec init, ne comporte qu’une roue avec une seule valeur (0).
 */
class CompteurImpl extends Compteur {
    // Représente le compteur, les valeurs maximales de chaque roue du compteur
    private var compteur: List[Int] = null
    // Représente la valeur courante du compteur pour chaque roue
    private var compteurCourant: Array[Int] = null
    // Stocke le nombre de valeurs restantes possibles
    private var compteurValRestantes: Int = 0

    def init(l: List[Int]): Unit = {
        // La méthode n'accepte pas de liste vide ni de liste comprenant des valeurs négatives
        // En cas de liste vide ou comprenant des valeurs négatives, ne fait rien et laisse les initialisations à null
        if (l != Nil) {
            for (elem <- l) {
                if (elem < 0) return
            }
            compteur = l
            // Toutes les valeurs sont initialisées à 0 par défaut
            compteurCourant = new Array[Int](this.compteur.length)
            // Initialise le compteur du nombre valeurs restantes possibles
            compteurValRestantes = valPossibles() - 1
        }
    }

    def courant(): List[Int] = {
        if (compteur == null) {
            null
        } else {
            compteurCourant.toList
        }
    }

    def suivant(): Unit = {
        // Si les deux compteurs sont identiques,
        // c'est que la valeur max est atteinte.
        if (compteur != null && compteurCourant != null && compteurCourant.toList != compteur) {
            var roueActuelle = compteur.length - 1
            while (roueActuelle >= 0) {
                // Si la roue actuelle n'est pas à sa valeur max, l'incrémente
                if (compteurCourant(roueActuelle) < compteur(roueActuelle)) {
                    // Incrémente la roue actuelle
                    compteurCourant(roueActuelle) += 1
                    // Décrémente le nombre de valeurs restantes possibles
                    compteurValRestantes -= 1
                    return
                } else {
                    // Si la roue actuelle est à sa valeur max et qu'il y a d'autres roues à gauche,
                    // met la roue actuelle à 0 et incrémente la roue à sa gauche.
                    var roueGauche = roueActuelle - 1
                    // Vérifie que la roue existe
                    if (roueGauche >= 0) {
                        // Si la roue à gauche n'est pas à sa valeur max, l'incrémente
                        if (compteurCourant(roueGauche) < compteur(roueGauche)) {
                            // remet à 0 tous les compteurs de droite
                            for (j <- roueActuelle to compteur.length - 1) {
                                compteurCourant(j) = 0
                            }
                            compteurCourant(roueGauche) += 1
                            compteurValRestantes -= 1
                            return
                        } else {
                            // Si la roue gauche est à sa valeur max, recommence l'opération
                            // en avançant
                            roueActuelle = roueGauche
                        }
                    }
                }
            }
        }
    }
    
    def suivantPossible(): Boolean = {
        if (compteurCourant != null) {
            compteurCourant.toList != compteur
        } else false
    }

    def valPossibles(): Int = {
        if (compteur != null) {
            var res = 1
            for (i <- 0 to compteur.length - 1) {
                res = res * (compteur(i) + 1) // Ajout de 1 car commence à 0
            }
            res
        } else 0
    }

    def valRestantes(): Int = {
        compteurValRestantes
    }

}