package compteur

/**
 * La classe des compteurs paramétrés par une liste non vide de valeurs maximale pour chaque roue.
 *  La liste ne peut être vide et les valeurs sont forcément positives ou nulles
 */
class CompteurImpl extends Compteur {
  var compteur: List[Int] = List()
  var arrayValCourante: Array[Int] = new Array[Int](0)
  def init(l: List[Int]): Unit = {
    this.compteur = l
    this.initValCourante()
  }
  def courant(): List[Int] = {
    arrayValCourante.toList
  }
  def suivant(): Unit = {
    this.arrayValCourante(0) = 1
  }
  def suivantPossible(): Boolean = {
    arrayValCourante(0) == 0
  }
  def valPossibles = 2
  def valRestantes = 0
  /**
   * Initialise le compteur stockant la valeur courante
   * Toutes les valeurs sont initialisées à 0 par défaut
   */
  def initValCourante(): Unit = {
    this.arrayValCourante = new Array[Int](this.compteur.length)
  }
}