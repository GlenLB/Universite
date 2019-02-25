package compteur

import org.junit._
import org.junit.Assert._

/**
 * Tests pour CompteurImplem.scala
 */
class TestCompteur {
    /** Compteur utilisé pour les tests */
    val c: Compteur = new CompteurImpl()
    
    /**
     * Test1 : initialise un compteur comportant une roue pouvant avoir pour valeur 0 ou 1
     * et réalise une série de tests sur ce compteur
     */
    @Test
    def testCompteur1Roue() = {
        // Initialise un compteur comportant une roue. Valeur actuelle 0, valeur max 1.
        c.init(List(1))
        // Doit rendre true (encore 1 valeur possible)
        assertTrue(c.suivantPossible)
        // Le compteur permet 2 valeurs possibles, 0 et 1
        assertEquals(2, c.valPossibles)
        // Le compteur actuel est List(0)
        assertEquals(List(0), c.courant)
        // Il reste 1 valeur possible à incrémenter
        assertEquals(1, c.valRestantes)
        
        // Incrémente le compteur
        c.suivant
        
        // Le compteur actuel est List(1)
        assertEquals(List(1), c.courant)
        // Le compteur est à sa valeur max
        assertFalse(c.suivantPossible)
        // Il reste 0 valeur possible à incrémenter
        assertEquals(0, c.valRestantes)
        
        // Incrémente le compteur
        // Pas de changement attendu car la valeur max du compteur est atteinte
        c.suivant
        
        // Le compteur actuel est List(1)
        assertEquals(List(1), c.courant)
        // Le compteur est à sa valeur max
        assertFalse(c.suivantPossible)
        // Il reste 0 valeur possible à incrémenter
        assertEquals(0, c.valRestantes)
    }

    /**
     * Test2 : initialise un compteur comportant 3 roues
     * et réalise une série de tests sur ce compteur
     */
    @Test
    def testCompteurPlusieursRoues() = {
        // Initialise un compteur comportant 3 roues. Valeur actuelle 000, valeur max 123.
        c.init(List(1, 2, 3))
        // Le compteur permet 24 valeurs possibles
        assertEquals(24, c.valPossibles)
        // Doit rendre true (encore 23 valeurs possibles)
        assertTrue(c.suivantPossible)
        // Le compteur actuel est List(0, 0, 0)
        assertEquals(List(0, 0, 0), c.courant)
        // Il reste 23 valeurs possible à incrémenter
        assertEquals(23, c.valRestantes)
        
        // Incrémente le compteur
        c.suivant
        
        // Le compteur actuel est List(0, 0, 1)
        assertEquals(List(0, 0, 1), c.courant)
        // Doit rendre true (encore 22 valeurs possibles)
        assertTrue(c.suivantPossible)
        // Il reste 22 valeur possible à incrémenter
        assertEquals(22, c.valRestantes)
        
        // Incrémente le compteur
        c.suivant
        
        // Le compteur actuel est List(0, 0, 2)
        assertEquals(List(0, 0, 2), c.courant)
        // Doit rendre true (encore 21 valeurs possibles)
        assertTrue(c.suivantPossible)
        // Il reste 21 valeur possible à incrémenter
        assertEquals(21, c.valRestantes)
        
        // Réalise 3 incrémentations
        for(i <- 1 to 3) {
            c.suivant
        }
        
        // Le compteur actuel est List(0, 1, 1)
        assertEquals(List(0, 1, 1), c.courant)
        // Doit rendre true
        assertTrue(c.suivantPossible)
        // Il reste 18 valeur possible à incrémenter
        assertEquals(18, c.valRestantes)
        
        // Réalise 18 incrémentations (jusqu'à la valeur max)
        for(i <- 1 to 18) {
            c.suivant
        }
        
        // Le compteur actuel est la valeur max List(1, 2, 3)
        assertEquals(List(1, 2, 3), c.courant)
        // Doit rendre false
        assertFalse(c.suivantPossible)
        // Il reste 0 valeur possible à incrémenter
        assertEquals(0, c.valRestantes)
        
        // Incrémente le compteur
        // Le compteur ne doit pas s'incrémenter car il est à sa valeur max
        c.suivant
        
        // Le compteur actuel est la valeur max List(1, 2, 3)
        assertEquals(List(1, 2, 3), c.courant)
        // Doit rendre false
        assertFalse(c.suivantPossible)
        // Il reste 0 valeur possible à incrémenter
        assertEquals(0, c.valRestantes)
    }
    
    /**
     * Test1 : test de l'initialisation d'un compteur avec une liste vide
     */
    @Test
    def testInitListeVide() = {
        // Réalisation d'opérations avant l'initialisation
        // Doivent rendre null ou 0 selon la méthode
        assertEquals(null, c.courant())
        assertEquals(0, c.valPossibles)
        assertEquals(0, c.valRestantes)
        assertFalse(c.suivantPossible)
        // Initialisation d'un compteur avec une liste vide
        // On choisit que l'initialisation d'une liste vide ne fait rien
        c.init(Nil)
        // Les compteur sont bien initialisés à null
        assertEquals(null, c.courant())
    }
}
