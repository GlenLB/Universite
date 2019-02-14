package compteur

import org.junit.Test
import org.junit.Assert._



class TestCompteur {
  val c:Compteur= new CompteurImpl()
  
  
  @Test
  def test1{
    c.init(List(1))
    assertTrue(c.suivantPossible)
    assertEquals(2,c.valPossibles)
    assertEquals(List(0),c.courant)
    assertTrue(c.suivantPossible)
    c.suivant
    assertEquals(List(1),c.courant)
    assertFalse(c.suivantPossible)
    //... A compléter!
  }
}
