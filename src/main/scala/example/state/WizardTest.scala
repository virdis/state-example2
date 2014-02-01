package example.state

import State._

object WizardTest {

  sealed trait Action
  case object HitWithAxe extends Action
  case object HitWithSword extends Action
  case object GenericHeal extends Action
  case object HealWithElixir extends Action
  case object BlackPoison extends Action


  case class Wizard(name: String, health: Int)

  def wizardGame(actions: List[Action]) = for {
    _ <- sequence( actions.map( a => modify((w: Wizard) => (a,w) match {
      case (HitWithAxe, w) => Wizard( w.name, w.health - 10)
      case (HitWithSword, w) => Wizard( w.name, w.health - 20)
      case (GenericHeal, w) => Wizard( w.name, w.health + 10)
      case (HealWithElixir, w) => Wizard( w.name, w.health + 100 )
      case (BlackPoison, w) => Wizard( w.name, 0)
    })))
    s <- get
  } yield s

}
