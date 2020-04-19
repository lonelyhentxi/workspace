case class Address(street: String, city: String, country: String)

case class Person(name: String, age: Int, address: Address)

object Person {
  def apply(name: String, age: Int, address: Address): Person = new Person(name, age, address)

  def unapply(p: Person): Option[(String, Int, Address)] = Some((p.name, p.age, p.address))
}