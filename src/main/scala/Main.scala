case class User(firstName: String, age: Int)

@main def main: Unit =
  val userProps = props[User]
  println("Prop for firstName: " + userProps.firstName)
  println("Prop for age: " + userProps.age)

  val user = builder[User]
    .withFirstName("Danny")
    .withLastName("Glover")
    .build
  println(user)
