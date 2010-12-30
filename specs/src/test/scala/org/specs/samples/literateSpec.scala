/**
 * Copyright (c) 2007-2010 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.samples
import org.specs.specification._
import org.specs.matcher._
import org.specs.util._
import org.specs.form._
import org.specs.runner._
import org.specs._

class helloWorld extends HtmlSpecification("Hello World") {
  def greet = "hello"
  def greet(s: String) = s match {
    case "French" => "bonjour"
    case "German" => "hallo"
  }
  
"The greeting application" is <textile>
h3. Presentation

This new application should say "hello" in different languages.

For example,<ex>by default, saying hello by default should use English</ex> { greet must_== "hello"}
 
Then, other languages, like <ex>French and German should be supported too</ex> 
{ eg {
    greet("French") must_== "bonjour"
    greet("German") must_== "hallo"
  } 
}

<ex>Japanese should be supported also</ex> { notImplemented }

 </textile>
}
class tabsSpecification extends HtmlSpecificationWithJUnit("Tabs sample") {
 class ClubMember extends Form {
   new tabs() {
     new tab("Contact details") {
       tr(field("First name", "Eric"))
       tr(field("Last name", "Torreborre"))
     }
     new tab("Sports") {
       th2("Sport", "Years of practice")
       tr(field("Squash", 10))
       tr(field("Tennis", 5))
       tr(field("Windsurf", 2))
     }
   }
 }
 "A form with tabs" is <textile>
   { new ClubMember().toHtml }  
  </textile>
}

class fieldsFormSpecification extends HtmlSpecificationWithJUnit("Fields form") {
 class Person extends Form {
   val firstName = field("First name", "Eric")
   val lastName = field("Last name", "Torreborre")
   tr(firstName)
   tr(lastName)
 }
 "A form with fields" is <textile>
   { new Person().toHtml }  
  </textile>
}
class bagFormSpecification extends HtmlSpecification("Bag form") {
  case class Customer(name: String, age: Int)
  case class CustomerLine(name: String, age: Int) extends EntityLineForm[Customer] {
    // the prop method accepts a function here, taking the proper attribute on the "Entity"
    prop("Name", (_:Customer).name)(name)
    prop("Age", (_:Customer).age)(age)
  }
  class Customers(actualCustomers: Seq[Customer]) extends BagForm[Customer](actualCustomers)
  
  // usage example
 "A Bag Form" is <textile> {   
  new Customers(List(Customer("Eric", 36), Customer("Bob", 27))) {
    tr(CustomerLine("Eric", 36))
    tr(CustomerLine("Eric", 38))
    tr(CustomerLine("Bob", 27))
  }.report
 }  
  </textile>

}
class formSampleSpec extends PersonForms {
  "Forms can be used in a Literate specification" is <textile>

This is a Person form, checking that the initials are set properly on a Person object:
{ 
  val address = Address(37, "Nando-cho")
  val person = Person("Eric", "Torreborre", address, List("Jerome", "Olivier"))

  "Initials are automatically populated" inForm
   new PersonForm(person) {
    firstName("Eric")       
    initials("et")
    friends("Jerome", "Olivier")
    address.set { a =>
                  a.number(37)
                  a.street("Nando-cho")}
    lastName("Torreborre")
   }
}

  </textile>
}
class tableFormSpec extends HtmlSpecificationWithJUnit("Table form") {
  case class Person(n: String, a: Int) extends LineForm {
    field("Name", n)
    prop("Age", 35)(a)
  }
 "A table form with one line form per line" is <textile> { 
   new TableForm("Persons") {
     tr(Person("Eric", 35))
     tr(Person("Eric again", 35))
   }.reportTo(this) 
 }  
  </textile>
}

trait PersonBusinessEntities {
  case class Person(firstName: String, lastName: String, address: Address, friends: List[String]) {
    def initials = firstName(0).toString + lastName(0)
  }
  case class Address(number: Int, street: String)
}
class PersonForms extends HtmlSpecificationWithJUnit with PersonBusinessEntities {

  case class PersonForm(t: String, p: Person) extends Form(t) {
    def this(p: Person) = this("Customer", p)
    val firstName = prop("First Name", p.firstName)
    val lastName = prop("Last Name", p.lastName)
    val initials = prop("Initials", p.initials).matchesWith(beEqualToIgnoringCase(_))
    val friends =  propIterable("Friends", p.friends)
    val address = form(AddressForm("Home", p.address))

    tr(firstName, address)
    tr(lastName, initials)
    tr(friends)
  }
  case class AddressForm(t: String, address: Address) extends Form(t) {
    def this(a: Address) = this("Home", a)
    val number = prop("Number", address.number)
    val street = prop("Street", address.street)
    tr(number)
    tr(street)
  }
}
