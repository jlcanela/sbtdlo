/*
 * HlloWorld.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package 

object helloWorld extends Specification {
  "'hello world' has 11 characters" in {
     "hello world".size must be equalTo(11)
  }
  "'hello world' matches 'h.* w.*'" in {
     "hello world" must be matching("h.* w.*")
  }
}