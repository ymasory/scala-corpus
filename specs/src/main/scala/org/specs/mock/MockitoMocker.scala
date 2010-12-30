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
package org.mockito
import org.mockito.stubbing.Answer
import org.mockito.verification.VerificationMode

/**
 * This class is created to get an access to the MOCKING_PROGRESS Mockito package variable which is package protected.
 * Then it delegates all the methods to the Mockito static methods.
 * 
 * @see org.specs.mock.Mockito
 */
class MockitoMocker {
  def verify(mode: VerificationMode) = Mockito.verify(Mockito.mock(classOf[List[Int]]), mode)
  def mock[T](implicit m: scala.reflect.ClassManifest[T]): T = Mockito.mock(m.erasure).asInstanceOf[T]
  def mock[T](name: String)(implicit m: scala.reflect.ClassManifest[T]): T = Mockito.mock(m.erasure, name).asInstanceOf[T]
  def mock[T, A](implicit m: scala.reflect.ClassManifest[T], a: org.mockito.stubbing.Answer[A]): T = Mockito.mock(m.erasure, a).asInstanceOf[T]
  def smartMock[T](implicit m: scala.reflect.ClassManifest[T]): T = Mockito.mock(m.erasure, Mockito.RETURNS_SMART_NULLS).asInstanceOf[T]
  def spy[T](m: T): T = Mockito.spy(m)
  def when[V](v: V) = Mockito.when(v)
  def times(i: Int): org.mockito.internal.verification.Times = Mockito.times(i).asInstanceOf[org.mockito.internal.verification.Times]
  def verify[M <: AnyRef](inOrder: Option[InOrder], m: M, v: VerificationMode) = {
    inOrder match {
      case Some(ordered) => ordered.verify(m, v)
      case None => Mockito.verify(m, v)
    }
  }
  def verify[M](m: M, v: VerificationMode) = Mockito.verify(m, v)
  def doReturn[T](t: T) = Mockito.doReturn(t)
  def doAnswer[T](a: Answer[T]) = Mockito.doAnswer(a)
  def doThrow[E <: Throwable](e: E) = Mockito.doThrow(e)
  def doNothing = Mockito.doNothing
  def verifyNoMoreInteractions[T <: AnyRef](mocks: T*) = for (m <- mocks) Mockito.verifyNoMoreInteractions(m)
}
