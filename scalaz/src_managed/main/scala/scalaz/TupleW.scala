package scalaz

trait Tuples {
    
trait Tuple2W[A, B] extends PimpedType[Tuple2[A, B]] {
  def fold[Z](f: => (A, B) => Z): Z = {import value._; f(_1, _2)}
  def toIndexedSeq[Z](implicit ev: value.type <:< Tuple2[Z, Z]): IndexedSeq[Z] = {val zs = ev(value); import zs._; IndexedSeq(_1, _2)}
  def mapElements[AA, BB](_1: (A => AA) = identity[A] _, _2: (B => BB) = identity[B] _): (AA, BB) = (_1(value._1), _2(value._2))
}

implicit def ToTuple2W[A, B](t: (A, B)): Tuple2W[A, B] = new { val value = t } with Tuple2W[A, B]

  
trait Tuple3W[A, B, C] extends PimpedType[Tuple3[A, B, C]] {
  def fold[Z](f: => (A, B, C) => Z): Z = {import value._; f(_1, _2, _3)}
  def toIndexedSeq[Z](implicit ev: value.type <:< Tuple3[Z, Z, Z]): IndexedSeq[Z] = {val zs = ev(value); import zs._; IndexedSeq(_1, _2, _3)}
  def mapElements[AA, BB, CC](_1: (A => AA) = identity[A] _, _2: (B => BB) = identity[B] _, _3: (C => CC) = identity[C] _): (AA, BB, CC) = (_1(value._1), _2(value._2), _3(value._3))
}

implicit def ToTuple3W[A, B, C](t: (A, B, C)): Tuple3W[A, B, C] = new { val value = t } with Tuple3W[A, B, C]

  
trait Tuple4W[A, B, C, D] extends PimpedType[Tuple4[A, B, C, D]] {
  def fold[Z](f: => (A, B, C, D) => Z): Z = {import value._; f(_1, _2, _3, _4)}
  def toIndexedSeq[Z](implicit ev: value.type <:< Tuple4[Z, Z, Z, Z]): IndexedSeq[Z] = {val zs = ev(value); import zs._; IndexedSeq(_1, _2, _3, _4)}
  def mapElements[AA, BB, CC, DD](_1: (A => AA) = identity[A] _, _2: (B => BB) = identity[B] _, _3: (C => CC) = identity[C] _, _4: (D => DD) = identity[D] _): (AA, BB, CC, DD) = (_1(value._1), _2(value._2), _3(value._3), _4(value._4))
}

implicit def ToTuple4W[A, B, C, D](t: (A, B, C, D)): Tuple4W[A, B, C, D] = new { val value = t } with Tuple4W[A, B, C, D]

  
trait Tuple5W[A, B, C, D, E] extends PimpedType[Tuple5[A, B, C, D, E]] {
  def fold[Z](f: => (A, B, C, D, E) => Z): Z = {import value._; f(_1, _2, _3, _4, _5)}
  def toIndexedSeq[Z](implicit ev: value.type <:< Tuple5[Z, Z, Z, Z, Z]): IndexedSeq[Z] = {val zs = ev(value); import zs._; IndexedSeq(_1, _2, _3, _4, _5)}
  def mapElements[AA, BB, CC, DD, EE](_1: (A => AA) = identity[A] _, _2: (B => BB) = identity[B] _, _3: (C => CC) = identity[C] _, _4: (D => DD) = identity[D] _, _5: (E => EE) = identity[E] _): (AA, BB, CC, DD, EE) = (_1(value._1), _2(value._2), _3(value._3), _4(value._4), _5(value._5))
}

implicit def ToTuple5W[A, B, C, D, E](t: (A, B, C, D, E)): Tuple5W[A, B, C, D, E] = new { val value = t } with Tuple5W[A, B, C, D, E]

  
trait Tuple6W[A, B, C, D, E, F] extends PimpedType[Tuple6[A, B, C, D, E, F]] {
  def fold[Z](f: => (A, B, C, D, E, F) => Z): Z = {import value._; f(_1, _2, _3, _4, _5, _6)}
  def toIndexedSeq[Z](implicit ev: value.type <:< Tuple6[Z, Z, Z, Z, Z, Z]): IndexedSeq[Z] = {val zs = ev(value); import zs._; IndexedSeq(_1, _2, _3, _4, _5, _6)}
  def mapElements[AA, BB, CC, DD, EE, FF](_1: (A => AA) = identity[A] _, _2: (B => BB) = identity[B] _, _3: (C => CC) = identity[C] _, _4: (D => DD) = identity[D] _, _5: (E => EE) = identity[E] _, _6: (F => FF) = identity[F] _): (AA, BB, CC, DD, EE, FF) = (_1(value._1), _2(value._2), _3(value._3), _4(value._4), _5(value._5), _6(value._6))
}

implicit def ToTuple6W[A, B, C, D, E, F](t: (A, B, C, D, E, F)): Tuple6W[A, B, C, D, E, F] = new { val value = t } with Tuple6W[A, B, C, D, E, F]

  
trait Tuple7W[A, B, C, D, E, F, G] extends PimpedType[Tuple7[A, B, C, D, E, F, G]] {
  def fold[Z](f: => (A, B, C, D, E, F, G) => Z): Z = {import value._; f(_1, _2, _3, _4, _5, _6, _7)}
  def toIndexedSeq[Z](implicit ev: value.type <:< Tuple7[Z, Z, Z, Z, Z, Z, Z]): IndexedSeq[Z] = {val zs = ev(value); import zs._; IndexedSeq(_1, _2, _3, _4, _5, _6, _7)}
  def mapElements[AA, BB, CC, DD, EE, FF, GG](_1: (A => AA) = identity[A] _, _2: (B => BB) = identity[B] _, _3: (C => CC) = identity[C] _, _4: (D => DD) = identity[D] _, _5: (E => EE) = identity[E] _, _6: (F => FF) = identity[F] _, _7: (G => GG) = identity[G] _): (AA, BB, CC, DD, EE, FF, GG) = (_1(value._1), _2(value._2), _3(value._3), _4(value._4), _5(value._5), _6(value._6), _7(value._7))
}

implicit def ToTuple7W[A, B, C, D, E, F, G](t: (A, B, C, D, E, F, G)): Tuple7W[A, B, C, D, E, F, G] = new { val value = t } with Tuple7W[A, B, C, D, E, F, G]

  
trait Tuple8W[A, B, C, D, E, F, G, H] extends PimpedType[Tuple8[A, B, C, D, E, F, G, H]] {
  def fold[Z](f: => (A, B, C, D, E, F, G, H) => Z): Z = {import value._; f(_1, _2, _3, _4, _5, _6, _7, _8)}
  def toIndexedSeq[Z](implicit ev: value.type <:< Tuple8[Z, Z, Z, Z, Z, Z, Z, Z]): IndexedSeq[Z] = {val zs = ev(value); import zs._; IndexedSeq(_1, _2, _3, _4, _5, _6, _7, _8)}
  def mapElements[AA, BB, CC, DD, EE, FF, GG, HH](_1: (A => AA) = identity[A] _, _2: (B => BB) = identity[B] _, _3: (C => CC) = identity[C] _, _4: (D => DD) = identity[D] _, _5: (E => EE) = identity[E] _, _6: (F => FF) = identity[F] _, _7: (G => GG) = identity[G] _, _8: (H => HH) = identity[H] _): (AA, BB, CC, DD, EE, FF, GG, HH) = (_1(value._1), _2(value._2), _3(value._3), _4(value._4), _5(value._5), _6(value._6), _7(value._7), _8(value._8))
}

implicit def ToTuple8W[A, B, C, D, E, F, G, H](t: (A, B, C, D, E, F, G, H)): Tuple8W[A, B, C, D, E, F, G, H] = new { val value = t } with Tuple8W[A, B, C, D, E, F, G, H]

  
trait Tuple9W[A, B, C, D, E, F, G, H, I] extends PimpedType[Tuple9[A, B, C, D, E, F, G, H, I]] {
  def fold[Z](f: => (A, B, C, D, E, F, G, H, I) => Z): Z = {import value._; f(_1, _2, _3, _4, _5, _6, _7, _8, _9)}
  def toIndexedSeq[Z](implicit ev: value.type <:< Tuple9[Z, Z, Z, Z, Z, Z, Z, Z, Z]): IndexedSeq[Z] = {val zs = ev(value); import zs._; IndexedSeq(_1, _2, _3, _4, _5, _6, _7, _8, _9)}
  def mapElements[AA, BB, CC, DD, EE, FF, GG, HH, II](_1: (A => AA) = identity[A] _, _2: (B => BB) = identity[B] _, _3: (C => CC) = identity[C] _, _4: (D => DD) = identity[D] _, _5: (E => EE) = identity[E] _, _6: (F => FF) = identity[F] _, _7: (G => GG) = identity[G] _, _8: (H => HH) = identity[H] _, _9: (I => II) = identity[I] _): (AA, BB, CC, DD, EE, FF, GG, HH, II) = (_1(value._1), _2(value._2), _3(value._3), _4(value._4), _5(value._5), _6(value._6), _7(value._7), _8(value._8), _9(value._9))
}

implicit def ToTuple9W[A, B, C, D, E, F, G, H, I](t: (A, B, C, D, E, F, G, H, I)): Tuple9W[A, B, C, D, E, F, G, H, I] = new { val value = t } with Tuple9W[A, B, C, D, E, F, G, H, I]

  
trait Tuple10W[A, B, C, D, E, F, G, H, I, J] extends PimpedType[Tuple10[A, B, C, D, E, F, G, H, I, J]] {
  def fold[Z](f: => (A, B, C, D, E, F, G, H, I, J) => Z): Z = {import value._; f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)}
  def toIndexedSeq[Z](implicit ev: value.type <:< Tuple10[Z, Z, Z, Z, Z, Z, Z, Z, Z, Z]): IndexedSeq[Z] = {val zs = ev(value); import zs._; IndexedSeq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)}
  def mapElements[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ](_1: (A => AA) = identity[A] _, _2: (B => BB) = identity[B] _, _3: (C => CC) = identity[C] _, _4: (D => DD) = identity[D] _, _5: (E => EE) = identity[E] _, _6: (F => FF) = identity[F] _, _7: (G => GG) = identity[G] _, _8: (H => HH) = identity[H] _, _9: (I => II) = identity[I] _, _10: (J => JJ) = identity[J] _): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ) = (_1(value._1), _2(value._2), _3(value._3), _4(value._4), _5(value._5), _6(value._6), _7(value._7), _8(value._8), _9(value._9), _10(value._10))
}

implicit def ToTuple10W[A, B, C, D, E, F, G, H, I, J](t: (A, B, C, D, E, F, G, H, I, J)): Tuple10W[A, B, C, D, E, F, G, H, I, J] = new { val value = t } with Tuple10W[A, B, C, D, E, F, G, H, I, J]

  
trait Tuple11W[A, B, C, D, E, F, G, H, I, J, K] extends PimpedType[Tuple11[A, B, C, D, E, F, G, H, I, J, K]] {
  def fold[Z](f: => (A, B, C, D, E, F, G, H, I, J, K) => Z): Z = {import value._; f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11)}
  def toIndexedSeq[Z](implicit ev: value.type <:< Tuple11[Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z]): IndexedSeq[Z] = {val zs = ev(value); import zs._; IndexedSeq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11)}
  def mapElements[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK](_1: (A => AA) = identity[A] _, _2: (B => BB) = identity[B] _, _3: (C => CC) = identity[C] _, _4: (D => DD) = identity[D] _, _5: (E => EE) = identity[E] _, _6: (F => FF) = identity[F] _, _7: (G => GG) = identity[G] _, _8: (H => HH) = identity[H] _, _9: (I => II) = identity[I] _, _10: (J => JJ) = identity[J] _, _11: (K => KK) = identity[K] _): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK) = (_1(value._1), _2(value._2), _3(value._3), _4(value._4), _5(value._5), _6(value._6), _7(value._7), _8(value._8), _9(value._9), _10(value._10), _11(value._11))
}

implicit def ToTuple11W[A, B, C, D, E, F, G, H, I, J, K](t: (A, B, C, D, E, F, G, H, I, J, K)): Tuple11W[A, B, C, D, E, F, G, H, I, J, K] = new { val value = t } with Tuple11W[A, B, C, D, E, F, G, H, I, J, K]

  
trait Tuple12W[A, B, C, D, E, F, G, H, I, J, K, L] extends PimpedType[Tuple12[A, B, C, D, E, F, G, H, I, J, K, L]] {
  def fold[Z](f: => (A, B, C, D, E, F, G, H, I, J, K, L) => Z): Z = {import value._; f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12)}
  def toIndexedSeq[Z](implicit ev: value.type <:< Tuple12[Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z]): IndexedSeq[Z] = {val zs = ev(value); import zs._; IndexedSeq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12)}
  def mapElements[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL](_1: (A => AA) = identity[A] _, _2: (B => BB) = identity[B] _, _3: (C => CC) = identity[C] _, _4: (D => DD) = identity[D] _, _5: (E => EE) = identity[E] _, _6: (F => FF) = identity[F] _, _7: (G => GG) = identity[G] _, _8: (H => HH) = identity[H] _, _9: (I => II) = identity[I] _, _10: (J => JJ) = identity[J] _, _11: (K => KK) = identity[K] _, _12: (L => LL) = identity[L] _): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL) = (_1(value._1), _2(value._2), _3(value._3), _4(value._4), _5(value._5), _6(value._6), _7(value._7), _8(value._8), _9(value._9), _10(value._10), _11(value._11), _12(value._12))
}

implicit def ToTuple12W[A, B, C, D, E, F, G, H, I, J, K, L](t: (A, B, C, D, E, F, G, H, I, J, K, L)): Tuple12W[A, B, C, D, E, F, G, H, I, J, K, L] = new { val value = t } with Tuple12W[A, B, C, D, E, F, G, H, I, J, K, L]
}