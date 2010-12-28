package com.yuvimasory.flashcards

import java.io.{BufferedWriter, File, FileWriter}

private[flashcards] trait FlashcardTranslator {
  def translate(doc: Document, sides: Pages.Value, outFile: File): Unit
}

private[flashcards] object TxtTranslator extends FlashcardTranslator {

  override def translate(doc: Document, sides: Pages.Value, outFile: File): Unit = {
    val toWrite = doc.toString
    val out = new BufferedWriter(new FileWriter(outFile))
    try {
      out.write(toWrite)
    }
    finally {
      out.flush()
      out.close()
    }
  }
}

