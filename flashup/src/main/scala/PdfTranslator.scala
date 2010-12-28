package com.yuvimasory.flashcards

import java.lang.Math

import java.io.{File, FileOutputStream, StringReader}

import com.itextpdf.text.{Document => ITextDocument, _}
import com.itextpdf.text.Font._
import com.itextpdf.text.Utilities._

import com.itextpdf.text.pdf._

import StringUtils.LF
import CLI.ProgramName

private[flashcards] object PdfTranslator extends FlashcardTranslator {
  import PdfConstants._

  override def translate(flashDoc: Document, sides: Pages.Value, outFile: File): Unit = {
    try {
      val pdfDoc = new ITextDocument(flashDoc.hashCode.toString)
      pdfDoc addCreator ProgramName
      
      pdfDoc setPageSize PageRect
      val writer = PdfWriter getInstance (pdfDoc, new FileOutputStream(outFile))
      pdfDoc open()
      val canvas = writer.getDirectContentUnder
      (0 until flashDoc.cards.length) foreach { i =>
        val card = flashDoc.cards(i)
        val topLeftStr: String = flashDoc.topLeftValue getOrElse ""
        val topRightStr: String = flashDoc.topRightValue getOrElse ""
        val useNumbering: Boolean = flashDoc.useNumbering
        if (sides != Pages.Backs) {
          pdfDoc newPage()
          drawDirectiveVals(
            canvas,
            topLeftStr,
            topRightStr,
            if (useNumbering) {(i + 1).toString} else ""
          )
          drawFront(canvas, card.front)
        }
        if (sides != Pages.Fronts) {
          pdfDoc newPage()
          drawBack (canvas, card.back)
        }
      }
      
      pdfDoc close()
    }
    catch {
      case e => e.printStackTrace
    }
  }

  def drawFront(canvas: PdfContentByte, front: Front) {
    canvas saveState()
    canvas beginText()

    def writeFrontElements(rect: Rectangle, simulate: Boolean, fontSize: Int): (Boolean, Float) = {
      val ct = new ColumnText(canvas)
      ct setLeading (fontSize * LeadingMultiplier)

      front.frontEls foreach {
        case FrontElement(Stretch(spans)) => {
          spans foreach {
            case Span(text, dec) => {
              val font = dec match {
                case Plain => new Font(PlainFontBf, fontSize)
                case Mono => new Font(MonoFontBf, fontSize)
                case Italic => new Font(ItalicFontBf, fontSize)
                case Bold => new Font(BoldFontBf, fontSize)
              }
              ct addText (new Phrase(text, font)) //causes error on page warning on acroread
            }
          }
        }
        ct addText(new Phrase(LF + LF, new Font(PlainFontBf, fontSize))) //causes error on page warning on acroread
      }

      ct.setAlignment(Element.ALIGN_CENTER)
      ct setSimpleColumn(
        rect getLeft,
        rect getBottom,
        rect getRight,
        rect getTop
      )

      val fits = ColumnText.hasMoreText(ct.go(simulate)) == false
      val yLine = ct.getYLine

      (fits, yLine)
    }

    var curSize = FrontElementFontSize
    var foundFit = false
    var yLine = -1f
    while (foundFit == false && curSize >= MinFontSize) {
      val (fits, y) = writeFrontElements(FrontElementsRect, true, curSize)
      if (fits) {
        foundFit = true
        yLine = y
      }
      else
        curSize -= 1
    }
    if (foundFit == false)
      Console.err println("WARNING: Could not fit front page. Using minimum font size (" + MinFontSize + "), but text may go off page.")
    val sizeToUse = Math max (curSize, MinFontSize)
    val diff = yLine - FrontElementsRect.getBottom
    val centeredRect = new Rectangle(
      FrontElementsRect.getLeft,
      FrontElementsRect.getBottom + diff/2,
      FrontElementsRect.getRight,
      FrontElementsRect.getTop - diff/2)
    writeFrontElements(centeredRect, false, sizeToUse)

    canvas endText()
    canvas restoreState()
  }

  def drawBack(canvas: PdfContentByte, back: Back) {
    canvas saveState()
    canvas beginText()

    def writeBackElements(simulate: Boolean, fontSize: Int): Boolean = {
      val ct = new ColumnText(canvas)
      ct setLeading (fontSize * LeadingMultiplier)
      back.backEls foreach {
        case Line(stretch) => {
          stretch.spans foreach {
            case Span(text, dec) => {
              val font = dec match {
                case Plain => new Font(PlainFontBf, fontSize)
                case Mono => new Font(MonoFontBf, fontSize)
                case Italic => new Font(ItalicFontBf, fontSize)
                case Bold => new Font(BoldFontBf, fontSize)
              }
              ct addText (new Phrase(text, font)) //causes error on page warning on acroread
            }
          }
          ct addText(new Phrase(LF, new Font(PlainFontBf, fontSize))) //causes error on page warning on acroread
        }
        case CodeBlock(lines) => {
          lines foreach {
            case Line(stretch) => {
              stretch.spans foreach {
                case Span(text, _) => {
                  val width = MonoFontBf getWidthPoint (text, fontSize)
                  if (FrontElementsRect.getWidth < width)
                    return false
                  ct addText (new Phrase(text + LF, new Font(MonoFontBf, fontSize))) //causes error on page warning on acroread
                }
              }
            }
          }
        }
      }
      
      ct.setAlignment(Element.ALIGN_LEFT)
      ct.setSimpleColumn(
        BackRect getLeft,
        BackRect getBottom,
        BackRect getRight,
        BackRect getTop)
        
      ColumnText.hasMoreText(ct.go(simulate)) == false
    }

    var curSize = BackElementFontSize
    var foundFit = false
    while (foundFit == false && curSize >= MinFontSize) {
      if (writeBackElements(true, curSize))
        foundFit = true
      else
        curSize -= 1
    }
    if (foundFit == false)
      Console.err println("WARNING: Could not fit back page. Using minimum font size (" + MinFontSize + "), but text may go off page.")
    val toUse = Math max (curSize, MinFontSize)
    writeBackElements(false, toUse)

    canvas endText()
    canvas restoreState()
  }

  

  def drawDirectiveVals(canvas: PdfContentByte, topLeft: String, topRight: String, bottomCenter: String) {
    canvas saveState()
    canvas beginText()

    def chooseFontSize(str: String, maxSize: Double) = {
      var curSize = TopFontSize
      var foundFit = false
      while (foundFit == false && curSize >= MinFontSize) {
        val width = TopFontBf getWidthPoint (str, curSize)
        if (width <= maxSize)
          foundFit = true
        else
          curSize -= 1
      }
      if (foundFit == false)
        Console.err println("WARNING: Could not fit top element. Using minimum font size (" + MinFontSize + "), but text may exceed top element bounds.")
      Math max (curSize, MinFontSize)
    }

    canvas setFontAndSize (TopFontBf, chooseFontSize(topLeft, TopLeftWidth))
    canvas showTextAligned (Element.ALIGN_LEFT, topLeft, LRBMargin, PageRect.getHeight - TMargin, 0)

    canvas setFontAndSize (TopFontBf, chooseFontSize(topRight, TopRightWidth))
    canvas showTextAligned (Element.ALIGN_RIGHT, topRight, PageRect.getWidth - LRBMargin, PageRect.getHeight - TMargin, 0)

    canvas setFontAndSize (PageNumFontBf, PageNumFontSize)
    canvas showTextAligned (Element.ALIGN_CENTER, bottomCenter, PageRect.getWidth/2, TMargin - PageNumFontSize/2, 0)

    canvas endText()
    canvas restoreState()
  }
}

private[flashcards] object PdfConstants {

  val LeadingMultiplier = 1.25f

  val TMargin = millimetersToPoints(5.5f)
  val LRBMargin = millimetersToPoints(5.5f)

  val PageRect = new Rectangle(inchesToPoints(5), inchesToPoints(3))
  val FrontElementsRect = new Rectangle(LRBMargin, LRBMargin * 2, PageRect.getWidth - LRBMargin, PageRect.getHeight - (TMargin * 2))
  val BackRect = new Rectangle(LRBMargin, LRBMargin, PageRect.getWidth - LRBMargin, PageRect.getHeight - LRBMargin)

  val TopLeftWidth = (PageRect.getWidth - (LRBMargin + LRBMargin)) * (2./3.) - (0.5 * millimetersToPoints(5))
  val TopRightWidth = (PageRect.getWidth - (LRBMargin + LRBMargin)) * (1./3.) - (0.5 * millimetersToPoints(5))

  val MinFontSize = 6
  val TopFontSize = 10
  val PageNumFontSize = 10
  val FrontElementFontSize = 16
  val BackElementFontSize = 12

  val PlainFontBf = BaseFont createFont ("/freefont/FreeSans.ttf", BaseFont.WINANSI, BaseFont.EMBEDDED)
  val MonoFontBf = BaseFont createFont ("/freefont/FreeMono.ttf", BaseFont.WINANSI, BaseFont.EMBEDDED)
  val ItalicFontBf = BaseFont createFont ("/freefont/FreeSansOblique.ttf", BaseFont.WINANSI, BaseFont.EMBEDDED)
  val BoldFontBf = BaseFont createFont ("/freefont/FreeSansBold.ttf", BaseFont.WINANSI, BaseFont.EMBEDDED)
  val TopFontBf = BoldFontBf
  val PageNumFontBf = PlainFontBf
}
