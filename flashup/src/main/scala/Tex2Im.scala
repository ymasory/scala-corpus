package com.yuvimasory.flashcards

import java.io.File

import StringUtils.LF

object Tex2Im {

  def createTexDoc(fontSize: Int, texCode: String, headers: List[String]): String = {
    val builder = new StringBuilder()
    builder append "\\documentclass[" + fontSize.toString + "pt]{article}"
    builder append "\\usepackage{color}"
    builder append "\\usepackage[dvips]{graphicx}"
    builder append "\\pagestyle{empty}"

    builder append LF
    headers foreach {builder append _}
    builder append LF

    builder append "\\pagecolor{white}"
    builder append "\\begin{document}"
    builder append "{\\color{black}"
    builder append "\\begin{eqnarray*}"

    builder append LF
    builder append texCode
    builder append LF

    builder append "\\end{eqnarray*}}"
    builder append "\\end{document}"

    builder.toString
  }

  //then run:
  //latex -interaction=batchmode file.tex
  //dvips -o file.eps -E image.dvi
}
