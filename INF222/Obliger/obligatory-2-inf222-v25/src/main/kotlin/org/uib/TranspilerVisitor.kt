package org.uib

import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.body.VariableDeclarator
import com.github.javaparser.ast.expr.*
import com.github.javaparser.ast.stmt.ExpressionStmt
import com.github.javaparser.ast.visitor.VoidVisitorAdapter
import kotlin.jvm.optionals.getOrNull

class TranspilerVisitor(private val varToUnit: Map<String, String>) : VoidVisitorAdapter<StringBuilder>() {

    override fun visit(n: ClassOrInterfaceDeclaration, arg: StringBuilder) {
        arg.append("class ")
            .append(n.name)
            .append("{\n")
        super.visit(n, arg)
        arg.append("}\n")
        arg.append("new Example().main()")
    }

    override fun visit(n: ExpressionStmt, arg: StringBuilder) {
        // Need to use accept because n.expression is an Abstract type
        n.expression.accept(this, arg)
    }

    override fun visit(n: MethodDeclaration, arg: StringBuilder) {
        if (n.nameAsString.equals("main")) {
            arg.append("main() : ${n.type} {\n")
            super.visit(n, arg)
            arg.append("};\n")
        } else {
            arg.append("function ${n.name}(){\n")
            super.visit(n, arg)
            arg.append("};\n")
        }
    }

    override fun visit(n: VariableDeclarator, arg: StringBuilder) {
        if (n.comment.getOrNull() != null && n.initializer.getOrNull() != null) {
            arg.append("let ${n.name} : Unit = new Unit(\"${varToUnit[n.name.toString()]}\", ${n.initializer.get()});\n")
        } else {
            arg.append("let ${n.nameAsString} = ${n.initializer.getOrNull()};\n")
        }
    }

    override fun visit(n: BinaryExpr, arg: StringBuilder) {
        arg.append("(")
        n.left.accept(this, arg)
        arg.append(" ${n.operator.asString()} ")
        n.right.accept(this, arg)
        arg.append(")")
    }

    override fun visit(n: IntegerLiteralExpr, arg: StringBuilder) {
        arg.append(n)
    }

    override fun visit(n: NameExpr, arg: StringBuilder) {
        if(varToUnit.contains(n.toString())){
            arg.append("baseValue($n)")
        }else{
            arg.append(n)
        }

    }

    override fun visit(n: MethodCallExpr, arg: StringBuilder) {
        if (n.name.toString() == "println" && n.getArgument(0).toString().contains(Regex("[+-/*]"))) {
            if(varToUnit.keys.any{n.arguments[0].toString().contains(it)}){
                arg.append("console.log(\n new Unit(inferUnitKind(\"")
                var argument = n.arguments[0].toString().filter { !")(".contains(it) }
                argument = if (varToUnit.contains(argument.split(" ")[0])) argument else argument.subSequence(4,argument.length ).toString()
                argument.split(" ").forEach { expr ->
                    if(varToUnit.contains(expr.filter { it.isLetter() })){
                        arg.append("${varToUnit[expr.filter { it.isLetter() }]} ")
                    }else if ("+-*/".contains(expr)){
                        arg.append("$expr ")
                    }else{
                        while ("+-*/ ".contains(arg[arg.length - 1])){
                            arg.setLength(arg.length - 1)
                        }
                        arg.append(" ")
                    }
                }
                arg.setLength(arg.length - 1)
                arg.append("\"),")
            }else{
                arg.append("console.log(\n")
            }

            n.arguments[0].accept(this, arg)
            if(varToUnit.keys.any{n.arguments[0].toString().contains(it)}){
                arg.append(")")
            }
            arg.append("\n)\n")
        }
    }
}