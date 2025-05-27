import {Token,Tokenizer} from "./tokenizer";

class Unit {
    constructor(public kind: string, public val: number) { }
}

const unitScale = {
    m: 1,
    km: 1000,
    s: 1,
    min: 60,
    h: 3600,
}

function baseValue(unit: Unit): number {
    return unitScale[unit.kind] * unit.val;
}
function inferUnitKind(expr: string): string {
    const tokenizer = new Tokenizer(expr);
    const tokens: Token[] = tokenizer.tokenize();
    let unitStack: Token[] = [];

    for (let token of tokens) {
        if (unitStack.length == 0 && (token.kind == "PhysicalUnit" || token.kind == "Constant")) {
            unitStack.push(token)
            continue
        }
        if (token.kind == "PhysicalUnit" || token.kind == "Constant") {
            let op: Token = unitStack.pop();
            let left :Token= unitStack.pop();
            let right :Token= token;


            if (op == null || left == null) {
                console.log("error on op or left")
                break
            }

            if (left.value == "km") left.value = "m"
            if (right.value == "km") right.value = "m"

            if (left.value == "h" || left.value == "min") left.value = "s"
            if (right.value == "h" || right.value == "min") right.value = "s"



            if (!isCompatible(left, op, right)) {
                console.log("comp issue")
                token.value = "error";
                token.kind = "Constant"
                unitStack.push(token)
                break
            }

            if (left.kind == "Constant" && right.kind == "Constant") {
                token.kind = "Constant";
                token.value = eval(`${left.value}${op.value}${right.value}`).toString()
                unitStack.push(token)
                continue
            }

            if (left.value == right.value) {
                token.value = left.value

            }

            else if (op.kind == "OpMulDiv") {
                if (left.kind == "Constant") token.value = right.value
                else if (right.kind == "Constant") token.value = left.value
                else token.value = `${left.value}${op.value}${right.value}`
            } else {
                token.value = "Constant"
            }

            token.kind = "PhysicalUnit"

            token.value = reducedExpr(token.value)
            unitStack.push(token)
        }

        if (token.kind == "OpMulDiv" || token.kind == "OpAddSub") {
            unitStack.push(token)
        }
    }
    return unitStack.pop().value
}


export function reducedExpr(expr: string) :string {
    let tokeniser = new Tokenizer(expr)
    let tokens = tokeniser.tokenize()

    const numerator: Record<string, number> = {};
    const denominator: Record<string, number> = {};

    let operation: "multiply" | "divide" = "multiply";

    for (const token of tokens) {
        if (token.kind === "PhysicalUnit") {
            const unit = token.value;
            if(!['m', 's'].includes(unit)){
                continue
            }

            if (operation === "multiply") {
                numerator[unit] = (numerator[unit] || 0) + 1;
            } else {
                denominator[unit] = (denominator[unit] || 0) + 1;
            }
        } else if (token.kind === "OpMulDiv") {
            operation = token.value === "*" ? "multiply" : "divide";
        }
    }

    for (const unit in numerator) {
        if (denominator[unit]) {
            const minExp = Math.min(numerator[unit], denominator[unit]);
            numerator[unit] -= minExp;
            denominator[unit] -= minExp;

            if (numerator[unit] === 0) delete numerator[unit];
            if (denominator[unit] === 0) delete denominator[unit];
        }
    }

    const numeratorStr:string = Object.keys(numerator).join(" * ") || "1";
    const denominatorStr:string = Object.keys(denominator).join(" * ");

    return denominatorStr ? `${numeratorStr} / ${denominatorStr}` : numeratorStr;
}


function isCompatible(left: Token, op: Token, right: Token): boolean {

    //console.log(left.value + " " + op.value + " " + right.value)
    if (left.kind == "Constant" || right.kind == "Constant") {
        return true
    }

    if (op.kind == "OpMulDiv" && left.value != right.value) {
        if(op.value == "/"){
            if(left.value == "m" && right.value == "s")   {
                return true
            }
        }else {
            return true
        }
    }

    if (op.kind == "OpAddSub") {
        if (left.value == "m" && right.value == "m") {
            return true
        }

        if (left.value == "s" && right.value == "s") {
            return true
        }
    }
    return false
}
