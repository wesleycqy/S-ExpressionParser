# Wesley Chong
# Chapter 24 - 25 Assignment Part 1 & 2
# Dr David Owen




import s-exp as S
import lists as L

data Token:
  | lPar    # (
  | rPar    # )
  | op(s)   # + or *
  | num(n)  # [0-9]+, i.e., one or more digits
end

data ArithC:
  | numC(n :: Number)
  | plusC(l :: ArithC, r :: ArithC)
  | multC(l :: ArithC, r :: ArithC)
end


fun oni(chars :: List<String>) -> List<String>:
  cases (List) chars: 
    | empty => raise("onion-number called with empty list")
    | link(f, r) =>
      cases (List) r:
        | empty => chars 
        | link(ff, rr) =>  
          if is-some(string-to-number(ff)): # if there is a next chracter
            oni(link(f + ff, rr))  # join first and next
          else:  
            chars
          end
      end
  end
  #where:
  #oni([list: "1", "2"]) is [list: "12"]
end


fun onion-scanner(chars :: List<String>) -> List<Token>: 
  cases (List) chars:
    | empty => empty
    | link(f, r) =>
      if f == "(": # handles open parantheses
        link(lPar, onion-scanner(r))
      else if f == ")": # handles close parantheses
        link(rPar, onion-scanner(r))
      else if f == " ":
        onion-scanner(r)
      else if ((f == "+") or (f == "*")): # handles + and * operations
        link(op(f), onion-scanner(r))
      else: # handles numbers
        block:
          a = string-to-number(f)
        cases (Option) a:
            | none => raise(f + " is not a number.")
            | some(n) =>  link(num(string-to-number(oni(chars).first).value), onion-scanner(oni(chars).rest))
          end

        end
      end
  end

  #where:
  #onion-scanner(string-explode("+ 2 2")) is [list: op("+"), num(2), num(2)]
end



var T = empty

fun onion():  # <onion> ...

  cases (List) T:
    | empty => empty
    | link(f, r) =>
      cases (Token) f:
        | lPar => 
          block:
            T := T.rest #removes open parantheses
            onion()
          end
        | rPar =>
          block:
            T := T.rest #removes close parantheses
            onion()
          end
        | op(s) => 
          block:
            T := T.rest
            opFirstValue = onion() # gets first value of an operation 
            T := T.rest
            opSecondValue = onion() # gets second value of an operation 
            [list: f, opFirstValue , opSecondValue]
          end
        | num(n) => num(n)
          end
      end
end


fun parse(tokens :: List<Token>):
  block:
    T := tokens
    onion()
  end
end


fun parse-arith(s) -> ArithC:
  block:
    cases (Any) s:

      | num(n) => numC(n)

      | link(f, r) => 
        cases (List) s:
          | empty => raise("parse: unexpected empty list")
          | link(ff, rr) =>
            argL = L.get(rr, 0)
            argR = L.get(rr, 1)
            if ff.s == "+":
              plusC(parse-arith(argL), parse-arith(argR))
            else if ff.s == "*":
              multC(parse-arith(argL), parse-arith(argR))
            end
        end
        
      | empty => raise("parse: empty number or list")

      | else => raise("parse: not number or list")

    end
  end
  #where:
  #parse-arith(parse(onion-scanner(string-explode("3")))) is numC(3)
  #parse-arith(parse(onion-scanner(string-explode("(+ 1 2)")))) is plusC(numC(1), numC(2))
  #parse-arith(parse(onion-scanner(string-explode("(* (+ 1 2) (* 2 5))")))) is multC(plusC(numC(1), numC(2)), multC(numC(2), numC(5)))
end


fun interp(e :: ArithC) -> Number:
  cases (ArithC) e:
    | numC(n) => n
    | plusC(l, r) => interp(l) + interp(r)
    | multC(l, r) => interp(l) * interp(r)
  end
end



"Part 3: "
onion-scanner(string-explode("12"))
onion-scanner(string-explode("(+ 3 4)"))
onion-scanner(string-explode("(+ (* 2 3) 45)"))

parse(onion-scanner(string-explode("12")))
parse(onion-scanner(string-explode("(+ 3 4)")))
parse(onion-scanner(string-explode("(+ (* 2 3) 45)")))




"Part 4: "
parse-arith(parse(onion-scanner(string-explode("12"))))
parse-arith(parse(onion-scanner(string-explode("(+ 3 4)"))))
parse-arith(parse(onion-scanner(string-explode("(+ (* 2 3) 45)"))))

interp(parse-arith(parse(onion-scanner(string-explode("12")))))
interp(parse-arith(parse(onion-scanner(string-explode("(+ 3 4)")))))
interp(parse-arith(parse(onion-scanner(string-explode("(+ (* 2 3) 45)")))))
