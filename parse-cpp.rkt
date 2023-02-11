;; ydiff - a language-aware tool for comparing programs
;; Copyright (C) 2011-2013 Yin Wang (yinwang0@gmail.com)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


#lang racket

(require "structs.rkt")
(require "utils.rkt")
(require "parsec.rkt")

(provide parse-cpp)




;-------------------------------------------------------------
;                     scanner settings
;-------------------------------------------------------------

(define set-parameters
  (lambda ()
    (set-delims (list "("  ")"  "["  "]"  "{"  "}" ","  "`"  ";" "#"))

    (set-operators
     (list
      "<<="  ">>="   "->*"  "..."
      "&&"  "||"  ">>"  "<<"  "++"  "--"
      "=="  "!="  ">="  "<="  "+="  "-="  "*="  "/="  "^="  "&="  "|="
      "->"  ".*"  "::"
      "="  "+"  "-"  "*"  "/"  "%"  "<"  ">"  "!"  ":"  "?"  "."
      "^"  "|"  "&"  "~"
      ))

    (set-line-comment  (list "//"))
    (set-comment-start  "/*")
    (set-comment-end    "*/")
    (set-quotation-marks  '(#\" #\'))
    (set-significant-whitespaces
     (list #\newline #\linefeed #\u2028 #\u2029))))




;-------------------------------------------------------------
;                          parsers
;-------------------------------------------------------------

;; literals
(:: $id
    ($pred
     (lambda (t)
       (and (token? t)
            (id? (Node-elts t))))))


(::= $identifier 'identifier
     (@? ($$ "::"))
     (@* $id (@* $type-parameter) ($$ "::"))
     (@= 'id (@? ($$ "~")) $id))


;; (::= $identifier 'identifier
;;      (@? ($$ "::")) $scope-resolution (@? ($$ "~")) $id)


(:: $numeral-literal
    ($pred
     (lambda (t)
       (and (token? t)
            (numeral? (Node-elts t))))))

(:: $char-literal ($pred character?))
(:: $string-literal ($pred str?))
(:: $newline ($pred newline?))
(:: $comment ($pred comment?))


;; delimeters
(::  |,|   (@_ ","))
(::  |;|   (@~ ";"))
(::  |:|   (@_ ":"))
(::  |(|   (@~ "("))
(::  |)|   (@~ ")"))
(::  |[|   (@~ "["))
(::  |]|   (@~ "]"))
(::  |{|   (@~ "{"))
(::  |}|   (@~ "}"))

(::  |\n|  ($glob^ (@*^ $newline)))
(::  |;\n| (@or |;| |\n|))


(define old-seq @seq)

(set-seq
  (lambda ps
    (let ([psj (join ps |\n|)])
      (apply old-seq `(,|\n| ,@psj ,|\n|)))))



;; a hacky definition for macros
;; will fix later
(::= $macro-defintion 'macro
     (@~ "#")
     (@*^ (old-seq (@*^ (@and (@!^ ($$ "\\")) (@!^ $newline))) ($$ "\\") (@*^ $newline)))
     (old-seq (@*^ (@!^ $newline)) ($glob^ $newline) ($glob^ (@*^ $newline)))
)


(:: $directive
    (@or ($$ "ifdef")
         ($$ "define")
         ($$ "undef")
         ($$ "endif")))


;;------------------ starting point -----------------
(::= $program 'program
     (@* $statement)
)



(:: $statement
    (@or $macro-defintion
         $empty-statement
         $access-label
         $statement-block

         $if-statement
         $switch-statement
         $do-while-statement
         $while-statement
         $for-statement
         $continue-statement
         $break-statement

         $return-statement
         $labelled-statement
         $try-statement

         $namespace-definition
         $using-namespace

         $class-definition
         $function-definition
         $function-declaration
         $variable-definition
         $enum-declaration

         $extended-assembly
         $inline-assembly

         $expression-statement
))


(:: $empty-statement |;|)


(::= $enum-declaration 'enum
     (@~ "enum") (@? $identifier)
     |{|
       (@? (@.@  (@= 'name-value $identifier  (@? $initializer))  |,|))
     |}|
     |;|
)


(::= $access-label 'access-label
     $access-specifier (@~ ":"))


(::= $statement-block 'block
     |{|  (@* $statement)  |}|
)


(::= $namespace-definition 'namespace
     ($$ "namespace") $identifier
     |{|  (@* $statement)  |}|
)

(::= $using-namespace 'using-namespace
     ($$ "using") ($$ "namespace") $identifier)



;;--------------------------------------------
(::= $class-definition 'class

     (@or ($$ "class")
          ($$ "struct")
          ($$ "union"))

     (@* (@= 'declspec
             (@or ($$ "_declspec") ($$ "__declspec")) |(|  $expression  |)|))

     (@or (@= 'name $identifier |;| )

          (@...
           (@= 'name (@? $identifier)) (@? (@... (@_ ":") $base-clause))
           (@= 'body  |{|  (@* $statement)  |}|) )
          ))


(::= $base-clause 'bases
     (@.@ $base-specifier |,|)
)


(::= $base-specifier 'base-specifier
     (@? $access-specifier) $identifier)


(::= $access-specifier 'access-specifier
     (@or ($$ "public")
          ($$ "protected")
          ($$ "private")
          ($$ "virtual")))


;;---------- function definition and declaration ------------

(::= $function-declaration 'function-declaration
     (@? ($$ "typedef"))
     (@? $access-specifier) (@? $modifiers) (@? $type)
     (@= 'name (@or $identifier
                    (@... |(| ($$ "*") $identifier |)|)) )
     $formal-parameter-list
     (@? ($$ "const"))
     (@? $initializer)
)


(::= $function-definition 'function
     (@or (@... (@? $modifiers) $type
                (@= 'name $identifier ) $formal-parameter-list)

          (@... (@= 'name $identifier ) $formal-parameter-list))
     (@? $initializer)
     $function-body)


(::= $type 'type
     (@? $modifiers) (@or $primitive-type
                          $ctype
                          $identifier)
     (@* $type-parameter) (@* $ptr-suffix))


(::= $type-parameter 'type-parameter
     (@~ "<") (@.@ (@or $type $numeral-literal) |,|) (@~ ">"))


(::= $ptr-suffix 'suffix
     (@or ($$ "*")
          ($$ "&")))


(::= $formal-parameter-list 'parameters
     |(|  (@? (@.@ $type-variable |,|)) (@? |,| ($$ "..."))  |)|
)


(::= $type-variable 'type-variable
     (@? $modifiers) $type (@? $identifier) (@? $array-suffix))


(::= $function-body 'body
     |{|  (@* $statement)  |}|
)



(::= $variable-definition 'variable-definition
     $variable-declaration-list |;|
)


(:: $variable-declaration-list
    (@... (@? $modifiers) $type (@.@ $variable-declaration |,|)))


(::= $variable-declaration 'variable-declaration
     $identifier (@? $variable-declaration-suffix)
     (@? $initializer))


(::= $modifiers 'modifiers
     (@+ (@or ($$ "const")
              ($$ "static")
              ($$ "inline"))))

(:: $primitive-type
     (@or (@...
           (@or ($$ "signed")
                ($$ "unsigned"))
           (@or ($$ "int")
                ($$ "char")
                ($$ "long")
                ($$ "double")
                ($$ "float")))
          (@or ($$ "signed")
               ($$ "unsigned"))))

(::= $ctype 'ctype
     (@or ($$ "struct")
          ($$ "enum"))
     $identifier
)


(::= $variable-declaration-suffix 'suffix
     (@or (@... |[|  $expression  |]|))
)

(::= $initializer 'initializer
     (@or (@... (@_ "=") $expression)
          (@... (@_ ":") $expression)
          (@... (@_ "(") $expression (@_ ")"))))




(::= $if-statement 'if
     ($$ "if")  (@= 'test |(| $expression |)|) $statement
     (@? (@= 'else ($$ "else") $statement))
)



(::= $do-while-statement 'do-while
    ($$ "do") $statement
    (@= 'while-do ($$ "while")  (@= 'test |(| $expression |)|   ))
    |;|
)


(::= $while-statement 'while
     ($$ "while")  (@= 'test |(| $expression |)|   )
     $statement
)


(::= $for-statement 'for
     ($$ "for") (@= 'iter
                    |(| (@? $for-initaliser) |;|
                    (@? $expression)     |;|
                    (@? $expression)
                    |)|
                    )
     $statement
)


(::= $for-initaliser 'for-initializer
     (@or (@= 'variable-declaration
              $variable-declaration-list)

          $expression
))


(::= $continue-statement 'continue
     ($$ "continue") (@= 'label (@? $identifier)) |;|
)


(::= $break-statement 'break
     ($$ "break") (@= 'label (@? $identifier)) |;|
)


(::= $return-statement 'return
     ($$ "return") (@= 'value (@? $expression)) |;|
)



(::= $labelled-statement 'labelled-statement
    