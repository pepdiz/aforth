Red [author: Pep Diz]

afm: make object! [
; forth abstract machine
; conceptual forth machine where all forth primitives are programmed in red code rather than assembler
; conceptually is a STC (subroutined threaded code)
; there's no need for hardware register, only structures are PSP, RSP, TIB and DICT (no mem!)
; this also means all accesible user memory is in DICT
; this is a conceptual forth machine, it doesn't work like a real forth 
; ie, a real forth will stack a LIT word to handle a number while this machine stacks the number directly
;
; design:
;
;  dict is a map of forth words, each dict entry has a key (word name) and a value (word body)
;  word body is a block with two values, first a fn-class (a red function to do the type of word), there're two classes: colon words and code words, and thus two kind of functions (docolon and donative)
;  the second value in the body is a block with all words defining the word in case of colon words, or Red code (Red words) in case of native words (programmers coding native words must be aware of afm design)
;
;	dict: map! [
;	"fn-name" ['fn-class [body ...]]
;	...
;	]
;
;  fn-name, fn-class and body symbols are all lit-words  (it could be strings too, but let's go with symbols better)
;  do to red symbols limitations, fn-name and body symbols are currently strings while fn-class is a red symbol (lit-word)
;
; ej:
; dict: map! [
;  "dup"  ['donative [insert sp sp/1]]
;  "swap" ['donative [a: sp/1 b: sp/2 remove/part sp 2 insert sp a insert sp b ]]
; ]


;  to evaluate a forth word is to apply fn-class to its body

;  how to find a word in dictionary?  just accesing the map using the word name (the map key)! 
; implications of design for forth words using words or defining words:  just to see

; FEATURES
; - only float numbers, every number is stacked as a float

; CHANGES TODO:
; - handle different types of numbers, at least integers and floats in order to avoid inexactitude and allow accurate comparisions

; TODO:
; - colon words 
   	
; dictionary (initialised with native words)
  dict: make map! [
 "." ['donative [print sp/1 remove sp]]
 ".s" ['donative [print ["[" sp "]"]]]
 "*" ['donative [a: sp/1 b: sp/2 remove/part sp 2 insert sp (a * b)]]
 "/" ['donative [a: sp/1 b: sp/2 remove/part sp 2 insert sp b / a]]
 "+" ['donative [a: sp/1 b: sp/2 remove/part sp 2 insert sp a + b]]
 "-" ['donative [a: sp/1 b: sp/2 remove/part sp 2 insert sp b - a]]                                                                             
 "^" ['donative [a: sp/1 b: sp/2 remove/part sp 2 insert sp power b a]]
 "sqrt" ['donative [a: sp/1 remove sp insert sp sqrt a]]
 "and" ['donative [a: sp/1 b: sp/2 remove/part sp 2 insert sp a and b]] 
 "or" ['donative [a: sp/1 b: sp/2 remove/part sp 2 insert sp a or b]] 
 "xor" ['donative [a: sp/1 b: sp/2 remove/part sp 2 insert sp a xor b]] 
 "not" ['donative [a: sp/1 remove sp insert sp not a]]   
 "drop" ['donative [remove sp]]
 "dup" ['donative [insert sp sp/1]]
 "over" ['donative [insert sp sp/2]]
 "swap" ['donative [a: p/1 b: p/2 remove/part sp 2 insert sp a insert sp b]]
 "words" ['donative [keys-of dict]]  
 ">r" ['donative [insert rp sp/1 remove sp]]
 "r>" ['donative [insert sp rp/1 remove rp]]
  
  ]
   	
; parameter stack
    sp: copy []
    
; return stack
    rp: copy []    	

; TIB
    tib: copy []
	
; machine state, false: run mode, true: compile mode
  state: false	
	
; forth primitives
    next: does []
    dolist: does []
    exit: does []
        
; REPL, really in forth is REL (read-eval-loop) , no print
    read: does [ tib: copy [] append tib split trim/lines ask "afm> " " " ]
    rel: does [ forever [rp: copy [] read interpret]]
    interpret: does [eval tib]	; interpret is outer interpreter

    compile: function [w [any-word!] t [block!]]
    ; this function is the compile environment, it consumes tokens compiling them until ; is found (; is consumed too marking the exit of function)          
          f: func [b /local c] [either tail? b [[]] [either ";" = first b [[]] [append reduce [first b] f next b ] ]]
          if none = find t ";" [throw "end of definition [;] expected"] 
          if none <> find t ":" [throw "define a new word inside a word definition is not allowed"]
          c: f t 
          extend dict reduce [to-lit-word w 'docolon c]
          (length? c) + 1  ; hay que contar w (pero no el ; final que se lo come el while de eval)
    ]
   
    ; words should be a list of words, thus Red symbols (lit-words), sadly not all legal forth symbols are legal Red symbols (i.e. word :)
    ; so we have to use strings rather than symbols (alternatively we can prohibit traditional forth symbols to use alternative words, define in place of : and so on)
    eval: function [b [block!]] [  ; b is a block of strings (the words of the current word)
             while [not tail? b] [
                  t: first b  ; t is token   
                  if ":" = t [b: next b b: skip b compile first b next b]
                  ; word is number? push it, not number? guess what word
                  either number? attempt [n: to-float t] 
                     [insert sp n]
                     ; ejecutar un word es do reduce [to-word first (select dict w) second (select dict w)]  ;  apply dict/w/1 dict/w/2
                     [if none = attempt [w: select dict t do reduce [to-word first w second w]] [throw "word not found"]]
                  t: next t
                ]
             ]

    docolon: function [b [block!]] [ eval b ] ; just eval the word list (the block)    
    donative: function [b [block!] [ f: function [] b f ]   ; to do the block first define a function doing the block (because use of local variables in the block)
     

]
