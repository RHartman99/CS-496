
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNPAIR
    | TIMES
    | THEN
    | SETREF
    | SET
    | SEMICOLON
    | RPAREN
    | RBRACE
    | PROC
    | PLUS
    | PAIR
    | NEWREF
    | MINUS
    | LPAREN
    | LETREC
    | LET
    | LBRACE
    | ISZERO
    | INT of (
# 22 "parser.mly"
       (int)
# 29 "parser.ml"
  )
    | IN
    | IF
    | ID of (
# 23 "parser.mly"
       (string)
# 36 "parser.ml"
  )
    | EQUALS
    | EOF
    | END
    | ELSE
    | DIVIDED
    | DEREF
    | DEBUG
    | COMMA
    | BEGIN
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState98
  | MenhirState96
  | MenhirState95
  | MenhirState94
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState87
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState75
  | MenhirState74
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState60
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState50
  | MenhirState48
  | MenhirState47
  | MenhirState42
  | MenhirState41
  | MenhirState39
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState29
  | MenhirState23
  | MenhirState22
  | MenhirState21
  | MenhirState19
  | MenhirState17
  | MenhirState12
  | MenhirState9
  | MenhirState7
  | MenhirState0

# 8 "parser.mly"
  
open Ast

# 122 "parser.ml"

let rec _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv377) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv375) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_expr_) : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__ = 
# 144 "/home/ryan/.opam/system/lib/menhir/standard.mly"
    ( x )
# 144 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv376)) : 'freshtv378)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv381 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv379 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_expr_) : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_expr)), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_expr_ = 
# 231 "/home/ryan/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 161 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv380)) : 'freshtv382)
    | _ ->
        _menhir_fail ()

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | DEBUG ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | DEREF ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | IF ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | ISZERO ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LETREC ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NEWREF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | PAIR ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | PROC ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | SET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | SETREF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | DEBUG ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | DEREF ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | IF ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | ISZERO ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LETREC ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NEWREF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | PAIR ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | PROC ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | SET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | SETREF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | DEBUG ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | DEREF ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | IF ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | INT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | ISZERO ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LETREC ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NEWREF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | PAIR ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | PROC ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | SET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | SETREF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | DEBUG ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | DEREF ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | IF ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | INT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | ISZERO ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LETREC ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NEWREF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | PAIR ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | PROC ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | SET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | SETREF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState50 | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv237 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv233 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState47 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | DEBUG ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | DEREF ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | IF ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | INT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | ISZERO ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LETREC ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | NEWREF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | PAIR ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | PROC ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | SET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | SETREF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv234)
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv235 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_expr_ = 
# 229 "/home/ryan/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 408 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv236)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv238)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv241 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv239 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 143 "parser.mly"
                                  ( Mul(e1,e2) )
# 425 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv240)) : 'freshtv242)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv245 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | UNPAIR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv243 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 141 "parser.mly"
                                 ( Add(e1,e2) )
# 446 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv244)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53) : 'freshtv246)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv249 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv247 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 144 "parser.mly"
                                    ( Div(e1,e2) )
# 463 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv248)) : 'freshtv250)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv253 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | UNPAIR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv251 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 142 "parser.mly"
                                  ( Sub(e1,e2) )
# 484 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv252)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57) : 'freshtv254)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv259 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv257 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState58 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv255 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 140 "parser.mly"
                                    ( Debug(e) )
# 518 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv256)) : 'freshtv258)
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv260)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv265 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv263 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState60 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv261 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 152 "parser.mly"
                                      ( DeRef(e) )
# 554 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv262)) : 'freshtv264)
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv266)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv269 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv267 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState62 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | DEBUG ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | DEREF ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | IF ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | INT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | ISZERO ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LETREC ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | NEWREF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | PAIR ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | PROC ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | SET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | SETREF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv268)
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv270)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv273 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv271 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState64 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | DEBUG ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | DEREF ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | IF ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | INT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | ISZERO ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LETREC ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | NEWREF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | PAIR ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | PROC ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | SET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | SETREF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv272)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv274)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv277 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | UNPAIR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv275 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)), _), _, (e3 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 154 "parser.mly"
                                                      ( ITE(e1,e2,e3) )
# 711 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv276)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66) : 'freshtv278)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv283 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv281 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState67 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv279 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 150 "parser.mly"
                                       ( IsZero(e) )
# 745 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv280)) : 'freshtv282)
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67) : 'freshtv284)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv287 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 759 "parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv285 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 771 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState69 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | DEBUG ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | DEREF ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | IF ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | INT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | ISZERO ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LETREC ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | NEWREF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | PAIR ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | PROC ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | SET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | SETREF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv286)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69) : 'freshtv288)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv291 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 829 "parser.ml"
        ))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | UNPAIR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv289 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 847 "parser.ml"
            ))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 852 "parser.ml"
            ))), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 146 "parser.mly"
                                                    ( Let(x,e1,e2) )
# 860 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv290)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv292)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv295 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 872 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 876 "parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv293 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 888 "parser.ml"
            ))) * (
# 23 "parser.mly"
       (string)
# 892 "parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState72 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | DEBUG ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | DEREF ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | IF ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | INT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | ISZERO ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LETREC ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | NEWREF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | PAIR ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | PROC ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | SET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | SETREF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv294)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv296)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv299 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 950 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 954 "parser.ml"
        )))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | UNPAIR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv297 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 972 "parser.ml"
            ))) * (
# 23 "parser.mly"
       (string)
# 976 "parser.ml"
            )))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 981 "parser.ml"
            ))), (y : (
# 23 "parser.mly"
       (string)
# 985 "parser.ml"
            ))), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 147 "parser.mly"
                                                                               ( Letrec(x,y,e1,e2) )
# 995 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv298)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74) : 'freshtv300)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv305 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv303 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState75 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv301 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), _), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 159 "parser.mly"
                                      ( Sub(Int 0, e) )
# 1029 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv302)) : 'freshtv304)
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75) : 'freshtv306)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv311 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | DEBUG ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | DEREF ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | ID _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | IF ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | INT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | ISZERO ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LETREC ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | NEWREF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PAIR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PROC ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv309 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState77 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv307 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 157 "parser.mly"
                               (e)
# 1090 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)) : 'freshtv310)
        | SET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | SETREF ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77) : 'freshtv312)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv317 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv315 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState79 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv313 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 149 "parser.mly"
                                           ( App(e1,e2) )
# 1131 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)) : 'freshtv316)
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv318)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv323 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv321 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState81 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv319 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 151 "parser.mly"
                                       ( NewRef(e) )
# 1167 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv320)) : 'freshtv322)
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv324)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv327 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv325 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState83 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | DEBUG ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | DEREF ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | IF ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | INT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | ISZERO ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | LET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | LETREC ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | NEWREF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | PAIR ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | PROC ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | SET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | SETREF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv326)
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83) : 'freshtv328)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv333 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv331 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState85 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv329 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 145 "parser.mly"
                                                    ( Pair(e1,e2) )
# 1266 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv330)) : 'freshtv332)
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85) : 'freshtv334)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv339 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 1280 "parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv337 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 1296 "parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState87 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv335 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 1304 "parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 1310 "parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 148 "parser.mly"
                                                             ( Proc(x,e) )
# 1320 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv336)) : 'freshtv338)
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87) : 'freshtv340)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv343 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1334 "parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | UNPAIR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv341 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1352 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 1357 "parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 155 "parser.mly"
                                    ( Set(x,e) )
# 1364 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv342)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89) : 'freshtv344)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv347 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv345 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState90 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | DEBUG ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | DEREF ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | IF ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | INT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | ISZERO ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LETREC ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | NEWREF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | PAIR ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | PROC ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | SET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | SETREF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91) : 'freshtv346)
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv348)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv353 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv351 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState92 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv349 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 153 "parser.mly"
                                                          ( SetRef(e1,e2) )
# 1461 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv350)) : 'freshtv352)
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92) : 'freshtv354)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv357 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 1475 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 1479 "parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv355 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 1491 "parser.ml"
            ))) * (
# 23 "parser.mly"
       (string)
# 1495 "parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState94 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | DEBUG ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | DEREF ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | IF ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | INT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | ISZERO ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | LET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | LETREC ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | NEWREF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | PAIR ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | PROC ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | SET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | SETREF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv356)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94) : 'freshtv358)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv361 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 1553 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 1557 "parser.ml"
        )))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | UNPAIR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((('freshtv359 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 1575 "parser.ml"
            ))) * (
# 23 "parser.mly"
       (string)
# 1579 "parser.ml"
            )))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 1584 "parser.ml"
            ))), (y : (
# 23 "parser.mly"
       (string)
# 1588 "parser.ml"
            ))), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 160 "parser.mly"
                                                                                    ( Unpair(x,y,e1,e2) )
# 1599 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv360)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv362)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv373 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv371 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState98 in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv369 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (_menhir_stack, _menhir_s, (e : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 77 "parser.mly"
       (Ast.prog)
# 1626 "parser.ml"
            ) = 
# 109 "parser.mly"
                 ( AProg e )
# 1630 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv367) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 77 "parser.mly"
       (Ast.prog)
# 1638 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv365) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 77 "parser.mly"
       (Ast.prog)
# 1646 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv363) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 77 "parser.mly"
       (Ast.prog)
# 1654 "parser.ml"
            )) : (
# 77 "parser.mly"
       (Ast.prog)
# 1658 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv364)) : 'freshtv366)) : 'freshtv368)) : 'freshtv370)) : 'freshtv372)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | PLUS ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | TIMES ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98) : 'freshtv374)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv231) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv229) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((xs0 : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__) : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__) = _v in
    ((let _v : 'tv_exprs = let es =
      let xs = xs0 in
      
# 220 "/home/ryan/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 1689 "parser.ml"
      
    in
    
# 165 "parser.mly"
                                            ( es )
# 1695 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv227) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_exprs) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv225 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv221 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv219 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (es : 'tv_exprs)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_expr = 
# 156 "parser.mly"
                             ( BeginEnd(es) )
# 1719 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv220)) : 'freshtv222)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv223 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)) : 'freshtv226)) : 'freshtv228)) : 'freshtv230)) : 'freshtv232)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv113 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 1743 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 1747 "parser.ml"
        )))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv115 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 1756 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 1760 "parser.ml"
        )))) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv117 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 1769 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 1773 "parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv119 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv121 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv123 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv125 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1797 "parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv127 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 1806 "parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv129 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv131 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv133 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv135 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv137 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv139 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv141 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv143 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1850 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 1854 "parser.ml"
        )))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv145 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1863 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 1867 "parser.ml"
        )))) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv147 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1876 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 1880 "parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv149 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1889 "parser.ml"
        ))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv151 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1898 "parser.ml"
        ))) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv153 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1907 "parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv155 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv157 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv159 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv161 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv163 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv165 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv167 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv169 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv171 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv173 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv177 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv179 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv181 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv183 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv191 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv195 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv197 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2021 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv199 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2030 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 2034 "parser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv201 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv203 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv205 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv207 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv209 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2063 "parser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv211 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2072 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv213 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv215 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2086 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 2090 "parser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv218)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv103 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_v : (
# 23 "parser.mly"
       (string)
# 2117 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COMMA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv99 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2128 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ID _v ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv95 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2138 "parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    let (_v : (
# 23 "parser.mly"
       (string)
# 2143 "parser.ml"
                    )) = _v in
                    ((let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | RPAREN ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv91 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2154 "parser.ml"
                        ))) * (
# 23 "parser.mly"
       (string)
# 2158 "parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | EQUALS ->
                            let (_menhir_env : _menhir_env) = _menhir_env in
                            let (_menhir_stack : ((((('freshtv87 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2168 "parser.ml"
                            ))) * (
# 23 "parser.mly"
       (string)
# 2172 "parser.ml"
                            ))) = Obj.magic _menhir_stack in
                            ((let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            match _tok with
                            | BEGIN ->
                                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | DEBUG ->
                                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | DEREF ->
                                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | ID _v ->
                                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
                            | IF ->
                                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | INT _v ->
                                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
                            | ISZERO ->
                                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | LET ->
                                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | LETREC ->
                                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | LPAREN ->
                                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | NEWREF ->
                                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | PAIR ->
                                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | PROC ->
                                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | SET ->
                                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | SETREF ->
                                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | UNPAIR ->
                                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv88)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let (_menhir_env : _menhir_env) = _menhir_env in
                            let (_menhir_stack : ((((('freshtv89 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2220 "parser.ml"
                            ))) * (
# 23 "parser.mly"
       (string)
# 2224 "parser.ml"
                            ))) = Obj.magic _menhir_stack in
                            ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)) : 'freshtv92)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv93 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2235 "parser.ml"
                        ))) * (
# 23 "parser.mly"
       (string)
# 2239 "parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)) : 'freshtv96)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv97 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2250 "parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)) : 'freshtv100)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv101 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2261 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)) : 'freshtv104)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv105 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)) : 'freshtv108)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | DEBUG ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | DEREF ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | ID _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | IF ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | INT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | ISZERO ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | LET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | LETREC ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | NEWREF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | PAIR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | PROC ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | SET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | SETREF ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv84)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 23 "parser.mly"
       (string)
# 2348 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv75 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2359 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | DEBUG ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | DEREF ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | IF ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | INT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | ISZERO ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | LET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | LETREC ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | NEWREF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | PAIR ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | PROC ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | SET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | SETREF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv76)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv77 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2407 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)) : 'freshtv80)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv67 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_v : (
# 23 "parser.mly"
       (string)
# 2437 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv63 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2448 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LBRACE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv59 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2458 "parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | BEGIN ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | DEBUG ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | DEREF ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | ID _v ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
                    | IF ->
                        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | INT _v ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
                    | ISZERO ->
                        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | LET ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | LETREC ->
                        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | LPAREN ->
                        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | NEWREF ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | PAIR ->
                        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | PROC ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | SET ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | SETREF ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | UNPAIR ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17) : 'freshtv60)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv61 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2506 "parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)) : 'freshtv64)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv65 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2517 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)) : 'freshtv68)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv69 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)) : 'freshtv72)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | DEBUG ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | DEREF ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | ID _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | IF ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | INT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | ISZERO ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | LET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | LETREC ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | NEWREF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | PAIR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | PROC ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | SET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | SETREF ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19) : 'freshtv56)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | DEBUG ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | DEREF ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | ID _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | IF ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | INT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | ISZERO ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | LET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | LETREC ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | NEWREF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | PAIR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | PROC ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | SET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | SETREF ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv52)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | DEBUG ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | DEREF ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | IF ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | INT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | ISZERO ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | LET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | LETREC ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | MINUS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState22 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | DEBUG ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | DEREF ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | ID _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | IF ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | INT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | ISZERO ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | LET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | LETREC ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | NEWREF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | PAIR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | PROC ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | SET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | SETREF ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23) : 'freshtv50)
    | NEWREF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | PAIR ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | PROC ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | SET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | SETREF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 23 "parser.mly"
       (string)
# 2747 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv41 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2758 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv37 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2768 "parser.ml"
                ))) = Obj.magic _menhir_stack in
                let (_v : (
# 23 "parser.mly"
       (string)
# 2773 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | RPAREN ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv33 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2784 "parser.ml"
                    ))) * (
# 23 "parser.mly"
       (string)
# 2788 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | EQUALS ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv29 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2798 "parser.ml"
                        ))) * (
# 23 "parser.mly"
       (string)
# 2802 "parser.ml"
                        ))) = Obj.magic _menhir_stack in
                        ((let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | BEGIN ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                        | DEBUG ->
                            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                        | DEREF ->
                            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                        | ID _v ->
                            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
                        | IF ->
                            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                        | INT _v ->
                            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
                        | ISZERO ->
                            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                        | LET ->
                            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                        | LETREC ->
                            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                        | LPAREN ->
                            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                        | NEWREF ->
                            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                        | PAIR ->
                            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                        | PROC ->
                            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                        | SET ->
                            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                        | SETREF ->
                            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                        | UNPAIR ->
                            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv30)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv31 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2850 "parser.ml"
                        ))) * (
# 23 "parser.mly"
       (string)
# 2854 "parser.ml"
                        ))) = Obj.magic _menhir_stack in
                        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)) : 'freshtv34)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv35 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2865 "parser.ml"
                    ))) * (
# 23 "parser.mly"
       (string)
# 2869 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)) : 'freshtv38)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv39 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2880 "parser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)) : 'freshtv42)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv43 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2891 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)) : 'freshtv46)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 23 "parser.mly"
       (string)
# 2915 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv21 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2926 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | DEBUG ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | DEREF ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | IF ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | INT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | ISZERO ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | LET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | LETREC ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | NEWREF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | PAIR ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | PROC ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | SET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | SETREF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv22)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv23 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2974 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)) : 'freshtv26)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | DEBUG ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | DEREF ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | ID _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | IF ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | INT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | ISZERO ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LETREC ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | NEWREF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | PAIR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | PROC ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | SET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | SETREF ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv18)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "parser.mly"
       (int)
# 3045 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 22 "parser.mly"
       (int)
# 3055 "parser.ml"
    )) : (
# 22 "parser.mly"
       (int)
# 3059 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 138 "parser.mly"
              ( Int i )
# 3064 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv16)

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | DEBUG ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | DEREF ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IF ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | INT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | ISZERO ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LETREC ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NEWREF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | PAIR ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | PROC ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | SET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | SETREF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "parser.mly"
       (string)
# 3114 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x : (
# 23 "parser.mly"
       (string)
# 3124 "parser.ml"
    )) : (
# 23 "parser.mly"
       (string)
# 3128 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 139 "parser.mly"
             ( Var x )
# 3133 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv14)

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | DEBUG ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | DEREF ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | ID _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | IF ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | INT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | ISZERO ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LETREC ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | NEWREF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | PAIR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | PROC ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | SET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | SETREF ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39) : 'freshtv10)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | DEBUG ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | DEREF ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | ID _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | IF ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | INT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | ISZERO ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LETREC ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | NEWREF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | PAIR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | PROC ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | SET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | SETREF ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv6)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | DEBUG ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | DEREF ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IF ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | INT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | ISZERO ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LETREC ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NEWREF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | PAIR ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | PROC ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | SET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | SETREF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | END ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState42 in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__ = 
# 142 "/home/ryan/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 3294 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 77 "parser.mly"
       (Ast.prog)
# 3317 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEBUG ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEREF ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ISZERO ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LETREC ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NEWREF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PAIR ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PROC ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SETREF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 233 "/home/ryan/.opam/system/lib/menhir/standard.mly"
  

# 3376 "parser.ml"
