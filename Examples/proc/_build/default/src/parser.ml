
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TIMES
    | THEN
    | SND
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
    | FST
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
  | MenhirState96
  | MenhirState93
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState88
  | MenhirState86
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState80
  | MenhirState78
  | MenhirState76
  | MenhirState74
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState59
  | MenhirState57
  | MenhirState55
  | MenhirState54
  | MenhirState53
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState47
  | MenhirState45
  | MenhirState44
  | MenhirState39
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState31
  | MenhirState29
  | MenhirState27
  | MenhirState24
  | MenhirState18
  | MenhirState17
  | MenhirState16
  | MenhirState14
  | MenhirState12
  | MenhirState7
  | MenhirState4
  | MenhirState2
  | MenhirState0

# 8 "parser.mly"
  
open Ast

# 123 "parser.ml"

let rec _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv365) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv363) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_expr_) : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__ = 
# 144 "/home/ryan/.opam/system/lib/menhir/standard.mly"
    ( x )
# 145 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv364)) : 'freshtv366)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv369 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv367 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_expr_) : 'tv_separated_nonempty_list_SEMICOLON_expr_) = _v in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_expr)), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_expr_ = 
# 231 "/home/ryan/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 162 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv368)) : 'freshtv370)
    | _ ->
        _menhir_fail ()

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | DEBUG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | FST ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | ISZERO ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LETREC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | PAIR ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | SND ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | DEBUG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FST ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | ISZERO ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LETREC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | PAIR ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | SND ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | DEBUG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | FST ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | ISZERO ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LETREC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | PAIR ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | SND ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | DEBUG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FST ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | ISZERO ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LETREC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | PAIR ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SND ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState47 | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv221 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv217 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState44 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | DEBUG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | DEREF ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | FST ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | ID _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | ISZERO ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | LET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | LETREC ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | PAIR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | SND ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv218)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv219 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_expr_ = 
# 229 "/home/ryan/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 419 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv220)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44) : 'freshtv222)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv225 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv223 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 144 "parser.mly"
                                  ( Mul(e1,e2) )
# 436 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv224)) : 'freshtv226)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv229 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FST | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | SND | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv227 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 142 "parser.mly"
                                 ( Add(e1,e2) )
# 457 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv228)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv230)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv233 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv231 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 145 "parser.mly"
                                    ( Div(e1,e2) )
# 474 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv232)) : 'freshtv234)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv237 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FST | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | SND | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv235 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 143 "parser.mly"
                                  ( Sub(e1,e2) )
# 495 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv236)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54) : 'freshtv238)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv243 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv241 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState55 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv239 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 141 "parser.mly"
                                   ( Debug(e) )
# 529 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv240)) : 'freshtv242)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55) : 'freshtv244)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv249 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv247 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState57 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv245 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 155 "parser.mly"
                                      ( DeRef(e) )
# 565 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv246)) : 'freshtv248)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57) : 'freshtv250)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv255 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv253 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState59 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv251 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 147 "parser.mly"
                                  ( Fst(e) )
# 601 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv252)) : 'freshtv254)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59) : 'freshtv256)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv259 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv257 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState61 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | DEBUG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | DEREF ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | FST ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | ID _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | ISZERO ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | LET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | LETREC ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | PAIR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | SND ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv258)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv260)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv263 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv261 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState63 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | DEBUG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | DEREF ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | FST ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | ID _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | ISZERO ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | LET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | LETREC ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | PAIR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | SND ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv262)
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv264)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv267 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FST | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | SND | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv265 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)), _), _, (e3 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 157 "parser.mly"
                                                      ( ITE(e1,e2,e3) )
# 762 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv266)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv268)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv273 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv271 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState66 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv269 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 153 "parser.mly"
                                       ( IsZero(e) )
# 796 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)) : 'freshtv272)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66) : 'freshtv274)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv277 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 810 "parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv275 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 822 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState68 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | DEBUG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | DEREF ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | FST ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | ID _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
            | ISZERO ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | LET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | LETREC ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | PAIR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | SND ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69) : 'freshtv276)
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv278)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv281 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 882 "parser.ml"
        ))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FST | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | SND | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv279 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 900 "parser.ml"
            ))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 905 "parser.ml"
            ))), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 149 "parser.mly"
                                                    ( Let(x,e1,e2) )
# 913 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv280)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv282)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv285 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 925 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 929 "parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv283 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 941 "parser.ml"
            ))) * (
# 23 "parser.mly"
       (string)
# 945 "parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState71 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | DEBUG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | DEREF ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | FST ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | ID _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | ISZERO ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LETREC ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | PAIR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | SND ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv284)
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv286)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv289 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1005 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 1009 "parser.ml"
        )))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FST | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | SND | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv287 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1027 "parser.ml"
            ))) * (
# 23 "parser.mly"
       (string)
# 1031 "parser.ml"
            )))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 1036 "parser.ml"
            ))), (y : (
# 23 "parser.mly"
       (string)
# 1040 "parser.ml"
            ))), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 150 "parser.mly"
                                                                               ( Letrec(x,y,e1,e2) )
# 1050 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv288)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv290)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv295 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv293 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState74 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv291 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), _), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 162 "parser.mly"
                                      ( Sub(Int 0, e) )
# 1084 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv292)) : 'freshtv294)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74) : 'freshtv296)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv301 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DEBUG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DEREF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | FST ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | ID _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | INT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | ISZERO ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LETREC ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PAIR ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv299 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState76 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv297 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 160 "parser.mly"
                               (e)
# 1147 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv298)) : 'freshtv300)
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | SND ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv302)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv307 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv305 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState78 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv303 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 152 "parser.mly"
                                           ( App(e1,e2) )
# 1188 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv304)) : 'freshtv306)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv308)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv313 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv311 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState80 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv309 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 154 "parser.mly"
                                       ( NewRef(e) )
# 1224 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv310)) : 'freshtv312)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80) : 'freshtv314)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv317 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv315 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState82 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | DEBUG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | DEREF ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | FST ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | ID _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | ISZERO ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | LET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | LETREC ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | PAIR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | SND ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83) : 'freshtv316)
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82) : 'freshtv318)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv323 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv321 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState84 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv319 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 146 "parser.mly"
                                                    ( Pair(e1,e2) )
# 1325 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv320)) : 'freshtv322)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv324)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv329 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 1339 "parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv327 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 1355 "parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState86 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv325 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 1363 "parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 1369 "parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 151 "parser.mly"
                                                             ( Proc(x,e) )
# 1379 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv326)) : 'freshtv328)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv330)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv333 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1393 "parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FST | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | SND | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv331 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1411 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 1416 "parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 158 "parser.mly"
                                    ( Set(x,e) )
# 1423 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv332)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88) : 'freshtv334)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv337 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv335 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState89 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | DEBUG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | DEREF ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | FST ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | ID _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | ISZERO ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LETREC ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | PAIR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | SND ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv336)
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89) : 'freshtv338)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv343 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv341 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState91 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv339 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((((_menhir_stack, _menhir_s), _, (e1 : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 156 "parser.mly"
                                                          ( SetRef(e1,e2) )
# 1522 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv340)) : 'freshtv342)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91) : 'freshtv344)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv349 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv347 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState93 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv345 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 148 "parser.mly"
                                  ( Snd(e) )
# 1558 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv346)) : 'freshtv348)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv350)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv361 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv359 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState96 in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv357 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (_menhir_stack, _menhir_s, (e : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 78 "parser.mly"
       (Ast.prog)
# 1587 "parser.ml"
            ) = 
# 110 "parser.mly"
                 ( AProg e )
# 1591 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv355) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 78 "parser.mly"
       (Ast.prog)
# 1599 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv353) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 78 "parser.mly"
       (Ast.prog)
# 1607 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv351) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 78 "parser.mly"
       (Ast.prog)
# 1615 "parser.ml"
            )) : (
# 78 "parser.mly"
       (Ast.prog)
# 1619 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv352)) : 'freshtv354)) : 'freshtv356)) : 'freshtv358)) : 'freshtv360)
        | MINUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv362)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv215) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv213) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((xs0 : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__) : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__) = _v in
    ((let _v : 'tv_exprs = let es =
      let xs = xs0 in
      
# 220 "/home/ryan/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 1650 "parser.ml"
      
    in
    
# 166 "parser.mly"
                                            ( es )
# 1656 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv211) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_exprs) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv209 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv205 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv203 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (es : 'tv_exprs)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_expr = 
# 159 "parser.mly"
                             ( BeginEnd(es) )
# 1680 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv204)) : 'freshtv206)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv207 * _menhir_state) * _menhir_state * 'tv_exprs) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)) : 'freshtv210)) : 'freshtv212)) : 'freshtv214)) : 'freshtv216)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv97 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv99 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv101 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv103 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv105 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1724 "parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv107 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 1733 "parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv109 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv111 * _menhir_state)) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv113 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv115 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv117 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv119 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv121 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv123 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1777 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 1781 "parser.ml"
        )))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv125 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1790 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 1794 "parser.ml"
        )))) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv127 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1803 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 1807 "parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv129 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1816 "parser.ml"
        ))) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv131 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1825 "parser.ml"
        ))) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv133 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1834 "parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv135 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv137 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv139 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv141 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv143 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv145 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv147 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv149 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv151 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv153 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv155 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv157 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv159 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv161 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv163 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv165 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv171 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv173 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv179 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv181 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1958 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv183 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 1967 "parser.ml"
        ))) * (
# 23 "parser.mly"
       (string)
# 1971 "parser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv185 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv191 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv193 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2000 "parser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv195 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2009 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv197 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv199 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv202)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DEBUG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DEREF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | FST ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | ID _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | INT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | ISZERO ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LETREC ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | PAIR ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SND ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2) : 'freshtv92)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | DEBUG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | DEREF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | FST ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | ID _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | INT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | ISZERO ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | LETREC ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | PAIR ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | SND ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv88)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 23 "parser.mly"
       (string)
# 2156 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv79 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2167 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | DEBUG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | DEREF ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | FST ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | ID _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
            | ISZERO ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | LET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | LETREC ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | PAIR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | SND ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv80)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv81 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2217 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)) : 'freshtv84)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv71 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_v : (
# 23 "parser.mly"
       (string)
# 2247 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv67 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2258 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LBRACE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv63 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2268 "parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | BEGIN ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                    | DEBUG ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                    | DEREF ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                    | FST ->
                        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                    | ID _v ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
                    | IF ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                    | INT _v ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
                    | ISZERO ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                    | LET ->
                        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                    | LETREC ->
                        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                    | LPAREN ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                    | NEWREF ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                    | PAIR ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                    | PROC ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                    | SET ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                    | SETREF ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                    | SND ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv64)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv65 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2318 "parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)) : 'freshtv68)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv69 * _menhir_state)) * (
# 23 "parser.mly"
       (string)
# 2329 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)) : 'freshtv72)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv73 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)) : 'freshtv76)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | DEBUG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | DEREF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | FST ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | ID _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | INT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | ISZERO ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | LETREC ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | PAIR ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | SND ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv60)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | DEBUG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | DEREF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | FST ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | ID _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | INT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | ISZERO ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LETREC ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | PAIR ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | SND ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16) : 'freshtv56)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | DEBUG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | FST ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | ISZERO ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LETREC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | MINUS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState17 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | DEBUG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | DEREF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | FST ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | ID _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | INT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | ISZERO ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | LETREC ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | PAIR ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | SND ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv54)
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | PAIR ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | SND ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 23 "parser.mly"
       (string)
# 2567 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv45 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2578 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv41 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2588 "parser.ml"
                ))) = Obj.magic _menhir_stack in
                let (_v : (
# 23 "parser.mly"
       (string)
# 2593 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | RPAREN ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv37 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2604 "parser.ml"
                    ))) * (
# 23 "parser.mly"
       (string)
# 2608 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | EQUALS ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv33 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2618 "parser.ml"
                        ))) * (
# 23 "parser.mly"
       (string)
# 2622 "parser.ml"
                        ))) = Obj.magic _menhir_stack in
                        ((let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | BEGIN ->
                            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                        | DEBUG ->
                            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                        | DEREF ->
                            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                        | FST ->
                            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                        | ID _v ->
                            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
                        | IF ->
                            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                        | INT _v ->
                            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
                        | ISZERO ->
                            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                        | LET ->
                            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                        | LETREC ->
                            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                        | LPAREN ->
                            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                        | NEWREF ->
                            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                        | PAIR ->
                            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                        | PROC ->
                            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                        | SET ->
                            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                        | SETREF ->
                            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                        | SND ->
                            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24) : 'freshtv34)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv35 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2672 "parser.ml"
                        ))) * (
# 23 "parser.mly"
       (string)
# 2676 "parser.ml"
                        ))) = Obj.magic _menhir_stack in
                        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)) : 'freshtv38)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv39 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2687 "parser.ml"
                    ))) * (
# 23 "parser.mly"
       (string)
# 2691 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)) : 'freshtv42)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv43 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2702 "parser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)) : 'freshtv46)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv47 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2713 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)) : 'freshtv50)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 23 "parser.mly"
       (string)
# 2737 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUALS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv25 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2748 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | DEBUG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | DEREF ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | FST ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | ID _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
            | ISZERO ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | LET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | LETREC ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | NEWREF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | PAIR ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | PROC ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | SET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | SETREF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | SND ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27) : 'freshtv26)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv27 * _menhir_state) * (
# 23 "parser.mly"
       (string)
# 2798 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)) : 'freshtv30)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | DEBUG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | DEREF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | FST ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | ID _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | INT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
        | ISZERO ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | LETREC ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | PAIR ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | SND ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv22)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "parser.mly"
       (int)
# 2871 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv19) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 22 "parser.mly"
       (int)
# 2881 "parser.ml"
    )) : (
# 22 "parser.mly"
       (int)
# 2885 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 139 "parser.mly"
              ( Int i )
# 2890 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv20)

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | DEBUG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | FST ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | ISZERO ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LETREC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | PAIR ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | SND ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "parser.mly"
       (string)
# 2942 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x : (
# 23 "parser.mly"
       (string)
# 2952 "parser.ml"
    )) : (
# 23 "parser.mly"
       (string)
# 2956 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 140 "parser.mly"
             ( Var x )
# 2961 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv18)

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | DEBUG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | DEREF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | FST ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | ID _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | INT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | ISZERO ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LETREC ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | PAIR ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | SND ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv14)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | DEBUG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | DEREF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | FST ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | ID _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | INT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | ISZERO ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LETREC ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | PAIR ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | SND ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36) : 'freshtv10)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | DEBUG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | DEREF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | FST ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | ID _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | INT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | ISZERO ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | LETREC ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | NEWREF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | PAIR ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | PROC ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | SET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | SETREF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | SND ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv6)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | DEBUG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | FST ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | ISZERO ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LETREC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | PAIR ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | SND ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | END ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState39 in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_expr__ = 
# 142 "/home/ryan/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 3186 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

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
# 78 "parser.mly"
       (Ast.prog)
# 3209 "parser.ml"
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
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEBUG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FST ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ISZERO ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LETREC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NEWREF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PAIR ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PROC ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SETREF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SND ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 233 "/home/ryan/.opam/system/lib/menhir/standard.mly"
  

# 3270 "parser.ml"
