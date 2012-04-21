(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2010-2012                                               *)
(*    François Bobot                                                      *)
(*    Jean-Christophe Filliâtre                                           *)
(*    Claude Marché                                                       *)
(*    Guillaume Melquiond                                                 *)
(*    Andrei Paskevich                                                    *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Why3 to HTML *)

{

  open Format
  open Lexing
  open Why3

  let output_comments = ref true

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let make_table l =
    let ht = Hashtbl.create 97 in
    List.iter (fun s -> Hashtbl.add ht s ()) l;
    Hashtbl.mem ht

  let is_keyword1 = make_table
    [ "theory"; "end";
      "type"; "constant"; "function"; "predicate"; "inductive";
      "clone"; "use";
      "import"; "export"; "axiom"; "goal"; "lemma"; ]

  let is_keyword2 = make_table
    [ "match"; "with"; "let"; "in"; "if"; "then"; "else";
      "forall"; "exists";
      (* programs *)
      "as"; "assert"; "begin";
      "do"; "done"; "downto"; "else";
      "exception"; "val"; "for"; "fun";
      "if"; "in";
      "module"; "mutable";
      "rec"; "then"; "to";
      "try"; "while"; "invariant"; "variant"; "raise"; "label"; ]

  let get_loc lb =
    let p = Lexing.lexeme_start_p lb in
    p.pos_fname, p.pos_lnum, p.pos_cnum - p.pos_bol

}

let ident = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

rule scan fmt embedded = parse
  | "(*)" as s
          { pp_print_string fmt s; scan fmt embedded lexbuf }
  | "(***" as s
          { if embedded then pp_print_string fmt s else
              comment fmt false lexbuf;
            scan fmt embedded lexbuf }
  | "(**" as s
          { if embedded then pp_print_string fmt s else
              begin
                fprintf fmt "</pre>@\n";
                doc fmt [] lexbuf;
                fprintf fmt "<pre>@\n";
              end;
            scan fmt embedded lexbuf }
  | "(*" as s
          { if embedded then pp_print_string fmt s else
              begin
                fprintf fmt "<font class=\"comment\">(*";
                comment fmt true lexbuf;
                fprintf fmt "</font>";
              end;
            scan fmt embedded lexbuf }
  | ']' as c
          { if embedded then () else
              begin
                pp_print_char fmt c;
                scan fmt embedded lexbuf
              end
          }
  | eof   { () }
  | ident as s
    { if is_keyword1 s then
        fprintf fmt "<font class=\"keyword1\">%s</font>" s
      else if is_keyword2 s then
        fprintf fmt "<font class=\"keyword2\">%s</font>" s
      else begin
        let (* f,l,c as *) loc = get_loc lexbuf in
        (* Format.eprintf "  IDENT %s/%d/%d@." f l c; *)
        (* is this a def point? *)
        try
          let t = Doc_def.is_def loc in
          fprintf fmt "<a name=\"%s\">%s</a>" t s
        with Not_found ->
        (* is this a use point? *)
        try
          let id = Glob.locate loc in
          let fn, t = Doc_def.locate id in
          fprintf fmt "<a href=\"%s#%s\">%s</a>" fn t s
        with Not_found ->
        (* otherwise, just print it *)
          pp_print_string fmt s
      end;
      scan fmt embedded lexbuf }
  | "<"    { fprintf fmt "&lt;"; scan fmt embedded lexbuf }
  | ">"    { fprintf fmt "&gt;"; scan fmt embedded lexbuf }
  | "&"    { fprintf fmt "&amp;"; scan fmt embedded lexbuf }
  | "\n"   { newline lexbuf; fprintf fmt "\n"; scan fmt embedded lexbuf }
  | '"'    { fprintf fmt "&quot;"; string fmt true lexbuf;
             scan fmt embedded lexbuf }
  | "'\"'"
  | _ as s { pp_print_string fmt s; scan fmt embedded lexbuf }

and comment fmt do_output = parse
  | "(*"   { if do_output then fprintf fmt "(*";
             comment fmt do_output lexbuf;
             comment fmt do_output lexbuf }
  | "*)"   { if do_output then fprintf fmt "*)" }
  | eof    { () }
  | "\n"   { newline lexbuf;
             if do_output then fprintf fmt "\n";
             comment fmt do_output lexbuf }
  | '"'    { if do_output then fprintf fmt "&quot;";
             string fmt do_output lexbuf;
             comment fmt do_output lexbuf }
  | "<"    { if do_output then fprintf fmt "&lt;";
             comment fmt do_output lexbuf }
  | "&"    { if do_output then fprintf fmt "&amp;";
             comment fmt do_output lexbuf }
  | "'\"'"
  | _ as s { if do_output then pp_print_string fmt s;
             comment fmt do_output lexbuf }

and string fmt do_output = parse
  | "\n"   { newline lexbuf;
             if do_output then fprintf fmt "\n";
             string fmt do_output lexbuf }
  | '"'    { if do_output then fprintf fmt "&quot;" }
  | "<"    { if do_output then fprintf fmt "&lt;";
             string fmt do_output lexbuf }
  | ">"    { if do_output then fprintf fmt "&gt;";
             string fmt do_output lexbuf }
  | "&"    { if do_output then fprintf fmt "&amp;";
             string fmt do_output lexbuf }
  | "\\" _
  | _ as s { if do_output then pp_print_string fmt s;
             string fmt do_output lexbuf }

and doc fmt headings = parse
  | "*)"   { () }
  | eof    { () }
  | "\n"   { newline lexbuf;
             fprintf fmt "\n";
             doc fmt headings lexbuf }
  | '{' (['1'-'6'] as c)
           { let n = Char.code c - Char.code '0' in
             fprintf fmt "<h%d>" n;
             doc fmt (n::headings) lexbuf }
  | '{''h' { raw_html fmt 0 lexbuf; doc fmt headings lexbuf }
  | '{'    { fprintf fmt "{"; doc fmt (0::headings) lexbuf }
  | '}'    { match headings with
              | [] -> fprintf fmt "}"; doc fmt headings lexbuf
              | n :: r ->
                  if n >= 1 then fprintf fmt "</h%d>" n else
                    fprintf fmt "}";
                  doc fmt r lexbuf
           }
  | '['    { pp_print_string fmt "<code>";
             scan fmt true lexbuf; 
             pp_print_string fmt "</code>";
             doc fmt headings lexbuf }
  | '"'    { fprintf fmt "&quot;"; doc fmt headings lexbuf }
  | '\''   { fprintf fmt "&apos;"; doc fmt headings lexbuf }
  | '&'    { fprintf fmt "&amp;"; doc fmt headings lexbuf }
  | "<"    { fprintf fmt "&lt;"; doc fmt headings lexbuf }
  | ">"    { fprintf fmt "&gt;"; doc fmt headings lexbuf }
  | _ as c { pp_print_char fmt c; doc fmt headings lexbuf }


and raw_html fmt depth = parse
  | eof    { () }
  | "\n"   { newline lexbuf;
             fprintf fmt "\n";
             raw_html fmt depth lexbuf }
  | '{'    { fprintf fmt "{"; raw_html fmt (succ depth) lexbuf }
  | '}'    { if depth = 0 then () else
               begin
                 fprintf fmt "{";
                 raw_html fmt (pred depth) lexbuf
               end }
  | _ as c { pp_print_char fmt c; raw_html fmt depth lexbuf }




{

  let do_file fmt fname =
    (* input *)
    let cin = open_in fname in
    let lb = Lexing.from_channel cin in
    lb.Lexing.lex_curr_p <-
      { lb.Lexing.lex_curr_p with Lexing.pos_fname = fname };
    (* output *)
    fprintf fmt "<pre>@\n";
    scan fmt false lb;
    fprintf fmt "</pre>@\n";
    close_in cin

}

(*
Local Variables:
compile-command: "unset LANG; make -C ../.. bin/why3doc.opt"
End:
*)
