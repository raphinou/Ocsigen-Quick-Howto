(* ************************************************************************** *)
(* Project: Ocsigen Quick Howto : Page                                        *)
(* Description: Example of a simple page (The famous "Hello World"!)          *)
(* Author: db0 (db0company@gmail.com, http://db0.fr/)                         *)
(* Latest Version is on GitHub: http://goo.gl/sfvvq                           *)
(* ************************************************************************** *)

open Eliom_content
open Html5.D
open Eliom_parameter
open String

(* ************************************************************************** *)
(* Application                                                                *)
(* ************************************************************************** *)

module Example =
  Eliom_registration.App
    (struct
      let application_name = "example"
     end)

(* ************************************************************************** *)
(* Service declaration                                                        *)
(* ************************************************************************** *)

let main =
  Eliom_service.service
    ~path:[]
    ~get_params:(string "name")
    ()

(* ************************************************************************** *)
(* Service definition                                                         *)
(* ************************************************************************** *)

let _ =

  Example.register
    ~service:main
    (fun name () ->
      Lwt.return
	(html
	   (head (title (pcdata (String.concat " " ["Hello World of Ocsigen";name] ))) [])
	   (body [h1 [pcdata (String.concat " " ["Hello World of Ocsigen";name] )];
		  p [pcdata "Welcome to my first Ocsigen website."]])))
