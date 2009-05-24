(* Ocsimore
 * Copyright (C) 2009
 * Laboratoire PPS - Université Paris Diderot - CNRS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(**
   @author Vincent Balat
   @author Boris Yakobowski
*)

let ( ** ) = Eliom_parameters.prod
let ( >>= ) = Lwt.bind

open User_sql.Types


let register_services () =
  let add_message_service =
    Eliom_services.new_post_coservice'
      ~keep_get_na_params:false
      ~name:"forum_add"
      ~post_params:
      (Eliom_parameters.string "actionname" **
         ((Eliom_parameters.sum
             (Forum.eliom_message "parent")
             (Forum.eliom_forum "forum")) **
            (Eliom_parameters.opt (Eliom_parameters.string "subject") **
               Eliom_parameters.string "content")))
      ()
  in

  Eliom_predefmod.Any.register 
    ~service:add_message_service
    (fun sp () (actionname, (parent, (subject, text))) ->

     (match parent with
        | Eliom_parameters.Inj2 forum -> (* new messages *)
            Lwt.return (forum, None)
        | Eliom_parameters.Inj1 parent_id -> (* comment *)
            (* We do not require the user to be allowed to read the message ... 
               (Forum_sql.get_message and not Forum_data.get_message) *)
            Forum_sql.get_message ~message_id:parent_id () >>= fun m ->
            Lwt.return (m.Forum_sql.Types.m_forum, Some parent_id))
       >>= fun (forum, parent_id) ->

       Forum.get_role sp forum >>= fun _role -> (* VVV : why is role not used here ? *)

       if actionname = "save" 
       then
         (Lwt.catch
            (fun () ->
               Users.get_user_data sp >>= fun u ->
               Forum_data.new_message ~sp ~forum 
                 ~creator_id:u.user_id ?subject ?parent_id ~text ()
               >>= fun _ ->
               Eliom_predefmod.Redirection.send ~sp 
                 Eliom_services.void_hidden_coservice'
            )
            (function
               | Ocsimore_common.Permission_denied ->
                   Polytables.set
                     (Eliom_sessions.get_request_cache sp)
                     Ocsimore_common.tmp 
                     [Forum.Forum_action_info 
                        (Forum.Msg_creation_not_allowed (forum, parent_id))
                     ];
                   Eliom_predefmod.Action.send ~sp ()
               | e -> Lwt.fail e)
         )
       else begin (* preview *)
         Polytables.set
           (Eliom_sessions.get_request_cache sp)
           Ocsimore_common.tmp 
           [Forum.Forum_action_info
              (Forum.Preview ((forum, parent_id), text))];
         Eliom_predefmod.Action.send ~sp ()
       end
    );


  (* Moderation *)
  let moderate_message_service =
    Eliom_services.new_post_coservice'
      ~keep_get_na_params:false
      ~name:"forum_moderate"
      ~post_params:(Forum.eliom_message "msg")
      ()
  in

  Eliom_predefmod.Action.register 
    ~service:moderate_message_service
    (fun sp () msg ->
       Forum_data.set_moderated ~sp ~message_id:msg ~moderated:true
    );

(* AEFF
  (* Deletion *)
  let delete_message_service =
    Eliom_services.new_post_coservice'
      ~keep_get_na_params:false
      ~name:"forum_delete_message"
      ~post_params:(Forum.eliom_message "msg")
      ()
  in

  Eliom_predefmod.Action.register 
    ~service:delete_message_service
    (fun sp () msg ->
       Forum_data.set_deleted ~sp ~message_id:msg ~deleted:true
    );
*)

  (add_message_service,
   moderate_message_service)
