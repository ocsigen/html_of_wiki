open Utils.Operators

type how_cmdline_t = ([`Dir | `Raw | `Silent], unit) Glcmd.commandline
let hcl_ref : how_cmdline_t option ref = ref None
let how_cmd_fun f = fun n p ->
  match !hcl_ref with
  | Some hcl -> f n p hcl
  | None -> assert false



type how_config_key = [ `Api
                      | `Build_dir
                      | `Client
                      | `Def_subp
                      | `Deploy
                      | `Dir
                      | `Force
                      | `Link_check
                      | `Manual
                      | `Raw
                      | `Server
                      | `Silent
                      | `Templates
                      | `Versions
                      | `Assets
                      | `Img
                      | `Csw
                      | `Menu ]

let how_config : how_config_key Conf.elt =
  Conf.(mapping [pair "deploy" `Deploy (bool () ~default:false)
                   ~doc:"Builds the website for deployement purpose (instead of testing).";
                 pair "templates" `Templates (sequence (string ()
                                                          ~check:Sys.file_exists
                                                          ~err:"Template does not exist.")
                                                ~default:[])
                   ~doc:"The list of templates, outermost first.";
                 pair "versions" `Versions (sequence (string ())
                                              ~check:(fun s -> s <> [])
                                              (* ~default:(infer_versions ()) *)
                                              ~err:"Version list cannot be empty.")
                   ~doc:"The list of version directories.";
                 pair "build-dir" `Build_dir (string () ~default:"_build")
                   ~doc:"Name of the build directory.";
                 (* remote *)
                 pair "force" `Force (bool () ~default:false)
                   ~doc:"Overwrites the build directory if it exists when the build starts.";
                 pair "link-check" `Link_check (mapping [pair "raw" `Raw (bool () ~default:false)
                                                           ~doc:"Doesn't converts linkchecker's output to Json when true.";
                                                         pair "silent" `Silent (bool () ~default:false)
                                                           ~doc:"No output at all when true."])
                   ~doc:"Behaviour of the command `how check links'.";
                 pair "manual" `Manual (string () (* ~default:(infer_manual ()) *))
                   ~doc:"Path to the manual directory.";
                 pair "api" `Api (either (string ())
                                    (mapping [pair "dir" `Dir (string ())
                                                ~doc:"The path to the API directory.";
                                              pair "client" `Client (string ())
                                                ~doc:"The path to the client API directory.";
                                              pair "server" `Server (string ())
                                                ~doc:"The path to the server API directory.";
                                              pair "default-subproject" `Def_subp (string ())
                                                ~doc:"Default subproject's name. Obsolete."]
                                    (* ~default:(infer_api ()) *)))
                   ~doc:"API information. When just a string, the path to the API directory.";
                 pair "assets" `Assets (either (string ())
                                          (mapping [pair "dir" `Dir (string ())
                                                      ~doc:"The path to the asset directory.";
                                                    pair "images" `Img (string ())
                                                      ~doc:"The path to the image directory."]))
                   ~doc:"Asset information. When just a string, the path to the image + asset directory.";
                 pair "menu" `Menu (bool () ~default:false)
                   ~doc:"Whether to include or not a side menu (cf. doctree extension).";
                 pair "client-server-switch" `Csw (bool () ~default:false)
                   ~doc:"Whether to include or not a client/server switch on compatible pages (cf. extension)."])


let printr s = (fun _ _ -> print_endline s)

let config_help_cmd _ _ = Conf.doc_of_conf how_config |> print_endline

let help_cmd = how_cmd_fun (fun _ cmd_path hcl -> Glcmd.help ~prefix:"how" cmd_path hcl |> print_string)





(* let rec string_of_conf_structure_error_t = function
 *   | `Bool -> "bool"
 *   | `String -> "string"
 *   | `Mapping -> "mapping"
 *   | `Sequence -> "sequence"
 *   | `Float -> "float"
 *   | `Null -> "null"
 *   | `Either (a, b) -> Printf.sprintf "either (%s) or (%s)"
 *                         (string_of_conf_structure_error_t a)
 *                         (string_of_conf_structure_error_t b)
 *
 * let () =
 *   let open Printf in
 *   let ic = open_in ".how.yml" in
 *   Conf.(
 *     try
 *       let res = Conf.parse Conf.yaml_parser how_config ic in
 *       (yaml_printer (tag_printer_of_conf how_config)) res |> print_endline
 *     with
 *     | Unexpected_labels ls -> printf "unex labels: %s\n" (String.concat ", " ls)
 *     | Missing_required_pairs ls -> printf "miss req pairs: %s\n" (String.concat ", " ls)
 *     | Structure_error {got; expected; inside} -> printf "Error inside \"%s\": expected %s and got %s\n"
 *                                                    inside
 *                                                    (string_of_conf_structure_error_t expected)
 *                                                    (string_of_conf_structure_error_t got)
 *   );
 *   close_in ic *)



let how_commandline =
  Glcmd.[anonymous help_cmd ~doc:"Displays available commands";
         command "help" help_cmd
           ~args:[multiple String ~valname:"CMD" ~doc:"The command to describe."]
           ~doc:"Displays help for a given command.";
         command "build" (printr "build")
           ~args:[arg "dir" String `Dir ~short:"d" (*~default:"_build"*) ~valname:"DIR"
                    ~doc:"The directory to build in.";
                  multiple File ~valname:"VERSION" ~doc:"The versions of the documentation to build."]
           ~doc:"Build the documentation of the project.";
         prefix "check"
           [anonymous (printr "check all") ~doc:"Perform all checks.";
            command "links" (printr "check links")
              ~args:[arg "dir" File `Dir ~short:"d" ~valname:"DIR"
                       ~doc:"The directory to check in.";
                     flag "raw" `Raw ~short:"r"
                       ~doc:"Prints the raw output of [linkchecker] (ie. no Json formatting).";
                     flag "silent" `Silent ~short:"s"
                       ~doc:"Perform the check but outputs nothing."]
              ~doc:"Run [linkchecker] on the build directory to check for deadlinks.";
            command "config" (printr "check config")
              ~doc:"Checks the validity of the configuration file."];
         prefix "config"
           [anonymous (printr "config help") ~doc:"Configuration file related commands.";
            command "help" config_help_cmd
              ~doc:"Lists the available configuration options";
            command "check" (printr "check config")
              ~doc:"Checks the validity of the configuration file.";
            command "infer" (printr "inferring.")
              ~doc:"Prints a minimal configuration inferred using the structure of the current directory."];
         prefix "init"
           [anonymous (printr "init")
              ~doc:"Creates an inferred configuration file in the current directory.";
            command "infer" (printr "init infer")
              ~doc:"Creates an inferred configuration file in the current directory.";
            command "default" (printr "init default")
              ~doc:"Creates a default configuration file in the current directory."]]

let () =
  hcl_ref := Some how_commandline;
  Glcmd.run how_commandline
