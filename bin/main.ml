
let red = "\x1b[31m";;
let white = "\x1b[37m";;

let exec_cmd cmd =
    let ic = Unix.open_process_in cmd in 
    let s = input_line ic in 
    let () = close_in ic in
    s;;

let entry color label value =
    String.concat "" [color; label; white; ": "; value; "\n"];;

let username = exec_cmd "whoami";;
let hostname = exec_cmd "uname --nodename";;

let os_name = exec_cmd "lsb_release --short --id";;
let os = exec_cmd "lsb_release --short --description";;

let kernel_version = exec_cmd "uname --kernel-release";;

let uptime = let s = exec_cmd "uptime --pretty" in 
    String.sub s 3 (String.length s - 3);;

let package_count =
    let command = match os_name with
    | "Ubuntu" | "Debian" -> Some ("dpkg --list | wc --lines", "dpkg")
    | _ -> None in
    match command with
    | Some (cmd, manager) -> String.concat "" [(exec_cmd cmd); " ("; manager; ")"] 
    | None -> "Cant find package manager: Unsupported OS";;

let shell =
    let shell_exe = Sys.getenv "SHELL" in 
    Filename.basename shell_exe;;

let terminal =
    let wt_session = Sys.getenv "WT_SESSION" in
    let is_wt = (String.length wt_session) > 0 in
    match is_wt with
    | true -> "Windows Terminal"
    | false -> exec_cmd "$(ps -p $(ps -p $$ -o ppid=) o args=) --version";;

let cpu = exec_cmd "lscpu | grep 'Model name' | cut --fields 2 --delimiter ':' | awk '{$1=$1}1'";;
let gpu = exec_cmd "lspci | grep -e '3D' -e 'VGA' -e 'Display'";;

let memory =
    let free = exec_cmd "free --mebi | grep 'Mem' | cut --fields 2 --delimiter ':' | awk '{$1=$1}1'" in 
    let list = List.map int_of_string (Str.split (Str.regexp " ") free) in 
    let total = List.nth list 0 in
    let used = List.nth list 1 in 
    let usage = (float_of_int used) /. (float_of_int total) *. 100. |> int_of_float in
    String.concat "" [string_of_int used; "MiB / "; string_of_int total; "MiB"; " ("; string_of_int usage; "%)"];;

(*
let color_blocks =
    let rec first_row i =
        if i = 8 then "\x1b[0m\n"
        else Printf.sprintf "\x1b[4%dm   " i ^ first_row (i + 1) in
    let rec second_row i =
        if i = 16 then "\x1b[0m\n"
        else Printf.sprintf "\x1b[48;5;%dm   " i ^ second_row (i + 1) in
    first_row 0 ^ second_row 8;;
*)

let () =
    let hostname_lenth = (String.length username) + (String.length hostname) + 1 in
    let s = String.concat "" [
        String.concat "" [red; username; white; "@"; red; hostname; "\n"];
        String.concat "" [white; (String.make hostname_lenth '-'); "\n"];
        entry red "OS" os;
        entry red "Kernel" kernel_version;
        entry red "Uptime" uptime;
        entry red "Packages" package_count;
        entry red "Shell" shell;
        entry red "Terminal" terminal;
        entry red "CPU" cpu;
        entry red "GPU" gpu;
        entry red "Memory" memory;
        (* color_blocks; *)
    ] in 
    print_string s;;
