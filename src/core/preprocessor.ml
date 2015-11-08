(* Working preprocessor. Still needs to be integrated into
the project appropriately, but here it is. See 
src/tests/preprocessor_example.txt for an example of a 
file that would be handled by this *)


(* open the file, which I should figure out how to close *)
let myfile = open_in "input.txt" in

(* read in the lines one by one into a list *)
let rec input_lines file =
	match try [input_line file] with End_of_file -> [] with
		[] -> []
		| line -> line @ input_lines file

	in

(* Function for removing comments now *)
let remove_comments lines =
		
	let rec eachlinehandler state_tuple current_string =

		(* grab stuff from tuples *)
		let comment_state = fst state_tuple in
		let current_list = snd state_tuple in
		
		(* first check if length of string is 0 *)
		if String.length current_string == 0 then
			( comment_state, []  )
		else		
			try
				let pound_index = String.index current_string '#' in
				let end_diff = (String.length current_string) - (pound_index+ 1) in
				let ahalf = [String.sub current_string 0 pound_index;] in
				let bhalf = String.sub current_string (pound_index + 1) end_diff in

				if comment_state then
					let choice_tuple = (false, []) in	
					let result_tuple = eachlinehandler choice_tuple bhalf in
					(fst result_tuple, current_list @ (snd result_tuple))
				else
					let choice_tuple = (true, []) in	
					let result_tuple = eachlinehandler choice_tuple bhalf in
					(fst result_tuple, (current_list @ (ahalf @ (snd result_tuple))))
			with
				Not_found ->
					if comment_state then		
						(true, [])	
					else		
						(false, current_string :: [])	
	in


	(* now use the recursive line handler to do things *)

	let remove_comment_aux aux_tuple next_line = 
		(* cumulative list and whether we're starting with a comment *)
		let start_with_comment = fst aux_tuple in
		let list_so_far = snd aux_tuple in

		(* eachlinehandler spits out (still comment?, [list, of, strings] *)	
		let result_tuple = eachlinehandler (start_with_comment, []) next_line in		
		let new_string_tokens = snd result_tuple in

		(* put the small strings together into one line again, backwards *) 
		(fst result_tuple, String.concat "" new_string_tokens :: list_so_far)
		in

	(* call auxiliary function with the lines, then reverse the output *)	
	let results = List.fold_left remove_comment_aux (false, []) lines in
	List.rev (snd results)
	in


(* read in all lines, then remove the comments *)
let lineList = remove_comments (input_lines myfile) in


(* this is where the indent-removal magic happens*)

let rec process_indents current_list current_indent_level = 

	(* returns whether string is only whitespaces *)	
	let only_whitespace my_string = 
		let length = String.length my_string in
		let rec check_whitespace pos = 
			if pos == length then true
			else 
				let item = String.get my_string pos in
				if (item == '\t' || item == ' ') then 
					true && check_whitespace (pos + 1)
				else false
		in check_whitespace 0 
		in

	(* counts the number of tabs in the left side *)
	let count_tabs my_string =
		let length = String.length my_string in
		let rec count_tabs_rec pos =
			if String.get my_string pos == '\t' then
				1 + count_tabs_rec (pos + 1)
			else 0 in
		if length == 0 then 0 else count_tabs_rec 0
		in

	(* make new line *)	
	let make_new_line my_string =  
		try  	
			let colon_index = String.rindex my_string ':' in
			String.concat "" [(String.sub my_string 0 colon_index); " {"; ]
		with
			Not_found -> String.concat ""  [my_string; "; ";]
		in

	(* generates a string of n number of tabs together *)	
	let generate_n_tabs n =
		let rec tab_list tabs = 
			if tabs <= 0 then []
			else 
				"\t" :: (tab_list ( tabs - 1)) in 
		String.concat "" (tab_list n) 
		in

	(* n is the number of brackets we need, 
	old_level is the indentation level we left, so we can
	properly tab and indent, and make everyting look nice*)
	let generate_n_close_brackets n old_level= 
		let rec bracket_list brackets level= 
			if brackets <= 0 then []
			else
				let rest_of_list = bracket_list (brackets - 1) (level - 1) in 
				generate_n_tabs (level - 1) :: "} \n" :: rest_of_list in 
		String.concat "" (bracket_list n old_level) 
		in

	match current_list with
		(* if we have *) 
		| [] -> String.concat "" [ generate_n_close_brackets current_indent_level 
			current_indent_level;] 
		| head :: tail -> 
			if only_whitespace head then
				(* okay just do the next line *)
				process_indents tail current_indent_level	
			else
				(* Finds the closing brackets necessary based
				on the indentation level *)
				let new_indent_level = count_tabs head in 
				let close_needed = current_indent_level - new_indent_level in
				let new_close_brackets = generate_n_close_brackets close_needed 
					current_indent_level in 
				if (String.length new_close_brackets) > 0 then 
					String.concat "" [	(* We have to close some brackets*) 
						new_close_brackets;
						(make_new_line head);"\n";	
						process_indents tail new_indent_level;] 
	
				else
					String.concat "" [
						(* stick on same indent level *)
						make_new_line head; "\n";
						process_indents tail new_indent_level;] 
	in

print_endline (process_indents lineList 0) ;;

