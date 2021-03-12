open Printf
type sheet_data = DATA of float | UNDEFINED | EMPTY | DIV_BY_ZERO
type sheet = sheet_data list list
type index = float * float  (* Will be type-checked *)
type range = index * index

exception InvalidIndex (* When Destination index is invalid or insufficient for the output *)
exception InvalidInput (* For cases when sheet is invalid *)
exception InvalidData

(* O(n): length function finds the length of a given list tail recursively*)
let rec length l n = match l with []->n | x::xs->length xs (n+1)

(* O(n): Reverses the contents of any given list using tail recursion *)
let rec reverse_ l revl = match l with 
	[] -> revl |
	x::xs -> reverse_ xs (x::revl)
let reverse l = reverse_ l []

(* O(n): Returns the map of the list under unary function f. Here n is the length of the list l *)
let rec map l f = 
	match l with 
		[] -> [] |
		x::xs -> (f x)::(map xs f)

(* O(l): Splits the string s on commas. Here l is the length of the string. *)
let data_of_string (s:string): sheet_data = try DATA(float_of_string s) with e -> EMPTY

(* O(l): Gets the sheet from string csv of rows. Here l is the total length of all the strings *)
let rec getsheet (str_rows:string list) (temps:sheet) (templen:int): sheet = 
	match str_rows with
		[] -> reverse temps |
		row::rows ->  
			let str_cells = String.split_on_char ',' row in 
			let len = (length str_cells 0) in
				if (templen = -1)||(templen=len) then
					getsheet rows ((map str_cells data_of_string)::temps) len
				else
					raise InvalidData

(* O(l): Takes the name of the csv file as input and returns the sheet. Here l is the total lenght of the file. *)
let initialize (file:string): sheet = 
	let in_channel = open_in file in
	let filetext = really_input_string in_channel (in_channel_length in_channel) in
    close_in in_channel;
    let str_rows = String.split_on_char '\n' filetext in 
    getsheet str_rows [] (-1)

let rec print_sheet (s:sheet): unit = match s with
	[] -> () |
	[]::rows -> (printf "%s" "\n";print_sheet rows) |
	(data::datas)::rows -> match data with 
		DATA(x) -> 
			if (x=(infinity)) then 
				(printf "  +%f  \t\t" x;print_sheet (datas::rows))
			else if (x=(-.infinity)) then
				(printf "  %f  \t\t" x;print_sheet (datas::rows)) 
			else (printf "%f\t\t" x;print_sheet (datas::rows)) |
		UNDEFINED -> (printf "%s\t\t" "undefined";print_sheet (datas::rows)) |
		EMPTY -> (printf "%s\t\t" "   ##   ";print_sheet (datas::rows)) |
		DIV_BY_ZERO -> (printf "%s\t\t" "div-by-0";print_sheet (datas::rows))

(* For every function first the destination will be checked:-
	if it is check_index valid and something else is invalid then UNDEFINED is filled in the cell
	else the evaluated value is filled in the cell *)

(* O(m+n): Checks whether index is valid wrt sheet(m*n) or not *)
let check_index (s:sheet) (i:index): bool =
	match i with (left,right) -> match s with
	row::rows->if ((left-.float_of_int(int_of_float(left))=0.) && 
			(right-.float_of_int(int_of_float(right))=0.) &&
			(int_of_float(left) >= 0) && 
			(int_of_float(right) >= 0) && 
			(int_of_float(left) < (length s 0)) && 
			(int_of_float(right) < (length row 0))) then true else raise InvalidIndex| 
	_ -> raise InvalidInput

(* O(m+n): Checks whether range is valid wrt sheet *)
let check_range (s:sheet) (r:range): bool = 
	match r with ((l1,r1),(l2,r2)) ->
	match s with (row::rows) ->
		((l1-.float_of_int(int_of_float(l1))=0.) && 
		(r1-.float_of_int(int_of_float(r1))=0.) && 
		(l2-.float_of_int(int_of_float(l2))=0.) && 
		(r2-.float_of_int(int_of_float(r2))=0.) &&  (*Checked whether all indices are ints or not*)
		(r2-.r1>=0.) && (l2-.l1>=0.) &&
		(int_of_float(l2) < (length s 0)) && 
		(int_of_float(r2) < (length row 0))) |(* Checked extractability *)
	_ -> raise InvalidInput

(* O(m+n): Checks whether range is valid wrt sheet and whether index can sufficiently accomodate range *)
let check_range_index_row (s:sheet) (r:range) (i:index): bool = 
	match r with ((l1,r1),(l2,r2)) ->
	match i with (left,right) ->
	match s with (row::rows) ->
		((l1-.float_of_int(int_of_float(l1))=0.) && 
		(r1-.float_of_int(int_of_float(r1))=0.) && 
		(l2-.float_of_int(int_of_float(l2))=0.) && 
		(r2-.float_of_int(int_of_float(r2))=0.) &&  (*Checked whether all indices are ints or not*)
		(* (int_of_float(r2-.r1) < ((length row 0)- int_of_float(right))) && *)
		(int_of_float(l2-.l1) < ((length s 0)- int_of_float(left))) && (* Checked writability *)
		(r2-.r1>=0.) && (l2-.l1>=0.) &&
		(int_of_float(l2) < (length s 0)) && 
		(int_of_float(r2) < (length row 0))) |(* Checked extractability *)
	_ -> raise InvalidInput

(* O(m+n): Checks whether range is valid wrt sheet and whether index can sufficiently accomodate range *)
let check_range_index_col (s:sheet) (r:range) (i:index): bool = 
	match r with ((l1,r1),(l2,r2)) ->
	match i with (left,right) ->
	match s with (row::rows) ->
		((l1-.float_of_int(int_of_float(l1))=0.) && 
		(r1-.float_of_int(int_of_float(r1))=0.) && 
		(l2-.float_of_int(int_of_float(l2))=0.) && 
		(r2-.float_of_int(int_of_float(r2))=0.) &&  (*Checked whether all indices are ints or not*)
		(int_of_float(r2-.r1) < ((length row 0)- int_of_float(right))) &&
		(* (int_of_float(l2-.l1) < ((length s 0)- int_of_float(left))) && (* Checked writability *) *)
		(r2-.r1>=0.) && (l2-.l1>=0.) &&
		(int_of_float(l2) < (length s 0)) && 
		(int_of_float(r2) < (length row 0))) |(* Checked extractability *)
	_ -> raise InvalidInput	

(* O(m+n): Checks whether range is valid wrt sheet and whether index can sufficiently accomodate range *)
let check_range_index (s:sheet) (r:range) (i:index): bool = 
	match r with ((l1,r1),(l2,r2)) ->
	match i with (left,right) ->
	match s with (row::rows) ->
		((l1-.float_of_int(int_of_float(l1))=0.) && 
		(r1-.float_of_int(int_of_float(r1))=0.) && 
		(l2-.float_of_int(int_of_float(l2))=0.) && 
		(r2-.float_of_int(int_of_float(r2))=0.) &&  (*Checked whether all indices are ints or not*)
		(int_of_float(r2-.r1) < ((length row 0)- int_of_float(right))) &&
		(int_of_float(l2-.l1) < ((length s 0)- int_of_float(left))) && (* Checked writability *)
		(r2-.r1>=0.) && (l2-.l1>=0.) &&
		(int_of_float(l2) < (length s 0)) && 
		(int_of_float(r2) < (length row 0))) |(* Checked extractability *)
	_ -> raise InvalidInput

(* O(1): Checks compatibility of two valid ranges of the sheet *)
let range_compatibility (r1:range) (r2:range): bool = match (r1,r2) with
	(((l11,r11),(l12,r12)),((l21,r21),(l22,r22)))->
		(l12-.l11=l22-.l21 && r12-.r11=r22-.r21)

(* O(m*n): Assigns the data to the index in the sheet tail recursively *)
let rec assign_ (s:sheet) (data:sheet_data) (r:int) (c:int) (tempr:sheet_data list) (temps:sheet): sheet = match s with row::rows->
	(if r = 0 then match row with data_orig::datas ->
		(if c = 0 then 
			(reverse temps)@(((reverse tempr)@(data::datas))::rows)
		else assign_ (datas::rows) data r (c-1) (data_orig::tempr) temps ) |
		_ -> raise InvalidInput
	else assign_ rows data (r-1) c tempr (row::temps)) |
	_ -> raise InvalidInput
(* O(m*n): Calls helper function *)
let assign (s:sheet) (data:sheet_data) (i:index): sheet =
	if (check_index s i) then
		match i with (left,right) -> assign_ s data (int_of_float(left)) (int_of_float(right)) [] []
	else
		raise InvalidInput

(* O(m*n): Assigns sub-sheet of size p*q to original sheet of m*n at index i *)
let rec assign_range_ (s:sheet) (s1:sheet) (inti:int*int) (c_: int) (tempr:sheet_data list) (temps:sheet): sheet =
	match inti with (r,c) -> (* Row changes made to inti col changes to c_ c is original as it will be required in all rows *)
	match s with
	(data::datas)::rows -> 
	(if r=0 then (* Get updated row tempr *)
		if c_=0 then
			(match s1 with 
			[] -> (* Return reverse of temps *)
				(reverse temps)@s |
			[]::rows1 -> (* Add reverse of tempr to temps. Move to next row this row has been assigned *)
				(* (if (print_sheet s)&&(print_sheet s1) then printf "%s" "1\n"; *)
				assign_range_ rows rows1 inti c [] (((reverse tempr)@(data::datas))::temps)(* ) *) |
			(data1::datas1)::rows1 -> (* Replace data with data1 *)
				(* (if (print_sheet s)&&(print_sheet s1) then printf "%s" "2\n"; *)
				assign_range_ (datas::rows) (datas1::rows1) inti c_ (data1::tempr) temps)(* ) *)
		else (* No replacement: Move forward in the current row *)
			(match s1 with 
			[] -> (* Return reverse of temps *)
				(reverse temps)@s |
			(data1::datas1)::rows1 -> (* Replace data with data1 *)
				(* ((if (print_sheet s)&&(print_sheet s1) then printf "%s" "3\n"; *)
				assign_range_ (datas::rows) s1 inti (c_-1) (data::tempr) temps(* ) *) |
			_ -> raise InvalidInput)
	else (* Move forward in s: add data::datas to temps *)
		(* (if (print_sheet s)&&(print_sheet s1) then printf "%s" "4\n"; *)
		assign_range_ rows s1 (r-1,c) c_ tempr ((data::datas)::temps))(* ) *) |
	[]::rows -> 
		(match s1 with 
			[]::rows1 ->
				(* (if (print_sheet s)&&(print_sheet s1) then printf "%s" "5\n"; *)
				assign_range_ rows rows1 inti c [] ((reverse tempr)::temps)(* ) *) |
			_ -> raise InvalidInput) |
	[] ->
		reverse temps
(* O(m*n): Assigns sub-sheet of size p*q to original sheet of m*n at index i by calling helper function *)
let assign_range (s:sheet) (s1:sheet) (i:index): sheet =
	match i with (r,c) -> 
	assign_range_ s s1 (int_of_float(r),int_of_float(c)) (int_of_float(c)) [] []

(* O(m*n): Extracts sub-sheet of size p*q from original sheet of m*n *)
let rec extract_range_ (s:sheet) (intr:(int*int)*(int*int)) (c1_: int) (c2_:int) (tempr:sheet_data list) (temps:sheet): sheet =
	match intr with ((r1,c1),(r2,c2)) -> (* Row changes made to intr col changes to c1_ and c2_ c1 and c2 are original as they will be required in all rows *)
	match s with (data::datas)::rows -> 
	(if r1=0 then (* Get c1_ to c2_ from current row in tempr *)
		if r2<r1 then (* return reverse of temps *)
			reverse temps
		else (* r1=0 and r2>=0 => Either we discard data or add to tempr *)
			if c1_=0 then (* Get upto c2 of the current row *)
				if c2_<c1_ then (* Add reverse of tempr to temps and discard rest data *)
					extract_range_ rows ((r1,c1),(r2-1,c2)) c1 c2 [] ((reverse tempr)::temps)
				else (* Add data to tempr *)
					extract_range_ (datas::rows) intr c1_ (c2_-1) (data::tempr) temps
			else (* Move forward int the row: Discard data *)
				extract_range_ (datas::rows) intr (c1_-1) (c2_-1) tempr temps
	else (* Move forward in the sheet *)
		extract_range_ rows ((r1-1,c1),(r2-1,c2)) c1 c2 tempr temps ) |
	[]::rows ->
		extract_range_ rows ((r1,c1),(r2-1,c2)) c1 c2 [] ((reverse tempr)::temps) |
	[] ->
		reverse temps
(* O(m*n): Extracts sub-sheet of size p*q from original sheet of m*n by calling helper function *)
let extract_range (s:sheet) (r:range): sheet =
	match r with ((r1,c1),(r2,c2)) -> 
	extract_range_ s ((int_of_float(r1),int_of_float(c1)),(int_of_float(r2),int_of_float(c2))) (int_of_float(c1)) (int_of_float(c2)) [] []

(* O(m*n): Creates a m*n sheet with EMPTY entries using tail recursion *)
let rec create_empty_ (m:int) (n:int) (temp_sheet:sheet): sheet = 
	if m=0 then temp_sheet
	else match temp_sheet with 
		[] -> create_empty_ (m-1) n [[]] |
		x::xs -> if n=0 then create_empty_ (m-1) n (x::temp_sheet) else create_empty_ m (n-1) ((EMPTY::x)::xs)
(* O(m*n): Checkes input and calls helper function *)
let create_empty (m:int) (n:int): sheet = 
	if (m>0 && n>0) then create_empty_ m n []
	else raise InvalidInput

(* O(n): Creates a data list of size n: Used for creating sheet_data lists of 0.s or +-infinitys *)
let rec create (n:int) (data:sheet_data) (tempr:sheet_data list): sheet_data list = 
	if n=0 then tempr else create (n-1) data ((data::tempr))

(* All functions are O(m*n) when m*n is the dimentions of the sheet s in that function *)
let rec count (s:sheet) (c:sheet_data): sheet_data =
	match c with DATA(currentcount) -> 
	(match s with
		[] -> c |
		[]::rows -> count rows c |
		(data::datas)::rows ->
			(match data with 
				DATA(x) -> count (datas::rows) (DATA(currentcount+.1.)) |
				_ -> count (datas::rows) (DATA(currentcount)))) |
	_ -> raise InvalidInput
let full_count (s:sheet) (r:range) (i:index): sheet =
	if (check_index s i) && (check_range s r) then
		assign s (count (extract_range s r) (DATA(0.0))) i
	else
		assign s UNDEFINED i
let rec row_count_ (s:sheet) (temps:sheet): sheet =
	match s with 
		[] -> reverse temps |
		row::rows -> row_count_ rows ([(count [row] (DATA(0.0)))]::temps) 
let row_count (s:sheet) (r:range) (i:index): sheet =
	if (check_index s i) && (check_range_index_row s r i) then
		assign_range s (row_count_ (extract_range s r) []) i
	else
		assign s UNDEFINED i
let rec col_count_ (s:sheet) (tempr:sheet_data list) (tempr1:sheet_data list): sheet =
	match s with 
		[] -> [tempr] |
		[]::rows -> col_count_ rows (reverse tempr1) [] |
		(data::datas)::rows ->
		(match tempr with (DATA(currentcount))::counts ->
			(match data with 
				DATA(x) -> col_count_ (datas::rows) counts ((DATA(currentcount+.1.))::tempr1) |
				_ -> col_count_ (datas::rows) counts ((DATA(currentcount))::tempr1)) |
		_ -> raise InvalidInput)
let col_count (s:sheet) (r:range) (i:index): sheet =
	if (check_index s i) && (check_range_index_col s r i) then
		match r with ((r1,c1),(r2,c2)) ->
		assign_range s (col_count_ (extract_range s r) (create (int_of_float(c2-.c1) + 1) (DATA(0.0)) []) []) i
	else
		assign s UNDEFINED i

(* All functions are O(m*n) when m*n is the dimentions of the sheet s in that function *)
let rec sum (s:sheet) (c:sheet_data): sheet_data  =
	match c with DATA(currentsum) -> 
	(match s with
		[] -> c |
		[]::rows -> sum rows c |
		(data::datas)::rows ->
			(match data with 
				DATA(x) -> sum (datas::rows) (DATA(currentsum+.x)) |
				_ -> sum (datas::rows) (DATA(currentsum)))) |
	_ -> raise InvalidInput
let full_sum (s:sheet) (r:range) (i:index): sheet =
	if (check_index s i) && (check_range s r) then
		assign s (sum (extract_range s r) (DATA(0.0))) i
	else
		assign s UNDEFINED i
let rec row_sum_ (s:sheet) (temps:sheet): sheet =
	match s with 
		[] -> reverse temps |
		row::rows -> row_sum_ rows ([(sum [row] (DATA(0.0)))]::temps) 
let row_sum (s:sheet) (r:range) (i:index): sheet =
	if (check_index s i) && (check_range_index_row s r i) then
		assign_range s (row_sum_ (extract_range s r) []) i
	else
		assign s UNDEFINED i
let rec col_sum_ (s:sheet) (tempr:sheet_data list) (tempr1:sheet_data list): sheet =
	match s with 
		[] -> [tempr] |
		[]::rows -> col_sum_ rows (reverse tempr1) [] |
		(data::datas)::rows ->
		(match tempr with (DATA(currentsum))::sums ->
			(match data with 
				DATA(x) -> col_sum_ (datas::rows) sums ((DATA(currentsum+.x))::tempr1) |
				_ -> col_sum_ (datas::rows) sums ((DATA(currentsum))::tempr1)) |
		_ -> raise InvalidInput)
let col_sum (s:sheet) (r:range) (i:index): sheet =
	if (check_index s i) && (check_range_index_col s r i) then
		match r with ((r1,c1),(r2,c2)) ->
		assign_range s (col_sum_ (extract_range s r) (create (int_of_float(c2-.c1) + 1) (DATA(0.0)) []) []) i
	else
			assign s UNDEFINED i

(* All functions are O(m*n) when m*n is the dimentions of the sheet s in that function *)
let rec perform_avgop_range (s:sheet) (s1:sheet) (tempr:sheet_data list) (temps:sheet): sheet =
	match s with 
	[] -> reverse temps |
	[]::rows -> 
		(match s1 with 
			[]::rows1 -> perform_avgop_range rows rows1 [] ((reverse tempr)::temps) |
			 _ -> raise InvalidInput) |
	(data::datas)::rows -> 
	(match s1 with (data1::datas1)::rows1 ->
		(match (data,data1) with (DATA(x),DATA(f)) ->
			(if (f=0.) then perform_avgop_range (datas::rows) (datas1::rows1) (UNDEFINED::tempr) temps
			else perform_avgop_range (datas::rows) (datas1::rows1) (DATA(x/.f)::tempr) temps) |
		_ -> perform_avgop_range (datas::rows) (datas1::rows1) (data::tempr) temps) |
	_ -> raise InvalidInput)
let full_avg (s:sheet) (r:range) (i:index): sheet =
	if (check_index s i) && (check_range s r) then
		match ((sum (extract_range s r) (DATA(0.0))),count (extract_range s r) (DATA(0.0))) with 
		(DATA(num),DATA(denom)) ->
			if (denom<>0.0) then assign s (DATA((num/.denom))) i
			else assign s UNDEFINED i |
		_ -> raise InvalidInput
	else
		assign s UNDEFINED i
let row_avg (s:sheet) (r:range) (i:index): sheet =
	if (check_index s i) && (check_range_index_row s r i) then
		let (subsheet:sheet) = (extract_range s r) in
		let (sheet_sum:sheet) = (row_sum_ subsheet []) in
		let (sheet_count:sheet) = (row_count_ subsheet []) in
		let (avgsheet:sheet) = perform_avgop_range sheet_sum sheet_count [] [] in
		assign_range s avgsheet i
	else
		assign s UNDEFINED i	
let col_avg (s:sheet) (r:range) (i:index): sheet =
	if (check_index s i) && (check_range_index_col s r i) then
		match r with ((r1,c1),(r2,c2)) ->
		let (subsheet:sheet) = (extract_range s r) in
		let (sheet_sum:sheet) = (col_sum_ subsheet (create (int_of_float(c2-.c1) + 1) (DATA(0.0)) []) []) in
		let (sheet_count:sheet) = (col_count_ subsheet (create (int_of_float(c2-.c1) + 1) (DATA(0.0)) []) []) in
		let (avgsheet:sheet) = perform_avgop_range sheet_sum sheet_count [] [] in
		assign_range s avgsheet i
	else
		assign s UNDEFINED i	

(* All functions are O(m*n) when m*n is the dimentions of the sheet s in that function *)
let rec minm (s:sheet) (c:sheet_data): sheet_data  =
	match c with DATA(currentmin) -> 
	(match s with
		[] -> c |
		[]::rows -> minm rows c |
		(data::datas)::rows ->
			(match data with 
				DATA(x) -> minm (datas::rows) (DATA(min currentmin x)) |
				_ -> minm (datas::rows) (DATA(currentmin)))) |
	_ -> raise InvalidInput
let full_min (s:sheet) (r:range) (i:index): sheet =
	if (check_index s i) && (check_range s r) then
		assign s (minm (extract_range s r) (DATA(infinity))) i
	else
		assign s UNDEFINED i
let rec row_min_ (s:sheet) (temps:sheet): sheet =
	match s with 
		[] -> reverse temps |
		row::rows -> row_min_ rows ([(minm [row] (DATA(infinity)))]::temps) 
let row_min (s:sheet) (r:range) (i:index): sheet =
	if (check_index s i) && (check_range_index_row s r i) then
		assign_range s (row_min_ (extract_range s r) []) i
	else
		assign s UNDEFINED i
let rec col_min_ (s:sheet) (tempr:sheet_data list) (tempr1:sheet_data list): sheet =
	match s with 
		[] -> [tempr] |
		[]::rows -> col_min_ rows (reverse tempr1) [] |
		(data::datas)::rows ->
		(match tempr with (DATA(currentmin))::mins ->
			(match data with 
				DATA(x) -> col_min_ (datas::rows) mins ((DATA(min currentmin x))::tempr1) |
				_ -> col_min_ (datas::rows) mins ((DATA(currentmin))::tempr1)) |
		_ -> raise InvalidInput)
let col_min (s:sheet) (r:range) (i:index): sheet =
	if (check_index s i) && (check_range_index_col s r i) then
		match r with ((r1,c1),(r2,c2)) ->
		assign_range s (col_min_ (extract_range s r) (create (int_of_float(c2-.c1) + 1) (DATA(infinity)) []) []) i
	else
		assign s UNDEFINED i

(* All functions are O(m*n) when m*n is the dimentions of the sheet s in that function *)
let rec maxm (s:sheet) (c:sheet_data): sheet_data  =
	match c with DATA(currentmax) -> 
	(match s with
		[] -> c |
		[]::rows -> maxm rows c |
		(data::datas)::rows ->
			(match data with 
				DATA(x) -> maxm (datas::rows) (DATA(max currentmax x)) |
				_ -> maxm (datas::rows) (DATA(currentmax)))) |
	_ -> raise InvalidInput
let full_max (s:sheet) (r:range) (i:index): sheet =
	if (check_index s i) && (check_range s r) then
		assign s (maxm (extract_range s r) (DATA(-.infinity))) i
	else
		assign s UNDEFINED i
let rec row_max_ (s:sheet) (temps:sheet): sheet =
	match s with 
		[] -> reverse temps |
		row::rows -> row_max_ rows ([(maxm [row] (DATA(-.infinity)))]::temps) 
let row_max (s:sheet) (r:range) (i:index): sheet =
	if (check_index s i) && (check_range_index_row s r i) then
		assign_range s (row_max_ (extract_range s r) []) i
	else
		assign s UNDEFINED i
let rec col_max_ (s:sheet) (tempr:sheet_data list) (tempr1:sheet_data list): sheet =
	match s with 
		[] -> [tempr] |
		[]::rows -> col_max_ rows (reverse tempr1) [] |
		(data::datas)::rows ->
		(match tempr with (DATA(currentmax))::maxs ->
			(match data with 
				DATA(x) -> col_max_ (datas::rows) maxs ((DATA(max currentmax x))::tempr1) |
				_ -> col_max_ (datas::rows) maxs ((DATA(currentmax))::tempr1)) |
		_ -> raise InvalidInput)
let col_max (s:sheet) (r:range) (i:index): sheet =
	if (check_index s i) && (check_range_index_col s r i) then
		match r with ((r1,c1),(r2,c2)) ->
		assign_range s (col_max_ (extract_range s r) (create (int_of_float(c2-.c1) + 1) (DATA(-.infinity)) []) []) i
	else
		assign s UNDEFINED i

(* All functions are O(m*n) when m*n is the dimentions of the sheet s in that function *)
let rec perform_binop_const (s:sheet) (f:float) (binop:string) (tempr:sheet_data list) (temps:sheet): sheet =
	match s with 
	[] -> reverse temps |
	[]::rows -> perform_binop_const rows f binop [] ((reverse tempr)::temps) |
	(data::datas)::rows ->
		match data with
		DATA(x) ->
			(if (binop = "add") then
				perform_binop_const (datas::rows) f binop (DATA(x+.f)::tempr) temps		
			else if (binop = "subt") then
				perform_binop_const (datas::rows) f binop (DATA(x-.f)::tempr) temps
			else if (binop = "mult") then
				perform_binop_const (datas::rows) f binop (DATA(x*.f)::tempr) temps
			else if (binop = "div") then
				if (f=0.) then perform_binop_const (datas::rows) f binop (DIV_BY_ZERO::tempr) temps
				else perform_binop_const (datas::rows) f binop (DATA(x/.f)::tempr) temps
			else raise InvalidInput) |
		_ -> perform_binop_const (datas::rows) f binop (UNDEFINED::tempr) temps

let add_const (s:sheet) (r:range) (f:float) (i:index): sheet =
	if (check_index s i) && (check_range_index s r i) then
		assign_range s (perform_binop_const (extract_range s r) f "add" [] []) i
	else
		assign s UNDEFINED i
let subt_const (s:sheet) (r:range) (f:float) (i:index): sheet =
	if (check_index s i) && (check_range_index s r i) then
		assign_range s (perform_binop_const (extract_range s r) f "subt" [] []) i
	else
		assign s UNDEFINED i
let mult_const (s:sheet) (r:range) (f:float) (i:index): sheet =
	if (check_index s i) && (check_range_index s r i) then
		assign_range s (perform_binop_const (extract_range s r) f "mult" [] []) i
	else
		assign s UNDEFINED i
let div_const (s:sheet) (r:range) (f:float) (i:index): sheet =
	if (check_index s i) && (check_range_index s r i) then
		assign_range s (perform_binop_const (extract_range s r) f "div" [] []) i
	else
		assign s UNDEFINED i

(* All functions are O(m*n) when m*n is the dimentions of the sheet s in that function *)
let rec perform_binop_range (s:sheet) (s1:sheet) (binop:string) (tempr:sheet_data list) (temps:sheet): sheet =
	match s with 
	[] -> reverse temps |
	[]::rows -> 
		(match s1 with 
			[]::rows1 -> perform_binop_range rows rows1 binop [] ((reverse tempr)::temps) |
			 _ -> raise InvalidInput) |
	(data::datas)::rows -> 
	(match s1 with (data1::datas1)::rows1 ->
		(match (data,data1) with (DATA(x),DATA(f)) ->
			(if (binop = "add") then
				perform_binop_range (datas::rows) (datas1::rows1) binop (DATA(x+.f)::tempr) temps		
			else if (binop = "subt") then
				perform_binop_range (datas::rows) (datas1::rows1) binop (DATA(x-.f)::tempr) temps
			else if (binop = "mult") then
				perform_binop_range (datas::rows) (datas1::rows1) binop (DATA(x*.f)::tempr) temps
			else if (binop = "div") then
				if (f=0.) then perform_binop_range (datas::rows) (datas1::rows1) binop (DIV_BY_ZERO::tempr) temps
				else perform_binop_range (datas::rows) (datas1::rows1) binop (DATA(x/.f)::tempr) temps
			else raise InvalidInput) |
		_ -> perform_binop_range (datas::rows) (datas1::rows1) binop (UNDEFINED::tempr) temps) |
	_ -> raise InvalidInput)

let add_range (s:sheet) (r:range) (r1:range) (i:index): sheet =
	if (check_index s i) && (check_range_index s r i) && (range_compatibility r r1) then
		assign_range s (perform_binop_range (extract_range s r) (extract_range s r1) "add" [] []) i
	else
		assign s UNDEFINED i
let subt_range (s:sheet) (r:range) (r1:range) (i:index): sheet =
	if (check_index s i) && (check_range_index s r i) && (range_compatibility r r1) then
		assign_range s (perform_binop_range (extract_range s r) (extract_range s r1) "subt" [] []) i
	else
		assign s UNDEFINED i
let mult_range (s:sheet) (r:range) (r1:range) (i:index): sheet =
	if (check_index s i) && (check_range_index s r i) && (range_compatibility r r1) then
		assign_range s (perform_binop_range (extract_range s r) (extract_range s r1) "mult" [] []) i
	else
		assign s UNDEFINED i
let div_range (s:sheet) (r:range) (r1:range) (i:index): sheet =
	if (check_index s i) && (check_range_index s r i) && (range_compatibility r r1) then
		assign_range s (perform_binop_range (extract_range s r) (extract_range s r1) "div" [] []) i
	else
		assign s UNDEFINED i
