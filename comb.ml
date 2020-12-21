(*Practica competitiva comb.*)
(*
Para este problema he usado el triangulo de pascal con programcion
dinamica ya que al guardar todas las soluciones previas a la requerida
tan solo hace falta sumar la solucion de comb(m-1, n-1) mas comb(m-1, n),
ademas de esta forma evitamos sobrepasar el rango positivo de int salvo
cuando la solucion esperada supere el valor de max_int obviamente.
*)
let comb (m, n) =
	let s = m-n (*'Constante' para saber a partir de que fila debemos    *)
	in          (*parar de añadir 1 al principio de las filas inferiores.*)
	let rec next_fil lo lfil = match lo with
		[] -> lfil (*Funcion para crear una lista que contenga las soluciones*)
		|          (*correspondientes a la fila inferior de la recivida.     *)
		h1::[] -> lfil
		|
		h1::h2::t -> let new_num = (h1+h2) (*La solucion de la fila inferior *)
			in if(new_num > 0) (*sera igual al de la superior de misma columna*)
				then new_num::(next_fil (h2::t) lfil) (*mas la de la izquierda.*)
				else raise(Failure "Superado el rango positivo de int")
	in (*Si alguna solucion es negativa significa que la requerida supera max_int.*)
	let rec bucle lfil k =
		let last_cell = if(k <= n) then [1] else [] (*Si el numero de soluciones*)
		in (*por fila supera al requerido, dejamos de añadir 1 al final de esta.*)
		let lfil = if(k <= s)
			then 1::(next_fil lfil last_cell) (*Al dejar de añadir 1 al principio *)
			else next_fil lfil last_cell (*y al final, se reducen las soluciones  *)
		in
		if(k < m) then bucle lfil (k+1) (*por cada fila, hasta conseguir la      *)
		else match lfil with            (*deseada cuando se llegue a la 'm' fila.*)
			[] -> 1   (*Se extrae la unica solucion de la lista.              *)
			|
			h::_ -> h (*Si esta vacia solo habra una (en el caso de comb(0,0))*)
	in if(m >= n)
		then bucle [] 1 (*Se comienza en la primera fila, generando la inferior*)
		else 0;;        (*por cada iteracion de la recursion.                  *)