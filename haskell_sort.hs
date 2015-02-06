--Joshua B. Collins 
--4/26/2013
--Heapsort


--grab from the top of the tree and then call function again to regenerate the tree for length of the list - 1. 
heapsort [] = []
heapsort [x] = [x]
heapsort x =  (heapsort (dropend heapify)) ++ (tol:[])

	where 
		dropend f = drop 1 f
		heapify = heap (length x) x 
		tol = head(heapify) 

--run through the list and switch proper nodes and roots
heap len x 
	   |len >  1 && child >  root  = heap (len-1) switch 
	   |len >  1 && child <= root =  heap (len-1) x
	   |len == 1 = x 

	where
	      switch = (fl++(child:[])++ml++(root:[]) ++bl)
	      n  =     div (len) 2
	      j  =     (len) - 1
	      fl =    (take (n-1) x)
	      bl =    drop len x 
	      ml =    reverse(drop ((length x)- j)(reverse (drop n x) )) 
	      root =  head ( (drop (n-1)  x) )
	      child = head (drop j x)
	      dropend f = reverse (drop 1 (reverse f))

