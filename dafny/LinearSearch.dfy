method LinearSearch(a:array<int>, l:int, u:int, e:int) returns (r:bool)
  requires a != null;
	requires 0 <= l;
	requires l < a.Length;
	requires u < a.Length;

	requires a.Length == 1;

  ensures r <==> exists k:: l <= k <= u && a[k] == e;
{
  var i := l;
  r := false;
  while (i <= u)
	  invariant i <= a.Length;
	  invariant a.Length > 0 ==> (forall k:: l <= k < i ==> a[k] != e);
  {
    if (a[i] == e)
	  {
	    r := true;
	    return;
	  }
    i := i + 1;
  }
}
