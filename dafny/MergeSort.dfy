
method MergeSort(a1:array<int>) returns (a:array<int>)
  requires a1 != null && a1.Length > 0;
  ensures a != null;
  ensures forall k:: forall l:: 0 <= k < l < a.Length ==> a[k] <= a[l];
{
  a := ms(a1, 0, a1.Length-1);
  return;
}

method ms(a1:array<int>, l:int, u:int) returns (a:array<int>)
  requires a1 != null;
  decreases u - l;
	requires 0 <= l < a1.Length;
	requires 0 <= u < a1.Length;

  ensures a != null;
{
  a := new int[a1.Length];
  assume forall k:: 0 <= k < a1.Length ==> a[k] == a1[k];
  if (l >= u)
  {
    return;
  }
  else
  {
		assume u >= l;
    var m:int := (l + u) / 2;
		assume l <= m <= u;
    a := ms(a, l, m);
    a := ms(a, m+1, u);
    a := merge(a, l, m, u);
    return;
  }
}

method merge(a1:array<int>, l:int, m:int, u:int) returns (a:array<int>)
	requires a1 != null;
	requires 0 <= l <= m <= u < a1.Length;
	requires forall i:: forall j:: l <= i <= j <= m ==> a1[i] <= a1[j]; // Left sorted
	requires forall i:: forall j:: m + 1 <= i <= j <= u ==> a1[i] <= a1[j]; // Right sorted

	requires a1.Length == 2;

	ensures a != null;
	ensures a.Length == a1.Length;
	ensures forall i:: forall j:: l <= i <= j <= u ==> a[i] <= a[j];
{
  a := new int[a1.Length];
  assume forall k:: 0 <= k < a1.Length ==> a[k] == a1[k]; // Keep

  assume forall i:: forall j:: m+1 <= i <= j <= u ==> a[i] <= a[j]; // Assume right sorted
	assume forall i:: forall j:: l <= i <= j <= m ==> a[i] <= a[j]; // Assume left sorted

  var buf := new int[u-l+1];

  var i:int := l;
  var j:int := m + 1;
  var k:int := 0;

  while (k < u-l+1)
  	invariant m+1 <= j;
  	invariant 0 <= k;
	  invariant l <= i;
	  invariant i <= j;
	  invariant i <= m+1;
	  invariant (i - l) + (j - m - 1) == k;
	  invariant k <= u - l + 1;
		invariant forall ip:: forall jp:: l <= ip <= jp <= m ==> a[ip] <= a[jp];
	  invariant forall i:: forall j:: m+1 <= i <= j <= u ==> a[i] <= a[j];

		invariant k > 0 ==> forall ip:: forall jp:: forall kp:: j <= jp <= u && i <= ip <= m && 0 <= kp < k ==> buf[kp] <= a[ip] && buf[kp] <= a[jp]; 

		invariant i > m ==> forall ip:: forall jp:: (0 <= ip < k && 
		                                             j <= jp <= u) ==> buf[ip] <= a[jp];
		invariant i <= m && j > u ==> forall ip:: forall jp:: (0 <= ip < k && 
		                                                       i <= jp <= m) ==> buf[ip] <= a[jp];
	  invariant i <= m && j <= u && a[i] <= a[j] ==> forall ip:: forall jp:: (0 <= ip < k &&
		                                                                        j <= jp <= u) ==>
																																						buf[ip] <= a[jp];

		invariant forall i:: forall j:: 0 <= i <= j < k ==> buf[i] <= buf[j];
  {
    if (i > m)
    {
      buf[k] := a[j];
      j := j + 1;
    }
    else if (j > u)
    {
      buf[k] := a[i];
      i := i + 1;
    }
    else if (a[i] <= a[j])
    {
      buf[k] := a[i];
      i := i + 1;
    }
    else
    {
      buf[k] := a[j];
      j := j + 1;
    }
    k := k + 1;
  }

  k := 0;
  while (k < u-l+1)
	invariant forall i:: forall j:: 0 <= i <= j < l - u + 1 ==> buf[i] == buf[j]; // Buf is sorted
	invariant a.Length == a1.Length;
	invariant l + k <= u+1;
	invariant k != 0 ==> (forall ki:: 0 <= ki < k ==> a[l + ki] == buf[ki])
	invariant forall i:: forall j:: l <= i <= j < l+k ==> a[i] <= a[j];
  {
    a[l + k] := buf[k];
    k := k + 1;
  }
}
