method BubbleSort(a:array<int>)
  requires a != null;
  ensures forall k:: forall l:: 0 <= k < l < a.Length ==> a[k] <= a[l]; // Sorted
  modifies a;
{
  var i:int := a.Length - 1;
  while(i > 0)
    invariant -1 <= i <= a.Length - 1;
    invariant a.Length == 0 <==>  i == -1;
    invariant forall k:: forall l:: i <= k < l < a.Length ==> a[k] <= a[l];
    invariant forall k:: forall l:: 0 <= k <= i && i+1 <= l <= a.Length - 1 ==>  a[k] <= a[l]; 
  {
    var j:int := 0;
    while (j < i)
      invariant 1 <= i <= a.Length -1 && 0 <= j <= i;
      invariant forall k:: forall l:: i <= k < l < a.Length ==>  a[k] <= a[l];
      invariant forall k:: forall l:: 0 <= k <= i && i+1 <= l <= a.Length - 1 ==>  a[k] <= a[l];
      invariant forall k:: 0 <= k < j ==>  a[k] <= a[j];
    {
      if (a[j] > a[j + 1]) {
        var t:int := a[j];
        a[j] := a[j + 1];
        a[j + 1] := t;
      }
      j := j + 1;
    }
    i := i - 1;
  }
}
