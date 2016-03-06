method InsertionSort(a:array<int>)
  requires a != null;
  ensures forall k:: forall l:: 0 <= k <= l < a.Length ==> a[k] <= a[l];
  modifies a;
{
  var i:int := 1;
  while(i < a.Length)
    invariant i <= a.Length <==> a.Length > 0;
    invariant a.Length > 0 ==> forall k:: forall l:: 0 <= k <= l < i ==> a[k] <= a[l];
  {
    assume a.Length > 0;
    assume i <= a.Length - 1;
    var t:int := a[i];
    var j:int := i - 1;
    while (j >= 0)
      invariant j == -1 ==> (forall k:: 0 <= k <= i ==> t <= a[k])
      invariant forall k:: forall l:: 0 <= k <= l < i ==> a[k] <= a[l];
      invariant j < i-1 ==> forall k::forall l:: 0 <= k <= l <= i ==> a[k] <= a[l];
      invariant j >= 0 && a[j] <= t ==> (forall k:: forall l:: j+1 <= k <= l < i ==> t <= a[k] <= a[l]);
    {
      if (a[j] <= t)
      {
        break;
      }
      a[j + 1] := a[j];
      j := j - 1;
    }
    a[j + 1] := t;
    i := i + 1;
  }
}
