///|
pub fn[E : Show] join_list(
  list : @list.List[E],
  separator? : String = ", ",
) -> String {
  match list {
    Empty => ""
    More(x, tail~) =>
      tail.fold(init=x.to_string(), (s, t) => "\{s}\{separator}\{t}")
  }
}

///|
pub fn[E : Show] join(data : Array[E], separator? : String = ", ") -> String {
  guard !data.is_empty() else { "" }
  data[1:data.length()].fold(init=data[0].to_string(), (s, t) => "\{s}\{separator}\{t}")
}
