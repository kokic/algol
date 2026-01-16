///|
pub enum Message {
  Empty
  Text(String)
  Label(String, String)
  Group(@list.List[Message])
  Child(Message, Message)
} derive(Show)

///|
pub impl Default for Message with default() -> Message {
  Empty
}

///|
pub impl Add for Message with add(self : Message, other : Message) -> Message {
  match (self, other) {
    (_, Empty) => self
    (Empty, _) => other
    (Text(s), Text(t)) => Text(s + t)
    (Group(xs), Group(ys)) => Group(xs + ys)
    (x, Group(ys)) => Group(ys.prepend(x))
    (Group(xs), y) => Group(xs.concat(@list.List::from_array([y])))
    (x, y) => Group(@list.List::from_array([x, y]))
  }
}

///|
pub fn Message::display(self : Message, step : Int) -> String {
  fn go(msg : Message, n : Int) -> String {
    let indent = " ".repeat(n)
    match msg {
      Empty => ""
      Text(text) => indent + text
      Label(name, value) => indent + "[\{name}]: \{value}"
      Group(list) => join_list(list.map(m => go(m, n)), separator="\n")
      Child(parent, child) => {
        let parent = go(parent, n)
        let child = go(child, n + step)
        "\{parent}\n\{child}"
      }
    }
  }

  go(self, 0)
}

///|
test "Tree" {
  assert_eq(
    Child(Text("Root"), Child(Text("Parent"), Text("Child"))).display(2),
    (
      #|Root
      #|  Parent
      #|    Child
    ),
  )
  assert_eq(
    Child(
      Text("R"),
      group([Text("A"), Text("B"), Child(Text("C"), Text("C.1"))]),
    ).display(2),
    (
      #|R
      #|  A
      #|  B
      #|  C
      #|    C.1
    ),
  )
}

///|
pub fn group(data : Array[Message]) -> Message {
  Group(@list.List::from_array(data))
}

///|
pub let tracing : @tracing.Tracing[Message] = @tracing.Tracing::new()

///|
pub fn[V] Message::exit(self : Message, name : String, value : V) -> V {
  tracing.add(Child(Text(": \{name}"), self))
  value
}
