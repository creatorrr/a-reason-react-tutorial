type item = {
  id: int,
  title: string,
  completed: bool,
};

type state = {items: list(item)};

type action =
  | AddItem(string)
  | ToggleDone(int);

let len = List.length;
let negate: ('a => bool, 'a) => bool = (f, x) => !f(x);
let compose = (f, g, x) => g(f(x));

let str = ReasonReact.string;
let arr = compose(Array.of_list, React.array);
let st = ReactDOMRe.Style.make;
let valueOf = evt => (evt |> ReactEvent.Form.target)##value;
let getKeyPressed = ReactEvent.Keyboard.key;

let partition_by: ('a => bool, list('a)) => (list('a), list('a)) =
  (fn, lst) => (List.filter(fn, lst), List.filter(negate(fn), lst));

module Todo = {
  [@react.component]
  let make = (~item, ~toggle) =>
    <div className="item">
      <input
        type_="checkbox"
        className="checkbox"
        checked={item.completed}
        onChange={_ => toggle(item.id)}
      />
      <span> {str(item.title)} </span>
    </div>;
};

module Footer = {
  [@react.component]
  let make = (~items) => {
    let (done_items, remaining_items) =
      items |> partition_by(i => i.completed);

    let num_items = len(items);
    let num_done = len(done_items);
    let num_remaining = len(remaining_items);

    <div
      className="footer"
      style={st(~textAlign="center", ~display="block", ())}>
      {str(string_of_int(num_items) ++ " items | ")}
      {str(string_of_int(num_done) ++ " done | ")}
      {str(string_of_int(num_remaining) ++ " remaining")}
    </div>;
  };
};

module Input = {
  [@react.component]
  let make = (~addItem) => {
    let (text, setText) = React.useState(() => "");

    let onChange = e => {
      /* Need to get event value before since valueOf is sync,
         but setText is async */
      let changed = valueOf(e);
      setText(_ => changed);
    };

    let onKeyPress = e =>
      getKeyPressed(e) == "Enter" ?
        {
          addItem(text);
          setText(_ => "");
        } :
        ();

    <input
      type_="text"
      value=text
      onChange
      onKeyPress
      autoFocus=true
      placeholder="Add todo"
    />;
  };
};

let new_item = (~id=0, title) => {title, id, completed: false};
let toggle_done = item => {...item, completed: !item.completed};

let reducer = ({items}, action) =>
  switch (action) {
  | AddItem(title) => {items: [new_item(~id=len(items), title), ...items]}
  | ToggleDone(id) =>
    let (matched, rest) = items |> partition_by(i => i.id == id);
    let completed_items = matched |> List.map(toggle_done);

    {items: completed_items @ rest};
  };

[@react.component]
let make = () => {
  let initial = {items: []};

  let ({items}, dispatch) = React.useReducer(reducer, initial);
  let addItem = title => AddItem(title) |> dispatch;
  let toggle = id => ToggleDone(id) |> dispatch;

  <div className="app">
    <div className="title"> {str("To do")} </div>
    <div className="items">
      {
        items
        |> List.sort((a, b) => a.id - b.id)
        |> List.map(item => <Todo item toggle />)
        |> arr
      }
    </div>
    <Input addItem />
    <Footer items />
  </div>;
};
